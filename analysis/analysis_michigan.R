###Michigan juvenile analysis

###Data acquired from Michigan DOC by Camilla Velloso (camilaspvelloso@gmail.com)

###Analysis by Riin Aljas (aljasriin@gmail.com)

###GET DATA----


###GET DATA----
library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(lubridate)
library(tidycensus)
library(gridExtra)
library(janitor)

lifers <- read_excel("data/source/michigan_2019.xlsx", 
                            col_types = c("numeric", "text", "text", 
                                          "text", "text", "numeric", "text", 
                                          "date", "date", "date", "date", "text", 
                                          "text", "date", "text", "text", "date", 
                                          "text", "date", "text", "text", "text", 
                                          "text"))
lifers <- clean_names(lifers)

population <- get_estimates(geography = "county", "population", variables = NULL, breakdown = NULL, 
                            breakdown_labels = NULL, year = 2018, state = "MI", key = "156fda6326a38745b31480cc7848c55e7f4fcf41")


#get 2017 (latest) prison population data from BJS
population <- read_csv("data/source/prison_population_2017.csv")

###LOOK INTO DATA

### how many unique people?

n_distinct(lifers$dc_number)
nrow(is.na(lifers$dc_number))

#368 <-- they're all in the data once 

###age----

t <-lifers %>%
  group_by(dc_number, age_at_offense) %>%
  count()

n_distinct(t$dc_number) # <-- also 368, all people have one age. 


lifers %>% 
  group_by(age_at_offense) %>%
  count() %>% 
  mutate(pct = n/nrow(lifers)*100) 

# age_at_offense     n   pct

# 1             14     4  1.09
# 2             15    46 12.5 
# 3             16   109 29.6 
# 4             17   209 56.8 

summary(lifers$age_at_offense)
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   16.00   17.00   16.42   17.00   17.00 

###race----

lifers %>%
  group_by(race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)

# race            people    pct
# <chr>            <int>  <dbl>
# 1 Asian                2  0.543
# 2 Black              258 70.1  
# 3 Hispanic            10  2.72 
# 4 Native American      1  0.272
# 5 White               96 26.1  
# 6 NA                   1  0.272

###gender---- 

lifers %>%
  group_by(gender) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)

# 1 female     10  2.72
# 2 male      358 97.3 

####geography----

nrow(is.na(lifers$county)) # no missing values 

by_county <- lifers %>%
  group_by(county) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100) %>%
  rename(NAME = county) #rename for population data joining 



###look how they line up with populations 


population$NAME <- gsub(" County, Michigan", "", population$NAME)  #<--clean up to line with existing data
population <- population %>% 
  filter(variable == "POP") 


by_county <- by_county %>%
  left_join(population, by = "NAME") %>%
  select(1:3, 6)

by_county <- 
  by_county %>%
  mutate(juvenile_per_person = people/value*100)



###time----

lifers <- lifers %>%
  mutate(resentence_year = year(resentence_date),
         offense_year = year(offense_date))
         
grid.arrange(
  (lifers %>%
     group_by(offense_year) %>%
     filter(!any(is.na(offense_year))) %>%
     summarise(people = n()) %>%
     ggplot(lifers, mapping = aes(offense_year, people))+
     geom_point()+
     geom_line()),
  (lifers %>% 
     group_by(resentence_year) %>%
     summarise(people = n()) %>% 
     ggplot(lifers, mapping = aes(resentence_year, people))+
     geom_point()+
     geom_line()), 
  ncol=2)



###Status----
lifers %>% 
  group_by(status) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)

#7% have been released, for 12% the status is pending. For rest of them there's no info

resentenced <- lifers %>%
  filter(resentenced == "TRUE")

resentenced %>% 
  group_by(status) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(resentenced)*100)

status   people    pct
<chr>     <int>  <dbl>
  <chr>     <int>  <dbl>
# 1 DECEASED      1  0.442
# 2 FALSE       154 68.1  
# 3 PENDING      45 19.9  
# 4 Released     26 11.5  

#The resentenced people who are resentenced but in the list have pending status
  
new_sentences <- resentenced %>%
  group_by(resentenced_results) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(resentenced)*100)

yes <- resentenced %>% 
  filter(resentenced_results != "25y - 40y") %>%
  filter(str_detect(resentenced_results, "^2"))
yes1 <- resentenced %>% 
  filter(str_detect(resentenced_results, "^3"))
yes3 <-resentenced %>% 
  filter(resentenced_results != "40y - 75y") %>%
  filter(str_detect(resentenced_results, "^4"))

new_sentences <- resentenced %>% 
  mutate(resentenced_results = ifelse(
    (resentenced_results %in% yes$resentenced_results),
    "20-60", 
    resentenced_results
  )) %>% 
  mutate(resentenced_results = ifelse(
      resentenced_results == ("25y - 40y"), 
       "20-40", 
       resentenced_results)) %>%
  mutate(resentenced_results = ifelse(
    (resentenced_results %in% yes1$resentenced_results),
    "30-60", 
    resentenced_results
  )) %>%
  mutate(resentenced_results = ifelse(
    (resentenced_results %in% yes3$resentenced_results),
    "40-60", 
    resentenced_results
  )) %>%
  mutate(resentenced_results = ifelse(
    resentenced_results == ("40y - 75y"), 
    "40-75", 
    resentenced_results))

new_sentences %>%
  group_by(resentenced_results) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(resentenced)*100)

# resentenced_results people    pct
# <chr>                <int>  <dbl>
# 1 20-40                    1  0.549
# 2 20-60                   65 35.7  
# 3 30-60                   70 38.5  
# 4 40-60                   29 15.9  
# 5 40-75                    1  0.549
# 6 DECEASED                 1  0.549
# 7 LIFE                    15  8.24 

## most people who got a new sentence got  20-60 or 30-60 years 
## look at it how it differs among offense code

unique(new_sentences$offense_code)

#"750.316B" "750.316A" "750.316"  "750.316C" 
# according to https://dhhs.michigan.gov/OLMWeb/ex/CrimeCodesExhibit/CrimeCodesExhibit.pdf we can create a look up table

crimes <- data.frame("offense_code" = c("750.316B", "750.316A", "750.316", "750.316C"),
                     "crime" = c("HOMICIDE - FELONY MURDER", "HOMICIDE-MURDER FIRST-DEGREE - PREMEDIDATED", 

new_sentences <- new_sentences %>% 
  left_join(crimes)

new_sentences %>%
  group_by(resentenced_results, offense_code) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(new_sentences)*100) %>%
  ggplot(new_sentences, mapping = aes(offense_code, pct))+
  geom_bar(stat = "identity")+
  facet_wrap(~resentenced_results, ncol = 2)
  
##who are the people who are resentenced for life

new_sentences %>%
  filter(resentenced_results == "LIFE") %>% View()

##almost half of them (7) are from Kent county, did Kent stand out before?
#when it comes to overall nr of juveniles per persson, then Kent is 18th, yet with 24 people it's 4th. 
#look into resentenced by county 


all_juv_in_county <- lifers %>%
  group_by(county) %>%
  summarise(total_people=n())

resentenced_in_county <- lifers %>%
  filter(resentenced == TRUE)%>%
  group_by(county) %>%
  summarise(total_people=n())

all_juv_by_race_resentenced <- 
  lifers %>%
  filter(resentenced == TRUE)%>%
  group_by(race) %>%
  summarise(total_people=n())

all_juv_by_race <- 
  lifers %>%
  group_by(race) %>%
  summarise(total_people=n())

all_juv_by_race_county <- 
  lifers %>%
  group_by(race, county) %>%
  summarise(total_people=n())


new_sentences %>%
  group_by(resentenced_results, county) %>%
  summarise(people = n()) %>%
  left_join(resentenced_in_county, by = "county")%>%
  mutate(pct_county= people/total_people*100) %>%
  ggplot(new_sentences, mapping = aes(resentenced_results, pct_county, color = resentenced_results))+
  geom_bar(stat = "identity")+
  facet_wrap(~county)

###does Kent county have a super strict judge? we don't know.

### add race to the mix

new_sentences %>%
  group_by(resentenced_results, county, race) %>%
  summarise(people = n()) %>%
  left_join(resentenced_in_county, by = "county")%>%
  mutate(pct_county= people/total_people*100) %>%
  ggplot(new_sentences, mapping = aes(race, pct_county, color = resentenced_results))+
  geom_bar(stat = "identity")+
  facet_wrap(~county)

new_sentences %>%
  filter(resentenced_results == "LIFE") %>%
  group_by(county, race) %>%
  summarise(people = n()) %>%
  left_join(resentenced_in_county, by = "county")%>%
  mutate(pct_county= people/total_people*100) %>%
  ggplot(new_sentences, mapping = aes(race, people, color = race))+
  geom_bar(stat = "identity")+
  facet_wrap(~county)

#don't see much racial disparity in resentenced for life. 

##look into possible acial and geographic disparities more wide than only in resentenced people 

race_result <- new_sentences %>%
  group_by(race, resentenced_results) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race_resentenced, by = "race")%>%
  mutate(pct_race= people/total_people*100)

###as a whole, more white people have been sentenced to life twice as often compared to black people. 
### is it because white people live more conservative counties? 
### we migth wanna pull in red or blue counties for that comparison 

#look the same overall 
lifers %>%
  group_by(race, resentenced) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race, by = "race")%>%
  mutate(pct_race= people/total_people*100)

##no racial disparity in overall resentencing, only in the type of resentenced results

### look the same by status
status_race <- lifers %>%
  group_by(race, status) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race, by = "race") %>%
  mutate(pct_race= people/total_people*100)
  
  ggplot(status_race, mapping = aes(status, pct_race, fill = status))+
  geom_bar(stat = "identity")+
  facet_wrap(~race)

### there's little difference between whites and black when it comes to years, more 20-60 in whites compared to blacks, 
###  yet more 40-60 in whites compared to blacks. 

status_county_race <- lifers %>%
  filter(race %in% c("Black", "White", "Hispanic"))%>%
  group_by(race, status, county) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race_county, by = c("county", "race")) %>%
  mutate(pct_race= people/total_people*100)
ggplot(status_county_race, 
       mapping = aes(race, pct_race, fill = status))+
  geom_bar(stat = "identity")+
  facet_wrap(~county)
  
### released are only in handful of counties

lifers %>%
  filter(status == 'Released') %>% 
  group_by(race) %>%
  summarise(people = n()) %>%
  left_join(all_juv_by_race, by = "race")%>%
  mutate(pct_race= people/total_people*100)

# race     people total_people pct_race
# <chr>     <int>        <int>    <dbl>
# 1 Black        17          258     6.59
# 2 Hispanic      1           10      10   
# 3 White         8           96     8.33

#Blacks are released the least, hispanics the most. for some reason hispanics have more pending and less false (no resentence trials), 
#blacks have the highest concentration of 20-60 resentenced year punishements. 

##hispanics have higher amount of lifers compared to blacks and whites, but also less 20/60 and 30/60 year sentences. They also 
#have highest % of Released people. 
  
  
status_race <- lifers %>%
  group_by(race, status) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race, by = c("race")) %>%
  mutate(pct_race= people/total_people*100)



status_race <- pivot_wider(status_race, 
                           names_from = race,
                           values_from = c(people, pct_race, total_people))


lifers %>%
  filter(race %in% c("Black", "White", "Hispanic")) %>%
  group_by(race, status) %>% 
  summarise(people = n()) %>%
  left_join(all_juv_by_race, by = c("race")) %>%
  mutate(pct_race= people/total_people*100) %>%
  ggplot(lifers, mapping = aes(status, pct_race, fill = status))+
  geom_bar(stat = "identity")+
  facet_wrap(~race)
  
###---by age 

lifers$current_age <- as.integer(lifers$current_age)
resentenced$current_age <- as.integer(resentenced$current_age)

summary(lifers$current_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 21.00   37.00   43.00   43.11   49.00   75.00       1 

summary(resentenced$current_age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 21      39      44      45      50      75       1 

released <- lifers %>%
  filter(status == "Released")
  
summary(released$current_age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.00   55.00   59.00   59.35   62.00   75.00

released %>% 
  group_by(current_age, race) %>%
  summarise(people = n()) %>%
  ggplot(released, mapping = aes(current_age, people))+
  geom_bar(stat = "identity")+
  facet_wrap(~race)+
  coord_flip()

#there are more old people who's been released among black people 
