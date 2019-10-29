###Michigan juvenile analysis

###Data acquired from Michigan DOC by Camilla Velloso (camilaspvelloso@gmail.com)

###Analysis by Riin Aljas (aljasriin@gmail.com)

###GET DATA----


###GET DATA----
library(readxl)
library(tidyverse)
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
                                 "HOMICIDE", "HOMICIDE-OPEN MURDER - STATUTORY SHORT FORM"))

new_sentences <- new_sentences %>% 
  left_join(crimes)

new_sentences %>%
  group_by(resentenced_results, offense_code) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(resentenced)*100) %>% 
  
