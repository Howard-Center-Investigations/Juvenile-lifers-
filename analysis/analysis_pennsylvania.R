###Pennsylvania juvenile analysis

###Data acquired from Maryland DOC by Camilla Velloso (camilaspvelloso@gmail.com)

###Analysis by Riin Aljas (aljasriin@gmail.com)

###GET DATA----


###GET DATA----
library(readxl)
library(tidyverse)
library(readr)
library(lubridate)
library(tidycensus)
library(gridExtra)

lifers <- read_excel("data/source/pennsylvania_091219.xlsx")

population <- get_estimates(geography = "county", "population", variables = NULL, breakdown = NULL, 
              breakdown_labels = NULL, year = 2018, state = "PA", key = "156fda6326a38745b31480cc7848c55e7f4fcf41")


##create an age column 
lifers <- lifers %>%
  mutate(age = as.period(interval(DOB, Commitment_Date, "years")))

lifers$age <- as.integer(substr(lifers$age, 1, 2))

###LOOK INTO DATA

### how many unique people?

n_distinct(lifers$dc_number)
#521

###age----

age <-  lifers %>%
  group_by(dc_number, age) %>%
  count()
age %>% group_by(age) %>% count() %>% mutate(pct = n/521*100) 

###one guy went to prison only when he was 35.

summary(lifers$age)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   17.00   18.00   18.24   19.00   35.00 

###race----

lifers %>%
  group_by(Race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)

# #race       people    pct
# <chr>       <int>  <dbl>
#   1 ALL OTHERS      2  0.410
# 2 BLACK         323 66.2  
# 3 HISPANIC       14  2.87 
# 4 WHITE         149 30.5  


lifers %>%
  group_by(Gender) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)
###gender---- 


# FEMALE     10  1.92
# 2 MALE      511 98.1 

####geography----

by_county <- lifers %>%
  group_by(`Committing County`) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100) %>%
  rename(NAME = `Committing County`)



#look how they line up with populations 


population$NAME <- gsub(" County, Pennsylvania", "", population$NAME)  #<--clean up to line with existing data
population <- population %>% 
  mutate(NAME = toupper(NAME)) %>%
  filter(variable == "POP") 


by_county <- by_county %>%
  left_join(population, by = "NAME") %>%
  select(1:3, 6)

by_county <- 
  by_county %>%
  mutate(juvenile_per_person = people/value)

###sentence time----

lifers <- lifers %>%
  mutate(sentence_year = year(Sentencing_Date), 
         commitment_year = year(Commitment_Date))
# grid.arrange(
#   (lifers %>%
#      group_by(sentence_year) %>%
#      filter(!any(is.na(sentence_year))) %>%
#      summarise(people = n()) %>%
#      ggplot(lifers, mapping = aes(sentence_year, people))+
#      geom_point()+
#      geom_line()),
#   (lifers %>% 
#      group_by(commitment_year) %>%
#      summarise(people = n()) %>% 
#      ggplot(lifers, mapping = aes(commitment_year, people))+
#      geom_point()+
#      geom_line()), 
#   ncol=2)



###how many are resentenced?----

by_status <- lifers %>% 
  group_by(Status) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100)

# Status                             people    pct
# <chr>                               <int>  <dbl>
#   1 Deceased                                3  0.576
#   2 Pending                                77 14.8  
#   3 Released                              209 40.1  
#   4 Released, then Readmitted               1  0.192
#   5 Resentenced (no longer Life-Life)     216 41.5  
#   6 Resentenced (no longer Life-Life)*      6  1.15 
#   7 Resentenced to Life-Life                9  1.73 
# #

#look at released/resentenced


lifers %>%
  filter(Status == "Released") %>%
  group_by(Race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers%>%
                             filter(Status == "Released"))*100)
 

# #race       people    pct
# <chr>       <int>  <dbl>
#   1 ALL OTHERS      2  0.410
# 2 BLACK         323 66.2  
# 3 HISPANIC       14  2.87 
# 4 WHITE         149 30.5 

# 
# Race     people   pct
# <chr>     <int> <dbl>
#   1 BLACK       160 76.6 
# 2 HISPANIC     17  8.13
# 3 WHITE        32 15.3 

##more black and hispanic people are getting out compared to white people?

whitesout <- lifers %>%
  filter(Race == "WHITE") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = round(people/nrow(lifers %>%
                         filter(Race == "WHITE")),2))%>%
  mutate(Race = "WHITE")

blacksout <- lifers %>%
  filter(Race == "BLACK") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = round(people/nrow(lifers %>%
                             filter(Race == "BLACK")),2)) %>%
  mutate(Race = "BLACK")

hispanicsout <- lifers %>%
  filter(Race == "HISPANIC") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = round(people/nrow(lifers %>%
                             filter(Race == "HISPANIC")),2)) %>%
  mutate(Race = "HISPANIC")


asiansout <- lifers %>%
  filter(Race == "ASIAN") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = round(people/nrow(lifers %>%
                                   filter(Race == "ASIAN")),2)) %>%
  mutate(Race = "ASIAN")

by_race_status <- bind_rows(blacksout, whitesout, hispanicsout, asiansout)


by_race_status <- pivot_wider(by_race_status, 
                 names_from = Race,
                 values_from = c(people,pct))
 

t <- by_race_status %>%
  select(Status, 
         nr_black_people = people_BLACK, 
         pct_BLACK, 
         nr_white_people = people_WHITE, 
         pct_WHITE, 
         nr_hispanic_people = people_HISPANIC, 
         pct_HISPANIC, 
         nr_asian_people = people_ASIAN, 
         pct_ASIAN)
  
#hispanics have been released/resentenced the most, then blacks then whites. 
#For blacks it might be also that they started to release them earlier, 
#already in 2000s, but that doesn't apply for hispanics 

###look at racial differences among counties


county <- lifers %>%
  group_by(`Committing County`) %>%
  summarise(total_people=n())

county_status <- lifers %>%
  group_by(`Committing County`, Status) %>%
  summarise(total_status = n()) %>%
  left_join(county) %>%
  mutate(pct_in_county = round((total_status/total_people)*100,2))
  
  ggplot(county_status, 
         mapping  = aes(Status, pct_in_county, color = Status))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Committing County`, ncol = 5)


released <- county_status %>%
  filter(Status == "Released")

resentenced <- county_status %>%
  filter(Status %in% 
           c("Resentenced (no longer Life-Life)", 
             "Resentenced (no longer Life-Life)*", 
             "Resentenced to Life-Life"))

pending <- county_status %>%
  filter(Status == "Pending")

#most of counties with high amount of pending people have very little amount 
#of people to begin with, exceptions are Dauphin and Delaware 

summary(released$pct_in_county)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 7.69   18.34   36.36   43.30   52.96  100.00 

#Most of countries where there are high % of released peopel are the ones
#with low numbers of juveniles to begin with, exception being Philadelphia, 
#which have a lot of released people. Delaware is the exception, having 26 total 
#juveniles but low number of released people. Allegheny has a high % of resentenced
# people.

county_race <- lifers %>%
  group_by(`Committing County`, Race) %>%
  summarise(total_people=n())

county_released_or_resentenced_race <- lifers %>%
  filter(!Status %in% c("Pending", "Resentenced to Life-Life", "Deceased"))%>%
  group_by(`Committing County`, Race) %>%
  summarise(total_status = n()) %>%
  left_join(county_race) %>%
  mutate(pct_in_county = round((total_status/total_people)*100,2))

ggplot(county_released_or_resentenced_race, 
       aes(Race, pct_in_county))+
  geom_bar(stat = "identity")+
  facet_wrap(~`Committing County`)









lifers %>%
  filter(!Status %in% c("Pending", "Resentenced to Life-Life", "Deceased")) %>%
           group_by(`Committing County`, Race) %>%
           summarise(people = n()) %>%
  
           ggplot(lifers, mapping = aes(sentence_year, people))+
           geom_point()+
           facet_wrap(~Race)



blacksoutyear <- lifers %>%
  filter(Race == "BLACK") %>%
  group_by(Status, sentence_year) %>%
  summarise(people= n()) %>%
  ggplot(lifers, mapping=aes(sentence_year, people))+
  geom_point()+
  geom_line()+
  facet_wrap(~Status)

lifers %>%
  filter(Race == "HISPANIC") %>%
  group_by(Status, sentence_year) %>%
  summarise(people= n()) %>%
  ggplot(lifers, mapping=aes(sentence_year, people))+
  geom_point()+
  geom_line()+
  facet_wrap(~Status)


lifers %>%
  filter(!Status %in% c("Pending", "Resentenced to Life-Life", "Deceased")) %>%
  group_by(sentence_year, `Committing County` ) %>%
  summarise(people = n()) %>%
  ggplot(lifers, mapping = aes(sentence_year, people))+
  geom_point()+
  facet_wrap(~`Committing County`)


##doesn't seem a lot of geographical disparity. 

###to do list----

### look into let out age 
### pull in populations 
###

county <- lifers %>%
  group_by(`Committing County`) %>%
  summarise(total_people=n())

county_race <- lifers %>%
  group_by(`Committing County`, Race) %>%
  summarise(total_people=n())

county_status <- lifers %>%
  group_by(`Committing County`, Status) %>%
  summarise(total_status = n()) %>%
  left_join(county) %>%
  mutate(pct_in_county = round((total_status/total_people)*100,2) )



