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

lifers <- read_excel("data/source/michigan.xlsx")

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


lifers %>% group_by(age_at_offense) %>% count() %>% mutate(pct = n/nrow(lifers)*100) 

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
  mutate(rank_pop = rank(value), 
         rank_juv = rank(people), 
         disparity = (rank_pop-rank_juv))

###counties with the most juveniles line up with the population 

###time----

lifers <- lifers %>%
  mutate(sentence_year = year(sentence_date), 
         offense_year = year(offense_date),
         
lifers$resentence_date <- as.Date(lifers$resentence_date)     
         
grid.arrange(
  (lifers %>%
     group_by(sentence_year) %>%
     filter(!any(is.na(sentence_year))) %>%
     summarise(people = n()) %>%
     ggplot(lifers, mapping = aes(sentence_year, people))+
     geom_point()+
     geom_line()),
  (lifers %>% 
     group_by(commitment_year) %>%
     summarise(people = n()) %>% 
     ggplot(lifers, mapping = aes(commitment_year, people))+
     geom_point()+
     geom_line()), 
  ncol=2)



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
  mutate(pct = people/nrow(lifers %>%
                             filter(Race == "WHITE"))) %>%
  mutate(Race = "WHITE")

blacksout <- lifers %>%
  filter(Race == "BLACK") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = people/nrow(lifers %>%
                             filter(Race == "BLACK"))) %>%
  mutate(Race = "BLACK")

hispanicsout <- lifers %>%
  filter(Race == "HISPANIC") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = people/nrow(lifers %>%
                             filter(Race == "HISPANIC"))) %>%
  mutate(Race = "HISPANIC")

asiansout <- lifers %>%
  filter(Race == "ASIAN") %>%
  group_by(Status) %>%
  summarise(people= n()) %>%
  mutate(pct = people/nrow(lifers %>%
                             filter(Race == "ASIAN"))) %>%
  mutate(Race = "ASIAN")

by_race_status <- bind_rows(blacksout, whitesout, hispanicsout, asiansout)

#hispanics have been released/resentenced the most, then blacks then whites. 
#For blacks it might be also that they started to release them earlier, 
#already in 2000s, but that doesn't apply for hispanics 

lifers %>%
  filter(!Status %in% c("Pending", "Resentenced to Life-Life", "Deceased")) %>%
  group_by(sentence_year, Race) %>%
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

### ADD ROW ABOUT CUMULATIVE NR OF PEOPLE BY YEAR 
### look into let out age 
### examine the diff by race in releasing 

