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


###how many are ree