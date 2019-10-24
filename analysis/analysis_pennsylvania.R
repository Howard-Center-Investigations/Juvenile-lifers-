###Pennsylvania

###GET DATA----
library(readxl)
library(tidyverse)
library(readr)
library(lubridate)
library(tidycensus)

lifers <- read_excel("data/pennsylvania_091219.xlsx")

population <- get_estimates(geography = "county", "population", variables = NULL, breakdown = NULL, 
              breakdown_labels = NULL, year = 2018, state = "PA", key = "156fda6326a38745b31480cc7848c55e7f4fcf41")

?get_estimates

populations <- 
  
?
##create an age column 
lifers <- lifers %>%
  mutate(age = as.period(interval(DOB, Commitment_Date, "years")))

lifers$age <- as.integer(substr(lifers$age, 1, 2))

###LOOK INTO DATA

### how many unique people?

n_distinct(lifersa$dc_number)
#521

#AGE---

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
  mutate(pct = people/521*100)

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
# 
# FEMALE     10  1.92
# 2 MALE      511 98.1 

####geography----

by_county <- lifers %>%
  group_by(`Committing County`) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100) %>%
  rename(NAME = `Committing County`)



#look how they line up with populations 


population$NAME <- gsub(" County, Pennsylvania", "", population$NAME) <- #clean up to line with existing data
population <- population %>% 
  mutate(NAME = toupper(NAME)) %>%
  filter(variable == "POP") 


by_county <- by_county %>%
  left_join(population, by = "NAME") %>%
  select(1:3, 7) %>%

by_county <- by_county %>% select(-pop_rank)


by_county <- 
  by_county %>%
  mutate(rank_pop = rank(value), 
         rank_juv = rank(people), 
         disparity = (rank_pop-rank_juv))

# the counties which stand out the most when it comes to juv pop vs overall pop are 
# 1 CLEARFIELD	4	0.7677543	79388	8	22.5	-14.5
# 2	CRAWFORD	4	0.7677543	85063	10	22.5	-12.5
# 3	JUNIATA	2	0.3838772	24704	2	13.5	-11.5
# 4	FAYETTE	5	0.9596929	130441	15	26.0	-11.0
# 5	DAUPHIN	14	2.6871401	277097	25	35.0	-10.0
# 6	TIOGA	2	0.3838772	40763	5	13.5	-8.5
# 7	BEAVER	5	0.9596929	164742	18	26.0	-8.0
# 8	ERIE	9	1.7274472	272061	24	31.5	-7.5
# 9	LEBANON	4	0.7677543	141314	16	22.5	-6.5
# 10	VENANGO	2	0.3838772	51266	7	13.5	-6.5

#when was sentenced?

lifers <- lifers %>%
  mutate(sentence_year = year(Sentencing_Date), 
         commitment_year = year(Commitment_Date))

lifers %>% 
  group_by(commitment_year) %>%
  summarise(people = n()) %>% 
  ggplot(lifers, mapping = aes(commitment_year, people))+
  geom_point()+
  geom_line()

# #1	1994	31
# 2	1995	25
# 3	1999	22
# 4	1990	21
# 5	1997	19
# 6	1982	18
# 7	1983	17
# 8	1992	17
# 9	1993	16
# 10	2003	16
# 11	1991	15
# 12	1977	14
# 13	1987	14
# 14	1996	14
# 15	1998	14
# 16	1976	13
# 17	2009	13
# 18	1979	12
# 19	1989	12
# 20	2000	12
# 21	2002	12
# 22	2008	12
# 23	1975	11
# 24	1988	11
# 25	1981	10
# 26	2007	10
# 27	2011	10
# 28	1973	9
# 29	1978	9
# 30	2005	9
# 31	2006	9
# 32	1985	8
# 33	2004	8
# 34	2010	8
# 35	2012	8
# 36	1980	7
# 37	1984	6
# 38	1986	6
# 39	2001	5
# 40	1970	4
# 41	1971	4
# 42	1974	4
# 43	1972	3
# 44	1953	1
# 45	1969	1
# 46	2013	1


###how many are resentenced?----

by_status <- lifers %>% 
  group_by(Status) %>%
  summarise(people = n()) %>%
  mutate(pct = people/521*100)

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


##doesn't seem a lo of geographical disparity. 

