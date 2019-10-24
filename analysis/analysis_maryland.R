###Maryland juvenile analysis

###Data acquired from Maryland DOC by Hannah Gaskill (hgaskill94@gmail.com)

###Analysis by Riin Aljas (aljasriin@gmail.com)

###GET DATA----

library(readxl)
library(tidyverse)
library(lubridate)
library(tidycensus)
library(ggplot2)
library(gridExtra)

lifers <- read_excel("data/source/maryland.xlsx")
lifers <- lifers[-c(364),]  #remove empty row
lifers <- lifers %>%
  rename(dc_number = `CL#`,
         offense_date = DATE_OF_OFFENSE) #standardise column names to reuse scripts

#get 2018 populations form census data 

population <- get_estimates(geography = "county", "population", 
                            variables = NULL, breakdown = NULL, 
                            breakdown_labels = NULL, year = 2018, state = "MD", key = "156fda6326a38745b31480cc7848c55e7f4fcf41")


##create an age column. use disposition date for the ones who didn't have offense date
## MD data people have explanation why it's ok to do it like that


lifers <- lifers %>%
  mutate(age = ifelse(lifers$offense_date %in% c(NA), 
                      interval(DOB, ADMISSION_DATE), 
                      interval(DOB, offense_date)))

#for some reason as.period(interval, $DOB, IMPOSITION_DATE(same for offense_date)
#gives me zeros, tried several ways, couldn't fix, to save time, will do it manually

lifers$age <- round(lifers$age/365/24/60/60)
nrow(is.na(lifers$age))


###LOOK INTO DATA----

### how many unique people?

n_distinct(lifers$dc_number)
#312 vs 363 observations 

##look into people who are in the data more than once 

t <- lifers %>% 
  group_by(dc_number) %>%
  count() %>%
  filter(n > 1)

t <- lifers%>%
  filter(dc_number %in% t$dc_number)

##there are 36 people who are double. 
##some of them have several offense dates and sentence start dates
## it might refer to resentence, the admission dates are only for once though 

## create a demographic dataframe with people once only, leave out
## jurisdiction because some people are in several jurisdictions 

lifers_dem <- lifers %>%
  group_by(dc_number, FIRST_NAME, MIDDLE_NAME, LAST_NAME, DOB, 
           ADMISSION_DATE, FACILITY, GENDER, Race, age) %>% 
  summarise(people = n())

#316 vs 312, 
#this means there are 4 people who have different ages, that comes down to diff 
#offense dates (they can have multiple, that's fine as we're interested in av age 

#age----

lifers_dem %>%
  group_by(age) %>%
  count() %>%
  mutate(pct = n/nrow(lifers_dem)*100) 
# age     n    pct
# <dbl> <int>  <dbl>
#  14     2   0.633
#  15    14   4.43 
#  16    66   20.9  
#  17   137   43.4  
#  18    88   27.8  
#  19     8   2.53 


summary(lifers_dem$age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   16.00   17.00   17.01   18.00   19.00

##take out age now to get unique values for other parameters 

lifers_dem <- lifers %>%
  group_by(dc_number, FIRST_NAME, MIDDLE_NAME, LAST_NAME, DOB, 
           ADMISSION_DATE, FACILITY, GENDER, Race)
n_distinct(lifers_dem$dc_number)

###race----

lifers_dem %>%
  group_by(Race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers_dem)*100)

                              
# <chr>                                <int>  <dbl>
# 1 Asian                                    2  0.551
# 2 Black                                  282  77.7  
# 3 Hispanic or Latino                      17  4.68 
# 4 Native American or Alaskan Native        2  0.551
# 5 Native Hawaiian or Pacific Islander      1  0.275
# 6 Other race                               7  1.93 
# 7 Unknown                                  1  0.275
# 8 White                                   51  14.0  

###gender----

lifers_dem %>%
  group_by(GENDER) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers_dem)*100)
# 
# 1 Female      4  1.10
# 2 Male      359 98.9 

####geography----

nrow(is.na(lifers$Jurisdiction)) <- #null

by_county <- lifers %>%
  group_by(Jurisdiction) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(lifers)*100) %>%
  rename(NAME = Jurisdiction) <- #rename for population data



#look how they line up with populations 


population$NAME <- gsub(", Maryland", "", population$NAME) 
population$NAME <- gsub("Baltimore city", "Baltimore City", population$NAME)  #<---clean up to line with existing data

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

#four counties stand out having more juveniles than they "should", but not much:
#They're Frederick, Allegany, Wahisngton and Montgomery 

#Dorchester, Wicomico, Talbot and Baltimore City, on the other hand have less than 
# they "should"


###change in years----

lifers <- lifers %>%
  mutate(admission_year = year(ADMISSION_DATE), 
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
     group_by(admission_year) %>%
     summarise(people = n()) %>% 
     #filter(admission_year > 2000)%>%
     ggplot(lifers, mapping = aes(admission_year, people))+
     geom_point()+
     geom_line()), 
  ncol=2)

##numbers are too small to draw any conclusions about 
## whether less judges sentence them for life after 
## supreme court's decision

###to do list-----

###ADD ROW ABOUT CUMULATIVE NR OF PEOPLE BY YEAR 
### LOOK INTO IMPOSITION DATE (
### will contact MD to get clarity about what's the sentence date, what's imposition date and how to understand who's with parole, 
### who's without parole and who's resentenced etc 