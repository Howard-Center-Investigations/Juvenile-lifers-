s### people we're missing (google): 711333

#get 2017 (latest) prison population data from BJS
population <- read_csv("data/source/prison_population_2017.csv")



### Florida juvenile lifers analysis 
library(tidyverse)
library(lubridate)

### LOAD DATA----

## rawdata gives us all data, lifers gives us juvenile lifers 
source("analysis/etl_florida.R")


###LOOK INTO DATA

### how many unique people?

n_distinct(lifers$dc_number)
#488

### age----

t <- lifers %>%
  group_by(dc_number, age) %>%
  count() 

### why is there 495 people against 488 original when grouped by date and dob?

t <- t %>% group_by(dc_number) %>%
  count() %>%
  filter(n>1)
#there are seven people who are in the data which two separate unique ages, why?

lifers %>% filter(dc_number %in% t$dc_number) 

#although they have different ages (several offense date), they have been sentenced for them on the sentence date and with life ii

#average ages

age <-  lifers %>%
  group_by(dc_number, age, dob) %>%
  count()
age %>% group_by(age) %>% count() %>% mutate(pct = n/495*100)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   16.00   16.00   16.23   17.00   17.00 

# age     n
# 
# 13     4
# 14    17
# 15    68
# 16   161
# 17   245

###race----

##Create unique dc_nr df for demographics stats, we leave out age, because some people have several ages due to crimes over time 
  
by_dc <- lifers %>%
  group_by(dc_number, last_name, first_name, middle_name, dob, gender, race) 

by_dc %>%
  group_by(race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(by_dc)*100)



total_population <- prison_population_2017 <- read_delim("data/source/prison_population_2017.csv", 
                                                         ";", escape_double = FALSE, col_types = cols(`American Indian/Alaska Native` = col_integer(), 
                                                                                                      Asian = col_integer(), Black = col_integer(), 
                                                                                                      `Did not report` = col_integer(), 
                                                                                                      Hispanic = col_integer(), `Native Hawaiian/Other Pacific Islander` = col_integer(), 
                                                                                                      Other = col_integer(), Total = col_integer(), 
                                                                                                      `Two or more races` = col_integer(), 
                                                                                                      Unknown = col_integer(), White = col_integer()), 
                                                         trim_ws = TRUE)
pop_fl <- total_population %>%
  filter(State == "Florida/d") %>%
  as.data.frame(toupper())

race <- 
  by_dc %>%
  group_by(race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(by_dc)*100) %>%
  as.data.frame(tolo())
race$race <- gsub("HISPANIC", "Hispanic", race$race)
race$race <- gsub("WHITE", "White",
                  race$race)
race$race <-  gsub("BLACK", "Black",
                   race$race)
race$race <-  gsub("ALL OTHERS", "Other",
                   race$race)

pop_fl<- pivot_longer(pop_fl, cols = c(2:12), names_to = "race", values_to = "total_people") 
left_join(race, pop_fl, by = "race") %>%
  select(-State) %>% 
  mutate(pct_own_race_total = round((people/total_people*100),2),
         pct_of_total_pop = round((total_people/sum(total_people)*100),2)) %>%
  mutate(disparity = (pct - pct_of_total_pop)) %>% View()


# race people       pct total_people pct_own_race_total pct_of_total_pop  disparity
# 1    Other      3  0.365408          239               1.26             0.24   0.125408
# 2    Black    561 68.331303        46493               1.21            47.26  21.071303
# 3 Hispanic     21  2.557856        12207               0.17            12.41  -9.852144
# 4    White    236 28.745432        39443               0.60            40.09 -11.344568


#there's no na-s, 
###gender----
# by_dc %>%
#   group_by(gender) %>%
#   summarise(people = n()) %>%
#   mutate(pct = people/nrow(by_dc)*100)
# 
# FEMALE     12  2.46
# 2 MALE      476 97.5 

####geography----

by_dc_county <- lifers %>%
  group_by(dc_number, last_name, first_name, middle_name, dob, gender, race, county) %>%
  count()

by_dc_county %>%
  group_by(county) %>%
  count() %>%
  View()
## the counties go mostly by population, only one which stands out a little bit is escambia but not for much 

####sentence date----

 lifers %>%
  group_by(dc_number, floor_date(sentence_date, "month")) %>%
  summarise(sentencedpeople = n()) %>%
  ggplot(lifers, mapping = aes(floor_date(sentence_date, "month"), sentencedpeople)) +
  geom_point()

lifers$sentence_date <- ymd(lifers$sentence_date)


#########
### Florida juvenile lifers analysis 
library(tidyverse)
library(arsenal)
library(lubridate)

### LOAD DATA----

## rawdata gives us all data, lifers gives us juvenile lifers 
source("analysis/etl_florida.R")


###LOOK INTO DATA

### how many unique people?

n_distinct(lifers$dc_number)
#488

### age----

t <- lifers %>%
  group_by(dc_number, age) %>%
  count() 

### why is there 495 people against 488 original when grouped by date and dob?

t <- t %>% group_by(dc_number) %>%
  count() %>%
  filter(n>1)
#there are seven people who are in the data which two separate unique ages, why?

lifers %>% filter(dc_number %in% t$dc_number) 

#although they have different ages (several offense date), they have been sentenced for them on the sentence date and with life ii

#average ages

age <-  lifers %>%
  group_by(dc_number, age, dob) %>%
  count()
age %>% group_by(age) %>% count() %>% mutate(pct = n/495*100)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 13.00   16.00   16.00   16.23   17.00   17.00 

# age     n
# 
# 13     4
# 14    17
# 15    68
# 16   161
# 17   245

###race----

##Create unique dc_nr df for demographics stats, we leave out age, because some people have several ages due to crimes over time 
  
by_dc <- lifers %>%
  group_by(dc_number, last_name, first_name, middle_name, dob, gender, race) 

by_dc %>%
  group_by(race) %>%
  summarise(people = n()) %>%
  mutate(pct = people/nrow(by_dc)*100)

# #race       people    pct
# <chr>       <int>  <dbl>
#   1 ALL OTHERS      2  0.410
# 2 BLACK         323 66.2  
# 3 HISPANIC       14  2.87 
# 4 WHITE         149 30.5  
# > 

####RELEASED----

#how many juveniles are released? use joined tables from inmate release data 

#first take out people with no info for ages for later

dobmissing <- release %>%
  filter(is.na(OffenseDate))
n_distinct(dobmissing$DCNumber)

juv <- release %>%
  filter(age < 18) <- #this gave us all juveniles 

n_distinct(juv$DCNumber)
15635
  
#try to distinct lifers there 
  
juv %>% group_by(prisonterm) %>% count() %>% View()

# we can do it by prisonterm, there's no na-s

juv <- juv %>%
  filter(prisonterm == 9999998)

n_distinct(juv$DCNumber)
112

deceased <- juv %>%
  filter(releasedateflag_descr == "deceased")

n_distinct(deceased$DCNumber)
###48 deceased

notdeceased <- juv %>%
  filter(releasedateflag_descr == "valid release date")

n_distinct(notdeceased$DCNumber)
##64 people 


releasedateflag_descr     n
<fct>                 <int>
  1 deceased                 68
  2 valid release date       94
  
#it's noteworthy that 

#the question remains though, what about people who were sentenced to life but then resentenced are 
#they in data with life or resentece

#what about the people who have additional terms additionally to 9999998 (life)
t <- release %>%
  filter(DCNumber %in% c(juv$DCNumber), prisonterm != 9999998, CaseNumber %in% c(juv$CaseNumber), 
         adjudicationcharge_descr %in% c(juv$adjudicationcharge_descr))
#most of people have charges for other crimes and lifers are released with no resentencing 

#can we miss people who are left in release data though?
x <- release %>%
  filter(age < 18)

x <- x%>%
  anti_join(juv, by = "DCNumber")

#look anything which might refer to lifers codes, turns out paroleterm and prisonterm have 999998 codes in them 

possibresent <- x %>% 
  filter(ParoleTerm == 9999998|
           ProbationTerm == 9999998) 

n_distinct(possibresent$DCNumber)  #24 people
  
#include only people who's "offence age" is still under 18, as they might just serve life for later crimes

y <- 
  possibresent %>%
  mutate(secondage = as.period(interval(possibresent$BirthDate, possibresent$OffenseDate), "years"))

possibresent$secondage <- substr(possibresent$secondage, 1, 2)

z <- release %>% 
  filter(DCNumber %in% possibresent$DCNumber, 
         prisonterm == 9999998)

possibresent <- possibresent %>%
  anti_join(z, by = "DCNumber")
n_distinct(possibresent$DCNumber)

#043260 - served life, now out 
#111071 - served life, now out
#462728 - no information 
#525062 - no information 
#860718 - no information
#E10085 - probation for life, not life sentence, 12 year old 
#E10086 - probation for life, not life sentence
#E14102 - probation for life, not life sentence - release date post 2010
#H19279 - no information- release date post 2010
#M07064 - no information
#Q09528 - no information 
#V40652	- probation for life
#X00096	- probation for life
#X75092 - no information 

<<<<<<< HEAD
# based on google
# resentenced to life 
# Jermaine Jones 
# Mazer Jean 
# P10751	BELL	RONALD
# P10752	MAESTAS	
# J11775	PHILLIPS	JOSHUA

post2010 <- lifers

post2010$sentence_date <- ymd(post2010$sentence_date)
post2010 <- post2010 %>% mutate(sentence_year = year(sentence_date))

post2010 <- post2010 %>% filter(sentence_year > 2010)

n_distinct(post2010$dc_number)


n_distinct(post2010$dc_number)

#436 people 

#take out jonathan felix, mistake in data

lifers <- lifers %>% filter(dc_number != "H36781")

#there's 6 people but their crimes were commmited not at the age of 18 

#023801	CREAMER	DENNIS - google says resentenced 
#	039295 BISSONETTE	ROY	-  google says resentenced


#043260 Billy Lynch - no information 
#056056 Ronnie Adams- no information



grid.arrange(lifers %>%
               group_by(dc_number, race, offense_year) %>%
               filter(!any(is.na(offense_year))) %>%
               summarise(people = n()) %>% 
               filter(str_detect(race, "WHITE|BLACK|HISPANIC")) %>%
               ggplot(lifers, mapping = aes(offense_year, people, color = race))+
               # geom_point()+
               # geom_line()+
               geom_smooth()+
               theme(legend.position="bottom"),
             (lifers %>%
                group_by(dc_number, race, sentence_year) %>%
                filter(!any(is.na(sentence_year))) %>%
                summarise(people = n()) %>% 
                filter(str_detect(race, "WHITE|BLACK|HISPANIC")) %>%
                #filter(admission_year > 2000)%>%
                ggplot(lifers, mapping = aes(sentence_year, people, color = race))+
                # geom_point()+
                # geom_line()+
                geom_smooth()+
                theme(legend.position="bottom")),
             ncol=2)




county <- lifers %>%
  group_by(dc_number, county, dob) %>%
  summarise(people=n())%>%
  select(-people) %>%
  group_by(county)%>%
  summarise(people=n())

population$NAME <- gsub(" County, Florida", "", population$NAME) %>% toupper()



population <- population %>% 
  filter(variable == "POP") 


by_county <- county %>%
  left_join(population, by = "NAME") %>%
  select(1:3, 6) 

by_county <- 
  by_county %>%
  mutate(juvenile_per_person = (people/value)*100000)

datatable(by_county)


=======

#there's 6 people but their crimes were commmited not at the age of 18 

#023801	CREAMER	DENNIS - google says resentenced 
#	039295 BISSONETTE	ROY	-  google says resentenced


#043260 Billy Lynch - no information 
#056056 Ronnie Adams- no information








>>>>>>> 251c698f4116e8cd629bab44883743ea43092ec8
#ONLY DAVID CREAMER 




  #there's no na-s, 
###gender----
# by_dc %>%
#   group_by(gender) %>%
#   summarise(people = n()) %>%
#   mutate(pct = people/nrow(by_dc)*100)
# 
# FEMALE     12  2.46
# 2 MALE      476 97.5 

####geography----

by_dc_county <- lifers %>%
  group_by(dc_number, last_name, first_name, middle_name, dob, gender, race, county) %>%
  count()

release %>% filter("FirstName"== "EUGENE") %>% View()
by_dc_county %>%
  group_by(county) %>%
  count() %>%
  View()
## the counties go mostly by population, only one which stands out a little bit is escambia but not for much 

## look into release data who have been released 

out <- release %>% 
  inner_join(lifers, by = "dc_number")

#Only one person, Herbert Johnson, but we knew this already 

#what about everyone
out2 <- inmate_all %>% 
  inner_join(rawdata, by = "dc_number")

out2 %>%
  group_by(sentence_length) %>%
  count() %>% View()

#join with extra data (offense, parole etc)

lifers_extended <- lifers %>% 
  left_join(active_offense, by = "dc_number") 

lifers_extended %>%
  group_by(ParoleTerm) %>%
  count() %>% View()
  





#######################


library(stringr)
library(lubridate)
t <- inmate_all


t$BirthDate <- gsub(" 0:00:00", "", t$BirthDate)
t$BirthDate <- mdy(t$BirthDate)

t$OffenseDate <- gsub(" 0:00:00", "", t$OffenseDate)
t$OffenseDate <- mdy(t$OffenseDate)

t$PrisonReleaseDate <- gsub(" 0:00:00", "", t$PrisonReleaseDate)
t$PrisonReleaseDate <- mdy(t$PrisonReleaseDate)

t$DateAdjudicated <- gsub(" 0:00:00", "", t$DateAdjudicated)
t$DateAdjudicated <- mdy(t$DateAdjudicated)

t <- t %>%
  mutate(age = as.period(interval(t$BirthDate, t$OffenseDate), "years")) 

t$age <- substr(t$age, 1, 2)

juv <- t %>%
  filter(age < 18) 


lifers_more <- lifers %>%
  left_join(active_offense, by = "dc_number")



pterms <- lifers_more %>%
  group_by(ParoleTerm) %>%
  count() 

pris_terms <- lifers_more %>%
  group_by(prisonterm) %>%
  count() 

# 
# look at the release data who had 99998 in prison term 

inmate_all %>% 
  filter(prisonterm == "9999998") %>%
  View()

juv %>% 
  filter(prisonterm == "9999998") %>%
  View()

lifers_more %>% filter(prisonterm == "0050000") %>% View()
 View(pris_terms)

u <- lifers_more %>% 
  group_by(sentence_length, prisonterm, ParoleTerm, dc_number) %>%
  summarise(people = n()) %>% View()

l <- u %>% 
  group_by(dc_number) %>%
  count() %>%
  filter(n > 1)
#337 people have several prisonterms 

u %>% 
  filter(dc_number %in% c(l$dc_number)) %>%
  View()

#prison_terms are coded with years #0050000 refers to 5 years, 0 months, 0 days 
#9999999 is death sentence
#let's check to be sure 

lifers_more %>%
  filter(prisonterm == "9999999") %>%View()


n_distinct(lifers$dc_number)
#488
n_distinct(lifers_more$dc_number)
#488
##if prisonterm is 9999999 or 9999908 they also have  9999998 (checked)
r <- lifers_more %>%
  filter(prisonterm == "9999998") %>%
n_distinct(r$dc_number)
#474 

#this means that there are 14 lifers who have some other prison term than lifer code

others <- anti_join(lifers_more, r, by = "dc_number")

#are they resentenced?

resentothers <- others %>% semi_join(resentence, by = "dc_number") 
n_distinct(resentothers$dc_number)
#8 

resentothers %>% group_by(prisonterm) %>% count()

#all of them have prison terms which refer to their new sentences, which brings hope to houw to find resentecned people!!

#who are the other 8 though, are those other sentences?

left_others <- others %>% anti_join(resentence, by = "dc_number") 

#bobby bryant 024399	BRYANT	BOBBY does not come up in inmate search, google shows that 
#he's serving time for another crime in federal prison, hence he was let out at some point
#he's not in release records though. something strange

#harry braswell has escaped
#billy williams is in calfornia 
## resentenced: 524357	RICHARDSON	ANDREW
##	D41982	GORDON	ROMALIS - in louisiana prison instead, told later he did i t

## resentenced: 524357	RICHARDSON	ANDREW
##              N18142 tracy wright - prob resentenced 
#               S24388	VALENZUELA	ORLANDO
#               X34147 FRANCES, ELVIS prob resentenced 

#billy mansfield - seems to be in florida instead  https://inmatelocator.cdcr.ca.gov/Details.aspx?ID=C45134

#add them to the resentenced data frame 

resentence <- rawdata %>% 
  filter(dc_number %in% c("G14126", "H30826", "K74916", "M38196", 
                          "R52379", "R64639", "Y38667", "166417", 
                          "524357", "N18142", "S24388", "X34147"))

others <- others %>%
  anti_join(resentence, by = "dc_number")

n<- lifers_more %>% anti_join(resentence, by = "dc_number")

#look up releasd people by prison term 

released_juv_lifers <- juv %>%
  filter(prisonterm == "9999998") 


#162 people, #most of them are deceased, 68 died, 94 has been released

released_juv_live <- released_juv_lifers %>%
  filter(releasedateflag_descr != "deceased") %>% 
  group_by(ParoleTerm) %>% 
  count() %>%
  View()
ParoleTerm
n
# 1	0000000	33
# 2	0150001	1
# 3	9999830	20
# 4	9999998	29
# 5	9999999	2
# 6	NA	9

dead_released_juv <- released_juv_lifers %>%
  filter(releasedateflag_descr == "deceased") %>%
  mutate(inprison = as.period(interval(DateAdjudicated, PrisonReleaseDate), 
                              "years"),
         age_of_death = as.period(interval(BirthDate, PrisonReleaseDate), 
                                  "years"))

dead_released_juv$inprison <- substr(dead_released_juv$inprison, 1, 2)
dead_released_juv$age_of_death <- substr(dead_released_juv$age_of_death, 1, 2)

n_distinct(dead_released_juv$dc_number)
# 48 dead 

test <- juv %>%
  filter(dc_number %in% c(released_juv_lifers$dc_number), 
         releasedateflag_descr == "deceased", prisonterm != "9999998")
n_distinct(test$dc_number)  

test %>% 
  group_by(dc_number, prisonterm) %>% 
  summarise(people = n()) %>%
            View()


#people that are resentenced but we not knof of = have yet not be released

#is there someone missing from our lifers ?

active_offense <- active_offense %>%
  rename(case_nr = CaseNumber)

alldata <-
  inner_join(rawdata, active_offense, by = c("dc_number", "case_nr")) %>% View()


young <- active_offense %>%
  filter(dc_number %in% c(rawdata$dc_number), 
         !dc_number %in% c(lifers$dc_number), 
         case_nr %in% c(rawdata$case_nr))

young %>%
group_by(prisonterm) %>%
count() %>%
View()

test <- young %>% 
  left_join(rawdata, by = NULL)

test$dob <- gsub(" 0:00:00", "", test$BirthDate)
test$BirthDate <- mdy(test$BirtesthDate)

test$OffenseDate <- gsub(" 0:00:00", "", test$OffenseDate)
test$OffenseDate <- mdy(test$OffenseDate)

test$PrisonReleaseDate <- gsub(" 0:00:00", "", test$PrisonReleaseDate)
test$PrisonReleaseDate <- mdy(test$PrisonReleaseDate)

test$DateAdjudicated <- gsub(" 0:00:00", "", test$DateAdjudicated)
test$DateAdjudicated <- mdy(test$DateAdjudicated)

test <- test %>%
  mutate(realage = as.period(interval(test$dob, test$OffenseDate), "years")) 

test %>%
  group_by(age) %>%
  count()
n_distinct(young$dc_number)

na <- test %>% filter(is.na(age))
n_distinct(na$dc_number)

test %>%
  filter(age < 18) %>%
  View()

test <- test %>%
  anti_join(na, by = "dc_number")
n_distinct(test$dc_number)

possible_lfrs <- test %>% filter(prisonterm == "9999998") 

n_distinct(possible_lfrs$dc_number)
test %>%
  group_by(prisonterm) %>%
  count() %>%
  View()

x <- test %>% 
  filter(!dc_number %in% c(possible_lfrs$dc_number, resentence$dc_number)) 

n_distinct(x$dc_number)

x <-x %>%  group_by(dc_number, prisonterm) %>%
  count() %>%
  View()


young %>% filter(p)


release %>%
  rename(DCNumber = dc_number) %% 







