library(tidyverse)
library(readr)
library(lubridate)

#ask about everyone with 7378, P58748, 094027 (1000 years Franklin), 640016 (100 y), J47682 (bad data probably), c(062991, 028464	 	032257, 076331 (cancelled but actually a lifer)
# ##there are three sentence dates 	2048-06-19 - is that a mistake or a sentence date for a lifer 455837
### ask explanations about the copmst code 

####GET THE DATA----
###pull in data sent from florida doc (data is up to date from 4/30/19)

rawdata <- read_csv("data/source/florida_043019.csv")

lifers <- rawdata %>% 
  filter(dc_number %in% c("028464", "032257", "050278", "062991", "076331") |
           sentence_length == "99998")

resentence <- rawdata %>% 
  filter(dc_number %in% c("G14126", "H30826", "K74916", "M38196", "R52379", "R64639", "Y38667", "166417"))

release <- readRDS("data/processed/floridareleased.RDS")

?readRDS

# ####RELEASE----

release_offenses <- read.csv("~/Desktop/INMATE_RELEASE_OFFENSES_CPS.txt")
release_full <- read.csv("~/Desktop/INMATE_RELEASE_ROOT.txt")
# 
# 
#n_distinct(release_full$DCNumber)
# # n_distinct(release_root$DCNumber)
# # 
# # #who's missing between two datasets (full has personal info, offense has offense info)
# # 
# # missing <- anti_join(release_full, release_root, by = "DCNumber")
# # n_distinct(missing$DCNumber)
# # 
# # missing2 <- anti_join(release_root, release_full, by = "DCNumber")
# # #we want to use release_full
# # 
# # ##who of released people were serving crimes underage?
# # 
# ##convert strings to dates
# release_full$BirthDate <- gsub(" 0:00:00", "", release_full$BirthDate)
# release_full$BirthDate <- mdy(release_full$BirthDate)
# 
# release_full$ReceiptDate <- gsub(" 0:00:00", "", release_full$ReceiptDate)
# release_full$ReceiptDate <- mdy(release_full$ReceiptDate)
# 
#release_full$PrisonReleaseDate <- gsub(" 0:00:00", "", release_full$PrisonReleaseDate)
#release_full$PrisonReleaseDate <- mdy(release_full$PrisonReleaseDate)
# # 
# # ##we need offense dates to create age when they commited the crimes
# # 
# release_full <- release_full %>%
#  left_join(release_offenses, by = "DCNumber")
# # 
# # n_distinct(release_full$DCNumber)
# # #we have doubles, but that's fine
# # 
# release_full$OffenseDate <- gsub(" 0:00:00", "", release_full$OffenseDate)
# release_full$OffenseDate <- mdy(release_full$OffenseDate)
# #
# # #how many people don't have offensedate
# # 
# offmissing <- release_full %>%
#   filter(is.na(release_full$OffenseDate))
# n_distinct(offmissing$DCNumber)
# 
# bdatemissing <- release_full %>%
#   filter(is.na(release_full$BirthDate))
# n_distinct(offmissing$DCNumber)
# 
# #only the one's which are missing from the offense data are missing (1937)
# 
# n_distinct(offmissing$DCNumber)
# 
# #calculate age during the offense for the  other ones
# 
# release_full <- release_full %>%
#   mutate(age = as.period(interval(release_full$BirthDate, release_full$OffenseDate), "years"))
# release_full$age <- substr(release_full$age, 1, 2)
# 
# saveRDS(release_full, file = "floridareleased.RDS")

# ####BACKGROUND INFO AND CLEAN UP----
# 
# ### how many unique people are in data?
# 
# n_distinct(rawdata$dc_number)
# 
# #5267
# 
# ### who did they gave us? how many are serving life now?
# 
# 
# summary(rawdata$age)
# 
# ###all crimes commited underage 
# 
# unique(rawdata$provisional_release_date)
# unique(rawdata$tentative_release_date)
# unique(rawdata$sentence_length)
# 
# rawdata %>%
#   group_by(tentative_release_date) %>%
#   count() %>% View()
# 
# ### look into people with no release dates 
# 
# rawdata %>% 
#   filter(provisional_release_date %in% c("NO CREDITS", "LIFE", "CANCELLED", "INTERSTATE")) %>%
#   group_by(provisional_release_date) %>% count() %>% collect()
# 
# 
# #who are they?
# #lifers 510 when grouped by dc number 
# lifers <- rawdata %>%
#   filter(provisional_release_date == "LIFE") 
# n_distinct(lifers$dc_number)
# ### who are the people who are serving life, but have other sentence length as well?
# 
# lifers %>% 
#   group_by(sentence_length) %>%
#   count() %>% View()
# ### look into anyone without 99998 (which refers to life)
# others <- lifers %>%
#   filter(sentence_length != 99998)
# 
# ### do we have anyone among "lifers" subset who has only the short sentence and not life?
# 
# t <- anti_join(others, lifers, by = "dc_number") 
# ###0 variables, so we don't, hence the second lenght refers to sentences reffering to other crimes, we can throw them out
# 
# #should we then instead use the 99998 for filtering out lifers?
# 
# lifers <- rawdata %>%
#   filter(sentence_length == 99998)
# 
# rawdata %>% filter(is.na(rawdata$sentence_length)) 
# rawdata %>% filter(sentence_length <100) %>% View()
# 
# ### now we have 811 people left. are there still doubles?
# 
# n_distinct(lifers$dc_number)
# 
# ## 
# 
# ### 483 people. what causes doubles?
# 
# t <- lifers %>%
#   group_by(first_name, last_name, dob, offense_code, offense_date, case_nr) %>%
#   count()
# #this gives us 709 rows 
# 
# #if the case nr is the same and offense date is the same, that means that a person is just commiting several crimes during one incident
# # hence we don't need to group by offense_code
# 
# t <- lifers %>%
#   group_by(first_name, last_name, dob, offense_date, case_nr) %>%
#   count()
# 
# ##this gives us 534 rows, who are the doubles?
# 
# ## seems that some cases are just double, e.g robert stern 
# 
# t <-lifers %>%
#   group_by(dc_number) %>%
#   count() %>% filter(n >1) %>%
#   ungroup() %>% View()
# 
# doubles <- left_join(t, lifers, by = "dc_number")
# 
# ###when looking into doubles we see that they're either just doubles or have commited life sentence worthy crimes in several days 
# ###that doesn't create problems for us, so to get the numbers, unique dc_number is enough right now
# 
# n_distinct(lifers$dc_number)
# ###483
# 
# 
# 
# ### there are 8 people who have life sentence code but no LIFE in the release date who are they? 7378 year
# 
# ### none of them seem to have actual life sentence, when we're using offender search. will follow up with the data people from florida.
# 
# unique(lifers$provisional_release_date)
# 
# lifers%>% filter(tentative_release_date != "LIFE")
# 
# ###who are interstate people?
# 
# rawdata %>% 
#   filter(tentative_release_date == "INTERSTATE") 
# ## two people, they're not lifers 
# 
# #are there any more options besides life and interstate? unique doesn't give answers as too many. lets check otherwise, this also 
# #gives option to sort dates properly to filter out strange dates 
# rawdata$tentative_release_date <- is.Date(rawdata$tentative_release_date)
# 
# rawdata <- rawdata %>%
#   mutate(newdate = mdy(tentative_release_date))
# 
# nodate <- rawdata %>% filter(is.na(rawdata$newdate)) 
# 
# unique(nodate$tentative_release_date)
# 
# rawdata %>% 
#   group_by(newdate) %>%
#   count(n = n()) %>%
#   View()
#  
# 
# ###people who serve longer sentences than 90
# 
# long <- rawdata %>% filter(sentence_length > 32850) 
# 
# n_distinct(long$dc_number)
# #take out lifers and see who they are
# 
# t <- anti_join(long, lifers, by = "dc_number")
# n_distinct(t$dc_number)
# 
# #there are 15 people who are serving more than 90 years but are not among LIFERS 
# 
# ###look into provisional release date with cancelled dates 
# c <- rawdata %>%
#   filter(provisional_release_date == "CANCELLED")
# 
# t <- anti_join(c, lifers, by = "dc_number")

### all but two are lifers, but not as lifers in our data, that was revealed when checking them one by one

# 028464 - Grubb, Terry O
# 032257  Cook, Donald
# 050278 Owens, Johnny
# 062991, DANIELS, ROBERT W
# 076331, MCMILLAN, MOSES W

# 
# 
# nocredits <- rawdata %>%
#     filter(provisional_release_date == "NO CREDITS") %>% 
#   group_by(sentence_length, newdate, dc_number) %>%
#     count() 
# 
# ### too much data to try to find something from "no credits" column 
# 
# ###let's look into the dates provisional date columns 
# rawdata %>% 
#   group_by(provisional_release_date) %>% 
#   count() %>%
#   View()
# 
# #there doesn't seem to be any strange dates in provisional release date   
# 
# rawdata %>%
#   group_by(provisional_release_date, dc_number) %>%
#   count() %>%
#   View()
# 
# rawdata %>% group_by(sentence_date) %>%
#   count() %>%
#   View()
# 
# ##there are three sentence dates 	2048-06-19 - is that a mistake or a sentence date for a lifer 455837, other dates make sense 
# 
# 
# rawdata %>% group_by(offense_date, dc_number) %>%
#   count() %>%
#   View()
# 
# ##for 137 inmates the offense date is missing 
# 
# rawdata %>%
#   group_by(case_nr) %>%
#   count() %>%
#   View()
# 
# rawdata %>%
#   group_by(case_nr) %>%
#   count() %>%
#   View()
# 
#  
# #347
# 
# unique(rawdata$inmate_compst_code)
# 
# lifers %>%
#   group_by(inmate_compst_code) %>%
#   count() %>%
#   View()
# ### look into offenses, is there a lifer with no murder?
# 
# murderers <- lifers %>%
#   filter(str_detect(offense_code, "MUR"))
# 
# n_distinct(lifers$dc_number)
# n_distinct(murderers$dc_number)
# 
# ### there are 745-413 lifers who doesn't have a murder charge? who are they?
# 
# nomurder <- anti_join(lifers, murderers, by = "dc_number")
# 
# nomurder %>%
#   group_by(dc_number, offense_code) %>%
#   count() %>%
#   View()
# 
# #do all lifers have life and capital felony code?
# 
# lifers %>%
#   group_by(felony_class) %>%
#   count() %>%
#   View()
# 
# #can't tell like this 
# 
# capitallife <- lifers %>%
#   filter(felony_class %in% c("LIFE", 
#                              "CAPITAL", 
#                              "1ST/LIFE"))
# 
# lesserfelony <- anti_join(lifers, capitallife, by = "dc_number")
# 
# n_distinct(lesserfelony$dc_number)
# 
# #there's 96 people who doesn't have "traditional" life felony, what do they have?
# 
# lesserfelony %>%
#   group_by(felony_class) %>%
#   count() 
# 
# ##look into only unknowns and which are the ones that have unknownn and more 
# 
# unkn <- lesserfelony %>% 
#   group_by(dc_number, felony_class) %>%
#   filter(felony_class != "2ND DEGREE") %>%
#   summarise(n = n()) %>%
#   spread(felony_class, n) 
# 
# 
# #There's only one person who has "unknown felony and something else, it's LIGHTSEY	SAMUEL 	077701  
# 
# #hence we can just look into what's up with unknowns 
# 
# lifers %>%
#   filter(felony_class == "UNKNOWN") %>%
#   View()
# 
# ###how many lifers were not principal offenders?
# 
# lifers %>% 
#   group_by(offense_qualifier) %>%
#   count()
# 
# ###729 are principals, 29 are principals in attempt, and only one is accessory, 2 conspiracy to commit and 11 is unknown 
# 
# lifers %>% 
#   group_by(offense_qualifier, dc_number, offense_code) %>%
#   count() %>% View()
# 
# b <- rawdata %>% 
#   filter(offense_qualifier == "UNKNOWN") 
# 
# rawdata %>% 
#   filter(dc_number %in% b$dc_number) %>%
#   View()
# #most are principals 
# 
# #check against all life and capital felony classes to see whether we have all lifers or need to add extra. 
# ##felony class can be life but that doesn't necessarily mean that the punishment was life 
# 
# fellifers <- rawdata %>%
#   filter(felony_class %in% c("LIFE", "CAPITAL", "1ST/LIFE"))
# 
# 
# fellifers <- anti_join(fellifers, lifers, by = "dc_number")
# 
# ### there's only two people who have 100 or longer sentences and they're already in our caveats, also one of them, 094027 has a 
# ## supreme court decision about his 3X1000 years with parole 

### who are the people who have 99998 but also a release date, we've checked against "cancelled" already?
t <- lifers %>% filter(tentative_release_date != "LIFE", provisional_release_date != 'CANCELLED')
n_distinct(t$dc_number)




#resentenced at somepoint, 

#G14126 OWENS, LAJEFFRIC A - as he was sentenced to life, but the inmate search says he'll get out and "sentence concluded"
#H30826	RIOUX	RUSSELL - had life, but restentenced to life with parole, inmate search and court decision gives us that info, data doesn't show 
#K74916	TINDALL	DARYL	LEVON - was resentenced for 61 years in 2011, no note in the data for that, we have that info from inmate search and 
#M38196	BERGER	JACKIE - resentenced  in 2017, no note in data about the date 
#R52379 GLAND, TYREE J - most probably resentenced, needs confirmation, inmagte search shows only 15 years + appeals a ltot 
#R64639	WALLE	JOSE - resentenced, strangely only one record in our data. 
#Y38667	JOHNSON	HERBERT - sentenced to life originally, but released by now 
#166417	MOSLEY	RONALD - resentenced to 30 years

#it makes sense to create a resentenced df for the time being, until we find out other resentenced people 

# resentence <- rawdata %>% 
#   filter(dc_number %in% c("G14126", "H30826", "K74916", "M38196", "R52379", "R64639", "Y38667", "166417"))



#66860	ENRIQUEZ	LESTER - sentenced to life, but the data has release date for 08/19/7378
#105370 ROBERSON, ROBERT L - sentenced to life actually 
#455837	WADLEY	KEVIN - sentenced to life, other sentence date 
#959652	WITTEMEN	KEITH - sentenced to life, but the data has release date for 08/19/7378
#B12280	RAZZ	DARNELL- sentenced to life, but the data has release date for 08/19/7378


#we don't have to remove anyone 
# ###CONCLUSION----
# 
# ##we've gone over all columns to find people who could be lifers, the indications are:
# 
# # - release date "LIFE"
# # - sentence length 99998
# # - all 15 people who are serving sentences which are over 100 years don't serve life sentence as a punishment 
# #       (checked 15 people one by one from the offender search)
# # - all but two are lifers, but not as lifers in our data, that was revealed when checking them one by one
# 
#         # 028464 - Grubb, Terry O
#         # 032257  Cook, Donald
#         # 050278 Owens, Johnny
#         # 062991, DANIELS, ROBERT W
#         # 076331, MCMILLAN, MOSES W	
# 
# 
# ##614009	JOHNSON	CLYDE	EDWARD, has by bow released, his resentence date for 100 years is wrong in the data though 
# 
# ### taking that all in account we have 483 (from lifers subset) + 5 = 489 lifers 
# 
# 
# ###CREATE FINAL DATAFRAME FOR LIFERS----
# #pull in missing people checked from inmate search (see above)
# lifers_from_raw <- lifers %>% select(1:19)
# lifers <- rawdata %>% 
#   filter(dc_number %in% c("028464", "032257", "050278", "062991", "076331" )) %>%
#   bind_rows(lifers_from_raw)



# ####RELEASE----
#
# n_distinct(release_full$DCNumber)
# n_distinct(release_root$DCNumber)
#
# #who's missing between two datasets (full has personal info, offense has offense info)
#
# missing <- anti_join(release_full, release_root, by = "DCNumber")
# n_distinct(missing$DCNumber)
#
# missing2 <- anti_join(release_root, release_full, by = "DCNumber")
# #we want to use release_full
#
# ##who of released people were serving crimes underage?
#
# ##convert strings to dates
# release_full$BirthDate <- gsub(" 0:00:00", "", release_full$BirthDate)
# release_full$BirthDate <- mdy(release_full$BirthDate)
#
# release_full$ReceiptDate <- gsub(" 0:00:00", "", release_full$ReceiptDate)
# release_full$ReceiptDate <- mdy(release_full$ReceiptDate)
#
# release_full$PrisonReleaseDate <- gsub(" 0:00:00", "", release_full$PrisonReleaseDate)
# release_full$PrisonReleaseDate <- mdy(release_full$PrisonReleaseDate)
#
# ##we need offense dates to create age when they commited the crimes
#
# release_full <- release_full %>%
#   left_join(release_root, by = "DCNumber")
#
# n_distinct(release_full$DCNumber)
# #we have doubles, but that's fine
#
# release_full$OffenseDate <- gsub(" 0:00:00", "", release_full$OffenseDate)
# release_full$OffenseDate <- mdy(release_full$OffenseDate)
#
# #how many people don't have offensedate
#
# offmissing <- release_full %>%
#   filter(is.na(release_full$OffenseDate))
# n_distinct(offmissing$DCNumber)
#
# bdatemissing <- release_full %>%
#   filter(is.na(release_full$BirthDate))
# n_distinct(offmissing$DCNumber)
#
# #only the one's which are missing from the offense data are missing (1937)
#
# n_distinct(offmissing$DCNumber)
#
# #calculate age during the offense for the  other ones
#
# release_full <- release_full %>%
#   mutate(age = as.period(interval(release_full$BirthDate, release_full$OffenseDate), "years"))

# 
