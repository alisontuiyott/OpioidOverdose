#Coroner_Data_Manipulation.R
#Cleans and manipulates the data used for the app for visualizing the data of the opioid epidemic provided by the Butler County Coroner 

#Set working directory
#setwd("~/MiamiAlison3/STA475/Coronor")
# Edited by: Alison Tuiyott, Lulu Liu, Rachael Lewis

#Install and load needed packages
#install.packages("readxl")
library(readxl)
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("sqldf")
library(sqldf)

#Import the data
data.2013<- read_excel("Miami U data 2013-2018.xlsx", sheet = "2013 Accidental Overdose")
data.2013 <- rename(data.2013, SUBSTANCE = SUBSTANCE..17,SUBSTANCE__1 = SUBSTANCE..18)
data.2014 <- read_excel("Miami U data 2013-2018.xlsx", sheet = "2014 Accidental Overdoses")
data.2014 <- rename(data.2014, SUBSTANCE = SUBSTANCE..18,SUBSTANCE__1 = SUBSTANCE..19)
data.2015 <- read_excel("Miami U data 2013-2018.xlsx", sheet = "2015 Accidental Overdoses")
data.2015 <- rename(data.2015, SUBSTANCE = SUBSTANCE..17, SUBSTANCE__1 = SUBSTANCE..18,SUBSTANCE__2 = SUBSTANCE..19)
data.2016 <- read_excel("Miami U data 2013-2018.xlsx", sheet = "2016 Accidental Overdoses")
data.2016 <- rename(data.2016, SUBSTANCE = SUBSTANCE..17, SUBSTANCE__1 = SUBSTANCE..18,SUBSTANCE__2 = SUBSTANCE..19)
data.2017 <- read_excel("Miami U data 2013-2018.xlsx", sheet = "2017 Accidental Overdoses")
data.2017 <- rename(data.2017, SUBSTANCE = SUBSTANCE..18, SUBSTANCE__1 = SUBSTANCE..19,SUBSTANCE__2 = SUBSTANCE..20)
data.2018 <- read_excel("Miami U data 2013-2018.xlsx", sheet = "2018 Accidental Overdoses")
data.2018 <- rename(data.2018, SUBSTANCE = SUBSTANCE..21, SUBSTANCE__1 = SUBSTANCE..22,SUBSTANCE__2 = SUBSTANCE..23)
# data.2019 <- read_excel("Miami U data.xlsx", sheet = "2019 Accidental Overdoses")
# data.2020 <- read_excel("Miami U data.xlsx", sheet = "2020 Accidental Overdoses")

#Add a year varaible to the data sets
data.2013$YEAR <- 2013
data.2014$YEAR <- 2014
data.2015$YEAR <- 2015
data.2016$YEAR <- 2016
data.2017$YEAR <- 2017
data.2018$YEAR <- 2018
# data.2019$YEAR <- 2019
# data.2020$YEAR <- 2020


#Make all of the tbls data frames
data.2013<-as.data.frame(data.2013)
data.2014<-as.data.frame(data.2014)
data.2015<-as.data.frame(data.2015)
data.2016<-as.data.frame(data.2016)
data.2017<-as.data.frame(data.2017)
data.2018<-as.data.frame(data.2018)
# data.2019<-as.data.frame(data.2019)
# data.2020<-as.data.frame(data.2020)



#combine all of the data tables as one
#https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
full_data<-full_join(data.2013, data.2014)
full_data<-full_join(full_data, data.2015)
full_data<-full_join(full_data, data.2016)
full_data<-full_join(full_data, data.2017)
full_data<-full_join(full_data, data.2018)
# full_data<-full_join(full_data, data.2019)
# full_data<-full_join(full_data, data.2020)

##Fix typos:-------------------------------------------------------------------------------------
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Madison Twp",
                                              "Madison Twp.", full_data$`CITY/TWP/CO OF INCIDENT`)

full_data$`PLACE OF INCIDENT` <- ifelse(full_data$`PLACE OF INCIDENT` == "Hotel/Suite",
                                              "Hotel", full_data$`PLACE OF INCIDENT`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Creekbed",
                                        "Creek", full_data$`PLACE OF DEATH`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Creek bank",
                                     "Creek", full_data$`PLACE OF DEATH`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Fairfield Twp",
                                              "Farifield Twp.", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "OHIO",
                                              "Ohio", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Unknown, OH",
                                              "Ohio", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`PLACE OF INCIDENT` <- ifelse(full_data$`PLACE OF INCIDENT` == "Residence",
                                        "Home", full_data$`PLACE OF INCIDENT`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Automobile",
                                     "Vehicle", full_data$`PLACE OF DEATH`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Other hom",
                                     "Other home", full_data$`PLACE OF DEATH`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Other Home",
                                     "Other home", full_data$`PLACE OF DEATH`)
full_data$`PLACE OF DEATH` <- ifelse(full_data$`PLACE OF DEATH` == "Wooded Area",
                                     "Wooded area", full_data$`PLACE OF DEATH`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Farifield",
                                              "Fairfield", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Middletwon",
                                              "Middletown", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Middletoen",
                                             "Middletown", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Someville",
                                             "Somerville", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Mont Co.",
                                              "Montgomery Co.", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP/CO OF INCIDENT` <- ifelse(full_data$`CITY/TWP/CO OF INCIDENT` == "Mont Co",
                                              "Montgomery Co.", full_data$`CITY/TWP/CO OF INCIDENT`)
full_data$`CITY/TWP OF DEATH` <- case_when(
  full_data$`CITY/TWP OF DEATH` == "Madison Twp" ~ "Madison Twp." ,
  full_data$`CITY/TWP OF DEATH` == "Fairfield Twp" ~ "Farifield Twp.",
  full_data$`CITY/TWP OF DEATH` == "Farifield" ~ "Fairfield" ,
  full_data$`CITY/TWP OF DEATH` %in% c("Middletwon","Middletoen","Middeltown") ~ "Middletown" ,
  full_data$`CITY/TWP OF DEATH` == "Someville" ~ "Somerville" ,
  full_data$`CITY/TWP OF DEATH` %in% c("Mont Co.","Mont Co") ~ "Montgomery Co.",
  full_data$`CITY/TWP OF DEATH` %in% c("Ham. Co.") ~ "Hamilton Co.",
  TRUE ~ full_data$`CITY/TWP OF DEATH`
  )

full_data$`PLACE OF INCIDENT` <- case_when(
  full_data$`PLACE OF INCIDENT` %in% c("Other Home","Other home") ~ "Home",
  full_data$`PLACE OF INCIDENT` %in% c("Creek bank","Creekbed") ~ "Creek",
  full_data$`PLACE OF INCIDENT` == "Wooded area" ~ "Wooded Area" ,
  full_data$`PLACE OF INCIDENT` %in% c("Automobile","Vehicle") ~ "Vehicle",
  full_data$`PLACE OF INCIDENT` %in% c("Workplace","Business") ~ "Business",
  full_data$`PLACE OF INCIDENT` %in% c("Hospital","Hospice","Nursing Home") ~ "Healthcare Facility",
  TRUE ~ full_data$`PLACE OF INCIDENT`
)

full_data$`PLACE OF DEATH` <- case_when(
  full_data$`PLACE OF DEATH` %in% c("Other Home","Other home") ~ "Home",
  full_data$`PLACE OF DEATH` %in% c("Creek bank","Creekbed") ~ "Creek",
  full_data$`PLACE OF DEATH` == "Wooded area" ~ "Wooded Area" ,
  full_data$`PLACE OF DEATH` %in% c("Automobile","Vehicle") ~ "Vehicle",
  full_data$`PLACE OF DEATH` %in% c("Workplace","Business") ~ "Business",
  full_data$`PLACE OF DEATH` %in% c("Hospital","Hospice","Nursing Home") ~ "Healthcare Facility",
  TRUE ~ full_data$`PLACE OF DEATH`
)

full_data$SEX <- ifelse(full_data$SEX == "M","Male", full_data$SEX)
full_data$SEX <- ifelse(full_data$SEX == "F","Female", full_data$SEX)
full_data$RACE <- ifelse(full_data$RACE == "W","White", full_data$RACE)
full_data$RACE <- ifelse(full_data$RACE == "B","Black", full_data$RACE)
full_data$RACE <- ifelse(full_data$RACE == "A","Asian", full_data$RACE)
full_data$RACE <- ifelse(full_data$RACE == "H","Hispanic", full_data$RACE)
full_data$RACE <- ifelse(full_data$RACE == "C","White", full_data$RACE)


#combine hotel and motel
full_data$`PLACE OF INCIDENT`[full_data$`PLACE OF INCIDENT`=="Hotel"]<-"Hotel/Motel"
full_data$`PLACE OF INCIDENT`[full_data$`PLACE OF INCIDENT`=="Motel"]<-"Hotel/Motel"
full_data$`PLACE OF DEATH`[full_data$`PLACE OF DEATH`=="Hotel"]<-"Hotel/Motel"
full_data$`PLACE OF DEATH`[full_data$`PLACE OF DEATH`=="Motel"]<-"Hotel/Motel"


#SUBSTANCE - drug name changes
full_data$SUBSTANCE <- ifelse(full_data$SUBSTANCE == "6AM-B", "Heroin",
                              ifelse(full_data$SUBSTANCE == "6AM-V", "Heroin",
                                     ifelse(full_data$SUBSTANCE == "6AM-U", "Heroin",
                                            ifelse(full_data$SUBSTANCE == "Unconfirmed - Heroin (6AM-U)", "Heroin",
                                                   ifelse(full_data$SUBSTANCE == "6AM-B & U", "Heroin",
                                                          ifelse(full_data$SUBSTANCE == "6AM Powder tested by police", "Heroin",
                                                                 ifelse(full_data$SUBSTANCE == "Morphine (Probable Heroin)", "Heroin", 
                                                                        ifelse(full_data$SUBSTANCE == "Cocaine*", "Cocaine",
                                                                               ifelse(full_data$SUBSTANCE == "Cocaine-U", "Cocaine",
                                                                                      ifelse(full_data$SUBSTANCE == "Diazapam", "Diazepam",
                                                                                             ifelse(full_data$SUBSTANCE == "6AM (Morphine)", "Heroin",
                                                                                                    ifelse(full_data$SUBSTANCE == "Mophine", "Morphine",
                                                                                                           ifelse(full_data$SUBSTANCE == "Morphine - Probable Heroin", "Heroin",
                                                                                                                  ifelse(full_data$SUBSTANCE == "Hydrocodone (U)", "Hydrocodone",
                                                                                                                         ifelse(full_data$SUBSTANCE == "Amphetamine", "Amphetamines",
                                                                                                                                full_data$SUBSTANCE
                                                                                                                         )))))))))))))))

#SUBSTANCE_1 - drug name changes
full_data$SUBSTANCE__1 <- ifelse(full_data$SUBSTANCE__1 == "6AM-B", "Heroin",
                                 ifelse(full_data$SUBSTANCE__1 == "6AM-V", "Heroin",
                                        ifelse(full_data$SUBSTANCE__1 == "6AM-U", "Heroin",
                                               ifelse(full_data$SUBSTANCE__1 == "Unconfirmed - Heroin (6AM-U)", "Heroin",
                                                      ifelse(full_data$SUBSTANCE__1 == "6AM-B & U", "Heroin",
                                                             ifelse(full_data$SUBSTANCE__1 == "6AM Powder tested by police", "Heroin",
                                                                    ifelse(full_data$SUBSTANCE__1 == "Morphine (Probable Heroin)", "Heroin", 
                                                                           ifelse(full_data$SUBSTANCE__1 == "Cocaine*", "Cocaine",
                                                                                  ifelse(full_data$SUBSTANCE__1 == "Cocaine-U", "Cocaine",
                                                                                         ifelse(full_data$SUBSTANCE__1 == "Diazapam", "Diazepam",
                                                                                                ifelse(full_data$SUBSTANCE__1 == "6AM (Morphine)", "Heroin",
                                                                                                       ifelse(full_data$SUBSTANCE__1 == "Mophine", "Morphine",
                                                                                                              ifelse(full_data$SUBSTANCE__1 == "Morphine - Probable Heroin", "Heroin",
                                                                                                                     ifelse(full_data$SUBSTANCE__1 == "Hydrocodone (U)", "Hydrocodone",
                                                                                                                            ifelse(full_data$SUBSTANCE__1 == "Fentanyl-U", "Fentanyl",
                                                                                                                                   ifelse(full_data$SUBSTANCE__1 == "Fentanyl - U", "Fentanyl",
                                                                                                                                          ifelse(full_data$SUBSTANCE__1 == "Fentanyl (U)", "Fentanyl",
                                                                                                                                                 ifelse(full_data$SUBSTANCE__1 == "Oxycodone (U)", "Oxycodone",
                                                                                                                                                        ifelse(full_data$SUBSTANCE__1 == "Morphine (U)", "Morphine",
                                                                                                                                                               ifelse(full_data$SUBSTANCE__1 == "Cocaine - U", "Cocaine",
                                                                                                                                                                      ifelse(full_data$SUBSTANCE__1 == "6AM-P", "Heroin",
                                                                                                                                                                             ifelse(full_data$SUBSTANCE__1 == "Probable Heroin", "Heroin",
                                                                                                                                                                                    ifelse(full_data$SUBSTANCE__1 == "Cannabinoid", "Cannabinoids",     
                                                                                                                                                                                           ifelse(full_data$SUBSTANCE__1 == "Cannbinoids", "Cannabinoids",
                                                                                                                                                                                                  ifelse(full_data$SUBSTANCE__1 == "Alprazolan", "Alprazolam",
                                                                                                                                                                                                         ifelse(full_data$SUBSTANCE__1 == "Amphetamine", "Amphetamines", 
                                                                                                                                                                                                                ifelse(full_data$SUBSTANCE__1 == "Benzodiazopines", "Benzodiazepines", 
                                                                                                                                                                                                                       ifelse(full_data$SUBSTANCE__1 == "Diphendydramine", "Diphenhydramine", 
                                                                                                                                                                                                                              full_data$SUBSTANCE__1))))))))))))))))))))))))))))
# SPECIAL CASE: 6AM-B / Fentanyl and 6AM-U / Cocaine and 59.3% CO
# full_data <- full_data %>%
#   mutate(SUBSTANCE = ifelse())


#SUBSTANCE_2 - drug name changes
full_data$SUBSTANCE__2 <- ifelse(full_data$SUBSTANCE__2 == "6AM-B", "Heroin",
                                 ifelse(full_data$SUBSTANCE__2 == "6AM-V", "Heroin",
                                        ifelse(full_data$SUBSTANCE__2 == "6AM-U", "Heroin",
                                               ifelse(full_data$SUBSTANCE__2 == "Unconfirmed - Heroin (6AM-U)", "Heroin",
                                                      ifelse(full_data$SUBSTANCE__2 == "6AM-B & U", "Heroin",
                                                             ifelse(full_data$SUBSTANCE__2 == "6AM Powder tested by police", "Heroin",
                                                                    ifelse(full_data$SUBSTANCE__2 == "Morphine (Probable Heroin)", "Heroin", 
                                                                           ifelse(full_data$SUBSTANCE__2 == "Cocaine*", "Cocaine",
                                                                                  ifelse(full_data$SUBSTANCE__2 == "Cocaine-U", "Cocaine",
                                                                                         ifelse(full_data$SUBSTANCE__2 == "Diazapam", "Diazepam",
                                                                                                ifelse(full_data$SUBSTANCE__2 == "6AM (Morphine)", "Heroin",
                                                                                                       ifelse(full_data$SUBSTANCE__2 == "Mophine", "Morphine",
                                                                                                              ifelse(full_data$SUBSTANCE__2 == "Morphine - Probable Heroin", "Heroin",
                                                                                                                     ifelse(full_data$SUBSTANCE__2 == "Hydrocodone (U)", "Hydrocodone",
                                                                                                                            ifelse(full_data$SUBSTANCE__2 == "Fentanyl-U", "Fentanyl",
                                                                                                                                   ifelse(full_data$SUBSTANCE__2 == "Fentanyl - U", "Fentanyl",
                                                                                                                                          ifelse(full_data$SUBSTANCE__2 == "Fentanyl (U)", "Fentanyl",
                                                                                                                                                 ifelse(full_data$SUBSTANCE__2 == "Oxycodone (U)", "Oxycodone",
                                                                                                                                                        ifelse(full_data$SUBSTANCE__2 == "Morphine (U)", "Morphine",
                                                                                                                                                               ifelse(full_data$SUBSTANCE__2 == "Cocaine - U", "Cocaine",
                                                                                                                                                                      ifelse(full_data$SUBSTANCE__2 == "6AM-P", "Heroin",
                                                                                                                                                                             ifelse(full_data$SUBSTANCE__2 == "Probable Heroin", "Heroin",
                                                                                                                                                                                    ifelse(full_data$SUBSTANCE__2 == "Cannabinoid", "Cannabinoids",     
                                                                                                                                                                                           ifelse(full_data$SUBSTANCE__2 == "Cannbinoids", "Cannabinoids",
                                                                                                                                                                                                  ifelse(full_data$SUBSTANCE__2 == "Alprazolan", "Alprazolam",
                                                                                                                                                                                                         ifelse(full_data$SUBSTANCE__2 == "Amphetamine", "Amphetamines", 
                                                                                                                                                                                                                ifelse(full_data$SUBSTANCE__2 == "Benzodiazopines", "Benzodiazepines", 
                                                                                                                                                                                                                       ifelse(full_data$SUBSTANCE__2 == "Diphendydramine", "Diphenhydramine",
                                                                                                                                                                                                                              ifelse(full_data$SUBSTANCE__2 == "Morphine - U", "Morphine",
                                                                                                                                                                                                                                     ifelse(full_data$SUBSTANCE__2 == "Cocaine (U)", "Cocaine",
                                                                                                                                                                                                                                            ifelse(full_data$SUBSTANCE__2 == "Probable 6AM-B", "Heroin",
                                                                                                                                                                                                                                                   ifelse(full_data$SUBSTANCE__2 == "Buprenophine", "Buprenorphine",
                                                                                                                                                                                                                                                          ifelse(full_data$SUBSTANCE__2 == "Veniafaxine", "Venlafaxine",       
                                                                                                                                                                                                                                                                 full_data$SUBSTANCE__2)))))))))))))))))))))))))))))))))



#Check data
sqldf("SELECT SUBSTANCE 
      FROM full_data
      GROUP BY SUBSTANCE
      ORDER BY SUBSTANCE")
sqldf("SELECT SUBSTANCE__1 
      FROM full_data
      GROUP BY SUBSTANCE__1
      ORDER BY SUBSTANCE__1")
sqldf("SELECT SUBSTANCE__2 
      FROM full_data
      GROUP BY SUBSTANCE__2
      ORDER BY SUBSTANCE__2")


#add a age variable to the data
full_data$GROUPING <- cut(full_data$AGE,  c(seq(-1,20,22), seq(20, 59, 10), seq(60, 101, 20)))



##Create indicator variables for each substance among SUBSTANCE, SUBSTANCE__1, & SUBSTANCE__2
full_data$Heroin <- ifelse(full_data$SUBSTANCE %in% c("Heroin","6AM-U / Cocaine","6AM-B / Fentanyl") |
                             full_data$SUBSTANCE__1 == "Heroin" | 
                             full_data$SUBSTANCE__2 == "Heroin",
                           1, 0)
full_data$Dextromethorphan <- ifelse(full_data$SUBSTANCE == "Dextromethorphan" | 
                                       full_data$SUBSTANCE__1 == "Dextromethorphan" | 
                                       full_data$SUBSTANCE__2 == "Dextromethorphan",
                                     1, 0)
full_data$Morphine <- ifelse(full_data$SUBSTANCE == "Morphine" | 
                               full_data$SUBSTANCE__1 == "Morphine" | 
                               full_data$SUBSTANCE__2 == "Morphine",
                             1, 0)
full_data$Cocaine <- ifelse(full_data$SUBSTANCE %in% c("Cocaine","6AM-U / Cocaine") | 
                              full_data$SUBSTANCE__1 == "Cocaine" | 
                              full_data$SUBSTANCE__2 == "Cocaine",
                            1, 0)
full_data$Hydrocodone <- ifelse(full_data$SUBSTANCE == "Hydrocodone" | 
                                  full_data$SUBSTANCE__1 == "Hydrocodone" | 
                                  full_data$SUBSTANCE__2 == "Hydrocodone",
                                1, 0)
full_data$Methadone <- ifelse(full_data$SUBSTANCE == "Methadone" | 
                                full_data$SUBSTANCE__1 == "Methadone" | 
                                full_data$SUBSTANCE__2 == "Methadone",
                              1, 0)
full_data$Oxycodone <- ifelse(full_data$SUBSTANCE == "Oxycodone" | 
                                full_data$SUBSTANCE__1 == "Oxycodone" | 
                                full_data$SUBSTANCE__2 == "Oxycodone",
                              1, 0)
full_data$Olanzapine <- ifelse(full_data$SUBSTANCE == "Olanzapine" | 
                                 full_data$SUBSTANCE__1 == "Olanzapine" | 
                                 full_data$SUBSTANCE__2 == "Olanzapine",
                               1, 0)
full_data$Fentanyl <- ifelse(full_data$SUBSTANCE %in% c("Fentanyl","6AM-B / Fentanyl","Fentanyl - U") | 
                               full_data$SUBSTANCE__1 == "Fentanyl" | 
                               full_data$SUBSTANCE__2 == "Fentanyl",
                             1, 0)
full_data$Oxymorphone <- ifelse(full_data$SUBSTANCE == "Oxymorphone" | 
                                  full_data$SUBSTANCE__1 == "Oxymorphone" | 
                                  full_data$SUBSTANCE__2 == "Oxymorphone",
                                1, 0)
full_data$Codeine <- ifelse(full_data$SUBSTANCE == "Codeine" | 
                              full_data$SUBSTANCE__1 == "Codeine" | 
                              full_data$SUBSTANCE__2 == "Codeine",
                            1, 0)
full_data$Clonazepam <- ifelse(full_data$SUBSTANCE == "Clonazepam" | 
                                 full_data$SUBSTANCE__1 == "Clonazepam" | 
                                 full_data$SUBSTANCE__2 == "Clonazepam",
                               1, 0)
full_data$Methamphetamine <- ifelse(full_data$SUBSTANCE %in% c("Methamphetamine","*Methamphetamine*") | 
                                      full_data$SUBSTANCE__1 %in% c("Methamphetamine","Methamphetamine - U") | 
                                      full_data$SUBSTANCE__2 %in% c("Methamphetamine","Methamphetamine-U"),
                                    1, 0)
full_data$Citalopram <- ifelse(full_data$SUBSTANCE == "Citalopram" | 
                                 full_data$SUBSTANCE__1 == "Citalopram" | 
                                 full_data$SUBSTANCE__2 == "Citalopram",
                               1, 0)
full_data$Quetiapine <- ifelse(full_data$SUBSTANCE == "Quetiapine" | 
                                 full_data$SUBSTANCE__1 == "Quetiapine" | 
                                 full_data$SUBSTANCE__2 == "Quetiapine",
                               1, 0)
full_data$Cyclobenzaprine <- ifelse(full_data$SUBSTANCE == "Cyclobenzaprine" | 
                                      full_data$SUBSTANCE__1 %in% c("Cyclobenzaprine","Cyclobenzapine") | 
                                      full_data$SUBSTANCE__2 == "Cyclobenzaprine",
                                    1, 0)
full_data$Ethanol <- ifelse(full_data$SUBSTANCE == "Ethanol" | 
                              full_data$SUBSTANCE__1 == "Ethanol" | 
                              full_data$SUBSTANCE__2 == "Ethanol",
                            1, 0)
full_data$Cannabinoids <- ifelse(full_data$SUBSTANCE == "Cannabinoids" | 
                                   full_data$SUBSTANCE__1 == "Cannabinoids" | 
                                   full_data$SUBSTANCE__2 == "Cannabinoids",
                                 1, 0)
full_data$Fluoxetine <- ifelse(full_data$SUBSTANCE == "Fluoxetine" | 
                                 full_data$SUBSTANCE__1 == "Fluoxetine" | 
                                 full_data$SUBSTANCE__2 == "Fluoxetine",
                               1, 0)
full_data$Alprazolam <- ifelse(full_data$SUBSTANCE == "Alprazolam" | 
                                 full_data$SUBSTANCE__1 == "Alprazolam" | 
                                 full_data$SUBSTANCE__2 == "Alprazolam",
                               1, 0)
full_data$Tramadol <- ifelse(full_data$SUBSTANCE == "Tramadol" | 
                               full_data$SUBSTANCE__1 == "Tramadol" | 
                               full_data$SUBSTANCE__2 == "Tramadol",
                             1, 0)
full_data$Amitriptyline <- ifelse(full_data$SUBSTANCE == "Amitriptyline" | 
                                    full_data$SUBSTANCE__1 == "Amitriptyline" | 
                                    full_data$SUBSTANCE__2 == "Amitriptyline",
                                  1, 0)
full_data$Butalbital <- ifelse(full_data$SUBSTANCE == "Butalbital" | 
                                 full_data$SUBSTANCE__1 == "Butalbital" | 
                                 full_data$SUBSTANCE__2 == "Butalbital",
                               1, 0)
full_data$Amphetamine <- ifelse(full_data$SUBSTANCE == "Amphetamines" | 
                                   full_data$SUBSTANCE__1 == "Amphetamines" | 
                                   full_data$SUBSTANCE__2 == "Amphetamines",
                                 1, 0)
full_data$Opiates <- ifelse(full_data$SUBSTANCE == "Opiates" | 
                              full_data$SUBSTANCE__1 == "Opiates" | 
                              full_data$SUBSTANCE__2 == "Opiates",
                            1, 0)
full_data$Doxylamine <- ifelse(full_data$SUBSTANCE == "Doxylamine" | 
                                 full_data$SUBSTANCE__1 == "Doxylamine" | 
                                 full_data$SUBSTANCE__2 == "Doxylamine",
                               1, 0)
full_data$Paroxetine <- ifelse(full_data$SUBSTANCE == "Paroxetine" | 
                                 full_data$SUBSTANCE__1 == "Paroxetine" | 
                                 full_data$SUBSTANCE__2 == "Paroxetine",
                               1, 0)
full_data$Doxepin <- ifelse(full_data$SUBSTANCE == "Doxepin" | 
                              full_data$SUBSTANCE__1 == "Doxepin" | 
                              full_data$SUBSTANCE__2 == "Doxepin",
                            1, 0)
full_data$Atenolol <- ifelse(full_data$SUBSTANCE == "Atenolol" | 
                               full_data$SUBSTANCE__1 == "Atenolol" | 
                               full_data$SUBSTANCE__2 == "Atenolol",
                             1, 0)
full_data$Diazepam <- ifelse(full_data$SUBSTANCE == "Diazepam" | 
                               full_data$SUBSTANCE__1 == "Diazepam" | 
                               full_data$SUBSTANCE__2 == "Diazepam",
                             1, 0)
full_data$Venlafaxine <- ifelse(full_data$SUBSTANCE == "Venlafaxine" | 
                                  full_data$SUBSTANCE__1 == "Venlafaxine" | 
                                  full_data$SUBSTANCE__2 == "Venlafaxine",
                                1, 0)
full_data$Buprenorphine <- ifelse(full_data$SUBSTANCE == "Buprenorphine" | 
                                    full_data$SUBSTANCE__1 == "Buprenorphine" | 
                                    full_data$SUBSTANCE__2 == "Buprenorphine",
                                  1, 0)
full_data$Carfentanil <- ifelse(full_data$SUBSTANCE == "Carfentanil" | 
                                  full_data$SUBSTANCE__1 == "Carfentanil" | 
                                  full_data$SUBSTANCE__2 == "Carfentanil",
                                1, 0)
full_data$Furanylfentanyl <- ifelse(full_data$SUBSTANCE == "Furanylfentanyl" | 
                                      full_data$SUBSTANCE__1 == "Furanylfentanyl" | 
                                      full_data$SUBSTANCE__2 == "Furanylfentanyl",
                                    1, 0)
full_data$Ephedrine <- ifelse(full_data$SUBSTANCE == "Ephedrine" | 
                                full_data$SUBSTANCE__1 == "Ephedrine" | 
                                full_data$SUBSTANCE__2 == "Ephedrine",
                              1, 0)
full_data$Naloxone <- ifelse(full_data$SUBSTANCE == "Naloxone" | 
                               full_data$SUBSTANCE__1 == "Naloxone" | 
                               full_data$SUBSTANCE__2 == "Naloxone",
                             1, 0)
full_data$Temazepam <- ifelse(full_data$SUBSTANCE == "Temazepam" | 
                                full_data$SUBSTANCE__1 == "Temazepam" | 
                                full_data$SUBSTANCE__2 == "Temazepam",
                              1, 0)
full_data$Gabapentin <- ifelse(full_data$SUBSTANCE == "Gabapentin" | 
                                 full_data$SUBSTANCE__1 == "Gabapentin" | 
                                 full_data$SUBSTANCE__2 == "Gabapentin",
                               1, 0)
full_data$Metoprolol <- ifelse(full_data$SUBSTANCE == "Metoprolol" | 
                                 full_data$SUBSTANCE__1 == "Metoprolol" | 
                                 full_data$SUBSTANCE__2 == "Metoprolol",
                               1, 0)
full_data$Aminoclonazepam <- ifelse(full_data$SUBSTANCE == "7-Aminoclonazepam" | 
                                      full_data$SUBSTANCE__1 == "7-Aminoclonazepam" | 
                                      full_data$SUBSTANCE__2 == "7-Aminoclonazepam",
                                    1, 0)
full_data$Pseudoephedrine <- ifelse(full_data$SUBSTANCE == "Pseudoephedrine" | 
                                      full_data$SUBSTANCE__1 == "Pseudoephedrine" | 
                                      full_data$SUBSTANCE__2 == "Pseudoephedrine",
                                    1, 0)
full_data$Phentermine <- ifelse(full_data$SUBSTANCE == "Phentermine" | 
                                  full_data$SUBSTANCE__1 == "Phentermine" | 
                                  full_data$SUBSTANCE__2 == "Phentermine",
                                1, 0)
full_data$Benzodiazepines <- ifelse(full_data$SUBSTANCE == "Benzodiazepines" | 
                                      full_data$SUBSTANCE__1 == "Benzodiazepines" | 
                                      full_data$SUBSTANCE__2 == "Benzodiazepines",
                                    1, 0)
full_data$Promethazine <- ifelse(full_data$SUBSTANCE == "Promethazine" | 
                                   full_data$SUBSTANCE__1 == "Promethazine" | 
                                   full_data$SUBSTANCE__2 == "Promethazine",
                                 1, 0)
full_data$Hydromorphone <- ifelse(full_data$SUBSTANCE == "Hydromorphone" | 
                                    full_data$SUBSTANCE__1 == "Hydromorphone" | 
                                    full_data$SUBSTANCE__2 == "Hydromorphone",
                                  1, 0)
full_data$Diphenhydramine <- ifelse(full_data$SUBSTANCE %in% c("Diphenhydramine","Diphenhydrdamine") | 
                                      full_data$SUBSTANCE__1 %in% c("Diphenhydramine","Diphenhydrdamine") | 
                                      full_data$SUBSTANCE__2 %in% c("Diphenhydramine","Diphenhydrdamine"),
                                    1, 0)
full_data$Nordiazepam <- ifelse(full_data$SUBSTANCE == "Nordiazepam" | 
                                  full_data$SUBSTANCE__1 == "Nordiazepam" | 
                                  full_data$SUBSTANCE__2 == "Nordiazepam",
                                1, 0)
full_data$Zolpidem <- ifelse(full_data$SUBSTANCE == "Zolpidem" | 
                               full_data$SUBSTANCE__1 == "Zolpidem" | 
                               full_data$SUBSTANCE__2 == "Zolpidem",
                             1, 0)
full_data$Mirtazepine <- ifelse(full_data$SUBSTANCE == "Mirtazepine" | 
                                  full_data$SUBSTANCE__1 == "Mirtazepine" | 
                                  full_data$SUBSTANCE__2 == "Mirtazepine",
                                1, 0)
full_data$Chlorpheniramine <- ifelse(full_data$SUBSTANCE == "Chlorpheniramine" | 
                                       full_data$SUBSTANCE__1 == "Chlorpheniramine" | 
                                       full_data$SUBSTANCE__2 == "Chlorpheniramine",
                                     1, 0)
full_data$Lorazepam <- ifelse(full_data$SUBSTANCE == "Lorazepam" | 
                                full_data$SUBSTANCE__1 == "Lorazepam" | 
                                full_data$SUBSTANCE__2 == "Lorazepam",
                              1, 0)
full_data$Oxazepam <- ifelse(full_data$SUBSTANCE == "Oxazepam" | 
                               full_data$SUBSTANCE__1 == "Oxazepam" | 
                               full_data$SUBSTANCE__2 == "Oxazepam",
                             1, 0)
full_data$Bupropion <- ifelse(full_data$SUBSTANCE == "Bupropion" | 
                                full_data$SUBSTANCE__1 == "Bupropion" | 
                                full_data$SUBSTANCE__2 == "Bupropion",
                              1, 0)
full_data$Trazodone <- ifelse(full_data$SUBSTANCE == "Trazodone" | 
                                full_data$SUBSTANCE__1 == "Trazodone" | 
                                full_data$SUBSTANCE__2 == "Trazodone",
                              1, 0)
full_data$Methylphenidate <- ifelse(full_data$SUBSTANCE == "Methylphenidate" | 
                                      full_data$SUBSTANCE__1 == "Methylphenidate" | 
                                      full_data$SUBSTANCE__2 == "Methylphenidate",
                                    1, 0)
full_data$Ibuprofen <- ifelse(full_data$SUBSTANCE == "Ibuprofen" | 
                                full_data$SUBSTANCE__1 == "Ibuprofen" | 
                                full_data$SUBSTANCE__2 == "Ibuprofen",
                              1, 0)
full_data$Acetaminophen <- ifelse(full_data$SUBSTANCE == "Acetaminophen" | 
                                    full_data$SUBSTANCE__1 == "Acetaminophen" | 
                                    full_data$SUBSTANCE__2 == "Acetaminophen",
                                  1, 0)
full_data$Demoxepam <- ifelse(full_data$SUBSTANCE == "Demoxepam" | 
                                full_data$SUBSTANCE__1 == "Demoxepam" | 
                                full_data$SUBSTANCE__2 == "Demoxepam",
                              1, 0)
full_data$Pregabalin <- ifelse(full_data$SUBSTANCE == "Pregabalin" | 
                                 full_data$SUBSTANCE__1 == "Pregabalin" | 
                                 full_data$SUBSTANCE__2 == "Pregabalin",
                               1, 0)
full_data$Chlordiazepoxide <- ifelse(full_data$SUBSTANCE == "Chlordiazepoxide" | 
                                       full_data$SUBSTANCE__1 == "Chlordiazepoxide" | 
                                       full_data$SUBSTANCE__2 == "Chlordiazepoxide",
                                     1, 0)
full_data$Phenobarbital <- ifelse(full_data$SUBSTANCE == "Phenobarbital" | 
                                    full_data$SUBSTANCE__1 == "Phenobarbital" | 
                                    full_data$SUBSTANCE__2 == "Phenobarbital",
                                  1, 0)
full_data$Aripiprazole <- ifelse(full_data$SUBSTANCE == "Aripiprazole" | 
                                   full_data$SUBSTANCE__1 == "Aripiprazole" | 
                                   full_data$SUBSTANCE__2 == "Aripiprazole",
                                 1, 0)
full_data$Hydroxyzine <- ifelse(full_data$SUBSTANCE == "Hydroxyzine" | 
                                  full_data$SUBSTANCE__1 == "Hydroxyzine" | 
                                  full_data$SUBSTANCE__2 == "Hydroxyzine",
                                1, 0)
full_data$Amlodipine <- ifelse(full_data$SUBSTANCE == "Amlodipine" | 
                                 full_data$SUBSTANCE__1 == "Amlodipine" | 
                                 full_data$SUBSTANCE__2 == "Amlodipine",
                               1, 0)
full_data$Amiodarone <- ifelse(full_data$SUBSTANCE == "Amiodarone" | 
                                 full_data$SUBSTANCE__1 == "Amiodarone" | 
                                 full_data$SUBSTANCE__2 == "Amiodarone",
                               1, 0)
full_data$Sertraline <- ifelse(full_data$SUBSTANCE == "Sertraline" | 
                                 full_data$SUBSTANCE__1 == "Sertraline" | 
                                 full_data$SUBSTANCE__2 == "Sertraline",
                               1, 0)
full_data$Carisoprodol <- ifelse(full_data$SUBSTANCE == "Carisoprodol" | 
                                   full_data$SUBSTANCE__1 == "Carisoprodol" | 
                                   full_data$SUBSTANCE__2 == "Carisoprodol",
                                 1, 0)
full_data$CarbonMonoxide <- ifelse(full_data$SUBSTANCE == "59.3% CO",
                                   1, 0)
# run from here
full_data$Acrylfentanyl <- ifelse(full_data$SUBSTANCE == "Acrylfentanyl",
                                   1, 0)
full_data$Acetylfentanyl <- ifelse(full_data$SUBSTANCE == "Acrylfentanyl" |
                                     full_data$SUBSTANCE__2 == "Acrylfentanyl",
                                  1, 0)
full_data$U47700 <- ifelse(full_data$SUBSTANCE == "U47700",
                                  1, 0)
full_data$Ocfentanil <- ifelse(full_data$SUBSTANCE == "Ocfentanil",
                           1, 0)
full_data$`4ANPP` <- ifelse(full_data$SUBSTANCE == "4-ANPP"|
                               full_data$SUBSTANCE__2 == "4-ANPP",
                               1, 0)
full_data$Cyclopropylfentanyl <- ifelse(full_data$SUBSTANCE__1 == "Cyclopropylfentanyl",
                               1, 0)
full_data$Paraflourobutyryfentanyl <- ifelse(full_data$SUBSTANCE__1 == "Paraflourobutyryfentanyl",
                                        1, 0)
full_data$Cocaethylene <- ifelse(full_data$SUBSTANCE__1 == "Cocaethylene",
                                             1, 0)
full_data$Norbuprenorphine <- ifelse(full_data$SUBSTANCE__1 == "Norbuprenorphine" |
                                       full_data$SUBSTANCE__2 == "Norbuprenorphine",
                                 1, 0)

#Extract years and months
full_data$Death_Year = substr(full_data$D.O.D.,1,nchar(full_data$D.O.D.)-6)
full_data$Death_Month = substr(full_data$D.O.D.,6,nchar(full_data$D.O.D.)-3)
full_data$Death_Month = ifelse(full_data$Death_Month == "01", "January",
                               ifelse(full_data$Death_Month == "02", "February",
                                      ifelse(full_data$Death_Month == "03", "March",
                                             ifelse(full_data$Death_Month == "04", "April",
                                                    ifelse(full_data$Death_Month == "05", "May",
                                                           ifelse(full_data$Death_Month == "06", "June",
                                                                  ifelse(full_data$Death_Month == "07", "July",
                                                                         ifelse(full_data$Death_Month == "08", "August", 
                                                                                ifelse(full_data$Death_Month == "09", "September", 
                                                                                       ifelse(full_data$Death_Month == "10", "October",
                                                                                              ifelse(full_data$Death_Month == "11", "November",
                                                                                                     ifelse(full_data$Death_Month == "12", "December",
                                                                                                            full_data$Death_Month))))))))))))

#Create new data frames for each year of data
#2013
year_2013 <- full_data %>% 
  select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
         Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
         Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
         Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamine, Opiates,    
         Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
         Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
         Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
         Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
         Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
         Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
         Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
         Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
         Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
  filter(YEAR == 2013)
#2014
year_2014 <- full_data %>% 
  select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
         Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
         Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
         Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamine, Opiates,    
         Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
           Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
         Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
         Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
         Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
         Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
         Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
         Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
         Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
  filter(YEAR == 2014)
#2015
year_2015 <- full_data %>% 
  select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
         Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
         Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
         Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamine, Opiates,    
         Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
           Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
         Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
         Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
         Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
         Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
         Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
         Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
         Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
  filter(YEAR == 2015)
#2016
year_2016 <- full_data %>% 
  select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
         Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
         Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
         Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamine, Opiates,    
         Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
           Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
         Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
         Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
         Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
         Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
         Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
         Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
         Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
  filter(YEAR == 2016)
#2017
year_2017 <- full_data %>% 
  select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
         Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
         Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
         Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamine, Opiates,    
         Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
           Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
         Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
         Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
         Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
         Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
         Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
         Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
         Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
  filter(YEAR == 2017)
# #2018
# year_2018 <- full_data %>% 
#   select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
#          Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
#          Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
#          Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamines, Opiates,    
#          Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
#          Diphenhydrdamine,  Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
#          Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
#          Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
#          Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
#          Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
#          Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
#          Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
#          Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
#   filter(YEAR == 2018)
# #2019
# year_2019 <- full_data %>% 
#   select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
#          Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
#          Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
#          Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamines, Opiates,    
#          Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
#          Diphenhydrdamine,  Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
#          Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
#          Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
#          Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
#          Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
#          Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
#          Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
#          Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
#   filter(YEAR == 2019)
# #2020
# year_2020 <- full_data %>% 
#   select(YEAR, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,      
#          Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
#          Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
#          Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamines, Opiates,    
#          Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
#          Diphenhydrdamine,  Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
#          Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
#          Aminoclonazepam, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
#          Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
#          Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
#          Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
#          Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
#          Amiodarone,        Sertraline,        Carisoprodol,      CarbonMonoxide) %>% 
#   filter(YEAR == 2020)

#Substances data frame for use in creating year data frames
substances<- c("Heroin", "Dextromethorphan", "Morphine", "Cocaine", "Hydrocodone", "Methadone",
               "Oxycodone", "Olanzapine", "Fentanyl", "Oxymorphone", "Codeine", "Clonazepam",
               "Methamphetamine", "Citalopram", "Quetiapine", "Cyclobenzaprine", "Ethanol", 
               "Cannabinoids","Fluoxetine","Alprazolam","Tramadol","Amitriptyline","Butalbital",
               "Amphetamine","Opiates","Doxylamine","Paroxetine","Doxepin","Atenolol", "Diazepam",
                "Venlafaxine","Buprenorphine","Carfentanil","Furanylfentanyl",
               "Ephedrine","Naloxone","Temazepam","Gabapentin","Metoprolol","Aminoclonazepam",
               "Pseudoephedrine","Phentermine","Benzodiazepines","Promethazine","Hydromorphone",
               "Diphenhydramine","Nordiazepam","Zolpidem","Chlorpheniramine","Lorazepam","Oxazepam",
               "Bupropion","Trazodone","Methylphenidate","Ibuprofen","Acetaminophen","Demoxepam",
               "Pregabalin","Chlordiazepoxide","Phenobarbital","Aripiprazole","Hydroxyzine",
               "Amlodipine","Amiodarone","Sertraline","Carisoprodol","CarbonMonoxide", "Mirtazepine")

#New data frames containing the substance, year, and counts
#2013
# year_2013_counts <- as.data.frame(substances)
# year_2013_counts$year <- 2013
# year_2013_counts$count <- colSums(year_2013[,2:ncol(year_2013)],  na.rm = TRUE)
# #2014
# year_2014_counts <- as.data.frame(substances)
# year_2014_counts$year <- 2014
# year_2014_counts$count <- colSums(year_2014[,2:ncol(year_2014)],  na.rm = TRUE)
# #2015
# year_2015_counts <- as.data.frame(substances)
# year_2015_counts$year <- 2015
# year_2015_counts$count <- colSums(year_2015[,2:ncol(year_2015)],  na.rm = TRUE)
# #2016
# year_2016_counts <- as.data.frame(substances)
# year_2016_counts$year <- 2016
# year_2016_counts$count <- colSums(year_2016[,2:ncol(year_2016)],  na.rm = TRUE)
# #2017
# year_2017_counts <- as.data.frame(substances)
# year_2017_counts$year <- 2017
# year_2017_counts$count <- colSums(year_2017[,2:ncol(year_2017)], na.rm = TRUE)
# # #2018
# # year_2018_counts <- as.data.frame(substances)
# # year_2018_counts$year <- 2017
# # year_2018_counts$count <- colSums(year_2018[,2:ncol(year_2018)], na.rm = TRUE)
# # #2019
# # year_2019_counts <- as.data.frame(substances)
# # year_2019_counts$year <- 2019
# # year_2019_counts$count <- colSums(year_2019[,2:ncol(year_2019)], na.rm = TRUE)
# # #2020
# # year_2020_counts <- as.data.frame(substances)
# # year_2020_counts$year <- 2020
# # year_2020_counts$count <- colSums(year_2017[,2:ncol(year_2020)], na.rm = TRUE)
# 
# YEAR_DATA <- as.data.frame(rbind(year_2013_counts, year_2014_counts, year_2015_counts,
#                                  year_2016_counts, year_2017_counts, year_2018_counts))
# # YEAR_DATA <- as.data.frame(rbind(YEAR_DATA, year_2018_counts))
# # YEAR_DATA <- as.data.frame(rbind(YEAR_DATA, year_2019_counts))
# # YEAR_DATA <- as.data.frame(rbind(YEAR_DATA, year_2020_counts))
# 
# colnames(YEAR_DATA) <- c("substance", "year", "count")

# How to calculate the year data stuff efficiently
YEAR_DATA <- mergedData %>%
  mutate(YEAR = year(D.O.D.)) %>%
  select(YEAR,Heroin:Norbuprenorphine,Methoxyacetylfentanyl:`4ANPP`) %>%
  group_by(YEAR) %>%
  #summarise_at(vars(Heroin:THC), sum, na.rm = TRUE) %>%
  summarise_at(vars(Heroin:`4ANPP`), sum, na.rm = TRUE) %>%
  #summarise_at(vars(Heroin:Norbuprenorphine), sum, na.rm = TRUE) %>%
  mutate(#Heroin = ifelse(YEAR == 2018,12,Heroin),
    #Cannabinoids = ifelse(YEAR == 2018,28,Cannabinoids),
    `4ANPP` = ifelse(YEAR == 2018,58,`4ANPP`)) %>%
  #select(-c(THC,`6MAM`)) %>%
  #select(-c(`6MAM`)) %>%
  select(-`4-ANPP`) %>%
  tidyr::gather("substance","count",Heroin:`4ANPP`) %>%
  rename(year = YEAR) %>%
  mutate(substance = ifelse(substance == "Amphetamines","Amphetamine",substance))
YEAR_DATA <- YEAR_DATA %>%
  filter(!substance %in% c("Fentanyl Analog +","Fentanyl Analog -","Not Tested")) #%>%

YEAR_DATA2 <- YEAR_DATA %>%
  left_join(categories,by=c("substance"="DrugName")) %>%
  group_by(Category,year) %>%
  summarise(count= sum(count,na.rm = T))
YEAR_DATA2 <- as.data.frame(na.omit(YEAR_DATA2))

#Save year data for use in line plot
save(YEAR_DATA,file="YEAR_DATA.Rdata")

#Query full_data, group by sex, race, and age grouping
Dem_Query <- as.data.frame(sqldf("SELECT SEX, RACE, GROUPING, SUM(Heroin) AS Heroin,
                                 SUM(Dextromethorphan) AS Dextromethorphan, SUM(Morphine) AS Morphine, SUM(Cocaine) AS Cocaine, SUM(Hydrocodone) AS Hydrocodone,      
                                 SUM(Methadone) AS Methadone, SUM(Oxycodone) AS Oxycodone, SUM(Olanzapine) AS Olanzapine, SUM(Fentanyl) AS Fentanyl, 
                                 SUM(Oxymorphone) AS Oxymorphone, SUM(Codeine) AS Codeine, SUM(Clonazepam) AS Clonazepam,
                                 SUM(Methamphetamine) AS Methamphetamine, SUM(Citalopram) AS Citalopram, SUM(Quetiapine) AS Quetiapine, SUM(Cyclobenzaprine) AS Cyclobenzaprine,
                                 SUM(Ethanol) AS Ethanol, SUM(Cannabinoids) AS Cannabinoids, SUM(Fluoxetine) AS Fluoxetine, SUM(Alprazolam) AS Alprazolam, 
                                 SUM(Tramadol) AS Tramadol, SUM(Amitriptyline) AS Amitriptyline, SUM(Butalbital) AS Butalbital, SUM(Amphetamine) AS Amphetamine, 
                                 SUM(Opiates) AS Opiates, SUM(Doxylamine) AS Doxylamine, SUM(Paroxetine) AS Paroxetine, SUM(Doxepin) AS Doxepin, SUM(Atenolol) AS Atenolol,
                                 SUM(Diazepam) AS Diazepam, SUM(Venlafaxine) AS Venlafaxine, SUM(Buprenorphine) AS Buprenorphine,
                                 SUM(Carfentanil) AS Carfentanil, SUM(Furanylfentanyl) AS Furanylfentanyl, SUM(Ephedrine) AS Ephedrine, SUM(Naloxone) AS Naloxone,
                                 SUM(Temazepam) AS Temazepam, SUM(Gabapentin) AS Gabapentin, SUM(Metoprolol) AS Metoprolol, SUM('Aminoclonazepam') AS '7-Aminoclonazepam',
                                 SUM(Pseudoephedrine) AS Pseudoephedrine, SUM(Phentermine) AS Phentermine, SUM(Benzodiazepines) AS Benzodiazepines, SUM(Promethazine) AS Promethazine,
                                 SUM(Hydromorphone) AS Hydromorphone, SUM(Diphenhydramine) AS Diphenhydramine, SUM(Nordiazepam) AS Nordiazepam, SUM(Zolpidem) AS Zolpidem,
                                 SUM(Mirtazepine) AS Mirtazepine, SUM(Chlorpheniramine) AS Chlorpheniramine, SUM(Lorazepam) AS Lorazepam, SUM(Oxazepam) AS Oxazepam,
                                 SUM(Bupropion) AS Bupropion, SUM(Trazodone) AS Trazodone, SUM(Methylphenidate) AS Methylphenidate, SUM(Ibuprofen) AS Ibuprofen,
                                 SUM(Acetaminophen) AS Acetaminophen, SUM(Demoxepam) AS Demoxepam, SUM(Pregabalin) AS Pregabalin, SUM(Chlordiazepoxide) AS Chlordiazepoxide,
                                 SUM(Phenobarbital) AS Phenobarbital, SUM(Aripiprazole) AS Aripiprazole, SUM(Hydroxyzine) AS Hydroxyzine, SUM(Amlodipine) AS Amlodipine, 
                                 SUM(Amiodarone) AS Amiodarone, SUM(Sertraline) AS Sertraline, SUM(Carisoprodol) AS Carisoprodol,  SUM(CarbonMonoxide) AS 'Carbon Monoxide',
                                  
                                

    
SUM(Methoxyacetylfentanyl) as Methoxyacetylfentanyl,
SUM(Ocfentanil) as Ocfentanil, SUM(Tetrahydrofuranfentanyl) as  Tetrahydrofuranfentanyl, 
SUM(`3Methylfentanyl`) as `3Methylfentanyl`, SUM(Acetylfentanyl) as Acetylfentanyl,          
SUM(Acrylfentanyl) as Acrylfentanyl,   SUM(Betahydroxythiofentanyl) as Betahydroxythiofentanyl,   
SUM(Butyrylfentanyl) as Butyrylfentanyl,         SUM(Parafluorobutyrylfentanyl) as Parafluorobutyrylfentanyl,
SUM(`U47700`) as `U47700`,                   SUM(Cyclopropylfentanyl) as Cyclopropylfentanyl,     
SUM(`4ANPP`) as  `4ANPP`
                                 FROM mergedData
                                 GROUP BY SEX, RACE, GROUPING")) 

#Transform from wide to long format
library(tidyr)
library(tidyverse)
# DEM_DATA <- gather(Dem_Query, substance, count, Heroin, Dextromethorphan, Morphine, Cocaine, Hydrocodone,  Methadone, Oxycodone, Olanzapine, Fentanyl, Oxymorphone, Codeine, Clonazepam,
#                    Methamphetamine, Citalopram, Quetiapine, Cyclobenzaprine, Ethanol, Cannabinoids, Fluoxetine,        
#                    Alprazolam, Tramadol, Amitriptyline, Butalbital, Amphetamines, Opiates,    
#                    Doxylamine,        Paroxetine,        Doxepin,         Atenolol,          Diazepam,         
#                    Diphenhydrdamine,  Venlafaxine,       Buprenorphine,     Carfentanil,       Furanylfentanyl,  
#                    Ephedrine,         Naloxone,          Temazepam,         Gabapentin,        Metoprolol,       
#                    `7-Aminoclonazepam`, Pseudoephedrine,   Phentermine,       Benzodiazepines,   Promethazine,     
#                    Hydromorphone,     Diphenhydramine,   Nordiazepam,       Zolpidem,          Mirtazepine,      
#                    Chlorpheniramine,  Lorazepam,         Oxazepam,          Bupropion,         Trazodone,  
#                    Methylphenidate,   Ibuprofen,         Acetaminophen,     Demoxepam,         Pregabalin,       
#                    Chlordiazepoxide,  Phenobarbital,     Aripiprazole,      Hydroxyzine,       Amlodipine,       
#                    Amiodarone,        Sertraline,        Carisoprodol,      `Carbon Monoxide`)
DEM_DATA <- gather(Dem_Query, substance, count, Heroin:`4ANPP`)

#change age grouping labels
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)== "(-1,20]"]<- "Under 21"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(20,30]"]<- "21-30"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(30,40]"]<- "31-40"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(40,50]"]<- "41-50"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(50,60]"]<- "51-60"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(60,80]"]<- "61-80"
levels(DEM_DATA$GROUPING)[levels(DEM_DATA$GROUPING)=="(80,100]"]<- "Over 80"

#Save DEM_DATA file for bar chart
save(DEM_DATA,file="DEM_DATA.Rdata")
#save(DEM_DATA,file="DEM_DATA_FINAL.Rdata")


##Geocoding Addresses:
#String manipulation of addresses
library(ggmap)
library(stringr)
library(RCurl)
library(rjson)
library(dplyr)

# full_data_m <- data.frame(full_data[,7],ncol=1)
# colnames(full_data_m) <- c("Addresses","Ncol")
# full_data_m <- full_data_m$Addresses
full_data_m <- matrix(full_data[,7],ncol=1)
full_data$streetAddress<-apply(full_data_m, 1, 
                             function(x) unlist(strsplit(x, split=","))[1])

# mergedData$streetAddress <- apply(full_data_m, 1, 
#                                   function(x) unlist(strsplit(x, split=","))[1])

full_data$AddressIncOrig <- paste(full_data$streetAddress, ", ", full_data$`CITY/TWP/CO OF INCIDENT`, ", ", "Ohio")
#mergedData$AddressIncOrig <- paste(mergedData$streetAddress, ", ", mergedData$`CITY/TWP/CO OF INCIDENT`, ", ", "Ohio")

save.image(file="FullData_April17.Rdata")

#Geocode addresses and 
geocodeBatch_attempt <- function(address) {
  #URL for batch requests
  URL=paste("http://open.mapquestapi.com/geocoding/v1/batch?key=", "pmU6RzBqItq3qoHv0vR0MdAFpiNVTlcc", 
            "&location=", address,"&outFormat='json'",sep = "") 
  
  URL <- gsub(" ", "+", URL)
  data<-getURL(URL)
  data <- fromJSON(data)
  
  p<-sapply(data$results,function(x){
    if(length(x$locations)==0){
      c(NA,NA)
    } else{
      c(x$locations[[1]]$displayLatLng$lat, x$locations[[1]]$displayLatLng$lng)   
    }})
  return(t(p))
}

temp <- {}
result <- {}
for(i in 1:nrow(full_data)){
  temp <- geocodeBatch_attempt(full_data$AddressIncOrig[i])
  result <- rbind(result, temp)
}
rm(temp)
result <- as.data.frame(result)
colnames(result) <- c("lat","lon")
full_data <- cbind(full_data,result)

#write.csv(result, file="AddressLatLong.csv")

#Append geocode results to full_data table
full_data$lon <- result$lon
full_data$lat <- result$lat
full_data$address <- result$address


#export the data
save(full_data, file = "full_data.Rdata")
#save(full_data, file = "full_data_FINAL.Rdata")

#Subset a dataframe of all of the cities and townships
city_township.d<-full_data %>% 
  mutate(YEAR = year(D.O.D.)) %>%
  filter(`CITY/TWP OF DEATH` %in% c("West Chester", "Wayne Twp.", "Trenton", "St. Clair Twp.", "Somerville", "Sharonville", "Seven Mile", "Ross Twp.", "Ross", "Reily Twp.", "Oxford Twp.", "Oxford", "New Miami", "Morgan Twp.", "Middletown", "Madison Twp.", "Liberty Twp.", "Lemon Twp.", "Hanover Twp.", "Hamilton", "Forest Park", "Fairfield Twp.", "Fairfield")
  )
city_township.i<- full_data %>% 
  mutate(YEAR = year(D.O.D.)) %>%
  filter(`CITY/TWP/CO OF INCIDENT` %in% c("West Chester", "Wayne Twp.", "Trenton", "St. Clair Twp.", "Somerville", "Sharonville", "Seven Mile", "Ross Twp.", "Ross", "Reily Twp.", "Oxford Twp.", "Oxford", "New Miami", "Morgan Twp.", "Middletown", "Madison Twp.", "Liberty Twp.", "Lemon Twp.", "Hanover Twp.", "Hamilton", "Forest Park", "Fairfield Twp.", "Fairfield") 
  )
save(city_township.d, file = "city_township.d.RData")
save(city_township.i, file = "city_township.i.RData")

# Finally, wrap up everything to load a single file into the app
save(full_data, YEAR_DATA,YEAR_DATA2,city_township.d,
     city_township.i,corp,townships,DEM_DATA,
     categories,file="coronerAppFinal2.Rdata")
