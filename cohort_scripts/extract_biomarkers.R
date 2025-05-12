library(devtools)
library(EHRBiomarkr)
library(aurum)
library(tidyverse)

cprdenvname <- ""
yaml <- ""

#Open the CPRD data
cprd <- CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion("31/10/2021")


#Load cohort and extract raw data:
analysis = cprd$analysis("all_patid")
biomarkers <- c("weight", "height", "bmi", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr")


for (i in biomarkers) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_medcodes")
  
  data <- cprd$tables$observation %>%
    inner_join(codes[[i]], by="medcodeid") %>%
    analysis$cached(raw_tablename, indexes=c("patid", "obsdate", "testvalue", "numunitid"))
  
  assign(raw_tablename, data)
  
}

#HbA1c
raw_hba1c_medcodes <- cprd$tables$observation %>%
  inner_join(codes$hba1c, by="medcodeid") %>%
  analysis$cached("raw_hba1c_medcodes", indexes=c("patid", "obsdate", "testvalue", "numunitid"))

biomarkers <- c("weight", "height", "bmi", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr")

#Clean biomarkers
analysis = cprd$analysis("all_patid")

#Load in the valid dates file
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")

#Reload all the tables
for (i in biomarkers) {
  raw_tablename <- paste0("raw_", i, "_medcodes")
  
  # Fetch the data using the dynamic name
  data <- data %>% analysis$cached(raw_tablename)
  
  # Assign it to the variable named as raw_tablename
  assign(raw_tablename, data)
}

#Cleaning loop - runs EHRBiomarkR and checks for valid dates.
for (i in biomarkers) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_medcodes")
  clean_tablename <- paste0("clean_", i, "_medcodes")
  
  data <- get(raw_tablename) %>%
    clean_biomarker_values(testvalue, i) %>%
    clean_biomarker_units(numunitid, i) %>%
    
    group_by(patid,obsdate) %>%
    summarise(testvalue=mean(testvalue, na.rm=TRUE)) %>%
    ungroup() %>%
    
    inner_join(valid_dates, by="patid") %>%
    filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
    
    select(patid, date=obsdate, testvalue) %>%
    
    analysis$cached(clean_tablename, indexes=c("patid", "date", "testvalue"))
  
  assign(clean_tablename, data)
  
}
