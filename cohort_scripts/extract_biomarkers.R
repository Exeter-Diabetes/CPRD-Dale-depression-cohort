library(EHRBiomarkr)
library(aurum)
library(tidyverse)

cprdenvname <- "CPRD_depression_data"
yaml <- ""

#Open the CPRD data
cprd <- CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion("31/10/2021")

#List of biomarkers to extract
biomarkers <- c("weight", "height", "bmi", "hdl", "triglyceride", "creatinine_blood", "ldl", "alt", "ast", "totalcholesterol", "dbp", "sbp", "acr")


#Load cohort and extract raw data:
analysis = cprd$analysis("all_patid")

for (i in biomarkers) {
  
  print(i)
  
  raw_tablename <- paste0("raw_", i, "_medcodes")
  
  data <- cprd$tables$observation %>%
    inner_join(codes[[i]], by="medcodeid") %>%
    analysis$cached(raw_tablename, indexes=c("patid", "obsdate", "testvalue", "numunitid"))
  
  assign(raw_tablename, data)
  
}

