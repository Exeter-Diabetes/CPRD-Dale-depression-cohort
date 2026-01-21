library(aurum)
library(tidyverse)

cprdenvname <- ""
yaml <- ""

#Depression hosp. codes
dep_hosp <- data.frame(icd10_code = c("F32", "F33"))
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)


analysis <- cprd$analysis("all_patid")
#Load in the censoring table
censoring <- censoring %>% analysis$cached("death_end_dat")


codesets <- cprd$codesets()
codesets$loadICD10CodeSet(codeSetDf = dep_hosp, name = "icd10_depression", version = "28/10/25", colname = "icd10_code")

#Load the codeset
hosp_dep <- codesets$getAllCodeSetVersion("28/10/25")
hosp_dep_codes <- hosp_dep$icd10_depression

#New name
raw_tablename <- "raw_depression_hospitalisation_icd10"

#Get data
data <- cprd$tables$hesDiagnosisEpi %>%
  inner_join(hosp_dep_codes, sql_on="LHS.ICD LIKE CONCAT(icd10,'%')") %>%
  analysis$cached(raw_tablename, indexes=c("patid", "epistart"))

#Assign to new var
assign(raw_tablename, data)

raw_primary_dep_hospitalisation_icd10 <- raw_depression_hospitalisation_icd10 %>% 
  filter(d_order == 1) %>% analysis$cached("raw_primary_dep_hospitalisation_icd10", indexes = c("patid"))

raw_secondary_dep_hospitalisation_icd10 <- raw_depression_hospitalisation_icd10 %>% 
  filter(d_order != 1) %>% analysis$cached("raw_secondary_dep_hospitalisation_icd10", indexes = c("patid"))


#ICD-10
raw_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  select(patid, date=epistart, code=ICD) %>%
  mutate(source="hes_icd10")

raw_secondary_dep_hospitalisation_icd10 <- raw_secondary_dep_hospitalisation_icd10 %>%
  select(patid, date=epistart, code=ICD) %>%
  mutate(source="hes_icd10")

#Clean date ranges and add in index dates
raw_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code)  %>% analysis$cached("clean_primary_dep_hospitalisation")

raw_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code) %>% analysis$cached("clean_secondary_dep_hospitalisation")


