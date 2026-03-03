library(aurum)
library(tidyverse)
library(survival)

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
clean_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code)  %>% analysis$cached("clean_primary_dep_hospitalisation")

clean_secondary_dep_hospitalisation_icd10 <- raw_secondary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code) %>% analysis$cached("clean_secondary_dep_hospitalisation")


#Next, we get the first date at which someone has a primary or secondary diagnosis 
#after index, And create an indicator where events prior to index are not allowed.
analysis <- cprd$analysis("dh_depsev")
index <- index %>% analysis$cached("AD_informed_index_dates")

#Keep dates for everyone not just AD users
# If I remove the people where an SSRI isn't the first drug, I lose so many people
# Is this surprising? probably not... but it's a lot higher in people with a 
# code for depression

index <- index %>% filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1) #I remove ~1/3rd of people but ~1/2 of hospitalisations?

first_primary_hospitalisation <- #Most are after index thank god
  clean_primary_dep_hospitalisation_icd10 %>%
  inner_join(index, by = "patid") %>% 
  mutate(after_index = ifelse(date > AD_informed_index_date, 1, 0)) %>%
  group_by(patid) %>% 
  window_order(date) %>%
  filter(row_number() == 1) %>%
  ungroup %>% 
  select(patid, date, code, after_index) %>% analysis$cached("primary_hospitalisation")


first_secondary_hospitalisation <- clean_secondary_dep_hospitalisation_icd10 %>%
  inner_join(index, by = "patid") %>% 
  mutate(after_index = ifelse(date > AD_informed_index_date, 1, 0)) %>%
  group_by(patid) %>% 
  window_order(date) %>%
  filter(row_number() == 1) %>%
  ungroup %>% 
  select(patid, date, code, after_index) %>% analysis$cached("secondary_hospitalisation")


