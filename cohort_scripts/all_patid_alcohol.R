library(tidyverse)
library(aurum)
rm(list=ls())

cprdenvname <- ""
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets <- cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "31/10/2021")


cohort_prefix <- "dh"


analysis = cprd$analysis(cohort_prefix)


############################################################################################

# Pull out all raw code instances and cache with 'all_patid' prefix

analysis = cprd$analysis("all_patid")

raw_alcohol_medcodes <- cprd$tables$observation %>%
  inner_join(codes$alcohol, by="medcodeid") %>%
  analysis$cached("raw_alcohol_medcodes", indexes=c("patid", "obsdate"))


############################################################################################
analysis = cprd$analysis("all_patid")
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")


# Clean: remove if before DOB or after lcd/deregistration/death, and re-cache


clean_alcohol_medcodes <- raw_alcohol_medcodes %>%
  inner_join(valid_dates, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
  select(patid, date=obsdate, alcohol_cat) %>%
  distinct() %>%
  analysis$cached("clean_alcohol_medcodes", indexes=c("patid", "date", "alcohol_cat"))


############################################################################################

# Find alcohol status according to algorithm at index dates

## Get index dates

analysis = cprd$analysis(cohort_prefix)

index_dates <- index_dates %>% analysis$cached("first_date_depbroad_code")
index_dates <- index_dates %>% rename(index_date=depbroad_diag_date)


## Join with alcohol codes on patid and retain codes before index date or up to 7 days after
pre_index_date_alcohol_codes <- index_dates %>%
  inner_join(clean_alcohol_medcodes, by="patid") %>%
  filter(datediff(date, index_date)<=7) %>%
  analysis$cached("pre_index_date_alcohol_merge", indexes=c("patid", "index_date", "alcohol_cat"))

## Find if ever previously a 'harmful' drinker (category 3)
harmful_drinker_ever <- pre_index_date_alcohol_codes %>%
  filter(alcohol_cat=="AlcoholConsumptionLevel3") %>%
  distinct(patid, index_date) %>%
  mutate(harmful_drinker_ever=1L)

## Find most recent code
### If different categories on same day, use highest
most_recent_code <- pre_index_date_alcohol_codes %>%
  mutate(alcohol_cat_numeric = ifelse(alcohol_cat=="AlcoholConsumptionLevel0", 0L,
                                      ifelse(alcohol_cat=="AlcoholConsumptionLevel1", 1L,
                                             ifelse(alcohol_cat=="AlcoholConsumptionLevel2", 2L,
                                                    ifelse(alcohol_cat=="AlcoholConsumptionLevel3", 3L, NA))))) %>%
  group_by(patid, index_date) %>%
  filter(date==max(date, na.rm=TRUE)) %>%
  filter(alcohol_cat_numeric==max(alcohol_cat_numeric, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("alcohol_interim_1", indexes=c("patid", "index_date"))

## Pull together
alcohol_cat <- index_dates %>%
  left_join(harmful_drinker_ever, by=c("patid", "index_date")) %>%
  left_join(most_recent_code, by=c("patid", "index_date")) %>%
  mutate(alcohol_cat_numeric=ifelse(!is.na(harmful_drinker_ever) & harmful_drinker_ever==1, 3L, alcohol_cat_numeric),
         
         alcohol_cat=case_when(
           alcohol_cat_numeric==0 ~ "None",
           alcohol_cat_numeric==1 ~ "Within limits",
           alcohol_cat_numeric==2 ~ "Excess",
           alcohol_cat_numeric==3 ~ "Harmful"
         )) %>%
  select(patid, index_date, alcohol_cat) %>%
  analysis$cached("alcohol", indexes=c("patid", "index_date"))
