#Script for defining ethnicity based on KY's script: 
#https://github.com/Exeter-Diabetes/CPRD-Cohort-scripts/blob/main/all_patid_ethnicity.R


####Required packages####
library(aurum)
library(tidyverse)
rm(list = ls())

#Connect to the MySQL database
cprdenvname <- "CPRD_depression_data"
yaml <- ""

#Get the ethnicity codes
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "01/06/2024")
names(codes) #Contains only ethnicity

#setup analysis
analysis = cprd$analysis("all_patid")

####5-cat ethnicity####
raw_gp_ethnicity <- cprd$tables$observation %>% 
  inner_join(codes$ethnicity_5cat, by="medcodeid") %>%
  select(patid, obsdate, medcodeid) %>%
  analysis$cached("raw_gp_ethnicity", indexes=c("patid", "obsdate", "medcodeid"))

#Clean 5-cat ethnicity
gp_5cat_ethnicity <- raw_gp_ethnicity %>% 
  inner_join(codes$ethnicity_5cat, by="medcodeid") %>%
  
  filter(ethnicity_5cat_cat!=5 | is.na(ethnicity_5cat_cat)) %>%                         # remove 'missing' codes
  
  group_by(patid, ethnicity_5cat_cat) %>%
  summarise(eth_code_count=n(),                                                     # code count per person per ethnicity category
            latest_date_per_cat=max(obsdate, na.rm=TRUE)) %>%                       
  ungroup() %>%
  
  group_by(patid) %>%
  filter(eth_code_count==max(eth_code_count, na.rm=TRUE)) %>%                       # only keep categories with most counts
  filter(n()==1 | latest_date_per_cat==max(latest_date_per_cat, na.rm=TRUE)) %>%    # keep if 1 row per person, or if on latest date
  filter(n()==1) %>%                                                                # keep if 1 row per person
  ungroup() %>% 
  
  select(patid, gp_5cat_ethnicity=ethnicity_5cat_cat) %>%
  analysis$cached("gp_5cat_ethnicity",unique_indexes="patid", indexes="gp_5cat_ethnicity")

####16-cat ethnicity####
gp_16cat_ethnicity <- raw_gp_ethnicity %>% 
  inner_join(codes$ethnicity_16cat, by="medcodeid") %>%
  
  filter(ethnicity_16cat_cat!=17 | is.na(ethnicity_16cat_cat)) %>%                  # remove 'missing' codes
  
  group_by(patid, ethnicity_16cat_cat) %>%
  summarise(eth_code_count=n(),                                                     # code count per person per ethnicity category
            latest_date_per_cat=max(obsdate, na.rm=TRUE)) %>%                       
  ungroup() %>%
  
  group_by(patid) %>%
  filter(eth_code_count==max(eth_code_count, na.rm=TRUE)) %>%                       # only keep categories with most counts
  filter(n()==1 | latest_date_per_cat==max(latest_date_per_cat, na.rm=TRUE)) %>%    # keep if 1 row per person, or if on latest date
  filter(n()==1) %>%                                                                # keep if 1 row per person
  ungroup() %>% 
  
  select(patid, gp_16cat_ethnicity=ethnicity_16cat_cat) %>%
  analysis$cached("gp_16cat_ethnicity",unique_indexes="patid", indexes="gp_16cat_ethnicity")

####Q-risk ethnicity####
gp_qrisk2_ethnicity <- raw_gp_ethnicity %>% 
  inner_join(codes$qrisk2_ethnicity, by="medcodeid") %>%
  
  filter(qrisk2_ethnicity_cat!=0 | is.na(qrisk2_ethnicity_cat)) %>%                 # remove 'missing' codes
  
  group_by(patid, qrisk2_ethnicity_cat) %>%
  summarise(eth_code_count=n(),                                                     # code count per person per ethnicity category
            latest_date_per_cat=max(obsdate, na.rm=TRUE)) %>%                       
  ungroup() %>%
  
  group_by(patid) %>%
  filter(eth_code_count==max(eth_code_count, na.rm=TRUE)) %>%                       # only keep categories with most counts
  filter(n()==1 | latest_date_per_cat==max(latest_date_per_cat, na.rm=TRUE)) %>%    # keep if 1 row per person, or if on latest date
  filter(n()==1) %>%                                                                # keep if 1 row per person
  ungroup() %>% 
  
  select(patid, gp_qrisk2_ethnicity=qrisk2_ethnicity_cat) %>%
  analysis$cached("gp_qrisk2_ethnicity",unique_indexes="patid", indexes="gp_qrisk2_ethnicity")

