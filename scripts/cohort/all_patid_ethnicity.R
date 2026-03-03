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

# Find ethnicity from HES codes

hes_ethnicity <- cprd$tables$hesPatient %>% 
  filter(!is.na(gen_ethnicity)) %>% 
  mutate(hes_5cat_ethnicity=case_when(gen_ethnicity==1 ~ 0L,
                                      gen_ethnicity==2 ~ 2L,
                                      gen_ethnicity==3 ~ 2L,
                                      gen_ethnicity==4 ~ 2L,
                                      gen_ethnicity==5 ~ 1L,
                                      gen_ethnicity==6 ~ 1L,
                                      gen_ethnicity==7 ~ 1L,
                                      gen_ethnicity==8 ~ 1L, # Other Asian -> South Asian as per GP codes
                                      gen_ethnicity==9 ~ 3L,
                                      gen_ethnicity==10 ~ 4L,    
                                      gen_ethnicity==11 ~ 3L),
         hes_16cat_ethnicity=case_when(gen_ethnicity==1 ~ 1L,     # Unspecified white -> British as per GP codes
                                       gen_ethnicity==2 ~ 12L,
                                       gen_ethnicity==3 ~ 13L,
                                       gen_ethnicity==4 ~ 14L,
                                       gen_ethnicity==5 ~ 8L,
                                       gen_ethnicity==6 ~ 9L,
                                       gen_ethnicity==7 ~ 10L,
                                       gen_ethnicity==8 ~ 11L,
                                       gen_ethnicity==9 ~ 15L,
                                       gen_ethnicity==10 ~ 7L,    # Unspecified mixed -> Other mixed as per GP records
                                       gen_ethnicity==11 ~ 16L),
         hes_qrisk2_ethnicity=case_when(gen_ethnicity==1 ~ 1L,     # Unspecified white -> British as per GP codes
                                        gen_ethnicity==2 ~ 6L,
                                        gen_ethnicity==3 ~ 7L,
                                        gen_ethnicity==4 ~ 9L,
                                        gen_ethnicity==5 ~ 2L,
                                        gen_ethnicity==6 ~ 3L,
                                        gen_ethnicity==7 ~ 4L,
                                        gen_ethnicity==8 ~ 5L,
                                        gen_ethnicity==9 ~ 8L,
                                        gen_ethnicity==10 ~ 9L,    
                                        gen_ethnicity==11 ~ 9L)) %>%
  analysis$cached("hes_ethnicity",unique_indexes="patid", indexes=c("hes_5cat_ethnicity", "hes_16cat_ethnicity", "hes_qrisk2_ethnicity"))


############################################################################################

## Combine GP and HES ethnicity
## Set 16- and QRISK-category ethnicity to missing if directly conflicts with 5-category ethnicity, before using HES ethnicity for missing
ethnicity <- cprd$tables$patient %>%
  select(patid) %>%
  left_join(gp_5cat_ethnicity, by="patid") %>%
  left_join(gp_16cat_ethnicity, by="patid") %>%
  left_join(gp_qrisk2_ethnicity, by="patid") %>%
  left_join((hes_ethnicity %>% select(-pracid)), by="patid") %>%
  
  mutate(gp_16cat_ethnicity=ifelse((gp_5cat_ethnicity==0 & (gp_16cat_ethnicity==8 | gp_16cat_ethnicity==9 | gp_16cat_ethnicity==10 | gp_16cat_ethnicity==11 | gp_16cat_ethnicity==12 | gp_16cat_ethnicity==13 | gp_16cat_ethnicity==14 | gp_16cat_ethnicity==15)) |
                                     (gp_5cat_ethnicity==1 & (gp_16cat_ethnicity==1 | gp_16cat_ethnicity==2 | gp_16cat_ethnicity==3 | gp_16cat_ethnicity==4 | gp_16cat_ethnicity==5 | gp_16cat_ethnicity==12 | gp_16cat_ethnicity==13 | gp_16cat_ethnicity==14 | gp_16cat_ethnicity==15)) |
                                     (gp_5cat_ethnicity==2 & (gp_16cat_ethnicity==1 | gp_16cat_ethnicity==2 | gp_16cat_ethnicity==3 | gp_16cat_ethnicity==6 | gp_16cat_ethnicity==8 | gp_16cat_ethnicity==9 | gp_16cat_ethnicity==10 | gp_16cat_ethnicity==11 | gp_16cat_ethnicity==15)), NA, gp_16cat_ethnicity),
         
         gp_qrisk2_ethnicity=ifelse((gp_5cat_ethnicity==0 & (gp_qrisk2_ethnicity==2 | gp_qrisk2_ethnicity==3 | gp_qrisk2_ethnicity==4 | gp_qrisk2_ethnicity==5 | gp_qrisk2_ethnicity==6 | gp_qrisk2_ethnicity==7 | gp_qrisk2_ethnicity==8)) |
                                      (gp_5cat_ethnicity==1 & (gp_qrisk2_ethnicity==1 | gp_qrisk2_ethnicity==6 | gp_qrisk2_ethnicity==7 | gp_qrisk2_ethnicity==8)) |
                                      (gp_5cat_ethnicity==2 & (gp_qrisk2_ethnicity==1 | gp_qrisk2_ethnicity==2 | gp_qrisk2_ethnicity==3 | gp_qrisk2_ethnicity==4 | gp_qrisk2_ethnicity==5 | gp_qrisk2_ethnicity==8)), NA, gp_qrisk2_ethnicity),
         
         ethnicity_5cat=coalesce(gp_5cat_ethnicity, hes_5cat_ethnicity),
         ethnicity_16cat=ifelse(!is.na(hes_5cat_ethnicity) & hes_5cat_ethnicity==ethnicity_5cat, coalesce(gp_16cat_ethnicity, hes_16cat_ethnicity), gp_16cat_ethnicity),
         ethnicity_qrisk2=ifelse(!is.na(hes_5cat_ethnicity) & hes_5cat_ethnicity==ethnicity_5cat, coalesce(gp_qrisk2_ethnicity, hes_qrisk2_ethnicity), gp_qrisk2_ethnicity)) %>%
  
  mutate(ethnicity_qrisk2=ifelse(is.na(ethnicity_qrisk2), 0L, ethnicity_qrisk2)) %>%
  
  select(patid, ethnicity_5cat, ethnicity_16cat, ethnicity_qrisk2) %>%
  
  analysis$cached("ethnicity", unique_indexes="patid")
