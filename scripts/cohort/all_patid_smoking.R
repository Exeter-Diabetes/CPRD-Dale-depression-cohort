library(tidyverse)
library(aurum)
rm(list=ls())

####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- ""
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()
codes = codesets$getAllCodeSetVersion(v = "31/10/2021")

#Load analysis
analysis = cprd$analysis("all_patid")
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")


#Extract smoking medcodes
raw_smoking_medcodes <- cprd$tables$observation %>%
  inner_join(codes$smoking, by="medcodeid") %>%
  inner_join(codes$qrisk2_smoking, by="medcodeid") %>%
  analysis$cached("raw_smoking_medcodes", indexes=c("patid", "obsdate"))


clean_smoking_medcodes <- raw_smoking_medcodes %>%
  inner_join(valid_dates, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
  select(patid, date=obsdate, medcodeid, smoking_cat, qrisk2_smoking_cat, testvalue, numunitid) %>%
  distinct() %>%
  analysis$cached("clean_smoking_medcodes", indexes=c("patid", "date", "smoking_cat", "qrisk2_smoking_cat"))

#Next load in the index dates and merge.
analysis <- cprd$analysis("all_patid")
clean_smoking_medcodes <- clean_smoking_medcodes %>% analysis$cached("clean_smoking_medcodes")

analysis <- cprd$analysis("dh")
index_dates <- index_dates %>% analysis$cached("first_date_depbroad_code")

analysis <- cprd$analysis("dh")
pre_index_date_smoking_codes <- index_dates %>%
  inner_join(clean_smoking_medcodes, by="patid") %>%
  filter(datediff(date, depbroad_diag_date)<=7) %>%
  analysis$cached("pre_index_date_smoking_merge", indexes=c("patid", "depbroad_diag_date", "smoking_cat", "qrisk2_smoking_cat"))

pre_index_date_smoking_codes <- pre_index_date_smoking_codes %>% rename(index_date = "depbroad_diag_date")

smoker_ever <- pre_index_date_smoking_codes %>%
  filter(smoking_cat=="Active smoker") %>%
  distinct(patid, index_date) %>%
  mutate(smoked_ever_flag=1L)

most_recent_code <- pre_index_date_smoking_codes %>%
  distinct(patid, index_date, date, smoking_cat) %>%
  group_by(patid, index_date) %>%
  filter(date==max(date, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-date) %>%
  mutate(fill=TRUE) %>%
  pivot_wider(id_cols=c(patid, index_date), names_from=smoking_cat, values_from=fill, values_fill=list(fill=FALSE)) %>%
  mutate(smoking_cat=ifelse(`Active smoker`==1 & `Non-smoker`==0 & `Ex-smoker`==0, "Active smoker",
                            ifelse(`Active smoker`==0 & `Ex-smoker`==1, "Ex-smoker",
                                   ifelse(`Active smoker`==0 & `Ex-smoker`==0 & `Non-smoker`==1, "Non-smoker", NA)))) %>%
  select(patid, index_date, most_recent_code=smoking_cat) %>%
  analysis$cached("smoking_interim_1", indexes=c("patid", "index_date"))


next_most_recent_code <- pre_index_date_smoking_codes %>%
  distinct(patid, index_date, date, smoking_cat) %>%
  group_by(patid, index_date) %>%
  filter(date!=max(date, na.rm=TRUE)) %>%
  filter(date==max(date, na.rm=TRUE)) %>%
  ungroup() %>%
  select(-date) %>%
  mutate(fill=TRUE) %>%
  pivot_wider(id_cols=c(patid, index_date), names_from=smoking_cat, values_from=fill, values_fill=list(fill=FALSE)) %>%
  mutate(smoking_cat=ifelse(`Active smoker`==1 & `Non-smoker`==0 & `Ex-smoker`==0, "Active smoker",
                            ifelse(`Active smoker`==0 & `Ex-smoker`==1, "Ex-smoker",
                                   ifelse(`Active smoker`==0 & `Ex-smoker`==0 & `Non-smoker`==1, "Non-smoker", NA)))) %>%
  select(patid, index_date, next_most_recent_code=smoking_cat) %>%
  analysis$cached("smoking_interim_2", indexes=c("patid", "index_date"))



index_dates <- index_dates %>% rename(index_date=depbroad_diag_date)

smoking_cat <- index_dates %>%
  left_join(smoker_ever, by=c("patid", "index_date")) %>%
  left_join(most_recent_code, by=c("patid", "index_date")) %>%
  left_join(next_most_recent_code, by=c("patid", "index_date")) %>%
  mutate(most_recent_code=coalesce(most_recent_code, next_most_recent_code),
         smoking_cat=ifelse(most_recent_code=="Non-smoker" & !is.na(smoked_ever_flag) & smoked_ever_flag==1, "Ex-smoker", most_recent_code)) %>%
  select(-c(most_recent_code, next_most_recent_code, smoked_ever_flag)) %>%
  analysis$cached("smoking_interim_3", indexes=c("patid", "index_date"))

qrisk2_smoking_cat <- pre_index_date_smoking_codes %>%
  filter(datediff(index_date, date) <= 1826) %>%
  group_by(patid, index_date) %>%
  filter(date==max(date, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(qrisk2_smoking=ifelse(is.na(testvalue) | qrisk2_smoking_cat==1 | medcodeid==1780396011 | (!is.na(numunitid) & numunitid!=39 & numunitid!=118 & numunitid!=247 & numunitid!=98 & numunitid!=120 & numunitid!=237 & numunitid!=478 & numunitid!=1496 & numunitid!=1394 & numunitid!=1202 & numunitid!=38), qrisk2_smoking_cat,
                               ifelse(testvalue<10, 2L,
                                      ifelse(testvalue<20, 3L, 4L)))) %>%
  analysis$cached("smoking_interim_4", indexes=c("patid", "index_date"))

smoking <- index_dates %>%
  left_join(smoking_cat, by=c("patid", "index_date")) %>%
  left_join(qrisk2_smoking_cat, by=c("patid", "index_date")) %>%
  mutate(qrisk2_smoking_cat_uncoded=case_when(qrisk2_smoking_cat==0 ~ "Non-smoker",
                                              qrisk2_smoking_cat==1 ~ "Ex-smoker",
                                              qrisk2_smoking_cat==2 ~ "Light smoker",
                                              qrisk2_smoking_cat==3 ~ "Moderate smoker",
                                              qrisk2_smoking_cat==4 ~ "Heavy smoker")) %>%
  analysis$cached("smoking", indexes=c("patid", "index_date"))
  
