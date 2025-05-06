#Script for creating the Depression cohort
#Note: in this script, we exclude severe mental illness last,
#As it is project-specific how these IDs are dealt with.
#The generation of tables in this script can seem verbose, but it is designed
#To be "plug and play" at any point in the QC, as required for different projects.


library(aurum)
library(tidyverse)


####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- "CPRD_depression_data"
yaml <- "C:/Users/dhand/.ssh/.aurum.yaml"

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()

#Get the depression codes
codes <- codesets$getAllCodeSetVersion(v = "12/07/2024")

#Check only depression codes have been grabbed
dep_qof_codes <- codes$depression_qof
dep_all_codes <- codes$depression_broad
dep_sympts_codes <- codes$add_sympts_dep

#NOTRUN: updating the broad depression to contain all codes in the QOF depression. 

#bring local
#dep_qof_codes_local <- dep_qof_codes %>% collect()
#dep_all_codes_local <- dep_all_codes %>% collect()

#Add the missing code to the df.
#dep_all <- dep_all_codes_local %>% 
#  bind_rows(tibble(medcodeid = as.character(3071801000006112), depression_broad_cat = "diagnosis"))

#dep_all %>%  codesets$loadMedCodeSet(category = "depression_broad_cat", name = "depression_broad_ext", version = "03/05/2025")

#inner join the code lists and check length to make sure all codes are present
#dep_check <- codesets$getAllCodeSetVersion("03/05/2025")
#dep_all_codes_ext <- dep_check$depression_broad_ext

#Check for missing codes
#missing_code <- dep_qof_codes %>% anti_join(dep_all_codes_ext) %>% count() #0 - all QOF codes are covered by the broad depression now.

#Check they are all listed as the same type. 
#code_match_length <- dep_all_codes_ext %>% inner_join(dep_qof_codes) %>% 
#  filter(depression_broad_ext_cat == depression_qof_cat) %>% count()

####Baseline QC####
#First check the quality of the data
cprd$tables$patient %>% count() #3984014 - total patient count in download
cprd$tables$patient %>% filter(acceptable ==1) %>% count() #3984014
cprd$tables$patient %>% filter(patienttypeid ==3) %>% count() #3984014

#Next, create a list of IDs to exclude according to practice exclusions
analysis <- cprd$analysis("all_patid")

#44 practices
practice_exclusion_ids <- cprd$tables$patient %>% 
  filter(pracid == "20024" | pracid == "20036" |pracid == "20091" |pracid == "20171" | pracid == "20178" |pracid == "20202" | pracid == "20254" | pracid == "20389" |pracid == "20430" |pracid == "20452" |
           pracid == "20469" | pracid == "20487" | pracid == "20552" | pracid == "20554" | pracid == "20640" | pracid == "20717" | pracid == "20734" | pracid == "20737" | pracid == "20740" | pracid == "20790" |
           pracid == "20803" | pracid == "20822" | pracid == "20868" | pracid == "20912" | pracid == "20996" | pracid == "21001" | pracid == "21015" | pracid == "21078" | pracid == "21112" | pracid == "21118" |
           pracid == "21172" | pracid == "21173" | pracid == "21277" | pracid == "21281" | pracid == "21331" | pracid == "21334" | pracid == "21390" | pracid == "21430" | pracid == "21444" | pracid == "21451" |
           pracid == "21529" | pracid == "21553" | pracid == "21558" | pracid == "21585") %>%
  analysis$cached("practice_exclusion_ids")

practice_exclusion_ids %>% count() #48912

#Exclude individuals with unknown gender
gender_exclusion_ids <- cprd$tables$patient %>% 
  filter(gender==3) %>%
  analysis$cached("gender_exclusion_ids") #349

#Check numbers
gender_exclusion_ids %>% count()
cprd$tables$patient %>% anti_join(practice_exclusion_ids, by="patid") %>% anti_join(gender_exclusion_ids, by="patid") %>% count() #3934754

####Define the depression cohort####
analysis <- cprd$analysis("dh")

#Extract depression symptoms
raw_depression_sympts_medcodes <- cprd$tables$observation %>%
  inner_join(dep_sympts_codes, by="medcodeid") %>%
  analysis$cached("raw_depression_sympts_medcodes", indexes=c("patid", "obsdate", "add_sympts_dep_cat"))

#Broad depression extended
raw_broad_depression_medcodes <- cprd$tables$observation %>%
  inner_join(dep_all_codes_ext, by="medcodeid") %>%
  analysis$cached("raw_broad_depression_ext_medcodes", indexes=c("patid", "obsdate", "depression_broad_ext_cat"))
  
#And QOF depression
raw_qof_depression_medcodes <- cprd$tables$observation %>%
  inner_join(dep_qof_codes, by="medcodeid") %>%
  analysis$cached("raw_qof_depression_medcodes", indexes=c("patid", "obsdate", "dep_qof_cat"))

#Re-load the broad and QOF codes
analysis <- cprd$analysis("dh")
raw_qof_depression_medcodes <- raw_qof_depression_medcodes %>% analysis$cached("raw_qof_depression_medcodes")
raw_broad_depression_ext_medcodes <- raw_broad_depression_medcodes %>% analysis$cached("raw_broad_depression_ext_medcodes")

####Ensure valid dates####
analysis <- cprd$analysis("all_patid")

#Load table which contains the least of GP, death, and deregistration dates: 
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")
valid_dates %>% count()


analysis <- cprd$analysis("dh")

#Find individuals with a valid QOF code
qof_ids <- raw_qof_depression_medcodes %>%
  inner_join(valid_dates, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
  distinct(patid) %>%
  analysis$cached("qof_depression_medcodes", unique_indexes="patid")

#And a valid broad depression id. 
broad_ext_medcodes <- raw_broad_depression_ext_medcodes %>%
  inner_join(valid_dates, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
  analysis$cached("raw_broad_depression_medcodes_valid", unique_indexes="patid")

broad_ext_ids <- raw_broad_depression_ext_medcodes %>%
  inner_join(valid_dates, by="patid") %>%
  distinct(patid) %>%
  analysis$cached("raw_broad_depression_ext_ids_valid", unique_indexes="patid")


####Recap 1####
analysis <- cprd$analysis("dh")
qof_ids <- qof_ids %>% analysis$cached("qof_depression_ids")

analysis <- cprd$analysis("all_patid")
practice_exclusion_ids <- practice_exclusion_ids %>% analysis$cached("practice_exclusion_ids")
gender_exclusion_ids <- gender_exclusion_ids %>% analysis$cached("gender_exclusion_ids")

#So far:
practice_exclusion_ids %>% count() #48912 removed by practice

cprd$tables$patient %>% anti_join(practice_exclusion_ids, by="patid") %>% 
  anti_join(gender_exclusion_ids, by="patid") %>% count() #3934754 after removing gender exclusions

cprd$tables$patient %>%  anti_join(practice_exclusion_ids, by="patid") %>% 
  anti_join(gender_exclusion_ids, by="patid") %>% inner_join(qof_ids) %>% count() #3934754

#Save the above table for later.
depression_cohort_ids <- qof_ids %>%
  anti_join(practice_exclusion_ids, by="patid") %>%
  anti_join(gender_exclusion_ids, by="patid") %>%
  analysis$cached("depression_cohort_ids_incsmi", unique_indexes="patid")


#Reload the codes
raw_qof_depression_medcodes <- raw_qof_depression_medcodes %>% analysis$cached("raw_qof_depression_medcodes")
raw_broad_depression_medcodes_valid <- raw_broad_depression_medcodes_valid %>% analysis$cached("raw_broad_depression_medcodes_valid")

#Now, we use the broad code list, and remove individuals where their first code is
#not a diagnostic code.
#obstype = 4 is filtered as these are family history codes.




  
