#Script for creating the Depression cohort
#Note: in this script, we exclude severe mental illness last,
#As it is project-specific how these IDs are dealt with.
#The generation of tables in this script can seem verbose, but it is designed 
#To track exclusion of individuals due to specific causes, and 
#To be "plug and play" at any point in the QC, as required for different projects.


library(aurum)
library(tidyverse)
rm(list = ls())

####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- "CPRD_depression_data"
yaml <- "C:/Users/dhand/.ssh/.aurum.yaml"


cprdenvname <- "analysis"
yaml <- "~/.aurum.yaml"


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
dep_all_codes_local <- dep_all_codes %>% collect()

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
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")

#So far:
practice_exclusion_ids %>% count() #48912 removed by practice

cprd$tables$patient %>% anti_join(practice_exclusion_ids, by="patid") %>% 
  anti_join(gender_exclusion_ids, by="patid") %>% count() #3934754 after removing gender exclusions

cprd$tables$patient %>%  anti_join(practice_exclusion_ids, by="patid") %>% 
  anti_join(gender_exclusion_ids, by="patid") %>% inner_join(qof_ids) %>% count() #3376433

#Save the above table for later.
depression_cohort_ids <- qof_ids %>%
  anti_join(practice_exclusion_ids, by="patid") %>%
  anti_join(gender_exclusion_ids, by="patid") %>%
  analysis$cached("depression_cohort_ids_incsmi", unique_indexes="patid")


#Reload the codes
analysis <- cprd$analysis("dh")
raw_qof_depression_medcodes <- raw_qof_depression_medcodes %>% analysis$cached("raw_qof_depression_medcodes")
raw_broad_depression_medcodes_valid <- raw_broad_depression_medcodes_valid %>% analysis$cached("raw_broad_depression_medcodes_valid")

#Next, we filter out codes which are indicative of family history, and find the first
#Broad depression date. 

raw_broad_depression_medcodes_valid %>% distinct(patid) %>% count() #3581936

first_date_depbroad_code <- raw_broad_depression_medcodes_valid %>% 
  filter(obstypeid != 4) %>% group_by(patid) %>% 
  summarise(depbroad_diag_date = min(obsdate)) %>%
  analysis$cached("first_date_depbroad_code", unique_index="patid")


first_date_depbroad_code %>% count() #3,581,691

#Count number of individuals with a broad depression code.
first_date_depbroad_code %>% filter(is.na(depbroad_diag_date)) %>% count() #No first codes have a missing date - awesome. 


#Borrow some of Alex's code - it worked great previously, but need to be adapted to including QF codes.

#Load in medcode terms <-
analysis <- cprd$analysis("medcode")
medcode_term <- term %>% analysis$cached("term")

analysis <- cprd$analysis("dh")
term_long <- raw_broad_depression_medcodes_valid %>%
  inner_join(first_date_depbroad_code, by="patid") %>%
  filter(obstypeid != 4) %>%
  filter(!is.na(depbroad_diag_date)) %>%
  filter(obsdate == depbroad_diag_date) %>%
  inner_join(medcode_term, by="medcodeid") %>%
  select(patid, term) %>% analysis$cached("broad_depression_medcode_terms")

term_long %>% summarise(n = n_distinct(patid)) #3581691

term_wide <- term_long %>%
  group_by(patid) %>%
  mutate(term_id = row_number()) %>%
  pivot_wider(names_from = term_id, values_from = term, names_prefix = "term_") %>%
  analysis$cached("broad_depression_medcode_terms_wide")

analysis <- cprd$analysis("dh")
term_wide <- term_wide %>% analysis$cached("broad_depression_medcode_terms_wide")

#From Alex's script - categorisation of the depression codes.
#Also need to include individuals who have a QOF code, as extracted earlier.
dephist_cat_values <- c("review", "review_invite", "history", "chronic_diag", "persistent_diag", 
                        "partial_remission", "recurrent_diag", "remission", 
                        "resolved", "trd_diag")
dephist_add_codes <- c("1680571000006118")

#Reload cached tables
analysis <- cprd$analysis("dh")
qof_dep <- qof_dep %>% analysis$cached("qof_depression_ids")
raw_broad_depression_medcodes_valid <- raw_broad_depression_medcodes_valid %>% analysis$cached("raw_broad_depression_medcodes_valid")
first_date_depbroad_code <- first_date_depbroad_code %>% analysis$cached("first_date_depbroad_code")

#This code just makes sure that this method of joining works fine in MySQL.. I 
#Have no idea how well it translates. 
#raw_broad_depression_medcodes_valid %>%
#  left_join(qof_dep %>% mutate(qof_depression = 1), by = "patid") %>%
#  mutate(qof_depression = coalesce(qof_depression, 0)) %>% filter(qof_depression == 1) %>% 
#  distinct(patid) %>% count()


#Code borrowed from Alex - basically forms the start of the depression cohort table
tmp_data <- raw_broad_depression_medcodes_valid %>%
  inner_join(first_date_depbroad_code, by = "patid") %>%
  filter(!is.na(depbroad_diag_date)) %>%
  filter(obsdate == depbroad_diag_date) %>%
  group_by(patid) %>%
  summarise(
    diagdepb_code_first = as.numeric(any(depression_broad_ext_cat == "diagnosis")),
    dephist_cat_first = as.numeric(any(depression_broad_ext_cat %in% dephist_cat_values)),
    dephist_add_codes = as.numeric(any(medcodeid %in% dephist_add_codes)),
    obs_count = n()
  ) %>%
  ungroup() %>%
  mutate(preexisting = as.numeric(dephist_cat_first == 1 | dephist_add_codes == 1 | diagdepb_code_first == 0)) %>% 
  analysis$cached("depression_cohort_interim_1")

#Add in a QOF column, indicating whether people have a QOF code
tmp_data_qof <- tmp_data %>% left_join(qof_dep %>% mutate(qof_depression = 1), by = "patid") %>%
    mutate(qof_depression = coalesce(qof_depression, 0)) %>% analysis$cached("depression_cohort_interim_2")



tmp_data_qof %>% filter(preexisting == 1) %>% count() 
#593018 People (~15%) have evidence of Depression pre-existing their diagnosis date. 

tmp_data_qof %>% filter(qof_depression == 1) %>% count() #3417137 - Same number 
#as the raw QOF IDs file - makes sense as no other filtering has been passed yet.

#merge the depression cohort interim_2 and the wide table to make the "first broad deptype" file.
max_obs_count <- tmp_data_qof %>%
  summarize(max_obs_count = max(obs_count)) %>%
  pull(max_obs_count)
max_obs_count #still 46. 

term_columns <- paste0("term_", 1:as.numeric(max_obs_count))

analysis = cprd$analysis("dh")
tmp_data_qof <- tmp_data_qof %>% analysis$cached("depression_cohort_interim_2")
term_wide <- term_wide %>% analysis$cached("broad_depression_medcode_terms_wide")

#Add terms to the file.
first_depbroad_type <- tmp_data_qof %>%
  inner_join(term_wide %>% select(patid, all_of(term_columns)), by = "patid") %>%
  analysis$cached("first_broad_depression_type", unique_index = "patid")

first_depbroad_type %>% count() #3581691 - good.

#Had a look at the file.. on second thought, I don't think ill add all the initial
#Codes in the cohort file, as it's too long, but the file above will still be 
#useful.
cohort_table_v1 <- first_date_depbroad_code %>% inner_join(tmp_data_qof, by = "patid") %>%
  analysis$cached("depression_cohort_interim_3")

#To Recap: 
#We've created the depression table with individuals where:
# - We have an index date for first episode of depression that pertains to a "diagnosis". 
# - We can filter in people who have a QOF code
# - We can filter out people who have evidence of depression before diag.
# - We can filter out individuals from a merged practice
# - We can filter out people who have undetermined (==3) gender

#We still need to:
# - Calculate Diagnosis age using date of birth, which also needs to be calculated
# - Add sex, HES linkage availability, IMD, ethnicity.
# - Add pre-existing SMI, and ethnicity where available. 

#For age, we first need to define date of birth, which is done using min_dob and
#Earliest medcode.
analysis <- cprd$analysis("all_patid")
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")

valid_dates_short <- valid_dates %>% select(patid, min_dob)

dob <- cprd$tables$observation %>%
  inner_join(valid_dates_short, by="patid") %>%
  filter(obsdate>=min_dob) %>%
  group_by(patid) %>%
  summarise(earliest_medcode=min(obsdate, na.rm=TRUE)) %>%
  ungroup() %>%
  analysis$cached("earliest_medcode", unique_indexes="patid")
  
