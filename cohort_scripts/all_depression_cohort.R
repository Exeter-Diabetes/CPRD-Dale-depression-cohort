library(aurum)
library(tidyverse)

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

#bring local
dep_qof_codes_local <- dep_qof_codes %>% collect()
dep_all_codes_local <- dep_all_codes %>% collect()

#Check all QOF codes are in broad depression codes.
nrow(dep_qof_codes_local) #195 codes
nrow(dep_all_codes_local) #63 additional codes.

#Check the overlap between code lists.
matching_codes <- dep_all_codes_local %>% filter(medcodeid %in% dep_qof_codes_local$medcodeid)
nrow(matching_codes) #194

missing_code <- dep_qof_codes_local %>%
  filter(!(medcodeid %in% dep_all_codes_local$medcodeid))

print(missing_code)



#1 code is missing - which one?

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

#Define the depression cohort
#first, calculate a list of individuals who have at least 1 QOF depression code that is diagnostic.

analysis <- cprd$analysis("dh")

raw_depression_sympts_medcodes <- cprd$tables$observation %>%
  inner_join(dep_sympts_codes, by="medcodeid") %>%
  analysis$cached("raw_depression_sympts_medcodes", indexes=c("patid", "obsdate", "add_sympts_dep_cat"))

  
