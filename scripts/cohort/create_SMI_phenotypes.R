library(aurum)
library(tidyverse)
library(data.table)
library(fuzzyjoin)


#Read in the SMI codelist
smi <- fread("")
smi_refined <- fread("")

#Upload to CPRD
smi <- smi %>%
  mutate(across(everything(), as.character))

smi_refined <- smi_refined %>% filter(!is.na(medcodeid))
  mutate(across(everything(), as.character))

smi_full <- smi %>% inner_join(smi_refined %>% select(term, qof_code, depression, bipolar, schizophrenia, other_psychosis, type), by = "term")
smi_full <- smi_full %>% distinct(medcodeid, .keep_all = T)

write.table(smi_full, "", row.names = F, quote = F, sep = "\t")

smi_full <- smi_full %>%
  mutate(across(everything(), ~ na_if(trimws(.x), "")))

smi_full <- smi_full %>%
  mutate(
    medcodeid        = medcodeid,          # keep as character -> BIGINT/VARCHAR in DB
    term             = term,               # character
    cleansedreadcode = cleansedreadcode,   # character (keeps any leading zeros)
    snomedtconceptid = snomedctconceptid,   # character -> BIGINT/VARCHAR in DB
    smi              = as.integer(smi),
    qof_code         = as.integer(qof_code),
    depression       = as.integer(depression),
    bipolar          = as.integer(bipolar),
    schizophrenia    = as.integer(schizophrenia),
    other_psychosis  = as.integer(other_psychosis),
    type             = type                # character
  ) %>% select(-c("cleansedreadcode", "snomedctconceptid", "_qofcode"))

#Upload
dbWriteTable(
  con2,
  name = "dh_smi_descriptive_table",
  value = smi_full,
  overwrite = FALSE,     # or TRUE if you want to replace
  append = FALSE,         # or FALSE if you’re creating it fresh
  field.types = c(
    medcodeid        = "BIGINT",      # or VARCHAR(20) if you prefer
    term             = "TEXT",
    smi              = "TINYINT",
    qof_code         = "TINYINT",
    depression       = "TINYINT",
    bipolar          = "TINYINT",
    schizophrenia    = "TINYINT",
    other_psychosis  = "TINYINT",
    type             = "VARCHAR(50)"
  )
)

#Load in CPRD code lists and SMI data and figure out the earliest of each SMI
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
analysis <- cprd$analysis("dh")

desc <- desc %>% analysis$cached("smi_descriptive_table")

#Read in the clean SMI dates
analysis <- cprd$analysis("all_patid")
clean_smi_icd10 <- clean_smi_icd10 %>% analysis$cached("clean_smi_icd10")
clean_smi_davidson_medcodes <- clean_smi_davidson_medcodes %>% analysis$cached("clean_smi_davidson_refined_medcodes")

#Next, we merge in the files
scz <- data.frame(ICD10 = c("F20", paste0("F20.",0:20)), schizophrenia = 1)
bip <- data.frame(ICD10 = c("F30", paste0("F30.", 0:20), "F31", paste0("F31.", 0:20)), bipolar = 1)
psy <- data.frame(ICD10 = c(paste0("F2", 1:9), paste0("F2", rep(1:9, each=20),".", 0:20)), other_psychosis = 1)

annotated_smi <- clean_smi_icd10 %>% distinct(patid, epistart, ICD, .keep_all = T) %>% 
  left_join(scz, copy = TRUE, by = c("ICD" = "ICD10")) %>%  
  left_join(bip, copy = TRUE, by = c("ICD" = "ICD10")) %>% 
  left_join(psy, copy = TRUE, by = c("ICD" = "ICD10")) %>% 
  mutate(schizophrenia = ifelse(is.na(schizophrenia), 0, schizophrenia),
         bipolar = ifelse(is.na(bipolar), 0, bipolar), 
         other_psychosis = ifelse(is.na(other_psychosis), 0, other_psychosis)) %>%
  analysis$cached("annotated_smi_icd10")




#Check patient-level SMI overlap
patient_level <- annotated_smi %>%
  group_by(patid) %>%
  summarise(
    schizophrenia   = as.integer(any(schizophrenia == 1)),
    bipolar         = as.integer(any(bipolar == 1)),
    other_psychosis = as.integer(any(other_psychosis == 1)),
    .groups = "drop"
  )

patient_level %>% filter(schizophrenia + bipolar + other_psychosis > 1) %>% count() #17711 with more than 1 SMI diagnosis
patient_level %>% filter(schizophrenia + bipolar + other_psychosis > 2) %>% count() #2740 with evidence of all 3.

#First SMI dates with coding information
first_schizophrenia_date_icd10 <- annotated_smi %>% filter(schizophrenia == 1) %>% group_by(patid) %>% 
  mutate(first_schizophrenia_date = min(epistart), source = "HES", .groups = "drop") %>%
  filter(first_schizophrenia_date == epistart) %>%
  mutate(type = "ICD10") %>%
  select(patid, first_schizophrenia_date, code = ICD, source, type) %>%
  analysis$cached("first_schizophrenia_date_icd10")

first_bipolar_date_icd10 <- annotated_smi %>% filter(bipolar == 1) %>% group_by(patid) %>% 
  mutate(first_bipolar_date = min(epistart), source = "HES", .groups = "drop") %>% 
  filter(first_bipolar_date == epistart) %>% 
  mutate(type = "ICD10") %>%
  select(patid, first_bipolar_date, code = ICD, source, type) %>%
  analysis$cached("first_bipolar_date_icd10")

first_other_psychosis_date_icd10 <- annotated_smi %>% filter(other_psychosis == 1) %>% group_by(patid) %>% 
  mutate(first_other_psychosis_date = min(epistart), source = "HES", .groups = "drop") %>% 
  filter(first_other_psychosis_date == epistart) %>% 
  mutate(type = "ICD10") %>%
  select(patid, first_other_psychosis_date, code = ICD, source, type) %>%
  analysis$cached("first_other_psychosis_date_icd10")

first_smi_date_icd10 <- annotated_smi %>% group_by(patid) %>% 
  mutate(first_smi_date = min(epistart), .groups = "drop") %>% 
  filter(first_smi_date == epistart) %>% 
  mutate(type = "ICD10") %>%
  select(patid, first_smi_date, code = ICD, source, type) %>%
  analysis$cached("first_smi_date_icd10")

#Now do the same for primary care

#Load primary care table
clean_smi_davidson_medcodes %>% distinct(patid) %>% count()
clean_smi_davidson_medcodes %>% inner_join(desc %>% select(medcodeid, depression, bipolar, schizophrenia, other_psychosis)) %>% 
  filter(depression == 0) %>% analysis$cached("annotated_smi_medcodes")

#Get the first date for each of these
annotated_smi_medcodes <- annotated_smi_medcodes %>% analysis$cached("annotated_smi_medcodes")

#First code must either be an ICD-10 code or a diagnostic code.
#Make first medcode dates and merge in ICD-10 data created above
analysis <- cprd$analysis("all_patid")
first_schizophrenia_dates <- annotated_smi_medcodes %>% filter(schizophrenia == 1) %>% 
  group_by(patid) %>%
  mutate(first_schizophrenia_date = min(obsdate), .groups = "drop") %>% mutate(source = "GP") %>%
  ungroup() %>% filter(first_schizophrenia_date == obsdate) %>% 
  select(patid, first_schizophrenia_date, code = medcodeid, source, type = smi_davidson_refined_cat) %>% 
  union_all(first_schizophrenia_date_icd10) %>%
    analysis$cached("first_schizophrenia_dates") #48,488

#Bipolar disorder
first_bipolar_dates <- annotated_smi_medcodes %>% filter(bipolar == 1) %>% 
  group_by(patid) %>%
  mutate(first_bipolar_date = min(obsdate), .groups = "drop") %>% mutate(source = "GP") %>%
  ungroup() %>% filter(first_bipolar_date == obsdate) %>%
  select(patid, first_bipolar_date, code = medcodeid, source, type = smi_davidson_refined_cat) %>% 
  union_all(first_bipolar_date_icd10) %>%
  analysis$cached("first_bipolar_dates") #75,231

#Other psychosis
first_other_psychosis_dates <- annotated_smi_medcodes %>% filter(other_psychosis == 1) %>% 
  group_by(patid) %>%
  mutate(first_other_psychosis_date = min(obsdate), .groups = "drop") %>% mutate(source = "GP") %>%
  ungroup() %>% filter(first_other_psychosis_date == obsdate) %>%
  select(patid, first_other_psychosis_date, code = medcodeid, source, type = smi_davidson_refined_cat) %>% 
  union_all(first_other_psychosis_date_icd10) %>%
  analysis$cached("first_other_psychosis_dates") #59,240

first_smi_dates <- annotated_smi_medcodes %>% 
  group_by(patid) %>%
  mutate(first_smi_date = min(obsdate), .groups = "drop") %>% mutate(source = "GP") %>%
  ungroup() %>% filter(first_smi_date == obsdate) %>% 
  select(patid, first_smi_date, code = medcodeid, source, type = smi_davidson_refined_cat) %>% 
  union_all(first_smi_date_icd10) %>%
  analysis$cached("first_smi_dates") #147,948 - seems a reasonable amount.

#Next, we calculate all the necessary metrics for the fields
# BIP exclusion - no diagnostic or remission code first
# SCZ exclusion - no diagnostic or remission code first
# Other psych - no diagnostic or remission code first

# These are the diagnoses
schizophrenia_diagnosis <- first_schizophrenia_dates %>% group_by(patid) %>% 
  slice_min(order_by = first_schizophrenia_date, with_ties = TRUE) %>%  group_by(patid, first_schizophrenia_date) %>%
  mutate(
    schizophrenia_dx_okay = all(type %in% c("diagnosis", "ICD10"))) %>% 
  distinct(patid, .keep_all = T) %>% mutate(schizophrenia_dx = 1) %>% 
  select(patid, first_schizophrenia_date, schizophrenia_dx_okay, schizophrenia_dx) %>%
  ungroup()

#If a single date,  
bipolar_diagnosis <- first_bipolar_dates %>% group_by(patid) %>% 
  slice_min(order_by = first_bipolar_date, with_ties = TRUE) %>%  group_by(patid, first_bipolar_date) %>%
  mutate(
    bipolar_dx_okay = all(type %in% c("diagnosis", "ICD10"))) %>% distinct(patid, .keep_all = T) %>% 
  mutate(bipolar_dx = 1) %>% 
  select(patid, first_bipolar_date, bipolar_dx_okay, bipolar_dx) %>% 
  ungroup()

other_psychosis_diagnosis <- first_other_psychosis_dates %>% group_by(patid) %>% 
  slice_min(order_by = first_other_psychosis_date, with_ties = TRUE) %>%  group_by(patid, first_other_psychosis_date) %>%
  mutate(
    other_psychosis_dx_okay = all(type %in% c("diagnosis", "ICD10"))) %>% distinct(patid, .keep_all = T) %>% 
  mutate(other_psychosis_dx = 1) %>% select(patid, first_other_psychosis_date, other_psychosis_dx_okay, other_psychosis_dx) %>% 
  ungroup()

smi_diagnosis <- first_smi_dates %>% group_by(patid) %>% 
  slice_min(order_by = first_smi_date, with_ties = TRUE) %>%  group_by(patid, first_smi_date) %>%
  mutate(
    smi_dx_okay = all(type %in% c("diagnosis", "ICD10"))) %>% distinct(patid, .keep_all = T) %>%
  mutate(smi_dx = 1) %>%
  select(patid, first_smi_date, smi_dx_okay, smi_dx) %>% 
  ungroup()

# Load in the cohort table
analysis <- cprd$analysis("dh")
smoking <- smoking %>% analysis$cached("smoking")
alcohol <- alcohol %>% analysis$cached("alcohol")

analysis <- cprd$analysis("dh")
dep_cohort_interim_7 <- dep_cohort_interim_7 %>% analysis$cached("depression_cohort_interim_7")

#Here, we re-model the cohort
full_dep_cohort <- dep_cohort_interim_7 %>% 
  left_join(alcohol %>% select(patid, alcohol_cat), by = "patid") %>%
  left_join(smoking %>% select(patid, smoking_cat, qrisk2_smoking_cat)) %>% 
  select(-incident_smi, -incident_smi_date, -preexisting_smi) %>% analysis$cached("depression_cohort_interim_8")


#Join in the diagnoses for SMI: 
dep_ids <- full_dep_cohort %>% 
  select(patid) %>% 
  left_join(schizophrenia_diagnosis, by = "patid") %>% 
  left_join(bipolar_diagnosis, by = "patid") %>% 
  left_join(other_psychosis_diagnosis, by = "patid") %>% 
  left_join(smi_diagnosis, by = "patid") %>% 
  mutate(
    across(
      c(schizophrenia_dx_okay, bipolar_dx_okay, other_psychosis_dx_okay, smi_dx_okay),
      ~ if_else(is.na(.x), 1, .x)
    )
  ) %>%
  mutate(
    across(
      c(schizophrenia_dx, bipolar_dx, other_psychosis_dx, smi_dx), 
      ~ if_else(is.na(.x), 0, .x)
    )
  ) %>% analysis$cached("all_smi_diagnoses")


# Add these rows to current data set
full_dep_cohort <- full_dep_cohort %>% left_join(dep_ids, by = "patid") %>% 
  analysis$cached("depression_cohort_interim_9")



