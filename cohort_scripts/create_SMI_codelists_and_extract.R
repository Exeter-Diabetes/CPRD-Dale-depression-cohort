library(aurum)
library(tidyverse)
library(data.table)

####Connection and code lists####
#Open connection
#Connect to the MySQL database
cprdenvname <- "CPRD_depression_data"
yaml <- ""

#open connection and get codes sets
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets = cprd$codesets()

#Load in the code set
smi <- fread("")
smi_form <- smi %>% mutate(medcodeid = as.character(medcodeid))

#Taken from https://www.thelancet.com/journals/lanepe/article/PIIS2666-7762(23)00022-4/fulltext
#DOI for code list: https://doi.org/10.17037/DATA.00002762
#Save the code set - will be updated later
codesets$loadMedCodeSet(codeSetDf = smi_form, name = "smi_davidson", version = "04/05/2025", colname = "medcodeid")

#Get the code set
smi_codes <- codesets$getAllCodeSetVersion(v = "04/05/2025")
smi_codes <- smi_codes$smi_davidson

#Extract the file
analysis <- cprd$analysis("dh")

raw_smi_davidson_medcodes <- cprd$tables$observation %>%
  inner_join(smi_codes, by="medcodeid") %>%
  analysis$cached("raw_smi_davidson_medcodes", indexes=c("patid", "obsdate", "smi_davidson_cat"))

raw_smi_davidson_medcodes %>% count()
raw_smi_davidson_medcodes %>% distinct(patid) %>% count() #110k - more than expected.

#Also do the hospital-based extractions.

#Find the ICD-10 codes
#Taken from https://pmc.ncbi.nlm.nih.gov/articles/PMC7612694/#sec2
#F20 - F31, F32.3, F33.3. 
#F32.3 and F33.3 entries will be removed later, as they are related to depression, 
#But for now, we extract everything.
icd10_dict <- fread("")

icd10_smi <- icd10_dict %>%
  filter(
    str_detect(CODE, "^F(2[0-9]|30|31)(\\.|$)") |  # F20 to F31 and sub-codes
      CODE %in% c("F32.3", "F33.3")                  # Specific codes
  )

#Upload these codes to the database
codesets$loadICD10CodeSet(icd10_smi, name = "icd10_smi", v = "05/05/2025", colname = "CODE")

#Reload the code set and check
smi_codes_icd10 <- codesets$getAllCodeSetVersion("05/05/2025")
smi_codes_icd10 <- smi_codes_icd10$icd10_smi

#Extract as a raw file.
raw_icd10_smi <- cprd$tables$hesDiagnosisEpi %>%
  inner_join(smi_codes_icd10, sql_on="LHS.ICD LIKE CONCAT(icd10,'%')") %>%
  analysis$cached("raw_smi_icd10", indexes=c("patid", "epistart"))
