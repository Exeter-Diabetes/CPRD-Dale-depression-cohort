cprdenvname <- ""
yaml <- ""
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

#codes
codes <- read_xlsx("C:/Users/dhand/OneDrive - King's College London/clinical_heterogeneity_in_depression/code_lists/Referral to seconary care mental health.xlsx")
codes$`SnomedCT code` <- as.character(codes$`SnomedCT code`)

#Load in the aurum dict
dict <- fread("C:/Users/dhand/Downloads/CPRD_CodeBrowser_202403_Aurum/CPRD_CodeBrowser_202403_Aurum/CPRDAurumMedical.txt")
dict_tst <- dict %>% filter(Term %in% codes$Description | SnomedCTConceptId %in% codes$`SnomedCT code`)

codesets <- cprd$codesets()

codesets$loadMedCodeSet(codeSetDf = dict_tst, category = "Term", version = "20/01/2026", name = "sec_ref_mhc", colname = "MedCodeId")
#codesets$deleteCodeSet("sec_ref_mhc")

sec_ref_mhc_codes <- codesets$getAllCodeSetVersion("20/01/2026")$sec_ref_mhc

analysis <- cprd$analysis("all_patid")
censoring <- censoring %>% analysis$cached("death_end_dat")

#Get the codes out of the data:
raw_sec_ref_mhc_medcodes <- cprd$tables$observation %>%
  inner_join(sec_ref_mhc_codes, by="medcodeid") %>%
  analysis$cached("raw_sec_ref_mhc_medcodes", indexes=c("patid", "obsdate", "sec_ref_mhc_cat"))

#raw_sec_ref_mhc_medcodes %>% distinct(patid) %>% count() #781508 - nearly 20%

clean_sec_ref_mhc_medcodes <- raw_sec_ref_mhc_medcodes %>% 
  inner_join(censoring %>% select(patid, min_dob, gp_death_end_date), by = "patid") %>%
  filter(obsdate >= min_dob & obsdate <= gp_death_end_date) %>%
  analysis$cached("clean_sec_ref_mhc_medcodes", indexes = c("patid", "obsdate"))

#Load in the index dates and create the "at index" table.
analysis <- cprd$analysis("dh_depsev")
index <- index %>% analysis$cached("AD_informed_index_dates")

#Merge index with New file and save. 
at_diag_referrals <- clean_sec_ref_mhc_medcodes %>% 
  inner_join(
    index %>% select(patid, AD_informed_index_date, AD_index_registration_90days),
    by = "patid"
  ) %>% 
  filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1) %>% 
  mutate(after_index = ifelse(obsdate > AD_informed_index_date, 1, 0)) %>%
  select(patid, AD_informed_index_date, obsdate, after_index) %>%
  group_by(patid) %>% 
  window_order(obsdate) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>%
  analysis$cached("referral_to_secondary_care")
