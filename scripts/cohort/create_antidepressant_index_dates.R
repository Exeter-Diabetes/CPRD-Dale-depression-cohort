library(aurum)
library(tidyverse)

#Connect
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

#Antidepressants
analysis <- cprd$analysis("all_patid")
ads <- ads %>% analysis$cached("clean_antidepressant_prodcodes") #3598070

# Cohort table
analysis <- cprd$analysis("dh")
cohort <- cohort %>% analysis$cached("depression_cohort_interim_9")

#Get their first antidepressant
analysis <- cprd$analysis("all_patid")

ads_earliest <- ads %>%
  group_by(patid) %>%
  filter(date == min(date, na.rm = TRUE)) %>%
  ungroup() %>% analysis$cached("clean_first_antidepressant")


# What do I need to do?
# Create the drug class table
# Add to the cohort table
# then add all the relevant filters - one person per row.
early_table <- ads_earliest %>%
  transmute(
    patid,
    first_antidep_date = date,
    drug_class
  ) %>%
  group_by(patid, first_antidep_date) %>%
  summarise(
    first_antidep_not_ssri = max(
      case_when(
        is.na(drug_class) ~ 0L,
        drug_class != "SSRI" ~ 1L,
        TRUE ~ 0L
      ),
      na.rm = TRUE
    ),
    any_antidepressant = 1L,
    .groups = "drop"
  )

# ---- Bind cohort table to early_table for further QC ----
ad_cohort <- cohort %>%
  select(patid, index_date, regstartdate) %>%
  left_join(early_table, by = "patid") %>%
  mutate(
    any_antidepressant = coalesce(any_antidepressant, 0L)
  )

# ---- Add:
# 1) Prescription before 04/01/2014
# 2) Prescription before depression codes (index_date)
# Set these to NA if any_antidepressant == 0 ----
ad_cohort <- ad_cohort %>%
  mutate(
    first_antidep_before_qof = case_when(
      any_antidepressant == 0L ~ NA,
      first_antidep_date < as.Date("2014-04-01") ~ 1L,
      TRUE ~ 0L
    ),
    first_antidep_before_code = case_when(
      any_antidepressant == 0L ~ NA,
      first_antidep_date < index_date ~ 1L,
      TRUE ~ 0L
    )
  ) %>%
  analysis$cached("antidepressant_index_dates_interim_1")

# Calculate the earliest of:
# The first antidepressant date if available
# The first code.

ad_cohort <- ad_cohort %>%
  rename(first_valid_dep_code_date = index_date) %>%
  mutate(
    # Determine combined index date
    ad_code_index_date = case_when(
      is.na(first_valid_dep_code_date) & is.na(first_antidep_date) ~ as.Date(NA),
      is.na(first_valid_dep_code_date) ~ first_antidep_date,
      is.na(first_antidep_date) ~ first_valid_dep_code_date,
      TRUE ~ pmin(first_valid_dep_code_date, first_antidep_date)
    ),
    
    # Identify which source determined the index
    dx_source = case_when(
      is.na(first_valid_dep_code_date) & is.na(first_antidep_date) ~ NA_character_,
      !is.na(first_valid_dep_code_date) & 
        (is.na(first_antidep_date) | first_valid_dep_code_date <= first_antidep_date) ~ "diagnostic_code",
      !is.na(first_antidep_date) &
        (is.na(first_valid_dep_code_date) | first_antidep_date < first_valid_dep_code_date) ~ "antidepressant_Rx"
    ),
    
    # Registration time in days (fallback to NA if missing)
    time_reg_ad_code_index = case_when(
      is.na(ad_code_index_date) | is.na(regstartdate) ~ NA_real_,
      TRUE ~ as.numeric(ad_code_index_date - regstartdate)
    ),
    
    # 90-day registration flag
    reg_90days_ad_code_index = case_when(
      is.na(time_reg_ad_code_index) ~ NA_integer_,
      time_reg_ad_code_index >= 90 ~ 1L,
      TRUE ~ 0L
    ),
    
    # 185-day registration flag
    reg_185days_ad_code_index = case_when(
      is.na(time_reg_ad_code_index) ~ NA_integer_,
      time_reg_ad_code_index >= 185 ~ 1L,
      TRUE ~ 0L
    )
  ) %>% analysis$cached("antidepressant_index_dates")


#Add this data to the cohort table
ad_cohort_final <- cohort %>% left_join(ad_cohort, by = "patid") %>% 
  select(-index_date) %>% analysis$cached("depression_cohort_interim_10")

#Re-arrange the columns rows.
ad_cohort_final <- ad_cohort_final %>% 
  rename(regstartdate = regstartdate.x) %>% select(-regstartdate.y) %>%
  mutate(age_at_ad_code_index = difftime(as.numeric(ad_code_index_date - dob)/365.25))
