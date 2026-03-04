library(aurum)
library(tidyverse)

#Connect
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

#AntidepressantsQ
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


ads_deduped <- ads_earliest %>%
  transmute(
    patid,
    first_antidep_date = date,
    chem_name,
    drug_class
  ) %>%
  distinct()


# ---- Step 2: Count distinct SSRI chem_names per patient per date ----
# We isolate SSRI rows only, then count distinct chem_names per
# patid + date. This is a plain GROUP BY + COUNT(DISTINCT) which
# MySQL handles without window functions.
#
# Patients with n_distinct_ssri > 1 were prescribed more than one
# chemically distinct SSRI on the same date and should be flagged.
ssri_counts <- ads_deduped %>%
  filter(drug_class == "SSRI") %>%
  group_by(patid, first_antidep_date) %>%
  summarise(
    n_distinct_ssri = n_distinct(chem_name),
    .groups = "drop"
  ) %>%
  mutate(
    multiple_ssri_same_date = case_when(
      n_distinct_ssri > 1L ~ 1L,
      TRUE                 ~ 0L
    )
  ) %>%
  select(patid, first_antidep_date, multiple_ssri_same_date)


# ---- Step 3: Build main early_table and join SSRI flag ----
# Left join preserves patients with no SSRI prescription
# (their multiple_ssri_same_date will be NA, coalesced to 0)
early_table <- ads_deduped %>%
  group_by(patid, first_antidep_date) %>%
  summarise(
    # 1 if any prescription on this date is not an SSRI, else 0
    first_antidep_not_ssri = max(
      case_when(
        is.na(drug_class)    ~ 0L,
        drug_class != "SSRI" ~ 1L,
        TRUE                 ~ 0L
      ),
      na.rm = TRUE
    ),
    any_antidepressant = 1L,
    .groups = "drop"
  ) %>%
  left_join(ssri_counts, by = c("patid", "first_antidep_date")) %>%
  mutate(
    # Patients with no SSRI at all will have NA after left join — set to 0
    multiple_ssri_same_date = coalesce(multiple_ssri_same_date, 0L))

# ---- Bind cohort table to early_table for further QC ----
ad_cohort <- cohort %>%
  select(patid, index_date, regstartdate) %>%
  left_join(early_table, by = "patid") %>%
  mutate(
    any_antidepressant = coalesce(any_antidepressant, 0L)
  )

# ---- Add:
# 1) Prescription before 04/01/2006
# 2) Prescription before depression codes (index_date)
# Set these to NA if any_antidepressant == 0 ----
ad_cohort <- ad_cohort %>%
  mutate(
    first_antidep_before_qof = case_when(
      any_antidepressant == 0L        ~ NA_integer_,
      first_antidep_date < "2006-04-01" ~ 1L,
      TRUE                            ~ 0L
    ),
    first_antidep_before_code = case_when(
      any_antidepressant == 0L          ~ NA_integer_,
      first_antidep_date < index_date   ~ 1L,
      TRUE                              ~ 0L
    )
  ) %>%
  analysis$cached("antidepressant_index_dates_interim_1")

# ---- Calculate the earliest of:
# The first antidepressant date if available
# The first code ----
ad_cohort <- ad_cohort %>%
  rename(first_valid_dep_code_date = index_date) %>%
  mutate(
    # Determine combined index date
    # Use coalesce for NULL-safe fallback, sql_min for two non-NULL values
    ad_code_index_date = case_when(
      !is.na(first_valid_dep_code_date) & !is.na(first_antidep_date) ~
        sql("CASE WHEN first_valid_dep_code_date <= first_antidep_date
                  THEN first_valid_dep_code_date
                  ELSE first_antidep_date END"),
      TRUE ~ coalesce(first_valid_dep_code_date, first_antidep_date)
    ),
    
    # Identify which source determined the index
    # Explicit NULL handling via is.na() checks rather than relying on
    # implicit NA propagation through comparisons
    dx_source = case_when(
      is.na(first_valid_dep_code_date) & is.na(first_antidep_date) ~ NA_character_,
      is.na(first_antidep_date)                                     ~ "diagnostic_code",
      is.na(first_valid_dep_code_date)                              ~ "antidepressant_Rx",
      first_valid_dep_code_date <= first_antidep_date               ~ "diagnostic_code",
      TRUE                                                          ~ "antidepressant_Rx"
    ),
    
    # Registration time in days
    # datediff is SQL-native in dbplyr so this is fine as-is
    time_reg_ad_code_index = case_when(
      is.na(ad_code_index_date) | is.na(regstartdate) ~ NA_real_,
      TRUE ~ datediff(ad_code_index_date, regstartdate)
    ),
    
    # 90-day registration flag
    reg_90days_ad_code_index = case_when(
      is.na(time_reg_ad_code_index)      ~ NA_integer_,
      time_reg_ad_code_index >= 90L      ~ 1L,
      TRUE                               ~ 0L
    ),
    
    # 183-day registration flag
    reg_183days_ad_code_index = case_when(
      is.na(time_reg_ad_code_index)      ~ NA_integer_,
      time_reg_ad_code_index >= 183L     ~ 1L,
      TRUE                               ~ 0L
    )
  ) %>%
  analysis$cached("antidepressant_index_dates")

# ---- Add to cohort table ----
analysis <- cprd$analysis("dh")

ad_cohort_final <- cohort %>%
  left_join(ad_cohort, by = "patid") %>%
  select(-regstartdate.y) %>%
  rename(regstartdate = regstartdate.x) %>%
  mutate(
    age_at_ad_code_index = datediff(ad_code_index_date, dob) / 365.25
  ) %>%
  analysis$cached("depression_cohort_interim_10")

#TESTING
#cprd$execSql("DROP TABLE all_patid_antidepressant_index_dates")
#cprd$execSql("DROP TABLE all_patid_antidepressant_index_dates_interim_1")
#cprd$execSql("DROP table all_patid_depression_cohort_interim_10")

