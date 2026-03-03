td <- full %>%
  # ---- dates ----
mutate(across(
  c(index_date.x, gp_death_end_date, dm_diag_date_all, hosp_date, dob),
  as.Date
)) %>%
  
  # ---- age at index (years) ----
mutate(
  age = as.numeric(index_date.x - dob) / 365.25
) %>%
  
  # ---- covariates: types + reference levels ----
mutate(
  gender = factor(gender),
  gender = relevel(gender, ref = "1"),
  
  ethnicity_5cat = factor(ethnicity_5cat),
  ethnicity_5cat = relevel(ethnicity_5cat, ref = "0"),
  
  imd_decile = factor(imd_decile),
  imd_decile = relevel(imd_decile, ref = "1"),
  
  alcohol_cat = factor(alcohol_cat),
  alcohol_cat = relevel(alcohol_cat, ref = "None"),
  
  smoking_cat = factor(smoking_cat),
  smoking_cat = relevel(smoking_cat, ref = "Non-smoker")
) %>%
  
  # ---- exclusions (before index) ----
filter(is.na(dm_diag_date_all) | dm_diag_date_all >= index_date.x) %>%
  filter(is.na(hosp_date)        | hosp_date        >= index_date.x) %>%
  
  # ---- times since index (days) ----
mutate(
  ctime = as.numeric(gp_death_end_date - index_date.x),  # censor time
  dtime = as.numeric(dm_diag_date_all - index_date.x),   # dm event time (NA ok)
  htime = as.numeric(hosp_date       - index_date.x)     # hosp time (NA ok)
) %>%
  
  # ---- positive follow-up only ----
filter(is.finite(ctime) & ctime > 0) %>%
  
  # ---- keep needed variables ----
select(
  patid, ctime, dtime, htime,
  gender, age, ethnicity_5cat, imd_decile, alcohol_cat, smoking_cat
) %>%
  
  # ---- tmerge block ----
{
  dat <- .
  
  td1 <- tmerge(
    data1 = dat,
    data2 = dat,
    id    = patid,
    tstop = ctime
  )
  
  td2 <- tmerge(
    td1, dat,
    id = patid,
    event_dm = event(dtime)
  )
  
  td3 <- tmerge(
    td2, dat,
    id = patid,
    hosp_tv = tdc(htime)
  )
  
  td3
}


fit_adj <- coxph(
  Surv(tstart, tstop, event_dm) ~ hosp_tv + gender + age,
  data = td
)
summary(fit_adj)
