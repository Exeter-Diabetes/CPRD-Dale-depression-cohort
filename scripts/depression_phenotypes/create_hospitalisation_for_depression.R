library(aurum)
library(tidyverse)
library(survival)

cprdenvname <- ""
yaml <- ""

#Depression hosp. codes
dep_hosp <- data.frame(icd10_code = c("F32", "F33"))
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)


analysis <- cprd$analysis("all_patid")
#Load in the censoring table
censoring <- censoring %>% analysis$cached("death_end_dat")


codesets <- cprd$codesets()
codesets$loadICD10CodeSet(codeSetDf = dep_hosp, name = "icd10_depression", version = "28/10/25", colname = "icd10_code")

#Load the codeset
hosp_dep <- codesets$getAllCodeSetVersion("28/10/25")
hosp_dep_codes <- hosp_dep$icd10_depression

raw_primary_dep_hospitalisation_icd10 <- raw_depression_hospitalisation_icd10 %>% 
  filter(d_order == 1) %>% analysis$cached("raw_primary_dep_hospitalisation_icd10", indexes = c("patid"))

raw_secondary_dep_hospitalisation_icd10 <- raw_depression_hospitalisation_icd10 %>% 
  filter(d_order != 1) %>% analysis$cached("raw_secondary_dep_hospitalisation_icd10", indexes = c("patid"))


#ICD-10
raw_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  select(patid, date=epistart, code=ICD) %>%
  mutate(source="hes_icd10")

raw_secondary_dep_hospitalisation_icd10 <- raw_secondary_dep_hospitalisation_icd10 %>%
  select(patid, date=epistart, code=ICD) %>%
  mutate(source="hes_icd10")

#Clean date ranges and add in index dates
clean_primary_dep_hospitalisation_icd10 <- raw_primary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code)  %>% analysis$cached("clean_primary_dep_hospitalisation")

clean_secondary_dep_hospitalisation_icd10 <- raw_secondary_dep_hospitalisation_icd10 %>%
  inner_join(censoring, by="patid") %>%
  filter(date>=min_dob & ((source=="gp" & date<=gp_death_end_date) | ((source=="hes_icd10" | source=="hes_opcs4") & (is.na(gp_death_end_date) | date<=gp_death_end_date)))) %>%
  select(patid, date, source, code) %>% analysis$cached("clean_secondary_dep_hospitalisation")


#Next, we get the first date at which someone has a primary or secondary diagnosis 
#after index, And create an indicator where events prior to index are not allowed.
analysis <- cprd$analysis("dh_depsev")
index <- index %>% analysis$cached("AD_informed_index_dates")

#Keep dates for everyone not just AD users
# If I remove the people where an SSRI isn't the first drug, I lose so many people
# Is this surprising? probably not... but it's a lot higher in people with a 
# code for depression

index <- index %>% filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1) #I remove ~1/3rd of people but ~1/2 of hospitalisations?

first_primary_hospitalisation <- #Most are after index thank god
  clean_primary_dep_hospitalisation_icd10 %>%
  inner_join(index, by = "patid") %>% 
  mutate(after_index = ifelse(date > AD_informed_index_date, 1, 0)) %>%
  group_by(patid) %>% 
  window_order(date) %>%
  filter(row_number() == 1) %>%
  ungroup %>% 
  select(patid, date, code, after_index) %>% analysis$cached("primary_hospitalisation")


first_secondary_hospitalisation <- clean_secondary_dep_hospitalisation_icd10 %>%
  inner_join(index, by = "patid") %>% 
  mutate(after_index = ifelse(date > AD_informed_index_date, 1, 0)) %>%
  group_by(patid) %>% 
  window_order(date) %>%
  filter(row_number() == 1) %>%
  ungroup %>% 
  select(patid, date, code, after_index) %>% analysis$cached("secondary_hospitalisation")

N <- nrow(cohort_collected)

# ── Collect and join index_date to hospitalisation tables ─────────────────────
primary_hosp <- first_primary_hospitalisation %>%
  collect() %>%
  left_join(cohort_collected %>% select(patid, index_date), by = "patid") %>%
  filter(!is.na(index_date)) %>%
  mutate(after_index = ifelse(date > index_date, 1, 0))

secondary_hosp <- first_secondary_hospitalisation %>%
  collect() %>%
  left_join(cohort_collected %>% select(patid, index_date), by = "patid") %>%
  filter(!is.na(index_date)) %>%
  mutate(after_index = ifelse(date > index_date, 1, 0))


pct_data <- tibble(
  Phenotype  = c("Primary Hospitalisation", "Secondary Hospitalisation"),
  n_incident = c(
    sum(primary_hosp$after_index   == 1, na.rm = TRUE),
    sum(secondary_hosp$after_index == 1, na.rm = TRUE)
  ),
  pct = n_incident / N * 100
)

p1 <- ggplot(pct_data, aes(x = Phenotype, y = pct, fill = Phenotype)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%\n(n = %s)", pct, format(n_incident, big.mark = ","))),
            vjust = -0.4, size = 3, fontface = "bold", colour = col_dark) +
  scale_fill_manual(values = c(
    "Primary Hospitalisation"   = col_dark,
    "Secondary Hospitalisation" = col_mid
  )) +
  scale_y_continuous(
    limits = c(0, max(pct_data$pct) * 1.25),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title    = "A. Incident Depression Hospitalisation",
    subtitle = "Pre-existing cases removed for each phenotype independently",
    x        = NULL,
    y        = "Percentage of Cohort (%)"
  ) +
  base_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

# ── Time to event histograms ──────────────────────────────────────────────────
primary_tte <- primary_hosp %>%
  filter(after_index == 1) %>%
  mutate(years_to_event = as.numeric(date - index_date) / 365.25) %>%
  filter(years_to_event >= 0)

secondary_tte <- secondary_hosp %>%
  filter(after_index == 1) %>%
  mutate(years_to_event = as.numeric(date - index_date) / 365.25) %>%
  filter(years_to_event >= 0)

median_primary   <- median(primary_tte$years_to_event)
median_secondary <- median(secondary_tte$years_to_event)

p2 <- ggplot(primary_tte, aes(x = years_to_event)) +
  geom_histogram(binwidth = 0.5, fill = col_dark, colour = "white", alpha = 0.9) +
  geom_vline(xintercept = median_primary,
             colour = col_orange, linewidth = 1, linetype = "dashed") +
  annotate("text",
           x     = median_primary + 0.2,
           y     = Inf,
           vjust = 1.5,
           hjust = 0,
           label = sprintf("Median: %.1f years", median_primary),
           colour = col_orange, fontface = "bold", size = 3) +
  scale_x_continuous(breaks = seq(0, ceiling(max(primary_tte$years_to_event)), by = 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "B. Time to Primary Hospitalisation",
    subtitle = "6-month bins; dashed line = median; pre-existing cases excluded",
    x        = "Years from Index Date",
    y        = "Number of Patients"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

p3 <- ggplot(secondary_tte, aes(x = years_to_event)) +
  geom_histogram(binwidth = 0.5, fill = col_mid, colour = "white", alpha = 0.9) +
  geom_vline(xintercept = median_secondary,
             colour = col_orange, linewidth = 1, linetype = "dashed") +
  annotate("text",
           x     = median_secondary + 0.2,
           y     = Inf,
           vjust = 1.5,
           hjust = 0,
           label = sprintf("Median: %.1f years", median_secondary),
           colour = col_orange, fontface = "bold", size = 3) +
  scale_x_continuous(breaks = seq(0, ceiling(max(secondary_tte$years_to_event)), by = 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "C. Time to Secondary Hospitalisation",
    subtitle = "6-month bins; dashed line = median; pre-existing cases excluded",
    x        = "Years from Index Date",
    y        = "Number of Patients"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

# ── Combine and save ──────────────────────────────────────────────────────────
combined <- p1 / (p2 | p3) +
  plot_annotation(
    title    = "Depression Hospitalisation Phenotypes",
    subtitle = sprintf("N = %s individuals with a valid antidepressant index date",
                       format(N, big.mark = ",")),
    caption  = "Index date: first antidepressant initiation. Pre-existing: hospitalisation prior to or at index date.",
    theme    = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(face = "bold", size = 13, colour = col_dark),
      plot.subtitle   = element_text(size = 10, colour = col_grey),
      plot.caption    = element_text(size = 7.5, colour = col_grey, hjust = 0)
    )
  )

ggsave("hospitalisation_plots.png", combined,
       width = 12, height = 10, dpi = 300, bg = "white")
