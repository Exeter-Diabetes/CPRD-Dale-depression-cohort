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

library(aurum)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(scales)

# ── 1. Load data ──────────────────────────────────────────────────────────────
analysis <- cprd$analysis("all_patid")
secref <- secref %>% analysis$cached("clean_sec_ref_mhc_medcodes")

analysis <- cprd$analysis("dh_clinhet")
cohort <- cohort %>% analysis$cached("depression_cohort_interim_4")

# ── 2. First referral ever per patient (no filtering yet) ─────────────────────
first_secref <- secref %>%
  group_by(patid) %>%
  window_order(obsdate) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(patid, obsdate, sec_ref_mhc_cat) %>%
  analysis$cached("first_secref", indexes = c("patid", "obsdate"))

# ── 3. Collect cohort and derive N ────────────────────────────────────────────
cohort_collected <- cohort %>%
  collect() %>%
  filter(!is.na(index_date))

N <- nrow(cohort_collected)

# ── 4. Collect, join index_date, derive flags ─────────────────────────────────
secref_collected <- first_secref %>%
  collect() %>%
  left_join(cohort_collected %>% select(patid, index_date), by = "patid") %>%
  filter(!is.na(index_date)) %>%
  mutate(
    after_index    = ifelse(obsdate > index_date, 1, 0),
    years_to_event = as.numeric(obsdate - index_date) / 365.25
  )

# ── 5. Categorise all cohort patients into three groups ───────────────────────
referral_status <- cohort_collected %>%
  select(patid) %>%
  left_join(secref_collected %>% select(patid, after_index), by = "patid") %>%
  mutate(status = case_when(
    after_index == 1   ~ "Referred",
    after_index == 0   ~ "Pre-existing Referral",
    is.na(after_index) ~ "Not Referred"
  ))

pct_data <- referral_status %>%
  count(status, name = "n") %>%
  mutate(
    pct    = n / N * 100,
    status = factor(status, levels = c("Referred", "Pre-existing Referral", "Not Referred"))
  )

# ── 6. Post-index only for time-to-event and category plots ──────────────────
secref_tte <- secref_collected %>%
  filter(after_index == 1 & years_to_event >= 0)

median_secref <- median(secref_tte$years_to_event)
pct_secref    <- nrow(secref_tte) / N * 100

# ── 7. Colour scheme ──────────────────────────────────────────────────────────
col_dark   <- "#2C3E50"
col_mid    <- "#2980B9"
col_green  <- "#27AE60"
col_orange <- "#E67E22"
col_grey   <- "#7F8C8D"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, colour = col_grey),
    axis.title       = element_text(size = 9),
    axis.text        = element_text(size = 8),
    panel.grid.minor = element_blank(),
    plot.margin      = margin(8, 8, 8, 8)
  )

# ── 8. Plot A: referral status (three groups) ─────────────────────────────────
p1 <- ggplot(pct_data, aes(x = status, y = pct, fill = status)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%\n(n = %s)", pct, format(n, big.mark = ","))),
            vjust = -0.4, size = 3.5, fontface = "bold", colour = col_dark) +
  scale_fill_manual(values = c(
    "Referred"              = col_dark,
    "Pre-existing Referral" = col_grey,
    "Not Referred"          = col_mid
  )) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title    = "A. Secondary Care Mental Health Referral Status",
    subtitle = "Pre-existing referrals (before index date) shown separately",
    x        = NULL,
    y        = "Percentage of Cohort (%)"
  ) +
  base_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

# ── 9. Plot B: time to first post-index referral ──────────────────────────────
p2 <- ggplot(secref_tte, aes(x = years_to_event)) +
  geom_histogram(binwidth = 0.5, fill = col_mid, colour = "white", alpha = 0.9) +
  geom_vline(xintercept = median_secref,
             colour = col_orange, linewidth = 1, linetype = "dashed") +
  annotate("text",
           x     = median_secref + 0.2,
           y     = Inf,
           vjust = 1.5,
           hjust = 0,
           label = sprintf("Median: %.1f years", median_secref),
           colour = col_orange, fontface = "bold", size = 3) +
  scale_x_continuous(breaks = seq(0, ceiling(max(secref_tte$years_to_event)), by = 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "B. Time to First Incident Secondary Care Mental Health Referral",
    subtitle = "6-month bins; dashed line = median; pre-existing referrals excluded",
    x        = "Years from Index Date",
    y        = "Number of Patients"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

# ── 10. Plot C: top 5 referral categories ────────────────────────────────────
top5_cats <- secref_tte %>%
  count(sec_ref_mhc_cat, name = "n") %>%
  arrange(desc(n)) %>%
  slice(1:5) %>%
  mutate(
    pct             = n / nrow(secref_tte) * 100,
    sec_ref_mhc_cat = str_wrap(sec_ref_mhc_cat, width = 30),
    sec_ref_mhc_cat = fct_reorder(sec_ref_mhc_cat, pct)
  )

p3 <- ggplot(top5_cats, aes(x = sec_ref_mhc_cat, y = pct, fill = sec_ref_mhc_cat)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%\n(n = %s)", pct, format(n, big.mark = ","))),
            hjust = -0.1, size = 3, fontface = "bold", colour = col_dark) +
  scale_fill_manual(values = setNames(
    c(col_dark, col_mid, col_green, col_orange, col_grey),
    levels(top5_cats$sec_ref_mhc_cat)
  )) +
  scale_y_continuous(
    limits = c(0, max(top5_cats$pct) * 1.3),
    labels = percent_format(scale = 1)
  ) +
  coord_flip() +
  labs(
    title    = "C. Top 5 Secondary Care Mental Health Referral Categories",
    subtitle = "Among incident referrals post index date",
    x        = NULL,
    y        = "Percentage of Incident Referrals (%)"
  ) +
  base_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_blank()
  )

# ── 11. Combine and save ──────────────────────────────────────────────────────
combined <- (p1 | p3) / p2 +
  plot_annotation(
    title    = "Secondary Care Mental Health Referrals in a UK Primary Care Depression Cohort",
    subtitle = sprintf("N = %s individuals with a valid antidepressant index date",
                       format(N, big.mark = ",")),
    caption  = "Index date: first antidepressant initiation. Pre-existing: referral prior to or at index date.",
    theme    = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(face = "bold", size = 13, colour = col_dark),
      plot.subtitle   = element_text(size = 10, colour = col_grey),
      plot.caption    = element_text(size = 7.5, colour = col_grey, hjust = 0)
    )
  )

ggsave("secref_plots.png", combined,
       width = 14, height = 10, dpi = 300, bg = "white")

print(combined)
