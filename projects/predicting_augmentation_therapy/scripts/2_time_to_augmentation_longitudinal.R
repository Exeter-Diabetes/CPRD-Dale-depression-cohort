# =============================================================================
# Time-Varying Cox Models: Clinical Phenotype Predictors of Augmentation
# =============================================================================
# Author:       [Your Name]
# Date:         2024
# Description:  Time-varying covariate Cox models examining clinical phenotypes
#               as predictors of time to first antipsychotic augmentation
#               (strict definition) in a UK primary care depression cohort.
#
# Exposures (all time-varying):
#   - TRD status
#   - SSRI switch (to different SSRI)
#   - Drug class switch (to non-SSRI AD)
#   - Depression recurrence (episode 2 onset)
#   - Secondary care MH referral
#   - Primary care hospitalisation
#   - Secondary care hospitalisation
#
# Design:
#   - Prevalent exposure (on or before ad_index_date) excluded per model
#   - Exposure flips 0 → 1 at time of incident event
#   - Exposure nulled if it occurs on or after exit date (prevents immortal
#     time bias)
#   - Counting process (tstart, tstop] format via tmerge()
#   - Static covariates: age at index, sex, year of diagnosis
#   - FDR correction across all exposure terms
#
# Outcome: strict antipsychotic augmentation
#   Censoring hierarchy:
#     1. Augmentation event (valid_ap_strict == 1)
#     2. First non-main-4 post-index AP (competing treatment)
#     3. Death or end of GP registration (gp_death_end_date)
# =============================================================================

library(aurum)
library(tidyverse)
library(survival)
library(ggtext)
library(patchwork)

# ── 1. Connect to CPRD ────────────────────────────────────────────────────────
cprdenvname <- "CPRD_depression_data"
yaml        <- ".aurum.yaml"
cprd        <- CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

# ── 2. Load and collect all tables ───────────────────────────────────────────
analysis <- cprd$analysis("dh_clinhet")

cohort <- cohort %>%
  analysis$cached("depression_cohort_interim_4") %>%
  select(patid, dob, ad_index_date, gender, gp_death_end_date) %>%
  filter(!is.na(ad_index_date)) %>%
  collect() %>%
  mutate(
    patid             = as.character(patid),
    age_at_ad_index   = as.numeric(ad_index_date - dob) / 365.25,
    year_of_diagnosis = as.integer(format(ad_index_date, "%Y")),
    gender            = factor(gender, levels = c(1, 2),
                               labels = c("Male", "Female"))
  )

trd <- trd %>%
  analysis$cached("trd_index_dates") %>%
  select(patid, trd_date) %>%
  distinct(patid, trd_date) %>%
  collect() %>%
  mutate(patid = as.character(patid))

switching <- switching %>%
  analysis$cached("switching") %>%
  select(patid, first_ssri_switch_date, first_class_switch_date) %>%
  collect() %>%
  mutate(patid = as.character(patid))

recurrence <- recurrence %>%
  analysis$cached("depression_episodes") %>%
  filter(episode_id == 2) %>%
  select(patid, first_recurrence_date = episode_start_date) %>%
  collect() %>%
  mutate(patid = as.character(patid))

# Secondary care referral: one row per patient (earliest referral)
secref <- secref %>%
  analysis$cached("clean_sec_ref_mhc_medcodes") %>%
  select(patid, first_secref_date = obsdate) %>%
  collect() %>%
  mutate(patid = as.character(patid)) %>%
  group_by(patid) %>%
  slice_min(first_secref_date, n = 1, with_ties = FALSE) %>%
  ungroup()

# Primary care hospitalisation: one row per patient (earliest)
primary_hosp <- primary_hosp %>%
  analysis$cached("clean_primary_dep_hospitalisation") %>%
  select(patid, first_primhosp_date = date) %>%
  collect() %>%
  mutate(patid = as.character(patid)) %>%
  group_by(patid) %>%
  slice_min(first_primhosp_date, n = 1, with_ties = FALSE) %>%
  ungroup()

# Secondary care hospitalisation: one row per patient (earliest)
second_hosp <- second_hosp %>%
  analysis$cached("clean_secondary_dep_hospitalisation") %>%
  select(patid, first_sechosp_date = date) %>%
  collect() %>%
  mutate(patid = as.character(patid)) %>%
  group_by(patid) %>%
  slice_min(first_sechosp_date, n = 1, with_ties = FALSE) %>%
  ungroup()

# Augmentation table
aug <- aug %>%
  analysis$cached("augmentation") %>%
  collect() %>%
  mutate(patid = as.character(patid)) %>%
  select(-ad_index_date)   # avoid duplicate with cohort join

# ── 3. Build cohort_phenos ────────────────────────────────────────────────────
cohort_phenos <- cohort %>%
  left_join(trd,          by = "patid") %>%
  left_join(switching,    by = "patid") %>%
  left_join(recurrence,   by = "patid") %>%
  left_join(secref,       by = "patid") %>%
  left_join(primary_hosp, by = "patid") %>%
  left_join(second_hosp,  by = "patid") %>%
  left_join(aug,          by = "patid")

# ── 4. Define outcome ─────────────────────────────────────────────────────────
base_data <- cohort_phenos %>%
  mutate(
    # Censoring hierarchy (strict definition)
    exit_date_strict = case_when(
      valid_ap_strict == 1                ~ first_ap_date_strict,
      !is.na(first_ap_date_supp_incident) ~ first_ap_date_supp_incident,
      TRUE                                ~ gp_death_end_date
    ),
    event_strict = valid_ap_strict,
    time_years   = as.numeric(exit_date_strict - ad_index_date) / 365.25
  ) %>%
  filter(time_years > 0)

# ── 5. Sanity checks ──────────────────────────────────────────────────────────
cat("\n── base_data sanity checks ──\n")
base_data %>% summarise(
  n_total          = n(),
  n_events         = sum(event_strict),
  pct_events       = mean(event_strict) * 100,
  any_missing_exit = sum(is.na(exit_date_strict)),
  flag_but_no_date = sum(valid_ap_strict == 1 & is.na(first_ap_date_strict)),
  date_but_no_flag = sum(valid_ap_strict == 0 & !is.na(first_ap_date_strict))
) %>% print()

# ── 6. Helper: build counting-process dataset for a single exposure ───────────
# Removes prevalent cases (exposure on or before index date)
# Nulls out exposure occurring on or after exit date (prevents immortal time bias)
make_tv_data <- function(data, exposure_date_col) {
  data %>%
    # Remove prevalent exposure
    filter(is.na(.data[[exposure_date_col]]) |
             .data[[exposure_date_col]] > ad_index_date) %>%
    mutate(
      exposure_time = as.numeric(.data[[exposure_date_col]] - ad_index_date) / 365.25,

      # Null out exposure if on or after exit — exposure cannot follow event
      exposure_time = ifelse(
        !is.na(exposure_time) & exposure_time >= time_years,
        NA_real_,
        exposure_time
      )
    ) %>%
    # Define survival outcome
    tmerge(
      data1 = .,
      data2 = .,
      id    = patid,
      event = event(time_years, event_strict)
    ) %>%
    # Add time-varying exposure: flips 0 → 1 at exposure_time
    tmerge(
      data1 = .,
      data2 = .,
      id    = patid,
      tv_exposure = tdc(exposure_time)
    )
}

# ── 7. Helper: fit Cox model and extract exposure term ────────────────────────
run_tv_cox <- function(tv_data, exposure_label) {
  model <- coxph(
    Surv(tstart, tstop, event) ~ tv_exposure +
      age_at_ad_index + gender + year_of_diagnosis,
    data = tv_data
  )

  coefs    <- coef(model)["tv_exposure"]
  se       <- sqrt(vcov(model)["tv_exposure", "tv_exposure"])
  hr       <- exp(coefs)
  hr_lower <- exp(coefs - 1.96 * se)
  hr_upper <- exp(coefs + 1.96 * se)
  pval     <- summary(model)$coefficients["tv_exposure", "Pr(>|z|)"]

  n_total   <- tv_data %>% distinct(patid) %>% nrow()
  n_exposed <- tv_data %>%
    group_by(patid) %>%
    summarise(ever_exposed = max(tv_exposure, na.rm = TRUE)) %>%
    summarise(n = sum(ever_exposed == 1)) %>%
    pull(n)

  data.frame(
    exposure  = exposure_label,
    hr        = round(hr,       2),
    hr_lower  = round(hr_lower, 2),
    hr_upper  = round(hr_upper, 2),
    p_value   = ifelse(pval < 0.001, "<0.001", as.character(round(pval, 3))),
    hr_ci     = sprintf("%.2f (%.2f-%.2f)", hr, hr_lower, hr_upper),
    n_total   = n_total,
    n_exposed = n_exposed,
    stringsAsFactors = FALSE
  )
}

# ── 8. Build time-varying datasets ────────────────────────────────────────────
cat("\nBuilding time-varying datasets...\n")
tv_trd          <- make_tv_data(base_data, "trd_date")
tv_ssri_switch  <- make_tv_data(base_data, "first_ssri_switch_date")
tv_class_switch <- make_tv_data(base_data, "first_class_switch_date")
tv_recurrence   <- make_tv_data(base_data, "first_recurrence_date")
tv_secref       <- make_tv_data(base_data, "first_secref_date")
tv_primhosp     <- make_tv_data(base_data, "first_primhosp_date")
tv_sechosp      <- make_tv_data(base_data, "first_sechosp_date")

# ── 9. Fit Cox models ─────────────────────────────────────────────────────────
cat("Fitting Cox models...\n")
tv_results <- rbind(
  run_tv_cox(tv_trd,          "TRD"),
  run_tv_cox(tv_ssri_switch,  "SSRI Switch"),
  run_tv_cox(tv_class_switch, "Class Switch"),
  run_tv_cox(tv_recurrence,   "Recurrence"),
  run_tv_cox(tv_secref,       "Secondary Care Referral"),
  run_tv_cox(tv_primhosp,     "Primary Care Hospitalisation"),
  run_tv_cox(tv_sechosp,      "Secondary Care Hospitalisation")
)

# ── 10. FDR correction ────────────────────────────────────────────────────────
tv_results <- tv_results %>%
  mutate(
    p_numeric   = as.numeric(ifelse(p_value == "<0.001", 0.0001, p_value)),
    p_fdr       = p.adjust(p_numeric, method = "fdr"),
    significant = p_fdr < 0.05
  )

cat("\n── Time-varying Cox results ──\n")
print(tv_results)

# ── 11. Forest plot ───────────────────────────────────────────────────────────
plot_data <- tv_results %>%
  mutate(
    estimate      = hr,
    conf_low      = hr_lower,
    conf_high     = hr_upper,
    y_pos         = rev(row_number()),
    or_label      = ifelse(
      significant,
      sprintf("%.2f (%.2f, %.2f) <b>*</b>", estimate, conf_low, conf_high),
      sprintf("%.2f (%.2f, %.2f)", estimate, conf_low, conf_high)
    ),
    display_label = exposure
  )

y_range    <- range(plot_data$y_pos)
base_theme <- theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

p_labels <- ggplot(plot_data, aes(x = 0, y = y_pos)) +
  geom_text(
    aes(label = display_label),
    hjust = 0, vjust = 0.5, size = 3, colour = "grey20"
  ) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5),
                     expand = expansion(0)) +
  base_theme

p_forest <- ggplot(plot_data, aes(x = estimate, y = y_pos)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50",
             linewidth = 0.5) +
  geom_errorbarh(
    aes(xmin = conf_low, xmax = conf_high),
    height = 0.2, linewidth = 0.7, colour = "black"
  ) +
  geom_point(size = 3, colour = "black") +
  scale_x_continuous(
    trans  = "log",
    breaks = c(1, 2, 5, 10, 25, 50),
    labels = c("1", "2", "5", "10", "25", "50"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5),
                     expand = expansion(0)) +
  labs(x = "Hazard Ratio (log scale)") +
  base_theme +
  theme(
    axis.text.x         = element_text(size = 10, colour = "grey20"),
    axis.title.x        = element_text(size = 11, colour = "grey20",
                                       margin = margin(t = 5)),
    axis.line.x         = element_line(colour = "grey20", linewidth = 0.8),
    axis.ticks.x        = element_line(colour = "grey20", linewidth = 0.8),
    axis.ticks.length.x = unit(3, "pt"),
    panel.grid.major.x  = element_line(colour = "grey88", linewidth = 0.4)
  )

p_or <- ggplot(plot_data, aes(x = 0, y = y_pos)) +
  geom_richtext(
    aes(label = or_label),
    hjust = 0, vjust = 0.5, size = 3, colour = "grey30",
    fill = NA, label.color = NA, label.padding = unit(0, "lines")
  ) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5),
                     expand = expansion(0)) +
  base_theme

combined <- p_labels + p_forest + p_or +
  plot_layout(widths = c(2, 3, 1.5)) +
  plot_annotation(
    title = "Clinical Phenotype Predictors of Antipsychotic Augmentation",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

ggsave(
  filename = "augmentation_tv_forest_plot.png",
  plot     = combined,
  width    = 16,
  height   = nrow(plot_data) * 0.5 + 2,
  dpi      = 300,
  bg       = "white"
)

cat("\nForest plot saved to augmentation_tv_forest_plot.png\n")
