library(ggplot2)
library(dplyr)
library(aurum)
library(dbplyr)
library(patchwork)
library(lubridate)
library(tidyverse)
library(tidyr)
library(broom)
library(ggtext)
library(patchwork)

cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

#Collect data for analysis
analysis <- cprd$analysis("dh_clinhet")
phq <- phq %>% analysis$cached("phq_scores_ad_index")
phq_cohort <- cohort %>% select(patid, ad_index_date, age_at_ad_code_index, gender, ethnicity_5cat, smoking_cat, 
                         alcohol_cat, imd_decile) %>%
  left_join(phq_final) %>% collect()

# Prepare modelling dataset
model_data <- phq_cohort %>%
  mutate(
    has_phq        = as.integer(!is.na(phq_score)),
    age_per_decade = age_at_ad_code_index / 10,
    gender         = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    ethnicity_5cat = factor(ethnicity_5cat,
                            levels = c("0", "1", "2", "3", "4"),
                            labels = c("White", "South Asian", "Black", "Other", "Mixed")),
    smoking_cat    = factor(smoking_cat,
                            levels = c("Non-smoker", "Ex-smoker", "Active smoker")),
    alcohol_cat    = factor(alcohol_cat,
                            levels = c("None", "Within limits", "Excess", "Harmful")),
    imd_decile     = factor(imd_decile, levels = 1:10,
                            labels = c("1 (Most Deprived)", "2", "3", "4", "5",
                                       "6", "7", "8", "9", "10 (Least Deprived)"))
  )

# Helper using manual Wald CIs for speed
extract_or <- function(model, var_label) {
  tidy(model, exponentiate = FALSE, conf.int = FALSE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      conf_low  = exp(estimate - 1.96 * std.error),
      conf_high = exp(estimate + 1.96 * std.error),
      estimate  = exp(estimate),
      variable  = var_label
    ) %>%
    select(term, estimate, conf_low, conf_high, p.value, variable)
}

# --- Models ---
m_age       <- glm(has_phq ~ age_per_decade + gender,
                   data = model_data, family = binomial())
m_sex       <- glm(has_phq ~ gender + age_per_decade,
                   data = model_data, family = binomial())
m_ethnicity <- glm(has_phq ~ ethnicity_5cat + age_per_decade + gender,
                   data = model_data, family = binomial())
m_smoking   <- glm(has_phq ~ smoking_cat + age_per_decade + gender,
                   data = model_data, family = binomial())
m_alcohol   <- glm(has_phq ~ alcohol_cat + age_per_decade + gender,
                   data = model_data, family = binomial())
m_imd       <- glm(has_phq ~ imd_decile + age_per_decade + gender,
                   data = model_data, family = binomial())

# --- Extract and process results ---
results <- bind_rows(
  extract_or(m_age,       "Age") %>% filter(term == "age_per_decade"),
  extract_or(m_sex,       "Sex (vs Male)") %>% filter(grepl("gender", term)),
  extract_or(m_ethnicity, "Ethnicity (vs White)") %>% filter(grepl("ethnicity", term)),
  extract_or(m_smoking,   "Smoking (vs Non-smoker)") %>% filter(grepl("smoking", term)),
  extract_or(m_alcohol,   "Alcohol (vs None)") %>% filter(grepl("alcohol", term)),
  extract_or(m_imd,       "IMD Decile (vs 1 - Most Deprived)") %>% filter(grepl("imd", term))
) %>%
  mutate(
    term = term %>%
      gsub("gender",         "", .) %>%
      gsub("ethnicity_5cat", "", .) %>%
      gsub("smoking_cat",    "", .) %>%
      gsub("alcohol_cat",    "", .) %>%
      gsub("imd_decile",     "", .) %>%
      trimws(),
    term        = ifelse(term == "age_per_decade", "Age (per decade)", term),
    p_fdr       = p.adjust(p.value, method = "fdr"),
    significant = p_fdr < 0.05,
    OR          = sprintf("%.2f", estimate),
    CI          = sprintf("(%.2f, %.2f)", conf_low, conf_high),
    p_fmt       = sprintf("%.3f", p.value),
    p_fdr_fmt   = sprintf("%.3f", p_fdr)
  )

print(results %>% select(variable, term, OR, CI, p_fmt, p_fdr_fmt, significant))

# --- Build plot data ---
var_order <- c(
  "Age",
  "Sex (vs Male)",
  "Ethnicity (vs White)",
  "Smoking (vs Non-smoker)",
  "Alcohol (vs None)",
  "IMD Decile (vs 1 - Most Deprived)"
)

header_rows <- results %>%
  distinct(variable) %>%
  mutate(
    term          = NA_character_,
    estimate      = NA_real_,
    conf_low      = NA_real_,
    conf_high     = NA_real_,
    p.value       = NA_real_,
    p_fdr         = NA_real_,
    significant   = FALSE,
    is_header     = TRUE,
    display_label = as.character(variable)
  )

term_rows <- results %>%
  mutate(
    is_header     = FALSE,
    display_label = paste0("  ", term)
  )

plot_data <- bind_rows(header_rows, term_rows) %>%
  mutate(variable = factor(variable, levels = var_order)) %>%
  arrange(variable, desc(is_header)) %>%
  mutate(
    y_pos    = rev(row_number()),
    or_label = ifelse(
      !is_header,
      ifelse(
        significant,
        sprintf("%.2f (%.2f, %.2f) <b>*</b>", estimate, conf_low, conf_high),
        sprintf("%.2f (%.2f, %.2f)", estimate, conf_low, conf_high)
      ),
      NA_character_
    )
  )

y_range   <- range(plot_data$y_pos)
base_theme <- theme_void() +
  theme(
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

# --- Panel 1: Labels ---
p_labels <- ggplot(plot_data, aes(x = 0, y = y_pos)) +
  geom_text(
    aes(label    = display_label,
        fontface = ifelse(is_header, "bold", "plain"),
        size     = ifelse(is_header, 3.5, 3)),
    hjust = 0, vjust = 0.5, colour = "grey20", show.legend = FALSE
  ) +
  scale_size_identity() +
  scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5), expand = expansion(0)) +
  base_theme

# --- Panel 2: Forest plot ---
p_forest <- ggplot(plot_data, aes(x = estimate, y = y_pos)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50", linewidth = 0.5) +
  geom_errorbarh(
    data = plot_data %>% filter(!is_header),
    aes(xmin = conf_low, xmax = conf_high),
    height = 0.2, linewidth = 0.7, colour = "black"
  ) +
  geom_point(
    data = plot_data %>% filter(!is_header),
    size = 3, colour = "black"
  ) +
  scale_x_continuous(
    trans  = "log",
    breaks = c(0.5, 0.75, 1, 1.25, 1.5, 2),
    labels = c("0.50", "0.75", "1.00", "1.25", "1.50", "2.00"),
    expand = expansion(mult = c(0.05, 0.05))
  ) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5), expand = expansion(0)) +
  labs(x = "Odds Ratio (log scale)") +
  base_theme +
  theme(
    axis.text.x  = element_text(size = 10, colour = "grey20"),
    axis.title.x = element_text(size = 11, colour = "grey20", margin = margin(t = 5)),
    axis.line.x  = element_line(colour = "grey20", linewidth = 0.8),
    axis.ticks.x = element_line(colour = "grey20", linewidth = 0.8),
    axis.ticks.length.x = unit(3, "pt"),
    panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.4)
  )

# --- Panel 3: OR + CI text ---
p_or <- ggplot(plot_data %>% filter(!is_header), aes(x = 0, y = y_pos)) +
  geom_richtext(
    aes(label = or_label),
    hjust = 0, vjust = 0.5, size = 3, colour = "grey30",
    fill = NA, label.color = NA, label.padding = unit(0, "lines")
  ) +
  scale_x_continuous(limits = c(0, 1), expand = expansion(0)) +
  scale_y_continuous(limits = c(y_range[1] - 0.5, y_range[2] + 0.5), expand = expansion(0)) +
  base_theme

# --- Combine with patchwork ---
combined <- p_labels + p_forest + p_or +
  plot_layout(widths = c(2, 3, 1.5)) +
  plot_annotation(
    title    = "Predictors of PHQ-9 Availability",
    subtitle = "Odds ratios with 95% CIs. * = significant after FDR correction.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, colour = "grey40"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

ggsave(
  filename = "phq9_forest_plot.png",
  plot     = combined,
  width    = 16,
  height   = nrow(plot_data) * 0.4 + 2,
  dpi      = 300,
  bg       = "white"
)
