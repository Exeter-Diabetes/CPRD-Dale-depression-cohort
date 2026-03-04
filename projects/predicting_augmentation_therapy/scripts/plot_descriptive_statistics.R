library(ggplot2)
library(dplyr)
library(patchwork)
library(aurum)


cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

analysis <- cprd$analysis("dh_clinhet")
cohort_interim_4 <- cohort_interim_4 %>% analysis$cached("cohort_interim_4")

# Colour scheme
plot_colours <- c(
  "Male"          = "#2C3E50",
  "Female"        = "#2980B9",
  "White"         = "#2C3E50",
  "South Asian"   = "#2980B9",
  "Black"         = "#27AE60",
  "Other"         = "#E67E22",
  "Mixed"         = "#7F8C8D",
  "Non-smoker"    = "#2C3E50",
  "Ex-smoker"     = "#2980B9",
  "Active smoker" = "#27AE60",
  "None"          = "#2C3E50",
  "Within limits" = "#2980B9",
  "Excess"        = "#E67E22",
  "Harmful"       = "#E74C3C"
)

# Collect only needed columns from the database
plot_data <- cohort_interim_4 %>%
  distinct(patid, .keep_all = TRUE) %>%
  select(patid, gender, age_at_ad_code_index, ethnicity_5cat,
         smoking_cat, alcohol_cat, first_antidepressant) %>%
  collect() %>%
  mutate(
    patid          = as.character(patid),
    gender         = factor(gender, levels = c(1, 2), labels = c("Male", "Female")),
    ethnicity_5cat = factor(ethnicity_5cat,
      levels = c(0, 1, 2, 3, 4),
      labels = c("White", "South Asian", "Black", "Other", "Mixed")),
    smoking_cat    = factor(smoking_cat,
      levels = c("Non-smoker", "Ex-smoker", "Active smoker")),
    alcohol_cat    = factor(alcohol_cat,
      levels = c("None", "Within limits", "Excess", "Harmful"))
  )

# Antidepressant counts — aggregated in DB before collecting
ad_counts <- cohort_interim_4 %>%
  distinct(patid, .keep_all = TRUE) %>%
  count(first_antidepressant) %>%
  collect() %>%
  filter(!is.na(first_antidepressant)) %>%
  mutate(
    n                    = as.numeric(n),
    pct                  = n / sum(n) * 100,
    first_antidepressant = reorder(first_antidepressant, n)
  )

# Shared theme
base_theme <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 13),
    axis.text.y     = element_text(size = 13),
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13)
  )

# Helper to add percentage labels to bar plots
pct_labels <- function(data, var) {
  data %>%
    filter(!is.na({{ var }})) %>%
    count({{ var }}) %>%
    mutate(pct = n / sum(n) * 100)
}

# Gender
gender_counts <- pct_labels(plot_data, gender)
p_gender <- ggplot(gender_counts, aes(x = gender, y = n, fill = gender)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = plot_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Gender", x = NULL, y = "Count") +
  base_theme

# Age
p_age <- ggplot(plot_data %>% filter(!is.na(age_at_ad_code_index)),
                aes(x = age_at_ad_code_index)) +
  geom_histogram(binwidth = 5, fill = "#2C3E50", colour = "white") +
  labs(title = "Age at Index", x = "Age", y = "Count") +
  base_theme

# Ethnicity
ethnicity_counts <- pct_labels(plot_data, ethnicity_5cat)
p_ethnicity <- ggplot(ethnicity_counts, aes(x = ethnicity_5cat, y = n, fill = ethnicity_5cat)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = plot_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Ethnicity", x = NULL, y = "Count") +
  base_theme

# Smoking
smoking_counts <- pct_labels(plot_data, smoking_cat)
p_smoking <- ggplot(smoking_counts, aes(x = smoking_cat, y = n, fill = smoking_cat)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = plot_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Smoking", x = NULL, y = "Count") +
  base_theme

# Alcohol
alcohol_counts <- pct_labels(plot_data, alcohol_cat)
p_alcohol <- ggplot(alcohol_counts, aes(x = alcohol_cat, y = n, fill = alcohol_cat)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = plot_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Alcohol Use", x = NULL, y = "Count") +
  base_theme

# Antidepressant
p_ad <- ggplot(ad_counts, aes(x = first_antidepressant, y = n, fill = first_antidepressant)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = setNames(
    colorRampPalette(c("#2C3E50", "#2980B9", "#27AE60", "#E67E22", "#7F8C8D"))(nrow(ad_counts)),
    levels(ad_counts$first_antidepressant)
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "First Antidepressant", x = NULL, y = "Count") +
  base_theme

# Combine and save
combined_plot <- (p_gender | p_age | p_ethnicity | p_smoking | p_alcohol) /
  p_ad +
  plot_layout(heights = c(1, 1.5))

ggsave(
  filename = "descriptive_plots.png",
  plot     = combined_plot,
  width    = 16,
  height   = 10,
  dpi      = 300
)
