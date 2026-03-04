#Create PHQ-9 scoring.
library(ggplot2)
library(dplyr)
library(patchwork)
library(lubridate)

#Connect
cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"

cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)
codesets <- cprd$codesets()

#Load in the codeset
phq9 <- fread("")

type <- c("score", "partial", "partial", "partial", "partial", "partial", "partial", 
          "partial", "partial", "partial", "category", "category", "category", 
          "score", "category", "category", "score", "score", "score", "score", 
          "score")

phq9 <- phq9 %>% mutate(type = type)

codesets$loadMedCodeSet(codeSetDf = phq9, category = "type", colname = "MedCodeId", name = "PHQ9", version = "05/09/25")

phq9_codes <- codesets$getAllCodeSetVersion(v = "05/09/25")
phq9_codes <- phq9_codes$PHQ9

analysis <- cprd$analysis("all_patid")

#Generate the raw extract
raw_phq9_medcodes <- cprd$tables$observation %>% inner_join(phq9_codes, by = "medcodeid") %>%
  analysis$cached("raw_PHQ9_medcodes", indexes = c("patid", "obsdate", "PHQ9_cat"))

#Load in death end date and filter out PHQs outside of acceptable ranges.
valid_dates <- valid_dates %>% analysis$cached("death_end_dat")

clean_phq9_medcodes <- raw_phq9_medcodes %>% 
  inner_join(valid_dates, by="patid") %>%
  filter(obsdate>=min_dob & obsdate<=gp_death_end_date) %>%
  analysis$cached("clean_PHQ9_medcodes", indexes = c("patid", "obsdate", "PHQ9_cat"))


#Depression data
analysis <- cprd$analysis("all_patid")
phq <- phq %>% analysis$cached("clean_PHQ9_medcodes")

analysis <- cprd$analysis("dh_clinhet")
cohort <- cohort %>% analysis$cached("depression_cohort_interim_4")


#To PHQ-9 medcodes, add in AD_index_date
phq_table <- phq %>% inner_join(cohort %>% select(patid, ad_index_date), by = "patid") %>% 
  select(patid, medcodeid, obsdate, ad_index_date, testvalue, PHQ9_cat)

#Hard-coded conversion table
category_lookup <- tibble::tribble(
  ~medcodeid,              ~category_score,
  "1839571000006114",      17,
  "1839561000006119",      12,
  "1839581000006112",      23.5,
  "1839551000006116",      7,
  "1839541000006118",      2
)




# Step 1: Find valid partial dates (exactly 9 distinct codes within date window)
valid_partial_dates <- phq_table %>%
  filter(PHQ9_cat == "partial") %>%
  filter(
    datediff(obsdate, ad_index_date) >= -30,
    datediff(obsdate, ad_index_date) <= 7
  ) %>%
  group_by(patid, obsdate) %>%
  summarise(n_codes = n_distinct(medcodeid), .groups = "drop") %>%
  filter(n_codes == 9) %>%
  select(patid, obsdate)

# Step 2: Sum valid partial codes into a single score per patient-date
partial_as_scores <- phq_table %>%
  filter(PHQ9_cat == "partial") %>%
  filter(
    datediff(obsdate, ad_index_date) >= -30,
    datediff(obsdate, ad_index_date) <= 7
  ) %>%
  semi_join(valid_partial_dates, by = c("patid", "obsdate")) %>%
  group_by(patid, obsdate, ad_index_date) %>%
  summarise(testvalue = sum(testvalue, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PHQ9_cat  = "score",
    medcodeid = NA
  )

# Step 3: Clean main table — remove NAs, apply date window, drop all partials,
# replace category testvalues with hardcoded scores, add back partial scores
phq_table_clean <- phq_table %>%
  filter(
    !(PHQ9_cat %in% c("score", "partial") & is.na(testvalue))
  ) %>%
  filter(
    datediff(obsdate, ad_index_date) >= -30,
    datediff(obsdate, ad_index_date) <= 7
  ) %>%
  filter(PHQ9_cat != "partial") %>%
  left_join(category_lookup, by = "medcodeid", copy = T) %>%
  mutate(
    testvalue = case_when(
      PHQ9_cat == "category" ~ category_score,
      TRUE                   ~ testvalue
    )
  ) %>%
  select(-category_score) %>%
  union_all(partial_as_scores)

# Step 4: Select score closest to index date, preferring score > partial > category
phq_final <- phq_table_clean %>%
  mutate(
    abs_days      = abs(datediff(obsdate, ad_index_date)),
    type_priority = case_when(
      PHQ9_cat == "score"    ~ 1L,
      PHQ9_cat == "partial"  ~ 2L,
      PHQ9_cat == "category" ~ 3L
    )
  ) %>%
  group_by(patid) %>%
  window_order(abs_days, type_priority) %>%
  mutate(row_num = row_number()) %>%
  ungroup() %>%
  filter(row_num == 1) %>%
  filter(testvalue >= 0, testvalue <= 27) %>%
  select(patid, phq_score = testvalue) %>% analysis$cached("phq_scores_ad_index")

#Rebind PHQ with cohort and perform some plots
phq <- cohort %>% select(patid, ad_index_date) %>% left_join(phq_final) %>% collect()


#Exploratory plot

phq_colours <- c(
"None"                = "#2C3E50",
"Mild"                = "#2980B9",
"Moderate"            = "#27AE60",
"Moderately Severe"   = "#E67E22",
"Severe"              = "#E74C3C"
)

base_theme <- theme_minimal(base_size = 12) +
  theme(
    axis.text.x     = element_text(angle = 30, hjust = 1, size = 13),
    axis.text.y     = element_text(size = 13),
    legend.position = "none",
    plot.title      = element_text(face = "bold", size = 13)
  )

# Prepare data
phq_plot <- phq %>%
  mutate(
    has_phq = !is.na(phq_score),
    phq_cat = case_when(
      is.na(phq_score)  ~ NA_character_,
      phq_score <= 4    ~ "None",
      phq_score <= 9    ~ "Mild",
      phq_score <= 14   ~ "Moderate",
      phq_score <= 19   ~ "Moderately Severe",
      TRUE              ~ "Severe"
    ) %>% factor(levels = c("None", "Mild", "Moderate", "Moderately Severe", "Severe")),
    index_year = year(ad_index_date)
  )

# --- Plot 1: Overall availability ---
availability_counts <- phq_plot %>%
  count(has_phq) %>%
  mutate(
    label = ifelse(has_phq, "Available", "Missing"),
    pct   = n / sum(n) * 100
  )

p_availability <- ggplot(availability_counts, aes(x = label, y = n, fill = label)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("Available" = "#27AE60", "Missing" = "#E67E22")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Overall PHQ-9 Availability", x = NULL, y = "Count") +
  base_theme

# --- Plot 2: Distribution of scores ---
p_scores <- ggplot(phq_plot %>% filter(!is.na(phq_score)), aes(x = phq_score)) +
  geom_histogram(binwidth = 1, fill = "#2C3E50", colour = "white") +
  scale_x_continuous(breaks = seq(0, 27, by = 3)) +
  labs(title = "Distribution of PHQ-9 Scores", x = "PHQ-9 Score", y = "Count") +
  base_theme

# --- Plot 3: Distribution of clinical categories ---
cat_counts <- phq_plot %>%
  filter(!is.na(phq_cat)) %>%
  count(phq_cat) %>%
  mutate(pct = n / sum(n) * 100)

p_categories <- ggplot(cat_counts, aes(x = phq_cat, y = n, fill = phq_cat)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.5, size = 4) +
  scale_fill_manual(values = phq_colours) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "PHQ-9 Clinical Categories", x = NULL, y = "Count") +
  base_theme

# --- Plot 4: PHQ availability by year of index ---
year_counts <- phq_plot %>%
  filter(!is.na(index_year)) %>%
  count(index_year, has_phq) %>%
  group_by(index_year) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(label = factor(
    ifelse(has_phq, "Available", "Missing"),
    levels = c("Missing", "Available")
  ))

p_by_year <- ggplot(year_counts, aes(x = factor(index_year), y = n, fill = label)) +
  geom_col(position = "stack") +
  geom_text(
    data = year_counts %>% filter(has_phq),
    aes(label = sprintf("%.1f%%", pct)),
    position = position_stack(vjust = 1),
    vjust = -0.5,
    size = 4
  ) +
  scale_fill_manual(values = c("Available" = "#27AE60", "Missing" = "#E67E22")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "PHQ-9 Availability by Year of Index", x = "Year", y = "Count", fill = NULL) +
  base_theme +
  theme(legend.position = "right")

# --- Combine and save ---
combined_plot <- (p_availability | p_scores | p_categories) /
  p_by_year +
  plot_layout(heights = c(1, 1.2))

ggsave(
  filename = "phq9_plots.png",
  plot     = combined_plot,
  width    = 16,
  height   = 10,
  dpi      = 300
)


