library(ggplot2)
library(dplyr)
library(aurum)
library(dbplyr)
library(patchwork)
library(lubridate)
library(tidyverse)

cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)


analysis <- cprd$analysis("all_patid")
ads <- ads %>% analysis$cached("clean_antidepressant_prodcodes") #3598070

analysis <- cprd$analysis("dh_clinhet")
cohort <- cohort %>% analysis$cached("cohort_interim_4")

ad_table <- ads %>% select(patid, date, chem_name, drug_class) %>% 
  inner_join(cohort %>% select(patid, any_antidepressant, ad_index_date), by = "patid") %>% 
  collect()

ad_table <- ad_table %>% arrange(patid, date)

# Step 1: Deduplicate consecutive same chem_name
ad_sequences <- ad_table %>%
  arrange(patid, date) %>%
  group_by(patid) %>%
  filter(chem_name != lag(chem_name, default = "")) %>%
  mutate(rx_number = row_number()) %>%
  filter(rx_number <= 3) %>%
  ungroup()

# Step 2: Pivot wide — first rx uses chem_name, 2nd/3rd use drug_class
ad_wide <- ad_sequences %>%
  mutate(value = ifelse(rx_number == 1, chem_name, drug_class)) %>%
  select(patid, rx_number, value) %>%
  pivot_wider(
    names_from   = rx_number,
    values_from  = value,
    names_prefix = "rx_"
  )

# Step 3: Build transition pairs (1->2 and 2->3 separately)
transitions_1_2 <- ad_wide %>%
  filter(!is.na(rx_2)) %>%
  filter(!rx_2 %in% c("MAOI", "tetracyclic", "SARI")) %>%
  count(from = rx_1, to = rx_2) %>%
  mutate(transition = "First to Second AD")

transitions_2_3 <- ad_wide %>%
  filter(!is.na(rx_2), !is.na(rx_3)) %>%
  filter(!rx_2 %in% c("MAOI", "tetracyclic", "SARI")) %>%
  filter(!rx_3 %in% c("MAOI", "tetracyclic", "SARI")) %>%
  count(from = rx_2, to = rx_3) %>%
  mutate(transition = "Second to Third AD")

# Step 4: Calculate row percentages (% of people on drug X who switch to Y)
transitions <- bind_rows(transitions_1_2, transitions_2_3) %>%
  group_by(transition, from) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

# Step 5: Order factors
chem_order  <- c("citalopram", "escitalopram", "fluoxetine", "paroxetine", "sertraline")
class_order <- c("SSRI", "SNRI", "tricyclic", "atypical")

transitions <- transitions %>%
  mutate(
    from = factor(from, levels = c(chem_order, class_order)),
    to   = factor(to,   levels = class_order)
  )

# Step 6: Heatmap
ggplot(transitions, aes(x = to, y = from, fill = pct)) +
  geom_tile(colour = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), size = 3.5, colour = "white", fontface = "bold") +
  facet_wrap(~ transition, scales = "free_y") +
  scale_fill_gradientn(
    colours = c("#2C3E50", "#2980B9", "#27AE60", "#E67E22", "#E74C3C"),
    name    = "% of patients"
  ) +
  labs(
    title    = "Antidepressant Transition Patterns",
    subtitle = "Row percentages show proportion switching to each class",
    x        = "Switched to",
    y        = "Switched from"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(size = 11, colour = "grey40"),
    strip.text       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid       = element_blank(),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

ggsave(
  filename = "ad_transition_heatmap.png",
  width    = 14,
  height   = 8,
  dpi      = 300,
  bg       = "white"
)
