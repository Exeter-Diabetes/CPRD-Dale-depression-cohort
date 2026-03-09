library(RMariaDB)
library(dplyr)
library(tidyr)

con2 <- dbConnect(MariaDB(),
                  dbname = "",
                  host = "",
                  port = ,
                  user = "",
                  password = ""
)

con3 <- dbConnect(MariaDB(),
                  dbname = "",
                  host = "",
                  port = ,
                  user = "",
                  password = ""
)

cohort <- con2 %>% dbReadTable("dh_clinhet_depression_cohort_interim_4")
ad <- con2 %>% dbReadTable("dh_clinhet_clean_antidepressants")


#Pull vars local so that we can run Chiara's exact code
ad_table <- ad %>% inner_join(cohort %>% select(patid, ad_index_date), by = "patid")
ad_table <- ad_table %>% arrange(patid, date)
ad_table <- ad_table %>% rename(issue_date = date) %>% distinct()


#Run Chiara's code - available at https://github.com/chiarafabbri/MDD_TRD_study/blob/master/scripts/extract_diagn_ADs_TRD_pheno.R
ad_table %>%
  group_by(patid, chem_name) %>%
  arrange(issue_date) %>%
  mutate(prev_drug_date = dplyr::lag(issue_date, n = 1, default = NA)) %>%
  mutate(diff_weeks_drug = as.numeric(difftime(issue_date, prev_drug_date, units = "weeks"))) %>%
  mutate(prescription_episode = ifelse (diff_weeks_drug > 26, seq_along(diff_weeks_drug), 1)) %>%
  mutate(prescription_episode = ifelse (is.na(prescription_episode), 999, prescription_episode)) %>%
  mutate(prescription_episode = ifelse ( prescription_episode == 1,
                                         NA, prescription_episode)) %>%
  fill(prescription_episode) %>%
  mutate(prescription_episode = ifelse (prescription_episode == 999, 1, prescription_episode)) %>%
  ungroup() -> ad_table2


ad_table2 %>%
  group_by(patid,chem_name,prescription_episode) %>%
  arrange(issue_date) %>%
  mutate(duration_prescr_ep = as.numeric(difftime(lead(issue_date,1), issue_date, units = "weeks"))) %>%
  mutate(prescription_episode_drug = sum(as.numeric(duration_prescr_ep),na.rm=T)) %>%
  mutate(adequate_prescr_period = ifelse((diff_weeks_drug <= 14 & !is.na(diff_weeks_drug)), 1,
                                         ifelse(is.na(diff_weeks_drug), NA, 0))) %>% # if there are > 14 weeks between two consecutive prescriptions the adherence to the drug could be low
  mutate(n_adequate_prescr = sum(adequate_prescr_period,na.rm=T)) %>%
  mutate(n_prescr = length(!is.na(adequate_prescr_period))) %>%
  mutate(adequate_prescr_prop = ifelse((!is.na(adequate_prescr_period) & !is.na(n_adequate_prescr) & n_adequate_prescr >0), n_adequate_prescr / n_prescr, NA)) %>%
  mutate(last_drug_prescr = ifelse(issue_date == max(issue_date), 'yes', 'no')) %>% # this says when each prescription episode for a drug ends
  ungroup() -> ad_table2


ad_table2 %>%
  group_by(patid) %>%
  mutate(adequate_prescr_mean = mean(adequate_prescr_prop, na.rm=T)) %>%
  mutate(adequate_prescr_sd = sd(adequate_prescr_prop, na.rm=T)) %>%
  ungroup() -> ad_table2


ad_table2 %>%
  group_by(patid) %>%
  arrange(issue_date) %>%
  mutate(drug_distinct = n_distinct(chem_name,na.rm=T)) %>%
  mutate(drug_switch = ifelse((chem_name != lead(chem_name,1) & last_drug_prescr == 'yes'), 'yes', 'no')) %>%
  mutate(prescription_episode_drug = ifelse((prescription_episode_drug == 0 & as.numeric(difftime(lead(issue_date,1), issue_date, units = "weeks")) <=14), as.numeric(difftime(lead(issue_date,1), issue_date, units = "weeks")), prescription_episode_drug)) %>%
  mutate(between_drugs_weeks = ifelse(drug_switch == 'yes', abs(as.numeric(difftime(issue_date, lead(issue_date,1), units = "weeks"))), 999)) %>%
  mutate(treatment_resistance_switch = ifelse((prescription_episode_drug >= 6 & between_drugs_weeks <= 14 & between_drugs_weeks > 0 &  drug_switch == 'yes' & drug_distinct > 2), 1, 0)) %>%
  mutate(treatment_resistance_drug = ifelse(sum(as.numeric(treatment_resistance_switch),na.rm=T) >= 2, 1, 0)) %>%
  ungroup() -> ad_table2

count <- ad_table2 %>%
  group_by(patid) %>%
  summarise(
    n_rep = sum(duplicated(chem_name)),  # count repeats within patid
    tot   = n(),                         # total rows for that patid
    .groups = "drop"
  )

count$diff <- ifelse(count$n_rep > 0, count$tot - count$n_rep, NA)
sub <- count[count$diff <= 2 & !is.na(count$diff),]

# if count$diff = 2, it means two valid (non-repeated drugs) switches were done
# if count$diff = 1, only one non-repeated drug switch was done, these should not be considered as TRD
del_drug <- sub[sub$diff == 1,] # to check number of individuals
colnames(del_drug) <- c('patid', 'n_rep_switch_drug', 'tot_drug_swtiches', 'diff_drug_sw')

dep2 <- merge(ad_table2, del_drug, by='patid', all.x=T)
dep2$treatment_resistance_drug <- ifelse((dep2$diff_drug_sw==1 & !is.na(dep2$diff_drug_sw)), 0, dep2$treatment_resistance_drug) #5.2% ever switch


#This code gets the start date after which two switches have occurred (IE the TRD date).
trd_dates <- dep2 %>%
  group_by(patid) %>%
  arrange(issue_date, .by_group = TRUE) %>%
  mutate(
    next_issue_date = lead(issue_date),
    next_chem       = lead(chem_name),
    # "start date of next AD" only for qualifying switches
    switch_to_date  = if_else(treatment_resistance_switch == 1,
                              next_issue_date,
                              as.Date(NA))
  ) %>%
  # keep only qualifying switches AFTER computing the lead on full data
  filter(treatment_resistance_switch == 1) %>%
  # optional safety: ensure there really is a next drug
  filter(!is.na(switch_to_date)) %>%
  summarise(
    trd_date = dplyr::nth(switch_to_date, 2),
    .groups = "drop"
  )

dep4 <- dep2 %>%
  left_join(trd_dates, by = "patid")


#sanity checks
dep4 %>% filter(treatment_resistance_drug == 1) %>% 
  filter(!is.na(trd_date)) %>% distinct(patid) %>% count()




dbExecute(con2, "
CREATE TABLE IF NOT EXISTS dh_clinhet_TRD (
  patid                        BIGINT,
  issue_date                   DATE,
  dosageid                     VARCHAR(255),
  quantity                     DOUBLE,
  quantunitid                  INT,
  duration                     BIGINT,
  chem_name                    VARCHAR(255),
  drug_class                   VARCHAR(255),
  ad_index_date                DATE,
  prev_drug_date               DATE,
  diff_weeks_drug              DOUBLE,
  prescription_episode         DOUBLE,
  duration_prescr_ep           DOUBLE,
  prescription_episode_drug    DOUBLE,
  adequate_prescr_period       DOUBLE,
  n_adequate_prescr            DOUBLE,
  n_prescr                     INT,
  adequate_prescr_prop         DOUBLE,
  last_drug_prescr             VARCHAR(10),
  adequate_prescr_mean         DOUBLE,
  adequate_prescr_sd           DOUBLE,
  drug_distinct                INT,
  drug_switch                  VARCHAR(10),
  between_drugs_weeks          DOUBLE,
  treatment_resistance_switch  DOUBLE,
  treatment_resistance_drug    DOUBLE,
  n_rep_switch_drug            INT,
  tot_drug_swtiches            INT,
  diff_drug_sw                 INT,
  trd_date                     DATE
);
")


dbExecute(con2, "START TRANSACTION;")

for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- dep4[idx, ]
  
  processed <- lapply(chunk, function(col) {
    if (is.character(col) || is.factor(col)) {
      paste0("'", gsub("'", "''", as.character(col)), "'")
    } else if (inherits(col, "Date")) {
      ifelse(is.na(col), "NULL", paste0("'", format(col, "%Y-%m-%d"), "'"))
    } else if (is.numeric(col)) {
      ifelse(is.na(col) | is.nan(col) | is.infinite(col),
             "NULL",
             format(col, scientific = FALSE))
    } else {
      rep("NULL", length(col))
    }
  })
  
  row_strings <- do.call(paste, c(processed, sep = ","))
  row_strings <- paste0("(", row_strings, ")")
  
  query <- paste0(
    "INSERT INTO dh_clinhet_TRD (",
    paste(
      c(
        "patid",
        "issue_date",
        "dosageid",
        "quantity",
        "quantunitid",
        "duration",
        "chem_name",
        "drug_class",
        "ad_index_date",
        "prev_drug_date",
        "diff_weeks_drug",
        "prescription_episode",
        "duration_prescr_ep",
        "prescription_episode_drug",
        "adequate_prescr_period",
        "n_adequate_prescr",
        "n_prescr",
        "adequate_prescr_prop",
        "last_drug_prescr",
        "adequate_prescr_mean",
        "adequate_prescr_sd",
        "drug_distinct",
        "drug_switch",
        "between_drugs_weeks",
        "treatment_resistance_switch",
        "treatment_resistance_drug",
        "n_rep_switch_drug",
        "tot_drug_swtiches",
        "diff_drug_sw",
        "trd_date"
      ),
      collapse = ", "
    ),
    ") VALUES ",
    paste(row_strings, collapse = ",")
  )
  
  dbExecute(con2, query)
  
  print(paste("Inserted chunk", i, "of", nchunks))
}

dbExecute(con2, "COMMIT;")

library(ggplot2)
library(dplyr)
library(forcats)
library(patchwork)

plot_colours <- c(
  "Switch 1" = "#2C3E50",
  "Switch 2" = "#2980B9",
  "Switch 3" = "#27AE60"
)

# ── Plot 1: Time from first antidepressant to TRD date ──────────────────────

trd_time <- dep4 %>%
  filter(treatment_resistance_drug == 1) %>%
  distinct(patid, ad_index_date, trd_date) %>%
  mutate(weeks_to_trd = as.numeric(difftime(trd_date, ad_index_date, units = "weeks")),
         years_to_trd = weeks_to_trd / 52) %>%
  filter(!is.na(years_to_trd) & years_to_trd >= 0)

p1 <- ggplot(trd_time, aes(x = years_to_trd)) +
  geom_histogram(binwidth = 0.25, fill = "#2C3E50", colour = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = median(years_to_trd)),
             colour = "#E74C3C", linetype = "dashed", linewidth = 0.8) +
  annotate("text",
           x = median(trd_time$years_to_trd) + 0.3,
           y = Inf, vjust = 2,
           label = paste0("Median: ", round(median(trd_time$years_to_trd), 1), " yrs"),
           colour = "#E74C3C", size = 3.5) +
  scale_x_continuous(breaks = seq(0, ceiling(max(trd_time$years_to_trd)), by = 1)) +
  labs(
    title = "A) Time from First Antidepressant to TRD",
    x = "Years to TRD",
    y = "Number of patients"
  ) +
  theme_minimal(base_size = 13)


# ── Plot 2: Most common antidepressant at each switch position ───────────────

switch_positions <- dep4 %>%
  filter(treatment_resistance_switch == 1) %>%
  group_by(patid) %>%
  arrange(issue_date) %>%
  mutate(switch_position = paste0("Switch ", row_number())) %>%
  ungroup() %>%
  filter(switch_position %in% c("Switch 1", "Switch 2", "Switch 3"))

switch_counts <- switch_positions %>%
  group_by(switch_position, chem_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(switch_position) %>%
  slice_max(n, n = 10) %>%
  ungroup() %>%
  mutate(chem_name = fct_reorder(chem_name, n))

p2 <- ggplot(switch_counts, aes(x = chem_name, y = n, fill = switch_position)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ switch_position, scales = "free_y") +
  coord_flip() +
  scale_fill_manual(values = plot_colours) +
  labs(
    title = "B) Most Common Antidepressants at Each Treatment Switch",
    x = NULL,
    y = "Number of prescriptions"
  ) +
  theme_minimal(base_size = 13) +
  theme(strip.text = element_text(face = "bold"))


# ── Combine into panelled plot ───────────────────────────────────────────────

combined <- p1 / p2 +
  plot_annotation(
    title = "Treatment-Resistant Depression Phenotype",
    subtitle = "Among patients meeting TRD criteria",
    theme = theme(
      plot.title    = element_text(size = 15, face = "bold"),
      plot.subtitle = element_text(size = 12, colour = "#7F8C8D")
    )
  )

combined

# ── Save ─────────────────────────────────────────────────────────────────────

ggsave("TRD_phenotype_panel.png", combined, width = 12, height = 14, dpi = 300)
