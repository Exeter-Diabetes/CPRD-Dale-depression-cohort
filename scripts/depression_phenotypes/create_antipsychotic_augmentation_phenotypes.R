#Come back to bipolar meds here.
library(ggplot2)
library(patchwork)
library(DBI)
library(RMariaDB)
library(tidyverse)

# ── 1. Load data ──────────────────────────────────────────────────────────────
con2 <- dbConnect(MariaDB())

cohort <- con2 %>% dbReadTable("dh_clinhet_depression_cohort_interim_4") %>% collect()
ad     <- con2 %>% dbReadTable("dh_clinhet_TRD") %>% collect()
ap     <- con2 %>% dbReadTable("all_patid_clean_antipsychotics_prodcodes") %>% collect()

exclude_drugs <- c("prochlorperazine", "promazine", "levomepromazine", 
                   "flupentixol", "amitriptyline")


# ── 2. Join, flag, and clean ──────────────────────────────────────────────────
ap_post <- ap %>%
  inner_join(cohort %>% select(patid, ad_index_date), by = "patid") %>%
  collect() %>%
  arrange(patid, issuedate) %>%
  mutate(
    valid_ap  = ifelse(issuedate > ad_index_date, 1, 0),
    chem_name = antipsychotics_cat %>%
      str_trim() %>%
      str_to_lower() %>%
      str_remove("\\s.*$")
  ) %>%
  select(-antipsychotics_cat) %>% filter(!chem_name %in% exclude_drugs)

# ── 3. Define main augmentation agents ───────────────────────────────────────
ap_names <- c("quetiapine", "risperidone", "olanzapine", "aripiprazole")

# ── 4. Exclusion flags ────────────────────────────────────────────────────────

# Any AP prescription before index (any drug)
# Used in: strict and loose definitions
invalid_antipsychotic_preex <- ap_post %>%
  group_by(patid) %>%
  filter(valid_ap == 0) %>%
  summarise(first_ap_date_preex = min(issuedate))

# Pre-index prescription of one of the main 4 specifically
# Used in: vloose definition only (pre-index non-main APs are tolerated)
invalid_antipsychotic_main_preex <- ap_post %>%
  group_by(patid) %>%
  filter(valid_ap == 0 & chem_name %in% ap_names) %>%
  summarise(first_ap_date_main_preex = min(issuedate))

# Post-index prescription of a non-main AP
# Used in: strict definition only (post-index non-main APs are tolerated in loose/vloose)
invalid_antipsychotic_supp_incident <- ap_post %>%
  group_by(patid) %>%
  filter(valid_ap == 1) %>%
  filter(!chem_name %in% ap_names) %>%
  summarise(first_ap_date_supp_incident = min(issuedate))

# ── 5. First valid AP by definition ──────────────────────────────────────────

# STRICT definition:
#   - No pre-index AP of any drug                  (all(valid_ap == 1))
#   - No post-index non-main AP                    (!any(!chem_name %in% ap_names & valid_ap == 1))
#   - Post-index prescription of one of the main 4 (chem_name %in% ap_names)
# Most conservative — cleanest new-user augmentation signal
first_valid_ap_strict <- ap_post %>%
  group_by(patid) %>%
  filter(all(valid_ap == 1)) %>%
  filter(!any(!chem_name %in% ap_names & valid_ap == 1)) %>%
  filter(chem_name %in% ap_names) %>%
  arrange(issuedate) %>%
  slice(1) %>%
  ungroup() %>%
  select(patid,
         first_ap_date_strict = issuedate,
         first_ap_name_strict = chem_name)

# LOOSE definition:
#   - No pre-index AP of any drug                  (all(valid_ap == 1))
#   - Post-index non-main APs are tolerated
#   - Post-index prescription of one of the main 4 (chem_name %in% ap_names)
# Middle ground — excludes prevalent AP users but allows concurrent non-main AP use
first_valid_ap_loose <- ap_post %>%
  group_by(patid) %>%
  filter(all(valid_ap == 1)) %>%
  filter(chem_name %in% ap_names) %>%
  arrange(issuedate) %>%
  slice(1) %>%
  ungroup() %>%
  select(patid,
         first_ap_date_loose = issuedate,
         first_ap_name_loose = chem_name)

# VERY LOOSE definition:
#   - No pre-index prescription of the main 4 specifically
#   - Pre-index non-main APs are tolerated (e.g. low-dose quetiapine for sleep pre-index)
#   - Post-index non-main APs are tolerated
#   - Post-index prescription of one of the main 4 (chem_name %in% ap_names)
# Most inclusive — only requires the patient to be naive to the main 4 agents pre-index
first_valid_ap_vloose <- ap_post %>%
  group_by(patid) %>%
  filter(valid_ap == 1) %>%
  filter(chem_name %in% ap_names) %>%
  arrange(issuedate) %>%
  slice(1) %>%
  ungroup() %>%
  select(patid,
         first_ap_date_vloose = issuedate,
         first_ap_name_vloose = chem_name)

# ── 6. Descriptive: first AP ever (any drug, any time relative to index) ─────
first_ap_ever <- ap_post %>%
  group_by(patid) %>%
  arrange(issuedate) %>%
  slice(1) %>%
  ungroup() %>%
  select(patid,
         first_ap_ever_date = issuedate,
         first_ap_ever_name = chem_name)

# ── 7. Build master AP table ──────────────────────────────────────────────────
ap_table <- cohort %>%
  select(patid, ad_index_date) %>%
  left_join(invalid_antipsychotic_preex,         by = "patid") %>%
  left_join(invalid_antipsychotic_main_preex,    by = "patid") %>%
  left_join(invalid_antipsychotic_supp_incident, by = "patid") %>%
  left_join(first_ap_ever,                       by = "patid") %>%
  left_join(first_valid_ap_strict,               by = "patid") %>%
  left_join(first_valid_ap_loose,                by = "patid") %>%
  left_join(first_valid_ap_vloose,               by = "patid") %>%
  mutate(
    # STRICT: augmentation naive (no pre-index AP of any kind),
    # initiates one of the main 4, never receives a non-main AP post-index
    valid_ap_strict = ifelse(
      is.na(first_ap_date_preex) &
      is.na(first_ap_date_supp_incident) &
      !is.na(first_ap_date_strict), 1, 0),

    # LOOSE: augmentation naive (no pre-index AP of any kind),
    # initiates one of the main 4, post-index non-main APs tolerated
    valid_ap_loose = ifelse(
      is.na(first_ap_date_preex) &
      !is.na(first_ap_date_loose), 1, 0),

    # VERY LOOSE: no pre-index use of the main 4 specifically,
    # initiates one of the main 4 post-index, prior non-main APs tolerated
    valid_ap_vloose = ifelse(
      is.na(first_ap_date_main_preex) &
      !is.na(first_ap_date_vloose), 1, 0)
  )

# ── Create table ──────────────────────────────────────────────────────────────
dbExecute(con2, "
CREATE TABLE dh_clinhet_augmentation (
  patid                        BIGINT,
  ad_index_date                DATE,
  -- Exclusion flag dates
  first_ap_date_preex          DATE,
  first_ap_date_main_preex     DATE,
  first_ap_date_supp_incident  DATE,
  -- First AP ever (descriptive)
  first_ap_ever_date           DATE,
  first_ap_ever_name           VARCHAR(100),
  -- First valid AP: strict definition
  first_ap_date_strict         DATE,
  first_ap_name_strict         VARCHAR(100),
  -- First valid AP: loose definition
  first_ap_date_loose          DATE,
  first_ap_name_loose          VARCHAR(100),
  -- First valid AP: very loose definition
  first_ap_date_vloose         DATE,
  first_ap_name_vloose         VARCHAR(100),
  -- Analysis flags
  valid_ap_strict              TINYINT,
  valid_ap_loose               TINYINT,
  valid_ap_vloose              TINYINT
);
")

# ── Chunk insert ──────────────────────────────────────────────────────────────
chunksize  <- 50000
total_rows <- nrow(ap_table)
nchunks    <- ceiling(total_rows / chunksize)

for (i in seq_len(nchunks)) {

  idx   <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- ap_table[idx, ]

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
    "INSERT INTO dh_clinhet_augmentation (",
    paste(
      c(
        "patid",
        "ad_index_date",
        "first_ap_date_preex",
        "first_ap_date_main_preex",
        "first_ap_date_supp_incident",
        "first_ap_ever_date",
        "first_ap_ever_name",
        "first_ap_date_strict",
        "first_ap_name_strict",
        "first_ap_date_loose",
        "first_ap_name_loose",
        "first_ap_date_vloose",
        "first_ap_name_vloose",
        "valid_ap_strict",
        "valid_ap_loose",
        "valid_ap_vloose"
      ),
      collapse = ", "
    ),
    ") VALUES ",
    paste(row_strings, collapse = ",")
  )

  dbExecute(con2, query)
  print(paste("Inserted chunk", i, "of", nchunks))
}


# colours
col_dark    <- "#2C3E50"
col_mid     <- "#2980B9"
col_green   <- "#27AE60"
col_orange  <- "#E67E22"
col_grey    <- "#7F8C8D"

base_theme <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.background   = element_rect(fill = "white", colour = NA),
    plot.title         = element_text(face = "bold", size = 11),
    plot.subtitle      = element_text(size = 9, colour = col_grey),
    axis.title         = element_text(size = 9),
    axis.text          = element_text(size = 8),
    panel.grid.minor   = element_blank(),
    plot.margin        = margin(8, 8, 8, 8)
  )

# ── 1. Percentage meeting each augmentation definition ────────────────────────
def_pct <- ap_table %>%
  summarise(
    Strict       = mean(valid_ap_strict)  * 100,
    Loose        = mean(valid_ap_loose)   * 100,
    `Very Loose` = mean(valid_ap_vloose)  * 100
  ) %>%
  pivot_longer(everything(), names_to = "Definition", values_to = "Percentage") %>%
  mutate(Definition = factor(Definition, levels = c("Strict", "Loose", "Very Loose")))

p1 <- ggplot(def_pct, aes(x = Definition, y = Percentage, fill = Definition)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),
            vjust = -0.5, size = 3, fontface = "bold", colour = col_dark) +
  scale_fill_manual(values = c(
    "Strict"     = col_dark,
    "Loose"      = col_mid,
    "Very Loose" = col_green
  )) +
  scale_y_continuous(
    limits = c(0, max(def_pct$Percentage) * 1.2),
    labels = percent_format(scale = 1)
  ) +
  labs(
    title    = "A. Cohort Meeting Each Augmentation Definition",
    subtitle = "As a proportion of the full depression cohort",
    x        = "Augmentation Definition",
    y        = "Percentage (%)"
  ) +
  base_theme +
  theme(
    legend.position    = "none",
    panel.grid.major.x = element_blank()
  )

# ── 2. Most frequent incident augmentation agents (strict definition) ─────────
agent_counts <- ap_table %>%
  filter(valid_ap_strict == 1) %>%
  count(first_ap_name_strict, name = "n") %>%
  mutate(
    pct                  = n / sum(n) * 100,
    first_ap_name_strict = str_to_title(first_ap_name_strict),
    first_ap_name_strict = fct_reorder(first_ap_name_strict, pct)
  )

p2 <- ggplot(agent_counts, 
             aes(x = first_ap_name_strict, y = pct, fill = first_ap_name_strict)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.1f%%", pct)),
            hjust = -0.15, size = 3, fontface = "bold", colour = col_dark) +
  scale_fill_manual(values = c(
    "Quetiapine"   = col_dark,
    "Aripiprazole" = col_mid,
    "Risperidone"  = col_green,
    "Olanzapine"   = col_orange
  )) +
  scale_y_continuous(
    limits = c(0, max(agent_counts$pct) * 1.2),
    labels = percent_format(scale = 1)
  ) +
  coord_flip() +
  labs(
    title    = "B. Incident Augmentation Agents (Strict Definition)",
    subtitle = "Percentage of augmented patients by first prescribed agent",
    x        = NULL,
    y        = "Percentage (%)"
  ) +
  base_theme +
  theme(
    legend.position   = "none",
    panel.grid.major.y = element_blank()
  )

# ── 3. Time to augmentation in years (strict definition) ─────────────────────
tta <- ap_table %>%
  filter(valid_ap_strict == 1) %>%
  mutate(years_to_aug = as.numeric(first_ap_date_strict - ad_index_date) / 365.25) %>%
  filter(years_to_aug >= 0)

median_tta <- median(tta$years_to_aug)

p3 <- ggplot(tta, aes(x = years_to_aug)) +
  geom_histogram(binwidth = 0.5, fill = col_mid, colour = "white", alpha = 0.9) +
  geom_vline(xintercept = median_tta,
             colour = col_orange, linewidth = 1, linetype = "dashed") +
  annotate("text",
           x      = median_tta + 0.2,
           y      = Inf,
           vjust  = 1.5,
           hjust  = 0,
           label  = sprintf("Median: %.1f years", median_tta),
           colour = col_orange, fontface = "bold", size = 3) +
  scale_x_continuous(breaks = seq(0, ceiling(max(tta$years_to_aug)), by = 1)) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "C. Time to Augmentation (Strict Definition)",
    subtitle = "6-month bins; dashed line = median time to augmentation",
    x        = "Years from Index to First Augmentation",
    y        = "Number of Patients"
  ) +
  base_theme +
  theme(panel.grid.major.x = element_blank())

# ── 4. Combine and save ───────────────────────────────────────────────────────
combined <- (p1 | p2) / p3 +
  plot_annotation(
    title   = "Antipsychotic Augmentation in a UK Primary Care Depression Cohort",
    caption = "Index date: first antidepressant initiation. Strict definition: no pre-index AP, no post-index non-main AP.",
    theme   = theme(
      plot.background = element_rect(fill = "white", colour = NA),
      plot.title      = element_text(face = "bold", size = 13, colour = col_dark),
      plot.caption    = element_text(size = 7.5, colour = col_grey, hjust = 0)
    )
  )

ggsave("augmentation_plots.png", combined,
       width = 12, height = 9, dpi = 300, bg = "white")

