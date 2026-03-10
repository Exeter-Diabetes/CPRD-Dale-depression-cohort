# ── 1. Connect and load tables ────────────────────────────────────────────────
analysis <- cprd$analysis("dh_clinhet")
cohort   <- cohort    %>% analysis$cached("depression_cohort_interim_4")
ad_table <- ad_table  %>% analysis$cached("clean_antidepressants")

con2 <- dbConnect(MariaDB(),)

# ── 2. Get index dates from cohort ────────────────────────────────────────────
cohort_clean <- cohort %>%
  distinct(patid, .keep_all = TRUE) %>%
  select(patid, ad_index_date) %>%
  collect() %>%
  mutate(patid = as.character(patid))

# ── 3. De-duplicate prescriptions and collect ─────────────────────────────────
ad_clean <- ad_table %>%
  distinct(patid, date, chem_name, drug_class) %>%
  collect() %>%
  mutate(patid = as.character(patid))

# ── 4. Get index SSRI (first prescription per patient) ───────────────────────
index_ad <- ad_clean %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_rx_date = date,
         index_chem    = chem_name)
# index drug_class is always SSRI per study design

# ── 5. Restrict to cohort patients only and join index info ───────────────────
subsequent <- ad_clean %>%
  inner_join(cohort_clean, by = "patid") %>%
  inner_join(index_ad,     by = "patid") %>%
  filter(date > ad_index_date)   # post-index prescriptions only

# ── 6. First switch to a different SSRI ───────────────────────────────────────
first_ssri_switch <- subsequent %>%
  filter(drug_class == "SSRI" & chem_name != index_chem) %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_ssri_switch_date = date,
         first_ssri_switch_name = chem_name)

# ── 7. First switch to a different drug class ─────────────────────────────────
first_class_switch <- subsequent %>%
  filter(drug_class != "SSRI") %>%
  group_by(patid) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(patid,
         first_class_switch_date  = date,
         first_class_switch_name  = chem_name,
         first_class_switch_class = drug_class)

# ── 8. Combine into one row per patient ───────────────────────────────────────
ad_switches <- cohort_clean %>%
  left_join(index_ad,          by = "patid") %>%
  left_join(first_ssri_switch,  by = "patid") %>%
  left_join(first_class_switch, by = "patid")

head(ad_switches)

# ── 9. Quick summary ──────────────────────────────────────────────────────────
ad_switches %>%
  summarise(
    n_total             = n(),
    n_any_rx            = sum(!is.na(first_rx_date)),
    n_ssri_switch       = sum(!is.na(first_ssri_switch_date)),
    n_class_switch      = sum(!is.na(first_class_switch_date)),
    pct_ssri_switch     = mean(!is.na(first_ssri_switch_date)) * 100,
    pct_class_switch    = mean(!is.na(first_class_switch_date)) * 100
  )

# ── Create table ──────────────────────────────────────────────────────────────
dbExecute(con2, "
CREATE TABLE dh_clinhet_switching (
  patid                      BIGINT,
  ad_index_date              DATE,
  -- Index antidepressant
  first_rx_date              DATE,
  index_chem                 VARCHAR(100),
  -- First switch to a different SSRI
  first_ssri_switch_date     DATE,
  first_ssri_switch_name     VARCHAR(100),
  -- First switch to a different drug class
  first_class_switch_date    DATE,
  first_class_switch_name    VARCHAR(100),
  first_class_switch_class   VARCHAR(100)
);
")

# ── Chunk insert ──────────────────────────────────────────────────────────────
chunksize  <- 50000
total_rows <- nrow(ad_switches)
nchunks    <- ceiling(total_rows / chunksize)

for (i in seq_len(nchunks)) {
  idx   <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- ad_switches[idx, ]

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
    "INSERT INTO dh_clinhet_switching (",
    paste(
      c(
        "patid",
        "ad_index_date",
        "first_rx_date",
        "index_chem",
        "first_ssri_switch_date",
        "first_ssri_switch_name",
        "first_class_switch_date",
        "first_class_switch_name",
        "first_class_switch_class"
      ),
      collapse = ", "
    ),
    ") VALUES ",
    paste(row_strings, collapse = ",")
  )

  dbExecute(con2, query)
  print(paste("Inserted chunk", i, "of", nchunks))
}
