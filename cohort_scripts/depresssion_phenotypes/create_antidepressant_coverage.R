library(aurum)
library(tidyverse)
library(data.table)
library(dbplyr)
library(lubridate)
library(RMariaDB)

# Helper: build numerator / last_rx / next_rx for a given window end column
make_parts <- function(win_end_col, suffix) {
  
  numer <- rx %>%
    filter(date <= .data[[win_end_col]]) %>%
    group_by(patid) %>%
    summarise(!!paste0("numer_", suffix) := sum(quantity), .groups = "drop")
  
  last_rx <- rx %>%
    filter(date <= .data[[win_end_col]]) %>%
    group_by(patid) %>%
    slice_max(date, n = 1, with_ties = FALSE) %>%
    transmute(
      patid,
      !!paste0("last_date_", suffix) := date,
      !!paste0("last_qty_", suffix) := quantity,
      !!paste0("last_end_", suffix) := rx_end
    )
  
  next_rx <- rx %>%
    inner_join(last_rx, by = "patid") %>%
    filter(date > .data[[paste0("last_date_", suffix)]]) %>%
    group_by(patid) %>%
    summarise(
      !!paste0("next_date_", suffix) := suppressWarnings(min(date)),
      .groups = "drop"
    )
  
  list(numer = numer, last_rx = last_rx, next_rx = next_rx)
}


#Connect and load in data
cprdenvname <- ""
yaml <- ""
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

analysis <- cprd$analysis("dh_depsev")
cohort <- cohort %>% analysis$cached("full_depression_cohort")

index <- index %>% analysis$cached("AD_informed_index_dates")
index <- index %>% filter(!is.na(AD_informed_index_date) & 
                            AD_index_registration_90days == 1 & 
                            any_antidepressant == 1)

ad <- ad %>% analysis$cached("ADs")

ad_index <- ad %>% inner_join(index %>% select(patid, AD_informed_index_date), by = "patid") %>% 
  arrange(patid, date) %>% 
  analysis$cached("antidepressant_coverage_interim_1")

ad_index <- ad_index %>% 
  inner_join(cohort %>% select(patid, gp_death_end_date))

#To local - too complex to figure out in MySQL
ad_local <- ad_index %>%
  arrange(patid, date) %>% 
  collect()

### PDC at one year after first Rx ###
# Taken from https://www.medrxiv.org/content/10.1101/2025.01.23.25321011v1.full.pdf

#standardise df for input
df <- ad_local %>%
  mutate(
    date = as.Date(date),
    gp_death_end_date = as.Date(gp_death_end_date),
    quantity = as.integer(quantity)
  ) %>%
  # remove invalid and implausible quantities - 5SD quantity is ~
  filter(quantity > 0, quantity <= 183)

# -------------------------
# 1) First antidepressant date per patient
# -------------------------
first_rx <- df %>%
  group_by(patid) %>%
  summarise(first_rx_date = min(date), .groups = "drop")

# -------------------------
# 2) Patient-level table with 1/3/5y windows
# -------------------------
pt <- df %>%
  distinct(patid, gp_death_end_date) %>%
  left_join(first_rx, by = "patid") %>%
  mutate(
    win_end_1 = (first_rx_date %m+% years(1)) - days(1),
    win_end_3 = (first_rx_date %m+% years(3)) - days(1),
    win_end_5 = (first_rx_date %m+% years(5)) - days(1),
    
    denom_1 = as.integer(win_end_1 - first_rx_date + 1),
    denom_3 = as.integer(win_end_3 - first_rx_date + 1),
    denom_5 = as.integer(win_end_5 - first_rx_date + 1),
    
    ok_fu_1 = gp_death_end_date >= win_end_1,
    ok_fu_3 = gp_death_end_date >= win_end_3,
    ok_fu_5 = gp_death_end_date >= win_end_5
  )

# -------------------------
# 3) RX rows (on/after initiation) + rx_end
# -------------------------
rx <- df %>%
  inner_join(
    pt %>% select(patid, first_rx_date, win_end_1, win_end_3, win_end_5),
    by = "patid"
  ) %>%
  filter(date >= first_rx_date) %>%
  mutate(
    rx_end = date + (quantity - 1)
  )

w1 <- make_parts("win_end_1", "1")
w3 <- make_parts("win_end_3", "3")
w5 <- make_parts("win_end_5", "5")

pdc_all <- pt %>%
  left_join(w1$numer, by = "patid") %>%
  left_join(w1$last_rx, by = "patid") %>%
  left_join(w1$next_rx, by = "patid") %>%
  
  left_join(w3$numer, by = "patid") %>%
  left_join(w3$last_rx, by = "patid") %>%
  left_join(w3$next_rx, by = "patid") %>%
  
  left_join(w5$numer, by = "patid") %>%
  left_join(w5$last_rx, by = "patid") %>%
  left_join(w5$next_rx, by = "patid") %>%
  
  mutate(
    # ---- 1 year ----
    numer_1 = coalesce(numer_1, 0L),
    overhang_1 = !is.na(last_end_1) & last_end_1 > win_end_1,
    numer_adj_1 = if_else(overhang_1 & is.na(next_date_1), numer_1 - last_qty_1, numer_1),
    denom_adj_1 = if_else(overhang_1 & !is.na(next_date_1), as.integer(next_date_1 - first_rx_date), denom_1),
    pdc_1yr = if_else(ok_fu_1, as.numeric(numer_adj_1) / denom_adj_1, NA_real_),
    
    # ---- 3 years ----
    numer_3 = coalesce(numer_3, 0L),
    overhang_3 = !is.na(last_end_3) & last_end_3 > win_end_3,
    numer_adj_3 = if_else(overhang_3 & is.na(next_date_3), numer_3 - last_qty_3, numer_3),
    denom_adj_3 = if_else(overhang_3 & !is.na(next_date_3), as.integer(next_date_3 - first_rx_date), denom_3),
    pdc_3yr = if_else(ok_fu_3, as.numeric(numer_adj_3) / denom_adj_3, NA_real_),
    
    # ---- 5 years ----
    numer_5 = coalesce(numer_5, 0L),
    overhang_5 = !is.na(last_end_5) & last_end_5 > win_end_5,
    numer_adj_5 = if_else(overhang_5 & is.na(next_date_5), numer_5 - last_qty_5, numer_5),
    denom_adj_5 = if_else(overhang_5 & !is.na(next_date_5), as.integer(next_date_5 - first_rx_date), denom_5),
    pdc_5yr = if_else(ok_fu_5, as.numeric(numer_adj_5) / denom_adj_5, NA_real_)
  ) %>%
  select(patid, first_rx_date, pdc_1yr, pdc_3yr, pdc_5yr)

# Upload this data to the cluster
con2 <- dbConnect(MariaDB())

dbExecute(con2, "
CREATE TABLE dh_depsev_antidepressant_coverage (
patid BIGINT,
first_rx_date DATE, 
pdc_1yr FLOAT,
pdc_3yr FLOAT,
pdc_5yr FLOAT
);
")

chunksize <- 50000
total_rows <- nrow(pdc_all)
nchunks <- ceiling(total_rows / chunksize)


for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- pdc_all[idx, ]
  
  # Process each column vector-wise
  # The following 'lapply' returns a list where each element is a character vector
  # with each value already appropriately escaped/quoted
  processed <- lapply(chunk, function(col) {
    if (is.character(col) || is.factor(col)) {
      # Convert factors to character; escape single quotes and wrap in quotes
      paste0("'", gsub("'", "''", as.character(col)), "'")
    } else if (inherits(col, "Date")) {
      # Format dates or use NULL
      ifelse(is.na(col), "NULL", paste0("'", format(col, "%Y-%m-%d"), "'"))
    } else if (is.numeric(col)) {
      # For numerics, if NA/NaN/Inf then return NULL (without quotes)
      ifelse(is.na(col) | is.nan(col) | is.infinite(col),
             "NULL",
             format(col, scientific = FALSE))
    } else {
      # Fallback for other data types
      rep("NULL", length(col))
    }
  })
  
  # Now combine the processed columns into row strings.
  # do.call(..., sep=",") is equivalent to pasting together the elements by row.
  row_strings <- do.call(paste, c(processed, sep = ","))
  # Wrap each row in parentheses
  row_strings <- paste0("(", row_strings, ")")
  
  # Build the full multi-row INSERT query
  query <- paste0(
    "INSERT INTO dh_depsev_antidepressant_coverage (",
    paste(
      c(
        "patid",
        "first_rx_date", 
        "pdc_1yr",
        "pdc_3yr",
        "pdc_5yr"
      ),
      collapse = ", "
    ),
    ") VALUES ",
    paste(row_strings, collapse = ",")
  )
  
  # Execute the query for this chunk
  dbExecute(con2, query)
  
  print(paste("Inserted chunk", i, "of", nchunks))
}
