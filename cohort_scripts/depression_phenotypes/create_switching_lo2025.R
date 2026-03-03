library(RMariaDB)
library(tidyverse)
library(data.table)

source("")

con2 <- dbConnect(MariaDB())

con3 <- dbConnect(MariaDB())

#Load in relevant tables
cohort <- con2 %>% dbReadTable("dh_depsev_full_depression_cohort")
ad <- con2 %>% dbReadTable("dh_depsev_ADs")
ad_index <- con2 %>% dbReadTable("AD_informed_index_dates")
index_dates <-con2 %>% dbReadTable("dh_depsev_first_AD_date_table")

# QCed cohort
ad_table <- ad %>% inner_join(ad_index %>% select(patid, AD_informed_index_date, AD_index_registration_90days)) %>%
  filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1)

ad_table <- ad_table %>% arrange(patid, date)
ad_table <- ad_table %>% rename(issue_date = date) %>% distinct()

ad_table <- ad_table %>% group_by(patid) %>% arrange(issue_date) %>% ungroup()

switching_table <- switch_Lo2025(
  df = ad_table, 
  id_col = "patid", 
  name_col = "chem_name", 
  class_col = "drug_class", 
  date_col = "issue_date",
  time_switch = 90, nudge_days = 5,
  pre_switch_count = 2, post_switch_count = 2, total_count = 3,
  identify_controls = TRUE,
  prescription_episode_days_control = 180,
  consecutive_prescriptions_days_control = 30)

#Clean up the table.
switching_table2 <- switching_table %>% 
  group_by(pt_id) %>% 
  mutate(switch_date = index_date + as.integer(switch_time)) %>% 
  ungroup()

dbExecute(con2, "
CREATE TABLE dh_depsev_antidepressant_switching (
pt_id BIGINT, 
cc VARCHAR(255),
pre_drug VARCHAR(255), 
pre_class VARCHAR(255), 
switch_time INT,
post_drug VARCHAR(255), 
post_class VARCHAR(255), 
index_date DATE, 
switch_date DATE
);
")

#If changed
#dbExecute(con2, "DROP TABLE dh_depsev_antidepressant_switching")

#Upload to MySQL server
chunksize <- 50000
total_rows <- nrow(switching_table2)
nchunks <- ceiling(total_rows / chunksize)


dbExecute(con2, "START TRANSACTION;")

for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- switching_table2[idx, ]
  
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
    "INSERT INTO dh_depsev_antidepressant_switching (",
    paste(
      c(
        "pt_id",
        "cc", 
        "pre_drug", 
        "pre_class", 
        "switch_time",
        "post_drug",
        "post_class",
        "index_date",
        "switch_date"
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
