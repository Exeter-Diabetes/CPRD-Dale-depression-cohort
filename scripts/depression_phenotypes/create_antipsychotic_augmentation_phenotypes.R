#Come back to bipolar meds here.

con2 <- dbConnect(MariaDB(),)

cohort <- con2 %>% dbReadTable("dh_clinhet_depression_cohort_interim_4")

#AD informed index dates
ad <- con2 %>% dbReadTable("dh_clinhet_TRD") %>% tibble()


#Read in the antipsychotics and mood stabilisers
ap <- con2 %>% dbReadTable("all_patid_clean_antipsychotics_prodcodes") %>% tibble()

ap_post <- ap %>% 
  inner_join(cohort %>% select(patid, ad_index_date), by = "patid") %>% 
  arrange(patid, issuedate) %>%
  mutate(valid_ap = ifelse(issuedate > ad_index_date, 1, 0))

#For now, we remove everyone who has an antipsychotic prescription of any sort prior to index
#Sensitivity where we censor people who
ap_post <- ap_post %>%
  mutate(antipsychotics_cat = antipsychotics_cat %>%
           str_trim() %>%
           str_to_lower() %>%
           str_remove("\\s.*$")) %>% rename(chem_name = antipsychotics_cat)

ap_names <- c("quetiapine", "risperidone", "olanzapine", "aripiprazole")

#Add invalid antipsychotic
invalid_antipsychotic_preex <- ap_post %>% group_by(patid) %>% 
filter(valid_ap == 0) %>% 
summarise(first_ap_date_preex = min(issuedate))

invalid_antipsychotic_main_preex <- ap_post %>% group_by(patid) %>% 
filter(valid_ap == 0 & chem_name %in% ap_names) %>% 
summarise(first_ap_date_main_preex = min(issuedate))

invalid_antipsychotic_supp_incident <- ap_post %>% group_by(patid) %>% 
filter(all(valid_ap == 1)) %>%
filter(!chem_name %in% ap_names) %>% 
summarise(first_ap_date_supp_incident = min(issuedate))

# Around 15k cases? - like 2.4%
valid_antipsychotic_main_incident <- ap_post %>% group_by(patid) %>% 
filter(all(valid_ap == 1)) %>% 
filter(chem_name %in% ap_names) %>% 
summarise(first_ap_date_main_incident = min(issuedate))

ap_table <- ap_post %>% select(patid) %>% distinct() %>%
left_join(cohort %>% select(patid, ad_index_date) %>%
left_join(invalid_antipsychotic_preex, by = "patid") %>%
left_join(invalid_antipsychotic_main_preex, by = "patid") %>% 
left_join(invalid_antipsychotic_supp_incident, by = "patid") %>%
left_join(valid_antipsychotic_main_incident, by = "patid") %>% 
mutate(valid_ap_strict = ifelse(is.na(first_ap_date_preex) & is.na(first_ap_date_supp_incident) & !is.na(first_ap_date_main_incident), 1, 0), 
      valid_ap_loose = ifelse(is.na(first_ap_date_preex) & !is.na(first_ap_date_main_incident), 1, 0),
      valid_ap_vloose = ifelse(is.na(first_ap_date_main_preex) & !is.na(first_ap_date_main_incident), 1, 0))

#Upload the table
dbExecute(con2, "
CREATE TABLE dh_depsev_augmentation (
patid BIGINT,
augmentation_ms DATE, 
augmentation_ap DATE
);
")

chunksize <- 50000
total_rows <- nrow(augmentation)
nchunks <- ceiling(total_rows / chunksize)


for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- augmentation[idx, ]
  
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
    "INSERT INTO dh_depsev_augmentation (",
    paste(
      c(
        "patid",
        "augmentation_ms",
        "augmentation_ap"
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
