con2 <- dbConnect(MariaDB(),)

#AD informed index dates
ad <- con2 %>% dbReadTable("dh_depsev_TRD_v2_0") %>% tibble()


#Read in the antipsychotics and mood stabilisers
ap <- con2 %>% dbReadTable("all_patid_clean_antipsychotics_prodcodes") %>% tibble()
ms <- con2 %>% dbReadTable("all_patid_clean_mood_stabilisers_prodcodes") %>% tibble()



#Antidepressant index dates
index <- dbReadTable(con2, "dh_depsev_AD_informed_index_dates") %>% tibble()

#only individuals with valid AD prescription
index <- index %>% 
  filter(!is.na(AD_informed_index_date) & 
           AD_index_registration_90days == 1 &
           any_antidepressant == 1) %>%
  select(patid, AD_informed_index_date)

#First, we remove all ap and ms prescriptions which are not in our current analysis set,
# Or before the antidepressant index date!
ap_post <- ap %>% 
  inner_join(index, by = "patid") %>% 
  arrange(patid, issuedate) %>%
  mutate(valid_ap = ifelse(issuedate > AD_informed_index_date, 1, 0))

ms_post <- ms %>% 
  inner_join(index, by = "patid") %>% 
  arrange(patid, issuedate) %>%
  mutate(valid_ms = ifelse(issuedate >= AD_informed_index_date, 1, 0)) 

#Create a list of individuals to exclude from AP phenotype
ap_exclusions <- ap_post %>% filter(valid_ap == 0) %>% distinct(patid) #69,000 excluded - ~ half of sample
ms_exclusion <- ms_post %>% filter(valid_ms == 0) %>% distinct(patid) # ~ half of sample


ap_post <- ap_post %>%
  mutate(antipsychotics_cat = antipsychotics_cat %>%
           str_trim() %>%
           str_to_lower() %>%
           str_remove("\\s.*$")) %>% rename(chem_name = antipsychotics_cat)

# Generate overall augmentation using chiara's code.
ap_post %>%
  group_by(patid, chem_name) %>%
  arrange(issuedate) %>%
  mutate(prev_drug_date = dplyr::lag(issuedate, n = 1, default = NA)) %>%
  mutate(diff_weeks_drug = as.numeric(difftime(issuedate, prev_drug_date, units = "weeks"))) %>%
  mutate(prescription_episode = ifelse (diff_weeks_drug > 24, seq_along(diff_weeks_drug), 1)) %>%
  mutate(prescription_episode = ifelse (is.na(prescription_episode), 999, prescription_episode)) %>%
  mutate(prescription_episode = ifelse ( prescription_episode == 1,
                                         NA, prescription_episode)) %>%
  fill(prescription_episode) %>%
  mutate(prescription_episode = ifelse (prescription_episode == 999, 1, prescription_episode)) %>%
  ungroup() -> ap_overall

#Add start and end to ap prescription episodes
ap_overall %>%
  group_by(patid,chem_name,prescription_episode) %>%
  arrange(issuedate) %>%
  mutate(AP_start = min(issuedate, na.rm=T)) %>%
  mutate(AP_end = max(issuedate, na.rm=T)) %>%
  ungroup() -> ap_overall

# Do the same for TRD table
ad %>%
  rename(issuedate = issue_date) %>% 
  group_by(patid,chem_name,prescription_episode) %>%
  arrange(issuedate) %>%
  mutate(AD_start = min(issuedate, na.rm=T)) %>%
  mutate(AD_end = max(issuedate, na.rm=T)) %>%
  ungroup() -> ad

#Keep only individuals who are definitely taking both antidepressants
ad_ap <- ad %>% 
  filter(patid %in% ap_overall$patid)

#Make sure names are harmonised for major cols prior to running next code
ap_short <- ap_overall %>% select(patid, issuedate, chem_name, prescription_episode, AP_start, AP_end)
ad_short <- ad_ap %>% select(patid, issuedate, chem_name, prescription_episode, AD_start, AD_end)

#Join together
ap_epi <- ap_short %>%
  distinct(patid, chem_name, prescription_episode, AP_start, AP_end)

ad_epi <- ad_short %>%
  distinct(patid, chem_name, prescription_episode, AD_start, AD_end)

#The number is way higher - like nearly 12% if we don't require to overlap
#With an antidepressant
first_aug <- ap_epi %>%
  inner_join(ad_epi, by = "patid", relationship = "many-to-many") %>%
  mutate(
    overlap_start = pmax(AP_start, AD_start),
    overlap_end   = pmin(AP_end, AD_end),
    overlap_days  = as.integer(overlap_end - overlap_start + 1L),
    aug_date      = overlap_start
  ) %>%
  filter(
    overlap_end >= overlap_start,
    overlap_days >= 30,
    AD_start < AP_start
  ) %>%
  group_by(patid) %>%
  slice_min(
    order_by = list(aug_date, grepl("chlorperazine", chem_name.x, ignore.case = TRUE)),
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

#Next, do mood stabilisers - could go with a looser definition? it's gonna
#Be a fraction of all people... anyway.
ms_post <- ms_post %>%
  mutate(mood_stabilisers_cat = mood_stabilisers_cat %>%
           str_trim() %>%
           str_to_lower() %>%
           str_remove("\\s.*$")) %>% rename(chem_name = mood_stabilisers_cat)


ms_post %>%
  group_by(patid, chem_name) %>%
  arrange(issuedate) %>%
  mutate(prev_drug_date = dplyr::lag(issuedate, n = 1, default = NA)) %>%
  mutate(diff_weeks_drug = as.numeric(difftime(issuedate, prev_drug_date, units = "weeks"))) %>%
  mutate(prescription_episode = ifelse (diff_weeks_drug > 24, seq_along(diff_weeks_drug), 1)) %>%
  mutate(prescription_episode = ifelse (is.na(prescription_episode), 999, prescription_episode)) %>%
  mutate(prescription_episode = ifelse ( prescription_episode == 1,
                                         NA, prescription_episode)) %>%
  fill(prescription_episode) %>%
  mutate(prescription_episode = ifelse (prescription_episode == 999, 1, prescription_episode)) %>%
  ungroup() -> ms_overall

ms_overall %>%
  group_by(patid,chem_name,prescription_episode) %>%
  arrange(issuedate) %>%
  mutate(AP_start = min(issuedate, na.rm=T)) %>%
  mutate(AP_end = max(issuedate, na.rm=T)) %>%
  ungroup() -> ms_overall

ms_short <- ms_overall %>% select(patid, issuedate, chem_name, prescription_episode, AP_start, AP_end)

ms_epi <- ms_short %>%
  distinct(patid, chem_name, prescription_episode, AP_start, AP_end)


first_ms <- ms_epi %>% #only like... 0.2% of people?
  inner_join(ad_epi, by = "patid", relationship = "many-to-many") %>%
  mutate(
    overlap_start = pmax(AP_start, AD_start),
    overlap_end   = pmin(AP_end, AD_end),
    overlap_days  = as.integer(overlap_end - overlap_start + 1L),
    aug_date      = overlap_start
  ) %>%
  filter(
    overlap_end >= overlap_start,
    overlap_days >= 30,
    AD_start < AP_start
  ) %>%
  group_by(patid) %>%
  slice_min(
    order_by = aug_date,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

#Get all the data together:
first_ms_short <- first_ms %>% select(patid, augmentation_ms = aug_date)
first_ap_short <- first_aug %>% select(patid, augmentation_ap = aug_date)

#Finalise
augmentation <- first_ms_short %>% bind_rows(first_ap_short)


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
