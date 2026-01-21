library(RMariaDB)
library(tidyverse)

con2 <- dbConnect(MariaDB())

con3 <- dbConnect(MariaDB())



#Get the data from CPRD:
#Depression cohort, T2D cohort, and PHQ-9
phq <-  dbReadTable(con2, "all_patid_clean_shaped_PHQ9")
t2d <- dbReadTable(con2, "alex_t2_diag_date")
dep <- dbReadTable(con2, "dh_depsev_full_depression_cohort")
ad <- dbReadTable(con2, "dh_depsev_AD_informed_index_dates")

#Load the region files and the original phq for the
loc <- dbReadTable(con3, "r_region")
phq_raw <- dbReadTable(con2, "dh_depsev_raw_phq9_medcodes")

#Can also load in the dep codes to get their relevant pracid at diagnosis.

#First, we find all PHQ-9s at diag.
phq_index <- phq %>% inner_join(dep %>% select(patid, index_date)) %>% left_join(ad %>% select(-index_date), by = "patid") %>% arrange(patid, obsdate)

#Now, we calculate what % of people have a PHQ9 at index within 30 days less 
# and within 7 days after to allow for taking time to record in GPs.
phq_index <- phq_index %>% 
  mutate(index_to_phq_difftime = as.numeric(difftime(obsdate, index_date, units = "days"))) %>%
  mutate(phq_at_index = ifelse(index_to_phq_difftime >= -30 & index_to_phq_difftime <= 7, 1, 0))

#~25% of people have a PHQ-9 at index.
phq_index %>% filter(phq_at_index == 1) %>% distinct(patid) %>% count()

#Next, create a new df and do the same, and then left join
phq_index_ad <- phq_index %>% filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1) %>%   
  mutate(index_to_phq_AD_difftime = as.numeric(difftime(obsdate, AD_informed_index_date, units = "days"))) %>%
  mutate(phq_at_index_AD = ifelse(index_to_phq_AD_difftime >= -30 & index_to_phq_AD_difftime <= 0, 1, 0))

phq_index_ad %>% filter(phq_at_index_AD == 1) %>% distinct(patid) %>% count() 
phq_index_ad %>% distinct(patid) %>% count() #56% of all with PHQ have a PHQ at index

phq_index %>% filter(phq_at_index == 1) %>% distinct(patid) %>% count()
phq_index  %>% distinct(patid) %>% count()


#Prioritise based on category
phq_index_ad_dist <- phq_index_ad %>%
  filter(phq_at_index_AD == 1) %>%
  group_by(patid) %>%
  filter(index_to_phq_AD_difftime == max(index_to_phq_AD_difftime)) %>% #Keep value closest to index.
  ungroup() %>%
  distinct() %>% #Remove any possible duplicate rows. 
  group_by(patid, obsdate) %>%
  # assign priority: score > partial > cat
  mutate(cat_priority = case_when(
    cat == "score"   ~ 1L,
    cat == "partial" ~ 2L,
    cat == "cat"     ~ 3L,
    TRUE             ~ 99L
  )) %>%
  slice_min(cat_priority, with_ties = FALSE) %>% #Slice to highest priority.
  ungroup()

#Still duplicated rows.
phq_index_ad_dist %>% count() #186540
phq_index_ad_dist %>% distinct(patid) %>% count() #186540.


#Save this table
phq_to_save <- phq_index_ad_dist %>% select(patid, phq_score)

dbExecute(con2, "
CREATE TABLE dh_depsev_PHQ_at_AD_index (
    patid              BIGINT UNSIGNED,
    phq_score          TINYINT UNSIGNED
);
")

chunksize <- 10000
total_rows <- nrow(phq_to_save)
nchunks <- ceiling(total_rows / chunksize)

for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- phq_to_save[idx, ]
  
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
    "INSERT INTO dh_depsev_PHQ_at_AD_index (",
    ' patid, phq_score
    ) VALUES ',
    paste(row_strings, collapse = ",")
  )
  
  # Execute the query for this chunk
  dbExecute(con2, query)
  
  print(paste("Inserted chunk", i, "of", nchunks))
}

#Load back in the data:
phq_to_save <- dbReadTable(con2, "dh_depsev_PHQ_at_AD_index")
