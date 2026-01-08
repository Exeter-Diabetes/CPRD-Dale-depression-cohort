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
    n_rep = sum(duplicated(chem_name)),  # count repeats within eid
    tot   = n(),                         # total rows for that eid
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
  group_by(eid) %>%
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

#Establish the connection again
con2 <- dbConnect(MariaDB(),
                  dbname = "",
                  host = "",
                  port = ,
                  user = "",
                  password = ""
)


# Define the chunk size and get row count
chunksize <- 100000
total_rows <- nrow(dep4)
nchunks <- ceiling(total_rows / chunksize)

# Start the transaction
dbExecute(con2, "START TRANSACTION;")

for (i in seq_len(nchunks)) {
  
  # Determine row indices for this chunk
  idx <- ((i - 1) * chunksize + 1):min(i * chunksize, total_rows)
  chunk <- dep4[idx, ]
  
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
    "INSERT INTO dh_depsev_TRD_v2_0 (",
    paste(
      c(
        "patid",
        "index_date",
        "issue_date",
        "datediff",
        "quantity",
        "duration",
        "chem_name",
        "drug_class",
        "AD_informed_index_date",
        "AD_index_registration_90days",
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
  
  # Execute the query for this chunk
  dbExecute(con2, query)
  
  print(paste("Inserted chunk", i, "of", nchunks))
}
