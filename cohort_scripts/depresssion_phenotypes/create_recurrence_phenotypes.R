library(aurum)
library(tidyverse)


#Function to create recurrent depression - 6 month window as reccurent
make_episode_starts <- function(data,
                                washout_days = 183L,
                                source_filter = NULL,
                                cache_name = NULL) {
  
  stopifnot(length(washout_days) == 1)
  
  # --- Optional source filter ---
  if (!is.null(source_filter)) {
    source_tbl <- tibble::tibble(
      source = unique(as.character(source_filter))
    )
    
    data <- data %>%
      dplyr::inner_join(source_tbl, by = "source", copy = TRUE)
  }
  
  episodes <- data %>%
    dplyr::distinct(patid, date) %>%
    dplyr::group_by(patid) %>%
    dbplyr::window_order(date) %>%
    dplyr::mutate(
      prev_date   = dplyr::lag(date),
      gap_days    = date - prev_date,
      new_episode = dplyr::if_else(
        is.na(prev_date) | gap_days >= washout_days,
        1L, 0L
      ),
      episode_id  = cumsum(new_episode)
    ) %>%
    dplyr::ungroup()
  
  episode_table <- episodes %>%
    dplyr::group_by(patid, episode_id) %>%
    dplyr::summarise(
      start_date = min(date),
      end_date   = max(date),
      n_events   = dplyr::n(),
      .groups    = "drop"
    )
  
  episode_starts <- episode_table %>%
    dplyr::select(
      patid,
      episode_id,
      episode_start_date = start_date,
      episode_end_date   = end_date
    ) %>%
    dplyr::group_by(patid) %>%
    dplyr::mutate(
      n_episodes = dplyr::n(),
      recurrent  = n_episodes >= 2L
    ) %>%
    dplyr::ungroup()
  
  if (!is.null(cache_name)) {
    episode_starts <- episode_starts %>% analysis$cached(cache_name)
  }
  
  episode_starts
}


cprdenvname <- ""
yaml <- ""
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)


#Load in index dates, depression codes, and prescriptions
analysis <- cprd$analysis("dh_depsev")
index <- index %>% analysis$cached("AD_informed_index_dates")
ad <- ad %>% analysis$cached("ADs")

analysis <- cprd$analysis("dh")
codes <- codes %>% analysis$cached("raw_broad_depression_medcodes_valid")


#Filter the tables down so that they match
index <- index %>% filter(!is.na(AD_informed_index_date) & AD_index_registration_90days == 1)

ad_short <- ad %>% inner_join(index, by = "patid") %>% 
  filter(date >= AD_informed_index_date) %>% select(patid, date) %>%
  mutate(source = "drug")

valid_codes <- data.frame(depression_broad_ext_cat = c("recurrent_diag", "diagnosis", "chronic_diag", "persistent_diag", "trd_diag"))

#Shorten to diagnostic or recurrent codes only - NOT reviews or resolution etc.
codes_short <- codes %>% inner_join(index, by = "patid") %>% 
  inner_join(valid_codes, by = "depression_broad_ext_cat", copy = T) %>% 
  filter(obsdate >= AD_informed_index_date) %>% select(patid, date = obsdate) %>%
  mutate(source = "diagnosis")

codes_short 

analysis <- cprd$analysis("dh_depsev")

ad_full <- ad_short %>%
  union_all(codes_short) %>%          # db-friendly bind_rows
  arrange(patid, date) %>%  
  analysis$cached("recurrent_depression_interim_1")

make_episode_starts(ad_full, source_filter = c("diagnosis", "drug"), cache_name = "recurrence_all6m")
make_episode_starts(ad_full, source_filter = c("diagnosis", "drug"), washout_days = 91, cache_name = "recurrence_all3m")
make_episode_starts(ad_full, source_filter = c("diagnosis"), cache_name = "recurrence_medcodes6m")
make_episode_starts(ad_full, source_filter = c("diagnosis"), washout_days = 91, cache_name = "recurrence_medcodes3m")

