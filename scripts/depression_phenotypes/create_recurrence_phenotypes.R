library(aurum)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)
library(survival)
library(purrr)
library(patchwork)

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


cprdenvname <- "CPRD_depression_data"
yaml <- ".aurum.yaml"
cprd = CPRDData$new(cprdEnv = cprdenvname, cprdConf = yaml)

analysis <- cprd$analysis("all_patid")
ads <- ads %>% analysis$cached("clean_antidepressant_prodcodes") #3598070

analysis <- cprd$analysis("dh")
codes <- codes %>% analysis$cached("raw_broad_depression_medcodes_valid")

analysis <- cprd$analysis("dh_clinhet")
cohort <- cohort %>% analysis$cached("depression_cohort_interim_4")


ad_table <- ads %>% inner_join(cohort %>% select(patid), by = "patid") %>%
  analysis$cached("clean_antidepressants", indexes = c("patid"))


valid_codes <- data.frame(
  depression_broad_ext_cat = c("recurrent_diag", "diagnosis", "chronic_diag", "persistent_diag", "trd_diag")
)

# Shorten to diagnostic or recurrent codes only - NOT reviews or resolution etc.
codes_short <- codes %>% 
  inner_join(cohort %>% select(patid, index_date), by = "patid") %>% 
  inner_join(valid_codes, by = "depression_broad_ext_cat", copy = TRUE) %>% 
  filter(obsdate >= index_date) %>% 
  select(patid, date = obsdate) %>%
  mutate(source = "diagnosis")

ad_table_short <- ad_table %>% 
  select(patid, date) %>% 
  mutate(source = "drug") 


# Arrange only here
ad_full <- ad_table_short %>%
  union_all(codes_short) %>%
  arrange(patid, date) %>%  
  analysis$cached("recurrent_depression_interim_1")

make_episode_starts(ad_full, source_filter = c("diagnosis", "drug"), cache_name = "recurrence_all6m")
make_episode_starts(ad_full, source_filter = c("diagnosis", "drug"), washout_days = 91, cache_name = "recurrence_all3m")
make_episode_starts(ad_full, source_filter = c("diagnosis"), cache_name = "recurrence_medcodes6m")
make_episode_starts(ad_full, source_filter = c("diagnosis"), washout_days = 91, cache_name = "recurrence_medcodes3m")


# Cache all these tables
all_3m <- all_3m %>% analysis$cached("recurrence_all3m")
all_6m <- all_6m %>% analysis$cached("recurrence_all6m")
code_3m <- code_3m %>% analysis$cached("recurrence_medcodes3m")
code_6m <- code_6m %>% analysis$cached("recurrence_medcodes6m")

#Plot
# =============================================================================
# Phenotype Comparison Plots
# Comparing: all_3m, all_6m, code_3m, code_6m
# =============================================================================



# -----------------------------------------------------------------------------
# 0. Colour scheme
# -----------------------------------------------------------------------------

plot_colours <- c(
  "Male"          = "#2C3E50",
  "Female"        = "#2980B9",
  "White"         = "#2C3E50",
  "South Asian"   = "#2980B9",
  "Black"         = "#27AE60",
  "Other"         = "#E67E22",
  "Mixed"         = "#7F8C8D",
  "Non-smoker"    = "#2C3E50",
  "Ex-smoker"     = "#2980B9",
  "Active smoker" = "#27AE60",
  "None"          = "#2C3E50",
  "Within limits" = "#2980B9",
  "Excess"        = "#E67E22",
  "Harmful"       = "#E74C3C"
)

# Map four distinct colours from the palette to the four phenotypes
pheno_order <- c(
  "All codes\n3m washout",
  "All codes\n6m washout",
  "Med codes\n3m washout",
  "Med codes\n6m washout"
)

pheno_cols <- c(
  "All codes\n3m washout"  = unname(plot_colours["Male"]),    # #2C3E50
  "All codes\n6m washout"  = unname(plot_colours["Female"]),  # #2980B9
  "Med codes\n3m washout"  = unname(plot_colours["Black"]),   # #27AE60
  "Med codes\n6m washout"  = unname(plot_colours["Other"])    # #E67E22
)

# -----------------------------------------------------------------------------
# 1. Collect all four tables from the database into memory
# -----------------------------------------------------------------------------

collect_phenotype <- function(tbl, label) {
  tbl %>%
    dplyr::distinct(patid, episode_id, episode_start_date, episode_end_date,
                    n_episodes, recurrent) %>%
    collect() %>%
    mutate(
      phenotype    = label,
      washout      = ifelse(grepl("3m", label), "3 months", "6 months"),
      source       = ifelse(grepl("All", label), "All codes", "Med codes only"),
      episode_days = as.integer(episode_end_date - episode_start_date)
    )
}

df_all3m  <- collect_phenotype(all_3m,  "All codes\n3m washout")
df_all6m  <- collect_phenotype(all_6m,  "All codes\n6m washout")
df_code3m <- collect_phenotype(code_3m, "Med codes\n3m washout")
df_code6m <- collect_phenotype(code_6m, "Med codes\n6m washout")

# One row per patient per phenotype (for patient-level summaries)
pat_level <- bind_rows(df_all3m, df_all6m, df_code3m, df_code6m) %>%
  distinct(patid, phenotype, washout, source, n_episodes, recurrent) %>%
  mutate(phenotype = factor(phenotype, levels = pheno_order))

all_episodes <- bind_rows(df_all3m, df_all6m, df_code3m, df_code6m) %>%
  mutate(phenotype = factor(phenotype, levels = pheno_order))


# =============================================================================
# PLOT 1 — % with recurrence
# =============================================================================

p1_data <- pat_level %>%
  group_by(phenotype, washout) %>%
  summarise(pct_recurrent = mean(recurrent, na.rm = TRUE) * 100, .groups = "drop")

p1 <- ggplot(p1_data, aes(x = phenotype, y = pct_recurrent, fill = phenotype)) +
  geom_col(width = 0.6, colour = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_recurrent)),
    vjust = -0.5, size = 3.5, fontface = "bold"
  ) +
  scale_fill_manual(values = pheno_cols) +
  scale_y_continuous(
    limits = c(0, 100),
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "% patients with recurrent depression",
    x = NULL, y = "% recurrent"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "none",
    plot.title         = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    axis.text.x        = element_text(size = 9),
    plot.background    = element_rect(fill = "white", colour = NA)
  )




# =============================================================================
# PLOT 2 — Distribution of n_episodes per patient
# =============================================================================

p2_data <- pat_level %>%
  mutate(
    episode_bin = case_when(
      n_episodes == 1 ~ "1",
      n_episodes == 2 ~ "2",
      n_episodes == 3 ~ "3",
      n_episodes == 4 ~ "4",
      n_episodes == 5 ~ "5",
      TRUE            ~ "6+"
    ),
    episode_bin = factor(episode_bin, levels = c("1", "2", "3", "4", "5", "6+"))
  ) %>%
  group_by(phenotype, episode_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(phenotype) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

p2 <- ggplot(p2_data, aes(x = episode_bin, y = pct, fill = phenotype)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, colour = "white") +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    position = position_dodge(width = 0.75),
    vjust    = -0.4,
    size     = 2.0,
    colour   = "grey20"
  ) +
  scale_fill_manual(
    values = pheno_cols,
    labels = gsub("\n", " ", pheno_order)
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "Distribution of episodes per patient",
    subtitle = "% of patients in each phenotype with that number of episodes",
    x        = "Number of episodes",
    y        = "% of patients",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "grey50"),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    plot.background    = element_rect(fill = "white", colour = NA)
  )




# =============================================================================
# PLOT 3 — Episode duration: overlapping semi-shaded density plots (log scale)
# =============================================================================

p3_data <- all_episodes %>%
  filter(episode_days > 0)  # exclude single-day episodes

p3 <- ggplot(p3_data, aes(x = episode_days, fill = phenotype, colour = phenotype)) +
  geom_density(alpha = 0.25, linewidth = 0.8) +
  scale_fill_manual(
    values = pheno_cols,
    labels = gsub("\n", " ", pheno_order)
  ) +
  scale_colour_manual(
    values = pheno_cols,
    labels = gsub("\n", " ", pheno_order)
  ) +
  scale_x_log10(
    labels = comma,
    breaks = c(1, 7, 30, 90, 180, 365, 730, 1500),
    limits = c(1, quantile(p3_data$episode_days, 0.99, na.rm = TRUE))
  ) +
  annotation_logticks(sides = "b", colour = "grey70") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Distribution of episode duration",
    subtitle = "Days from first to last event within an episode (log scale; single-day episodes excluded)",
    x        = "Episode duration (days)",
    y        = "Density",
    fill     = NULL,
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 12),
    plot.subtitle      = element_text(size = 9, colour = "grey50"),
    panel.grid.major.x = element_blank(),
    legend.position    = "bottom",
    legend.text        = element_text(size = 9),
    plot.background    = element_rect(fill = "white", colour = NA)
  )




# =============================================================================
# PLOT 4 — Observed cumulative recurrence over time
# =============================================================================

prepare_km <- function(df, label) {
  ep1 <- df %>%
    filter(episode_id == 1) %>%
    select(patid, ep1_start = episode_start_date, ep1_end = episode_end_date)
  
  ep2 <- df %>%
    filter(episode_id == 2) %>%
    select(patid, ep2_start = episode_start_date)
  
  ep1 %>%
    left_join(ep2, by = "patid") %>%
    mutate(
      phenotype          = label,
      time_to_recurrence = as.integer(
        if_else(!is.na(ep2_start), ep2_start - ep1_start, ep1_end - ep1_start)
      ),
      status = if_else(!is.na(ep2_start), 1L, 0L)
    ) %>%
    filter(time_to_recurrence > 0) %>%
    select(patid, phenotype, time_to_recurrence, status)
}

km_data <- bind_rows(
  prepare_km(df_all3m,  "All codes\n3m washout"),
  prepare_km(df_all6m,  "All codes\n6m washout"),
  prepare_km(df_code3m, "Med codes\n3m washout"),
  prepare_km(df_code6m, "Med codes\n6m washout")
) %>%
  mutate(phenotype = factor(phenotype, levels = pheno_order))

max_days <- 6000
day_seq  <- seq(0, max_days, by = 30)

recurrence_times <- km_data %>%
  filter(status == 1) %>%
  select(phenotype, time_to_recurrence)

total_per_phenotype <- km_data %>%
  group_by(phenotype) %>%
  summarise(total = n(), .groups = "drop")

cum_data <- map_dfr(pheno_order, function(p) {
  times <- recurrence_times %>% filter(phenotype == p) %>% pull(time_to_recurrence)
  total <- total_per_phenotype %>% filter(phenotype == p) %>% pull(total)
  tibble(
    phenotype  = p,
    day        = day_seq,
    n_recurred = map_int(day_seq, ~ sum(times <= .x)),
    total      = total
  )
}) %>%
  mutate(
    prop      = n_recurred / total,
    phenotype = factor(phenotype, levels = pheno_order)
  )

p4 <- ggplot(cum_data, aes(x = day, y = prop, colour = phenotype)) +
  geom_line(linewidth = 1.0) +
  scale_colour_manual(values = pheno_cols, labels = gsub("\n", " ", pheno_order)) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    labels = comma,
    breaks = seq(0, max_days, by = 1000)
  ) +
  labs(
    title    = "Observed cumulative recurrence over time",
    subtitle = "Proportion of all patients (including non-recurrers) with a second episode by each time point",
    x        = "Days from first episode start",
    y        = "Proportion recurred",
    colour   = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, colour = "grey50"),
    legend.position = "bottom",
    legend.text     = element_text(size = 9),
    plot.background = element_rect(fill = "white", colour = NA)
  )

# =============================================================================
# COMBINED PANEL
# =============================================================================

# Collect shared legend from p2 (most informative legend)
# =============================================================================
# COMBINED PANEL
# =============================================================================

p1_panel <- p1 + theme(legend.position = "none")
p2_panel <- p2 + theme(legend.position = "none")
p3_panel <- p3 + theme(legend.position = "none")
p4_panel <- p4 + theme(legend.position = "none")

# Build a standalone legend plot
legend_data <- tibble(
  phenotype = factor(gsub("\n", " ", pheno_order), levels = gsub("\n", " ", pheno_order)),
  y = 1
)

legend_plot <- ggplot(legend_data, aes(x = phenotype, y = y, fill = phenotype)) +
  geom_col(width = 0.1, colour = NA, alpha = 0) +  # invisible bars, legend keys remain
  scale_fill_manual(values = setNames(unname(pheno_cols), gsub("\n", " ", pheno_order))) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(alpha = 1))) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.text     = element_text(size = 12),
    legend.key.size = unit(0.7, "cm")
  )

combined <- (p1_panel + p2_panel) / (p3_panel + p4_panel) / legend_plot +
  plot_layout(heights = c(1, 1, 0.06)) +
  plot_annotation(
    title      = "Depression episode phenotype comparison",
    subtitle   = "Comparing all-code vs. med-code phenotypes across 3- and 6-month washout windows",
    tag_levels = "A",
    theme = theme(
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(size = 10, colour = "grey50"),
      plot.background = element_rect(fill = "white", colour = NA)
    )
  )

# Suppress tag label on the legend row
combined[[3]] <- combined[[3]] + plot_annotation(tag_levels = list(c("")))

ggsave("phenotype_comparison_panel.png", combined, width = 16, height = 12, dpi = 150, bg = "white")

message("Panel saved.")

message("Panel saved.")


