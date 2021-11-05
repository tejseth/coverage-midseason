library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(factoextra)
library(extrafont)
# font_import(prompt=FALSE)
loadfonts() 
library(ggsci) 
library(broom)
library(igraph)
library(tidyverse)

epa_data <- pull_s3("epa_rd/nfl_ep_epa.csv", bucket = "ml", season_start = 2014, season_end = 2021)
scheme_data <<- pull_s3("analytics/projections/nfl/%i/scheme.csv.gz", season_start = 2014, season_end = 2021)

coverage_data <- scheme_data %>%
  filter(rps == "P") 

coverage_data <- coverage_data %>%
  select(defense, defense_franchise_id, distance, down, game_id, offense,
         offense_franchise_id, quarter, season, week, defense_personnel, 
         blitz, num_pass_rush_players, coverage_scheme, 
         mofo_coverage_shown, mofo_coverage_played) %>%
  mutate(coverage_scheme = case_when(
    coverage_scheme %in% c("3S", "3C", "3DC", "3CL", "3CR") ~ "3",
    coverage_scheme %in% c("6R", "6L") ~ "6",
    TRUE ~ coverage_scheme
  )) %>%
  filter(coverage_scheme != "RZ") %>%
  mutate(rotate = ifelse(mofo_coverage_shown == mofo_coverage_played, 0, 1),
         open_play = ifelse(mofo_coverage_played == "O", 1, 0))

coverage_data %>% group_by(coverage_scheme) %>% tally(sort = T)
common_coverages <- c("3", "1", "2", "4", "6", "2M", "0")

coverage_data <- coverage_data %>%
  mutate(offense = case_when(
    offense == "SD" ~ "LAC",
    offense == "BLT" ~ "BAL",
    offense == "OAK" ~ "LV",
    offense == "HST" ~ "HOU",
    offense == "SL" ~ "LA",
    offense == "CLV" ~ "CLE", 
    offense == "ARZ" ~ "ARI",
    TRUE ~ offense
  )) %>%
  ungroup()

coverage_data <- coverage_data %>%
  mutate(defense = case_when(
    defense == "SD" ~ "LAC",
    defense == "BLT" ~ "BAL",
    defense == "OAK" ~ "LV",
    defense == "HST" ~ "HOU",
    defense == "SL" ~ "LA",
    defense == "CLV" ~ "CLE", 
    defense == "ARZ" ~ "ARI",
    TRUE ~ defense
  )) %>%
  ungroup()

coverage_season_rates <- coverage_data %>%
  group_by(defense, season) %>%
  summarize(plays = n(),
            open_rate = mean(open_play),
            rotate_rate = mean(rotate))

coverage_season_join <- coverage_data %>%
  filter(coverage_scheme %in% common_coverages) %>%
  group_by(defense, season) %>%
  count(coverage_scheme) %>%
  pivot_wider(
    id_cols = c(defense, season),
    names_from = coverage_scheme,
    values_from = n
  )
coverage_season_join[is.na(coverage_season_join)] <- 0

coverage_season_rates <- coverage_season_rates %>%
  left_join(coverage_season_join, by = c("defense", "season"))

coverage_season_rates <- coverage_season_rates %>%
  mutate(rate_0 = `0` / plays,
         rate_1 = `1` / plays,
         rate_2 = `2` / plays,
         rate_2M = `2M` / plays,
         rate_3 = `3` / plays,
         rate_4 = `4` / plays,
         rate_6 = `6` / plays) %>%
  select(-c(`0`, `1`, `2`, `2M`, `3`, `4`, `6`)) %>%
  ungroup()

X <- coverage_season_rates %>% 
  select(starts_with("rate")) %>%
  scale()  

set.seed(2014)
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model




