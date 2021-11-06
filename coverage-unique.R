library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(factoextra)
library(extrafont)
library(ggimage)
library(ggsci) 
library(broom)
library(igraph)
library(tidyverse)
library(ggthemes)
library(nflfastR)

epa_data <- pull_s3("epa_rd/nfl_ep_epa.csv", bucket = "ml", season_start = 2014, season_end = 2021)
scheme_data <<- pull_s3("analytics/projections/nfl/%i/scheme.csv.gz", season_start = 2014, season_end = 2021)

theme_reach <- function() {
  theme_fivethirtyeight() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 22, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      axis.title.x = element_text(size=17),
      axis.title.y = element_text(size=17),
      axis.text = element_text(size = 15)
    )
}

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
  select(contains("rate")) %>%
  scale()  

set.seed(2014)
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(X, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "An Even Clearer Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_reach() + 
  theme(panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank())

set.seed(2011)
# re-run K-Means with 6 clusters
K <- 6
kmeans6 <- kmeans(X, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans6$centers) # SCALED cluster centers/means  

# name clusters before pivoting
km_centers$Cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6') 

km_centers <- km_centers %>%
  rename(c('OPEN'='open_rate', 'ROTATE'='rotate_rate', # give predictors a shorter name for plotting
           '0'='rate_0', '1'='rate_1',
           '2'='rate_2', '2M'='rate_2M',
           '3'='rate_3', '4'='rate_4',
           '6'='rate_6')) %>% 
  pivot_longer(!Cluster, names_to = 'feature', values_to = 'z_val') # pivot data to make plotting easier  

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=Cluster)) + 
  geom_point(size = 5) + # plot points
  scale_color_brewer(palette="Dark2") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ Cluster, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "K-Means Cluster Makeups of Coverage Schemes") + 
  theme_minimal() + theme_reach() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 12, colour = "black", hjust = 0.5))
ggsave('1-coverage.png', width = 15, height = 10, dpi = "retina")


coverage_clusters <- tibble(cluster=kmeans6$cluster, defense=coverage_season_rates$defense, season=coverage_season_rates$season)

pca <- prcomp(X) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$Cluster <- as.factor(kmeans6$cluster) # add player clusters 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

coverage_clusters <- cbind(coverage_clusters, pc2)

distance <- function(a, b) sqrt((a - 0)^2 + (b - 0)^2)
coverage_clusters <- coverage_clusters %>%
  group_by(defense, season) %>%
  mutate(unique_level = distance(PC1, PC2))

coverage_clusters <- coverage_clusters %>%
  left_join(teams_colors_logos, by = c("defense" = "team_abbr"))

coverage_clusters_21 <- coverage_clusters %>%
  filter(season == 2021)

coverage_clusters_21 <- coverage_clusters_21 %>% 
  ungroup() %>%
  group_by(Cluster) %>%
  mutate(rank = row_number())

coverage_clusters_21 %>%
  ggplot(aes(x = Cluster, y = rank)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) + 
  theme_reach() +
  labs(x = "Cluster",
       y = "",
       title = "The Teams in Each Coverage Cluster in 2021",
       subtitle = "Through weeks 1-9 of the NFL season") +
  theme(panel.grid.major = element_line(size = 0.1),
        axis.text.y = element_blank())
ggsave('2-coverage.png', width = 15, height = 10, dpi = "retina")

coverage_clusters_21 %>%
  ggplot(aes(x = PC1, y = PC2,)) +
  geom_image(aes(image = team_logo_espn), asp = 16/9, size = 0.05) +
  geom_text(aes(x=PC1, y=PC2, label = as.integer(cluster)), nudge_x = -0.18, nudge_y = -0.18) +
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'Clustering Coverages for the 2021 NFL Season',
       subtitle = "Text next to each team's logo is the cluster they're apart of") + 
  theme_reach() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))
ggsave('3-coverage.png', width = 15, height = 10, dpi = "retina")



