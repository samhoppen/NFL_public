library(dplyr)
library(tidyverse)
library(nflreadr)

euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
}

manhattan_distance <- function(x, y) {
  # Input validation
  if (length(x) != length(y)) {
    stop("Vectors must have the same length")
  }
  
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both inputs must be numeric vectors")
  }
  
  # Calculate Manhattan distance as the sum of absolute differences
  distance <- sum(abs(x - y))
  
  return(distance)
}

# Define function to calculate the square root of sum of squares
square_rooted <- function(x) {
  sqrt(sum(x^2))
}

# Define cosine similarity function
cosine_similarity <- function(x, y) {
  numerator <- sum(x * y)
  denominator <- square_rooted(x) * square_rooted(y)
  numerator / denominator
}

playcallers <- read_csv("https://raw.githubusercontent.com/samhoppen/NFL_public/refs/heads/main/data/all_playcallers.csv")

primary_playcallers <- playcallers %>% 
  filter(season >= 2019) %>% 
  group_by(team, season, off_play_caller) %>% 
  summarize(games_coached = n(),
            last_week = max(week)) %>% 
  ungroup() %>% 
  group_by(team, season) %>% 
  filter(games_coached == max(games_coached)) %>% 
  mutate(coaches = n(), team = nflreadr::clean_team_abbrs(team)) %>% 
  filter(coaches == 1 | last_week >= 17) %>% 
  arrange(team, season) %>% 
  group_by(team) %>% 
  mutate(returning_playcaller = if_else(off_play_caller == lag(off_play_caller), 1, 0))

big_board <- read_csv("~/Documents/Data/big_board_2025.csv")

nfl_receiving <- read_csv("~/Documents/Data/nfl_team_route_frequency.csv") %>% 
  mutate(other_pct = `PFFRtHBScrn%`+`PFFRtRcvrScrn%`+`PFFRtPivot%`+`PFFRtWheel%`+`PFFRtComebk%`,
         team = nflreadr::clean_team_abbrs(teamAbbrevName)) %>% 
  filter(season == 2024) %>% 
  select(player = team, other_pct, cross_pct = `PFFRtCross%`, flare_pct = `PFFRtFlare%`,
         slant_pct = `PFFRtSlant%`, in_pct = `PFFRtIn%`, out_pct = `PFFRtOut%`, 
         hitch_pct = `PFFRtHitch%`, corner_pct = `PFFRtCorner%`, post_pct = `PFFRtPost%`, go_pct = `PFFRtGo%`) %>% 
  select(-c(other_pct, flare_pct))

cfb_receiving <- read_csv("~/Documents/Data/cfb_route_frequency.csv") %>% 
  mutate(other_pct = `PFFRtHBScrn%`+`PFFRtRcvrScrn%`+`PFFRtPivot%`+`PFFRtWheel%`+`PFFRtComebk%`,
         player = nflreadr::clean_player_names(player)) %>% 
  filter(Routes >= 50) %>% 
  group_by(playerId) %>% 
  filter(season == max(season)) %>% 
  ungroup() %>% 
  select(player, other_pct, cross_pct = `PFFRtCross%`, flare_pct = `PFFRtFlare%`,
         slant_pct = `PFFRtSlant%`, in_pct = `PFFRtIn%`, out_pct = `PFFRtOut%`, 
         hitch_pct = `PFFRtHitch%`, corner_pct = `PFFRtCorner%`, post_pct = `PFFRtPost%`, go_pct = `PFFRtGo%`) %>% 
  select(-c(other_pct, flare_pct))

big_board <- read_csv("~/Documents/Data/big_board_2025.csv") %>% 
  mutate(player = paste0(first_name, " ", last_name),
         player = nflreadr::clean_player_names(player))

prospects <- big_board %>% 
  filter(position == "WR", rank <= 150)

fin_data <- prospects %>% 
  select(player, rank) %>% 
  left_join(cfb_receiving)

write_csv(fin_data, "~/Documents/Data/wr_comp_data.csv")

all_prospect_comps <- data.frame()

all_prospects <- fin_data$player

for(prospect in all_prospects){
  player_szn <- 2025
  
  nfl_df <- nfl_receiving %>% 
    filter(season == player_szn | season == player_szn-1) %>% 
    mutate(player = team) %>% 
    select(player, cross_pct, slant_pct, in_pct, out_pct,
           hitch_pct, corner_pct, post_pct, go_pct)
  
  test_df <- fin_data %>% 
    filter(player == prospect) %>% 
    bind_rows(nfl_df)
  
  player_names <- test_df$player
  
  scaled_df <- scale(test_df %>% 
                       select(c(cross_pct, slant_pct, in_pct, out_pct,
                                hitch_pct, corner_pct, post_pct, go_pct)))
  
  all_distances <- matrix(nrow = nrow(scaled_df), ncol = nrow(scaled_df))
  
  for (i in 1:nrow(scaled_df)) {
    for (j in 1:nrow(scaled_df)) {
      all_distances[i, j] <- euclidean_distance(scaled_df[i, ], scaled_df[j, ])
    }
  }
  
  dist_df <- data.frame(all_distances)
  
  colnames(dist_df) <- player_names
  
  fin_dist_df <- dist_df %>% 
    cbind(test_df) %>% 
    pivot_longer(cols = -c(player, rank, cross_pct, slant_pct, in_pct, out_pct,
                           hitch_pct, corner_pct, post_pct, go_pct),
                 names_to = "team_comp", values_to = "distance") %>% 
    filter(player != team_comp) %>% 
    filter(!is.na(distance)) %>% 
    arrange(player, distance) %>% 
    group_by(player) %>% 
    mutate(dist_rank = row_number()) %>% 
    filter(player == prospect)
  
  scaled_df <- scale(test_df %>% 
                       select(c(cross_pct, slant_pct, in_pct, out_pct,
                                hitch_pct, corner_pct, post_pct, go_pct)))
  
  all_distances_man <- matrix(nrow = nrow(scaled_df), ncol = nrow(scaled_df))
  
  for (i in 1:nrow(scaled_df)) {
    for (j in 1:nrow(scaled_df)) {
      all_distances_man[i, j] <- manhattan_distance(scaled_df[i, ], scaled_df[j, ])
    }
  }
  
  man_df <- data.frame(all_distances_man)
  
  colnames(man_df) <- player_names
  
  fin_dist_man_df <- man_df %>% 
    cbind(test_df) %>% 
    pivot_longer(cols = -c(player, rank, cross_pct, slant_pct, in_pct, out_pct,
                           hitch_pct, corner_pct, post_pct, go_pct),
                 names_to = "team_comp", values_to = "distance_man") %>% 
    filter(player != team_comp) %>% 
    filter(!is.na(distance_man)) %>% 
    arrange(player, distance_man) %>% 
    group_by(player) %>% 
    mutate(dist_man_rank = row_number()) %>% 
    filter(player == prospect)
  
  scaled_df <- scale(test_df %>% 
                       select(c(cross_pct, slant_pct, in_pct, out_pct,
                                hitch_pct, corner_pct, post_pct, go_pct)))
  
  all_similarities <- matrix(nrow = nrow(scaled_df), ncol = nrow(scaled_df))
  
  for (i in 1:nrow(scaled_df)) {
    for (j in 1:nrow(scaled_df)) {
      all_similarities[i, j] <- cosine_similarity(scaled_df[i, ], scaled_df[j, ])
    }
  }
  
  
  sim_df <- data.frame(all_similarities)
  
  colnames(sim_df) <- player_names
  
  fin_sim_df <- sim_df %>% 
    cbind(test_df) %>% 
    pivot_longer(cols = -c(player, rank, cross_pct, slant_pct, in_pct, out_pct,
                           hitch_pct, corner_pct, post_pct, go_pct),
                 names_to = "team_comp", values_to = "sim_score") %>% 
    filter(player != team_comp) %>% 
    filter(!is.na(sim_score)) %>% 
    arrange(player, desc(sim_score)) %>% 
    group_by(player) %>% 
    mutate(sim_rank = row_number()) %>% 
    filter(player == prospect)
  
  
  fin_df <- fin_dist_df %>% left_join(fin_dist_man_df) %>% left_join(fin_sim_df)
  
  all_prospect_comps <- rbind(all_prospect_comps, fin_df)
}

fin_prospect_comps <- all_prospect_comps %>% 
  # filter(sim_rank <=3) %>% 
  filter(dist_man_rank<=3) %>% 
  mutate(season = 2025) %>% 
  left_join(primary_playcallers %>% select(team_comp = team, season, returning_playcaller)) %>% 
  arrange(rank, sim_rank)

length(unique(fin_prospect_comps$team_comp))

test_df %>% select(-rank) %>% clipr::write_clip()

# 
# 
# test_df <- fin_data %>% 
#   mutate(type = 'prospect') %>% 
#   bind_rows(nfl_receiving %>% mutate(type = 'team'))
# 
# write_csv(test_df, "~/Documents/Data/wr_comp_data2.csv")
# 
# 
# player_names <- test_df$player
# 
# scaled_df <- scale(test_df %>% 
#                      select(c(cross_pct, slant_pct, in_pct, out_pct,
#                               hitch_pct, corner_pct, post_pct, go_pct)))
# 
# all_distances <- matrix(nrow = nrow(scaled_df), ncol = nrow(scaled_df))
# 
# for (i in 1:nrow(scaled_df)) {
#   for (j in 1:nrow(scaled_df)) {
#     all_distances[i, j] <- euclidean_distance(scaled_df[i, ], scaled_df[j, ])
#   }
# }
# 
# dist_df <- data.frame(all_distances)
# 
# colnames(dist_df) <- player_names
# 
# fin_dist_df <- dist_df %>% 
#   cbind(test_df) %>% 
#   pivot_longer(cols = -c(player, rank, type, cross_pct, slant_pct, in_pct, out_pct,
#                          hitch_pct, corner_pct, post_pct, go_pct),
#                names_to = "team_comp", values_to = "distance") %>% 
#   filter(player != team_comp) %>% 
#   filter(!is.na(distance)) %>% 
#   arrange(player, distance) %>% 
#   group_by(player) %>% 
#   mutate(dist_rank = row_number()) %>% 
#   filter(type == 'prospect') %>% 
#   left_join(test_df %>% select(player, comp_type = type),
#             by = c("team_comp" = "player")) %>% 
#   filter(player %in% c("Antwane Wells", "Tre Harris"))
# 
# 
# scaled_df <- scale(test_df %>% 
#                      select(c(cross_pct, slant_pct, in_pct, out_pct,
#                               hitch_pct, corner_pct, post_pct, go_pct)))
# 
# all_similarities <- matrix(nrow = nrow(scaled_df), ncol = nrow(scaled_df))
# 
# for (i in 1:nrow(scaled_df)) {
#   for (j in 1:nrow(scaled_df)) {
#     all_similarities[i, j] <- cosine_similarity(scaled_df[i, ], scaled_df[j, ])
#   }
# }
# 
# 
# sim_df <- data.frame(all_similarities)
# 
# colnames(sim_df) <- player_names
# 
# fin_sim_df <- sim_df %>% 
#   cbind(test_df) %>% 
#   pivot_longer(cols = -c(player, rank, cross_pct, slant_pct, in_pct, out_pct,
#                          hitch_pct, corner_pct, post_pct, go_pct, type),
#                names_to = "team_comp", values_to = "sim_score") %>% 
#   filter(player != team_comp) %>% 
#   filter(!is.na(sim_score)) %>% 
#   arrange(player, desc(sim_score)) %>% 
#   group_by(player) %>% 
#   mutate(sim_rank = row_number()) %>% 
#   filter(player == 'Tetairoa McMillan')
# 
# 
# fin_df <- fin_dist_df %>% left_join(fin_dist_man_df) %>% left_join(fin_sim_df)
