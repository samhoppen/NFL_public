library(dplyr)
library(tidyverse)
library(nflreadr)

pbp <- nflfastR::load_pbp(2019:2024)

rosters <- nflreadr::load_rosters(2019:2024) %>% 
  filter(!is.na(gsis_id)) %>% 
  select(gsis_id, season, position) %>% 
  unique() %>% 
  group_by(gsis_id) %>% 
  filter(row_number() == 1)

nfl_team_rushing <- pbp %>% 
  filter(play_type != "no_play", qb_scramble != 1) %>% 
  group_by(posteam, season) %>% 
  summarize(carries = sum(rush_attempt),
            shotgun_carries = sum(rush_attempt[shotgun == 1])) %>% 
  ungroup() %>% 
  mutate(shotgun_rush_rate = shotgun_carries/carries)

nfl_team_rec <- pbp %>% 
  filter(play_type != "no_play", pass_attempt == 1, !is.na(receiver_player_id)) %>% 
  left_join(rosters,
            by = c("receiver_player_id" = "gsis_id",
                   "season" = "season")) %>% 
  group_by(posteam, season) %>% 
  summarize(tgt_share = sum(pass_attempt[position == 'RB'], na.rm = T)/sum(pass_attempt, na.rm = T))

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

big_board <- read_csv("~/Documents/Data/big_board_2025.csv") %>% 
  mutate(player = paste0(first_name, " ", last_name),
         player = nflreadr::clean_player_names(player))

nfl_rushing <- read_csv("~/Documents/Data/pff_nfl_rushing.csv")
cfb_rushing <- read_csv("~/Documents/Data/pff_cfb_rushing.csv") %>% 
  filter(!(player == "Zach Evans" & team != "OLE MISS"))

drafts <- nflreadr::load_draft_picks() %>% 
  filter(season >= 2019) %>% 
  mutate(player = nflreadr::clean_player_names(pfr_player_name))

college_stats <- cfb_rushing %>% 
  filter(attempts >= 25) %>% 
  group_by(player_id) %>% 
  filter(season == max(season)) %>% 
  ungroup() %>% 
  select(player, player_id, season, team, gap_attempts, zone_attempts, attempts) %>% 
  mutate(gap_pct = gap_attempts/(gap_attempts+zone_attempts),
         zone_pct = zone_attempts/(gap_attempts+zone_attempts),
         player = nflreadr::clean_player_names(player),
         player = if_else(player == "Joquavioius Marks", "Woody Marks", player))

nfl_stats <- nfl_rushing %>% 
  group_by(team_name, season) %>% 
  summarize(attempts = sum(gap_attempts)+sum(zone_attempts),
            gap_attempts = sum(gap_attempts),
            zone_attempts = sum(zone_attempts)) %>% 
  ungroup() %>% 
  mutate(gap_pct = gap_attempts/attempts,
         zone_pct = zone_attempts/attempts,
         team = nflreadr::clean_team_abbrs(team_name)) %>% 
  left_join(nfl_team_rushing,
            by = c("team" = "posteam",
                   "season" = "season")) %>% 
  left_join(nfl_team_rec,
            by = c("team" = "posteam",
                   "season" = "season"))

prospects <- big_board %>% 
  filter(position == "RB") %>% 
  filter(rank <=250) %>% 
  mutate(player = paste0(first_name, " ", last_name),
         player = nflreadr::clean_player_names(player),
         gsis_id = sub("https://www.nfldraftbuzz.com/Content/PlayerHeadShots/", "", headshot),
         season = 2025,
         round = 0, 
         pick = rank,
         team = "Prospect",
         games = 0,
         dr_av = 0) %>% 
  select(player, gsis_id, season, round, pick, team, games, dr_av)

cfb_tgts <- read_csv("~/Documents/Data/cfb_target_shares.csv") %>% 
  filter(position == "RB" | player %in% c("Malcolm Perry", "Antonio Gibson")) %>% 
  filter(!(player == "Zach Evans" & newestTeamAbbrevName != "MISS")) %>% 
  group_by(player, playerId) %>% 
  filter(season == max(season)) %>% 
  ungroup() %>% 
  select(player, playerId, tgt_share = `TeamTgt%`) %>% 
  mutate(player = nflreadr::clean_player_names(player),
         tgt_share = as.numeric(sub("%", "", tgt_share))/100) %>% 
  mutate(player = if_else(player == "JoQuavious Marks", "Woody Marks", player))

cfb_shotgun <- read_csv("~/Documents/Data/rb_shotgun_carries.csv") %>% 
  filter(position == "RB" | player %in% c("Malcolm Perry", "Antonio Gibson")) %>% 
  filter(!(player == "Zach Evans" & newestTeamAbbrevName != "MISS"), Rush >= 10) %>% 
  group_by(player, playerId) %>% 
  filter(season == max(season)) %>% 
  ungroup() %>% 
  select(player, playerId, shotgun_carries = Rush) %>% 
  mutate(player = nflreadr::clean_player_names(player)) %>% 
  mutate(player = if_else(player == "Joquavioius Marks", "Woody Marks", player)) %>% 
  left_join(cfb_tgts)

fin_data <- drafts %>% 
  filter(position == "RB", season >=2020) %>% 
  select(player, gsis_id, season, round, pick, team, games, dr_av) %>% 
  rbind(prospects) %>% 
  left_join(college_stats %>% select(-c(team, season)),
            by = c("player")) %>% 
  left_join(cfb_shotgun,
            by = c("player")) %>% #
  filter(!is.na(gap_pct), !is.na(tgt_share)) %>%
  arrange(season) %>% 
  mutate(shotgun_rush_rate = shotgun_carries/attempts) %>%
  select(player, gsis_id, season, round, pick, team, games, dr_av, gap_pct, zone_pct, shotgun_rush_rate, tgt_share)

#### SIM SCORES #####
euclidean_distance <- function(x, y) {
  sqrt(sum((x - y)^2))
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

all_prospect_comps <- data.frame()

all_prospects <- fin_data$player

for(prospect in all_prospects){
  player_szn <- fin_data %>% 
    filter(player == prospect) %>% 
    select(season) %>% 
    pluck(1)
  
  nfl_df <- nfl_stats %>% 
    filter(season == player_szn | season == player_szn-1) %>% 
    mutate(player = paste0(team, "_", season)) %>% 
    select(player, gap_pct, zone_pct, shotgun_rush_rate)
  
  test_df <- fin_data %>% 
    filter(player == prospect) %>% 
    bind_rows(nfl_df)
  
  player_names <- test_df$player
  
  scaled_df <- scale(test_df %>% 
                       select(c(gap_pct, zone_pct, shotgun_rush_rate)))
  
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
    pivot_longer(cols = -c(player, gsis_id, season, round, pick, team, games, dr_av, gap_pct, zone_pct, shotgun_rush_rate),
                 names_to = "team_comp", values_to = "distance") %>% 
    filter(player != team_comp) %>% 
    filter(!is.na(distance)) %>% 
    arrange(player, distance) %>% 
    group_by(player) %>% 
    mutate(dist_rank = row_number()) %>% 
    filter(player == prospect) %>% 
    left_join(nfl_df,
              by = c("team_comp" = "player"),
              suffix = c("_cfb", "_nfl"))
  
  
  scaled_df <- scale(test_df %>% 
                       select(c(gap_pct, zone_pct, shotgun_rush_rate)))
  
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
    pivot_longer(cols = -c(player, gsis_id, season, round, pick, team, games, dr_av, gap_pct, zone_pct, shotgun_rush_rate),
                 names_to = "team_comp", values_to = "sim_score") %>% 
    filter(player != team_comp) %>% 
    filter(!is.na(sim_score)) %>% 
    arrange(player, desc(sim_score)) %>% 
    group_by(player) %>% 
    mutate(sim_rank = row_number()) %>% 
    filter(player == prospect) %>% 
    left_join(nfl_df,
              by = c("team_comp" = "player"),
              suffix = c("_cfb", "_nfl"))
  
  
  fin_df <- fin_dist_df %>% left_join(fin_sim_df)
  
  all_prospect_comps <- rbind(all_prospect_comps, fin_df)
}



fin_prospect_comps <- all_prospect_comps %>% 
  filter(team == "Prospect", sim_rank <=3) %>% 
  mutate(comp_team = str_split(team_comp, "_") %>% map_chr(., 1),
         comp_year = str_split(team_comp, "_") %>% map_chr(., 2) %>% as.numeric()) %>% 
  left_join(primary_playcallers %>% select(comp_team = team, season, returning_playcaller)) %>% 
  select(player, gap_pct_cfb, zone_pct_cfb, shotgun_rush_rate_cfb, sim_score, sim_rank,
         comp_team, returning_playcaller) %>% 
  left_join(prospects %>% select(player, rank = pick)) %>% 
  arrange(rank, sim_rank)

# draft_table <- draft_picks %>% 
#   arrange(ovr_pick) %>% 
#   gt() %>% 
#   tab_header(title = md("**How all of the 1st round draft pick salaries will rank (in the # column) within the current market at each position**"),
#              subtitle = paste0("Since 2020")) %>% 
#   cols_label(ovr_pick = "Pick",
#              team_abbr = "Team",
#              apy = "Est. APY",
#              QB_num = "#",
#              QB_pct = "%ile",
#              RB_num = "#",
#              RB_pct = "%ile",
#              WR_num = "#",
#              WR_pct = "%ile",
#              TE_num = "#",
#              TE_pct = "%ile",
#              LT_num = "#",
#              LT_pct = "%ile",
#              RT_num = "#",
#              RT_pct = "%ile",
#              G_num = "#",
#              G_pct = "%ile",
#              C_num = "#",
#              C_pct = "%ile",
#              EDGE_num = "#",
#              EDGE_pct = "%ile",
#              IDL_num = "#",
#              IDL_pct = "%ile",
#              LB_num = "#",
#              LB_pct = "%ile",
#              CB_num = "#",
#              CB_pct = "%ile",
#              S_num = "#",
#              S_pct = "%ile") %>% 
#   cols_align(align = "center",
#              columns = everything()) %>%
#   cols_width(everything() ~ px(50)) %>% 
#   cols_width(team_abbr ~ 125) %>% 
#   cols_width(ends_with("pct") ~ px(60)) %>% 
#   cols_width(ends_with("num") ~ px(30)) %>% 
#   cols_width(apy ~ px(110)) %>% 
#   fmt_currency(columns = c(apy),
#                currency = "USD",
#                decimals = 0) %>% 
#   fmt_percent(columns = c(QB_pct, RB_pct, WR_pct, TE_pct, RT_pct, LT_pct, G_pct, C_pct,
#                           EDGE_pct, IDL_pct, LB_pct, CB_pct, S_pct),
#               decimals = 1) %>% 
#   tab_style(
#     style = list(
#       cell_borders(
#         sides = "left",
#         color = "black",
#         weight = px(3)
#       )
#     ),
#     locations = list(
#       cells_body(
#         columns = c(QB_num, EDGE_num)
#       )
#     )
#   ) %>% 
#   data_color(
#     columns = c(QB_pct, RB_pct, WR_pct, TE_pct, RT_pct, LT_pct, G_pct, C_pct,
#                 EDGE_pct, IDL_pct, LB_pct, CB_pct, S_pct),
#     alpha = 0.7,
#     colors = scales::col_numeric(
#       paletteer::paletteer_d(
#         palette = "RColorBrewer::GnBu"
#       ) %>% as.character(),
#       domain = full_val_range
#     )
#   ) %>%
#   tab_spanner(
#     label = html("QB"),
#     columns = c(QB_num, QB_pct)
#   ) %>%
#   tab_spanner(
#     label = html("RB"),
#     columns = c(RB_num, RB_pct)
#   ) %>%
#   tab_spanner(
#     label = html("WR"),
#     columns = c(WR_num, WR_pct)
#   ) %>%
#   tab_spanner(
#     label = html("TE"),
#     columns = c(TE_num, TE_pct)
#   ) %>%
#   tab_spanner(
#     label = html("RT"),
#     columns = c(RT_num, RT_pct)
#   ) %>%
#   tab_spanner(
#     label = html("LT"),
#     columns = c(LT_num, LT_pct)
#   ) %>%
#   tab_spanner(
#     label = html("G"),
#     columns = c(G_num, G_pct)
#   ) %>%
#   tab_spanner(
#     label = html("C"),
#     columns = c(C_num, C_pct)
#   ) %>%
#   tab_spanner(
#     label = html("EDGE"),
#     columns = c(EDGE_num, EDGE_pct)
#   ) %>%
#   tab_spanner(
#     label = html("IDL"),
#     columns = c(IDL_num, IDL_pct)
#   ) %>%
#   tab_spanner(
#     label = html("LB"),
#     columns = c(LB_num, LB_pct)
#   ) %>%
#   tab_spanner(
#     label = html("CB"),
#     columns = c(CB_num, CB_pct)
#   ) %>%
#   tab_spanner(
#     label = html("S"),
#     columns = c(S_num, S_pct)
#   ) %>% 
#   nflplotR::gt_nfl_wordmarks(locations = cells_body(c(draft_team))) %>% 
#   tab_source_note(
#     source_note = "Table: @SamHoppen | Data: PFF, nflreadr"
#   )
# 
# 
# # nflplotR::gt_nfl_logos(locations = cells_body(c(team_abbr)))
# gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/draft_pick_comp_25.png"),
#                        vwidth = 1500)

# fin_drafted_comps <- all_prospect_comps %>% 
#   filter(team != "Prospect") %>% 
#   mutate(draft_team = nflreadr::clean_team_abbrs(team),
#          comp_team = str_split(team_comp, "_") %>% map_chr(., 1),
#          comp_year = str_split(team_comp, "_") %>% map_chr(., 2) %>% as.numeric()) %>% 
#   group_by(player, gsis_id, season, round, pick, draft_team, gap_pct_cfb, zone_pct_cfb, shotgun_rush_rate_cfb, tgt_share_cfb) %>% 
#   summarize(draft_sim_score = max(sim_score[draft_team == comp_team & season-1 == comp_year]),
#             draft_sim_rank = max(sim_rank[draft_team == comp_team & season-1 == comp_year]),
#             rookie_sim_score = max(sim_score[draft_team == comp_team & season == comp_year]),
#             rookie_sim_rank = max(sim_rank[draft_team == comp_team & season == comp_year])) %>% 
#   left_join(primary_playcallers %>% select(draft_team = team, season, returning_playcaller)) %>% 
#   filter(round <= 3)


rb_draft_teams <- all_prospect_comps %>% 
  filter(team == "Prospect") %>% 
  mutate(comp_team = str_split(team_comp, "_") %>% map_chr(., 1),
         comp_year = str_split(team_comp, "_") %>% map_chr(., 2) %>% as.numeric()) %>% 
  filter(comp_team %in% c('DAL', 'LV', 'PHI', 'DET')) %>% 
  left_join(primary_playcallers %>% select(comp_team = team, season, returning_playcaller)) %>% 
  select(player, gap_pct_cfb, zone_pct_cfb, shotgun_rush_rate_cfb, tgt_share_cfb, sim_score, sim_rank,
         comp_team, returning_playcaller) %>% 
  mutate(comp_team = case_when(comp_team == "PHI" ~ "NO",
                               comp_team == "DET" ~ "CHI",
                               TRUE ~ comp_team)) %>% 
  arrange(comp_team, sim_rank)
