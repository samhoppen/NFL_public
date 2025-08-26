library(tidyverse)
library(dplyr)
library(nflfastR)
library(showtext)
library(ggridges)
library(ggthemes)
library(tidytext)
library(nflplotR)
library(nflreadr)
library(cowplot)
library(nflseedR)
library(gridExtra)
library(grid)
library(gtable)
library(httr)
library(jsonlite)
source("~/Documents/R/2025_offseason/offszn_resources_functions.R")

p <- function(v) {
  Reduce(f=paste0, x = v)
}


rosters <- load_rosters(2024) %>% 
  select(full_name, gsis_id, headshot_url, team, position) %>% 
  mutate(full_name = clean_player_names(full_name)) %>% 
  unique()

team_logos <- teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_logo_espn)

team_colors <- teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_color, team_color2)

team_names <- teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_name)

team_abbr <-  teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_nick)

cap_space <- read_csv("~/Documents/Data/Cap_Space_2025.csv") %>% 
  mutate(cap_diff = eff_cap_space-cap_space,
         cap_space_text = if_else(cap_space > 0,
                                  paste0("$",round(cap_space/1000000, 1), "M"),
                                  paste0("-$",round(abs(cap_space)/1000000, 1), "M"))) %>% 
  select(team_abbr, cap_space, cap_space_text, cap_diff)


pff_grades <- read_csv("~/Documents/Data/PFF_Grades_2024.csv") %>% 
  mutate(team_name = if_else(team_name == "LA", "LAR", team_name))

free_agents <- read_csv("~/Documents/Data/Free_Agents_2025.csv") %>% 
  filter(type != "Trade", type != "Extension") %>% 
  filter(!(type == "Signed" & team_2024==team_2025)) %>% 
  filter(snaps >= 0.25,
         !(pos %in% c("P", "LS", "K")),
         type %in% c("UFA", "RFA", "ERFA", "SFA", "Void")) %>% 
  left_join(team_abbr,
            by = c("team_2024" = "team_nick")) %>% 
  mutate(snap_rate = paste0(snaps*100, "%"),
         player = nflreadr::clean_player_names(player),
         player = case_when(player == "Jr Chauncey Gardner-Johnson" ~ "CJ Gardner-Johnson",
                            player == "Tedarrell Slaton" ~ "TJ Slaton",
                            player == "Michael Jackson" & team_abbr == "CAR" ~ "Mike Jackson",
                            player == "Kavontae Turpin" ~ "KaVontae Turpin",
                            player == "Patrick Jones" ~ "Pat Jones",
                            player == "Matt Judon" ~ "Matthew Judon",
                            TRUE ~ player)) %>% 
  left_join(pff_grades,
            by = c("player" = "name",
                   "team_abbr" = "team_name"))

free_agents$current_apy <- paste0('$',formatC(free_agents$current_apy, big.mark=',', format = 'f', digits =  0))
free_agents$guarantees <- paste0('$',formatC(free_agents$guarantees, big.mark=',', format = 'f', digits =  0))

draft_picks <- read_csv("~/Documents/Data/Draft_Picks_2025.csv") %>% 
  mutate(pick_info = paste0(pick_num, " (",round,"), ")) %>% 
  select(team = team_abbr, pick_info)

# teams <- draft_picks %>% 
#   group_by(team) %>% 
#   summarize(count = n()) %>% 
#   arrange(count)

draft_new <- read_csv("~/Documents/Data/Draft_Picks_2025.csv") %>%
  arrange(pick_num) %>%
  group_by(round) %>%
  mutate(round_pick = row_number(),
         ovr_pick = pick_num,
         team = team_abbr)


all_picks <- draft_picks %>% 
  group_by(team) %>% 
  summarize(all_picks = p(pick_info)) %>% 
  ungroup()

all_picks$all_picks = substr(all_picks$all_picks,1,nchar(all_picks$all_picks)-2)

.clear_cache()
games <- load_schedules() %>% 
  filter(season == 2024, week <= 18)

placing <- games %>% # mutate(sim = 1) %>% 
  select(sim = season, game_type, week, away_team, home_team, result) %>% 
  nflseedR::compute_division_ranks() %>%
  purrr::pluck("standings") %>% 
  mutate(division_rank = case_when(div_rank == 1 ~ paste0("1st in ", division),
                                   div_rank == 2 ~ paste0("2nd in ", division),
                                   div_rank == 3 ~ paste0("3rd in ", division),
                                   div_rank == 4 ~ paste0("4th in ", division)))

records <- games %>% 
  clean_homeaway() %>% 
  mutate(win = if_else(team_score > opponent_score, 1, 0),
         loss = if_else(team_score < opponent_score, 1, 0),
         tie = if_else(team_score == opponent_score, 1, 0)) %>% 
  group_by(team) %>% 
  summarize(wins = sum(win),
            losses = sum(loss),
            ties = sum(tie)) %>% 
  ungroup() %>% 
  left_join(placing %>% select(c(team, division_rank)),
            by = c("team"))%>% 
  mutate(record = if_else(ties == 0, paste0(wins,"-",losses, " (",division_rank,")"), paste0(wins, "-", losses, "-", ties, " (",division_rank,")"))) %>% 
  select(team, record) %>% 
  mutate(team = if_else(team == "LA", "LAR", team))

# team_needs <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/team_needs_2024.csv") %>% 
#   mutate(team = if_else(team == "JAC", "JAX", team))

# data in geom_tile:

tm_recap_chart_fn("SEA")
nfl_teams <- c("DAL", "MIN", "CHI")
nfl_teams <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LAR', 'SEA', 'SF')

for(t in nfl_teams){
  tm_recap_chart_fn(t)
  print(paste0(t, " done"))
}

test <- free_agents %>% 
  group_by(team_abbr_24) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

##########



# library(tidyverse)
# library(dplyr)
# library(nflfastR)
# library(showtext)
# library(ggridges)
# library(ggthemes)
# library(tidytext)
# library(nflplotR)
# library(nflreadr)
# library(cowplot)
# library(nflseedR)
# library(gridExtra)
# library(grid)
# library(gtable)
# library(httr)
# library(jsonlite)
# library(rvest)
# 
# p <- function(v) {
#   Reduce(f=paste0, x = v)
# }
# 
# 
# rosters <- load_rosters(2024) %>% 
#   select(full_name, gsis_id, headshot_url, team, position) %>% 
#   mutate(full_name = clean_player_names(full_name)) %>% 
#   unique()
# 
# team_logos <- teams_colors_logos %>% 
#   filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
#   select(team_abbr, team_logo_espn)
# 
# team_colors <- teams_colors_logos %>% 
#   filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
#   select(team_abbr, team_color, team_color2)
# 
# team_names <- teams_colors_logos %>% 
#   filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
#   select(team_abbr, team_name)
# 
# team_abbr <-  teams_colors_logos %>% 
#   filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
#   select(team_abbr, team_nick)
# 
# cap_link <- "https://overthecap.com/salary-cap-space"
# 
# cap_space <- cap_link %>% 
#   read_html() %>% 
#   html_element("table") %>% 
#   html_table() %>% 
#   left_join(team_abbr,
#             by = c("Team" = "team_nick")) %>% 
#   mutate(cap_space = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Cap Space`))),
#          eff_cap_space = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Effective Cap Space`))),
#          active_cap_spending = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Active Cap Spending`))),
#          dead_money = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Dead Money`)))) %>% 
#   select(team_abbr, cap_space, eff_cap_space, 
#          active_cap_spending, dead_money) %>% 
#   mutate(cap_diff = eff_cap_space-cap_space,
#          cap_space_text = if_else(cap_space > 0,
#                                   paste0("$",round(cap_space/1000000, 1), "M"),
#                                   paste0("-$",round(abs(cap_space)/1000000, 1), "M"))) %>% 
#   select(team_abbr, cap_space, cap_space_text, cap_diff)
# 
# # cap_space <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/Cap_Space_2024.csv") %>% 
# #   mutate(cap_diff = eff_cap_space-cap_space,
# #          cap_space_text = if_else(cap_space > 0,
# #                                   paste0("$",round(cap_space/1000000, 1), "M"),
# #                                   paste0("-$",round(abs(cap_space)/1000000, 1), "M"))) %>% 
# #   select(team_abbr, cap_space, cap_space_text, cap_diff)
# 
# 
# pff_grades <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/PFF Grades 2023.csv") %>% 
#   mutate(team_name = if_else(team_name == "LA", "LAR", team_name))
# 
# # make a POST request with action=get_free_agents&season=2023 in form data
# resp <- request("https://overthecap.com/wp-admin/admin-ajax.php") %>% 
#   req_body_form(
#     action = "get_free_agents",
#     season = 2025) %>%
#   req_perform()
# 
# # response includes table rows
# fa_table <- resp %>% resp_body_string() %>% 
#   str_glue(
#     '<table class="controls-table" id="table2024" cellspacing="0" align="center">
#     <thead>
#         <tr>
#             <th class="sortable">Player</th>
#             <th class="sortable sorttable_numeric">Pos.</th>
#             <th class="sortable">2023 Team</th>
#             <th class="sortable">2024 Team</th>
#             <th class="sortable">Type</th>
#             <th class="sortable">Snaps</th>
#             <th class="sortable">Age</th>
#             <th class="sortable">Current APY</th>
#             <th class="sortable mobile_drop">Guarantees</th>
#         </tr>
#     </thead>
#     <tbody>{.}</tbody>') %>% 
#   # turn it into valid html
#   minimal_html() %>% 
#   html_element("table") %>% 
#   html_table() %>% 
#   select(player = Player, pos = `Pos.`, team_2023 = `2023 Team`, team_2024 = `2024 Team`,
#          type = Type, snaps = Snaps, age = Age, current_apy = `Current APY`, guarantees = Guarantees) %>% 
#   left_join(team_abbr,
#             by = c("team_2023" = "team_nick")) %>% 
#   rename(team_abbr_23 = team_abbr) %>% 
#   left_join(team_abbr,
#             by = c("team_2024" = "team_nick")) %>% 
#   rename(team_abbr_24 = team_abbr) %>% 
#   mutate(current_apy = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", current_apy))),
#          guarantees = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", current_apy))),
#          snaps = as.numeric(gsub("%","", snaps))/100)
# 
# free_agents <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/Free_Agents_2024.csv") %>% 
#   filter(type != "Trade", type != "Extension") %>% 
#   filter(!(type == "Signed" & team_2023==team_2024)) %>% 
#   # filter(!(player))
#   filter(snaps >= 0.25,
#          !(pos %in% c("P", "LS", "K")),
#          type %in% c("UFA", "RFA", "ERFA", "SFA")) %>% 
#   left_join(team_abbr,
#             by = c("team_2023" = "team_nick")) %>% 
#   mutate(snap_rate = paste0(snaps*100, "%"),
#          player = nflreadr::clean_player_names(player),
#          player = case_when(player == "Jr Chauncey Gardner-Johnson" ~ "CJ Gardner-Johnson",
#                             player == "Yosuah Nijman" ~ "Yosh Nijman",
#                             player == "Jr Michael Pittman" ~ "Michael Pittman",
#                             player == "Jr Antoine Winfield" ~ "Antoine Winfield",
#                             player == "Jr Willie Gay" ~ "Willie Gay",
#                             player == "Jr Jon Runyan" ~ "Jon Runyan",
#                             player == "Iosua Opeta" ~ "Sua Opeta",
#                             player == "Michael Onwenu" ~ "Mike Onwenu",
#                             TRUE ~ player)) %>% 
#   left_join(pff_grades,
#             by = c("player" = "name",
#                    "team_abbr" = "team_name"))
# 
# free_agents$current_apy <- paste0('$',formatC(free_agents$current_apy, big.mark=',', format = 'f', digits =  0))
# free_agents$guarantees <- paste0('$',formatC(free_agents$guarantees, big.mark=',', format = 'f', digits =  0))
# 
# draft_picks <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/Draft_Picks_2024.csv") %>% 
#   mutate(pick_info = paste0(pick_num, " (",round,"), ")) %>% 
#   select(team = team_abbr, pick_info)
# 
# draft_new <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/Draft_Picks_2024.csv") %>%
#   arrange(pick_num) %>%
#   group_by(round) %>%
#   mutate(round_pick = row_number(),
#          ovr_pick = pick_num,
#          team = team_abbr)
# 
# 
# all_picks <- draft_picks %>% 
#   group_by(team) %>% 
#   summarize(all_picks = p(pick_info)) %>% 
#   ungroup()
# 
# all_picks$all_picks = substr(all_picks$all_picks,1,nchar(all_picks$all_picks)-2)
# 
# .clear_cache()
# games <- load_schedules() %>% 
#   filter(season == 2024, week <= 18)
# 
# placing <- games %>% # mutate(sim = 1) %>% 
#   select(sim = season, game_type, week, away_team, home_team, result) %>% 
#   nflseedR::compute_division_ranks() %>%
#   purrr::pluck("standings") %>% 
#   mutate(division_rank = case_when(div_rank == 1 ~ paste0("1st in ", division),
#                                    div_rank == 2 ~ paste0("2nd in ", division),
#                                    div_rank == 3 ~ paste0("3rd in ", division),
#                                    div_rank == 4 ~ paste0("4th in ", division)))
# 
# records <- games %>% 
#   clean_homeaway() %>% 
#   mutate(win = if_else(team_score > opponent_score, 1, 0),
#          loss = if_else(team_score < opponent_score, 1, 0),
#          tie = if_else(team_score == opponent_score, 1, 0)) %>% 
#   group_by(team) %>% 
#   summarize(wins = sum(win),
#             losses = sum(loss),
#             ties = sum(tie)) %>% 
#   ungroup() %>% 
#   left_join(placing %>% select(c(team, division_rank)),
#             by = c("team"))%>% 
#   mutate(record = if_else(ties == 0, paste0(wins,"-",losses, " (",division_rank,")"), paste0(wins, "-", losses, "-", ties, " (",division_rank,")"))) %>% 
#   select(team, record) %>% 
#   mutate(team = if_else(team == "LA", "LAR", team))
# 
# # team_needs <- read_csv("C:/Users/sphop/OneDrive/FantasyPros/NFL/Data/team_needs_2024.csv") %>% 
# #   mutate(team = if_else(team == "JAC", "JAX", team))
# 
# # data in geom_tile:
# 
# tm_recap_chart_fn("PIT")
# nfl_teams <- c("DAL", "MIN", "CHI")
# nfl_teams <- c('BUF', 'MIA', 'NE', 'NYJ', 'BAL', 'CIN', 'CLE', 'PIT', 'HOU', 'IND', 'JAX', 'TEN', 'DEN', 'KC', 'LAC', 'LV', 'DAL', 'NYG', 'PHI', 'WAS', 'CHI', 'DET', 'GB', 'MIN', 'ATL', 'CAR', 'NO', 'TB', 'ARI', 'LAR', 'SEA', 'SF')
# 
# for(t in nfl_teams){
#   tm_recap_chart_fn(t)
#   print(paste0(t, " done"))
# }
# 
# 




