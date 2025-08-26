source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)
library(rvest)
library(stringr)
library(httr2)
library(datapasta)
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")

edp <- read_csv(file.path(data_path, "big_board_2025.csv")) %>% 
  mutate(player = paste0(first_name, " ", last_name),
         player = nflreadr::clean_player_names(player),
         EDP = rank) %>% 
  select(-c(school, headshot))



log_trans <- function(base = exp(1)) {
  # Define the transformation function as the natural logarithm (or log to a specified base)
  trans <- function(x) log(x, base)
  
  # Define the inverse transformation function as exponentiation to the base
  inv <- function(x) base^x
  
  # Use the trans_new function to create a new transformation
  # with the specified name, transformation, inverse transformation,
  # appropriate breaks, and domain.
  trans_new(paste0("log-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}
#https://emojipedia.org/apple
team_abbr <- nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", 'SD', 'OAK', 'LAR'))) %>% 
  select(team_name, team_abbr)

# SR – Senior Bowl meeting. - reese's cup
# EW – East-West Shrine meeting - 
# COM – Combine meeting. - dumbell
# INT – Interested.
# VINT – Very Interested.
# PRO – Pro Day or campus meeting/workout.
# LOC – Local visit. Prospect making a local visit.
# T30 – Top 30 Private Visit. Prospect making an official Top 30 Visit.
# WOR – Private Workout. Members of an organization working out a player in private.
# STM – Some Type of Meeting. - 
# VIR – Virtual Meeting. - computer screen
# % – indicates more than one meeting at an event.
# ^ – has met with team at more than one event.
# # – indicates meeting set up outside of the 2024 Senior Bowl or the 2024 East-West Shrine Game

bowl <- "https://em-content.zobj.net/source/apple/391/bowling_1f3b3.png"
combine <- "https://em-content.zobj.net/source/apple/391/flexed-biceps_1f4aa.png"
pro_day <- "https://em-content.zobj.net/source/apple/391/american-football_1f3c8.png"
# local <- "https://em-content.zobj.net/source/apple/391/round-pushpin_1f4cd.png"
workout <- "https://em-content.zobj.net/source/apple/391/man-running_1f3c3-200d-2642-fe0f.png"
top_30 <- "https://em-content.zobj.net/source/apple/391/top-arrow_1f51d.png"#"C:/Users/sphop/OneDrive/FantasyPros/Logos/emojis/star.png" # 
virtual <- "https://em-content.zobj.net/source/apple/391/laptop_1f4bb.png"

draft_link <- "https://walterfootball.com/ProspectMeetingsByTeam2025.php"


draft_node <- draft_link %>%
  read_html() %>% 
  html_nodes(xpath = '//div | //li') %>% 
  html_text() %>% 
  data.frame()

colnames(draft_node) <- c("text")

fin_data <- draft_node %>%
  separate(col = text,
           into = c("player", "position", "meetings"),
           sep = ", ",
           remove = FALSE, # Set to FALSE if you want to keep the original column
           extra = "merge") %>% 
  filter(!(grepl("NFL", player)),
         !(grepl("By ", player)),
         !(grepl("Draft", player)),
         !(grepl("April ", player)),
         !(grepl("NBA", player)),
         !(grepl("NBA", position)),
         !(grepl("Headlines", player)),
         !(grepl("Jerks", player)),
         !(grepl("LegacyMenu", player)),
         !(grepl("Fantasy", player)),
         !(grepl("Premium", player)),
         !(grepl("hours", player)),
         !(grepl("Home", player)),
         !(grepl("SubMenu", player)),
         !(grepl("black background", player)),
         !(grepl("\\(", player)),
         !(player %in% c("\n", "\n\n", "", "Search", "Freeagents", "Humor", "Picks", "Season", "Uncategorized",
                         "Freeagents\n", "Humor\n", "Picks\n", "Season\n", "Uncategorized\n"))) %>% 
  separate(col = meetings,
           into = c("school", "meetings"),
           sep = " \\(",
           remove = TRUE) %>% 
  mutate(team_name = if_else(is.na(lag(position)), lag(player), NA_character_),
         meetings = gsub("\\)", "", meetings),
         player = gsub("\\^", "", player),
         player = nflreadr::clean_player_names(player)) %>% 
  fill(team_name, .direction = "down") %>% 
  mutate(team_name = str_trim(team_name),
         team_name = case_when(team_name == "Washington Redskins" ~ "Washington Commanders",
                               TRUE ~ team_name)) %>% 
  filter(!is.na(meetings)) %>% 
  left_join(team_abbr) %>% 
  mutate(multiple_meetings = if_else(grepl("\\^", text), "Multiple", "Single"),
         bowl_visit = if_else(grepl("SR", meetings) | grepl("EW", meetings),1,0),
         # ew_visit = if_else(grepl("EW", meetings),1,0),
         pro_visit = if_else(grepl("PRO", meetings),1,0),
         loc_visit = if_else(grepl("LOC", meetings),1,0),
         wor_visit = if_else(grepl("WOR", meetings),1,0),
         vir_visit = if_else(grepl("VIR", meetings),1,0),
         com_visit = if_else(grepl("COM", meetings),1,0),
         t30_visit = if_else(grepl("T30", meetings),1,0),
         pos = case_when(position == "Cornerback" ~ "CB",
                         position %in% c("Defensive Tackle", "Nose Tackle") ~ "IDL",
                         position == "Wide Receiver" ~ "WR",
                         position == "Running Back" ~ "RB",
                         position == "Quarterback" ~ "QB",
                         position == "Tight End" ~ "TE",
                         position == "Safety" ~ "S",
                         position == "Offensive Tackle" ~ "OT",
                         position %in% c("Offensive Guard", "Center") ~ "IOL",
                         position %in% c("Outside Linebacker", "Linebacker", "Inside Linebacker") ~ "LB",
                         position %in% c("Defensive Line", "3-4 Outside Linebacker", "Defensive End", "3-4 Defensive End") ~ "EDGE",
                         TRUE ~ position)) %>% 
  # mutate(edp = sample(1:50, n(), replace = TRUE)) %>% 
  mutate(player = case_when(player == "Andre Biggers" ~ "Zeek Biggers",
                            player == "Olu Oladejo" ~ "Oluwafemi Oladejo",
                            player == "Sam Brown" ~ "Samuel Brown",
                            player == "Cam Jackson" ~ "CamRon Jackson",
                            player == "Azareye'h Thomas" ~ "Azareyeh Thomas",
                            TRUE ~ player)) %>% 
  left_join(edp,
            by = c("player" = "player")) %>% 
  mutate(total_visits = pro_visit+wor_visit+vir_visit+com_visit+t30_visit)



# err <- fin_data %>% filter(is.na(EDP)) %>%
#   select(player, school.x, position.x, position.y) %>% unique()

players <- fin_data %>% 
  select(player, EDP) %>% 
  unique() %>% 
  arrange(EDP) %>% 
  group_by(player) %>% 
  mutate(count = n())
# fin_data %>% select(pos) %>% unique()  

off_visits <- fin_data %>% 
  filter(pos %in% c('QB', 'WR', 'TE', 'RB', 'OT', 'IOL'))


off_visits$pos <- factor(off_visits$pos, levels = c('OT', 'IOL', 'TE', 'WR', 'RB', 'QB'))#'S', 'CB', 'LB', 'EDGE', 'IDL', 
off_visits$team_abbr <- factor(off_visits$team_abbr, levels = nfl_teams)


off_order <- c('OT', 'IOL', 'TE', 'WR', 'RB', 'QB')
off_visits_chart <- ggplot() + 
  geom_point(data = off_visits %>% filter(total_visits > 1),
                      aes(x = EDP, y = factor(pos, off_order)),
                      shape = 1,
                      color = "green3",
                      size = 3)+
  ggimage::geom_image(data = off_visits %>% filter(com_visit == 1),
                      aes(x = EDP, y = factor(pos, off_order),
                          image = combine),
                      size = 0.1)+
  ggimage::geom_image(data = off_visits %>% filter(pro_visit == 1),
                      aes(x = EDP, y = factor(pos, off_order),
                          image = pro_day),
                      size = 0.125)+
  ggimage::geom_image(data = off_visits %>% filter(wor_visit == 1),
                      aes(x = EDP, y = factor(pos, off_order),
                          image = workout),
                      size = 0.125)+
  ggimage::geom_image(data = off_visits %>% filter(vir_visit == 1),
                      aes(x = EDP, y = factor(pos, off_order),
                          image = virtual),
                      size = 0.125)+
  ggimage::geom_image(data = off_visits %>% filter(t30_visit == 1),
             aes(x = EDP, y = factor(pos, off_order),
                 image = top_30),
             size = 0.125)+
  scale_x_continuous(trans=log_trans(10),
                     breaks = c(1, 5, 10, 32, 100, 250))+
  facet_wrap(. ~ team_abbr, nrow = 4) +
  theme_FE + 
  labs(title = paste0("Offensive draft prospect visits, by team (as of 4/23/25)"),
       subtitle = NULL,
       caption = "Figure: @SamHoppen | Data: Walter Football, Jack Lichtenstein",
       y = "",
       x = "Consensus Big Board Rank") +
  theme(axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4),
        # axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.y = unit(0.5, 'lines'),
        legend.position = "none",
        strip.text = element_nfl_wordmark())


brand_nfl_plot(off_visits_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/top_30_visits_offense_4_23.png"))


def_order <- c('S', 'CB', 'LB', 'EDGE', 'IDL')

def_visits <- fin_data %>% 
  filter(pos %in% def_order)


def_visits$pos <- factor(def_visits$pos, levels = c('S', 'CB', 'LB', 'EDGE', 'IDL'))#'S', 'CB', 'LB', 'EDGE', 'IDL', 
def_visits$team_abbr <- factor(def_visits$team_abbr, levels = nfl_teams)


def_visits_chart <- ggplot() + 
  geom_point(data = def_visits %>% filter(total_visits > 1),
             aes(x = EDP, y = factor(pos, def_order)),
             shape = 1,
             color = "green3",
             size = 3)+
  ggimage::geom_image(data = def_visits %>% filter(com_visit == 1),
                      aes(x = EDP, y = factor(pos, def_order),
                          image = combine),
                      size = 0.1)+
  ggimage::geom_image(data = def_visits %>% filter(pro_visit == 1),
                      aes(x = EDP, y = factor(pos, def_order),
                          image = pro_day),
                      size = 0.125)+
  ggimage::geom_image(data = def_visits %>% filter(wor_visit == 1),
                      aes(x = EDP, y = factor(pos, def_order),
                          image = workout),
                      size = 0.125)+
  ggimage::geom_image(data = def_visits %>% filter(vir_visit == 1),
                      aes(x = EDP, y = factor(pos, def_order),
                          image = virtual),
                      size = 0.125)+
  ggimage::geom_image(data = def_visits %>% filter(t30_visit == 1),
                      aes(x = EDP, y = factor(pos, def_order),
                          image = top_30),
                      size = 0.125)+
  scale_x_continuous(trans=log_trans(10),
                     breaks = c(1, 5, 10, 32, 100, 250))+
  facet_wrap(. ~ team_abbr, nrow = 4) +
  theme_FE + 
  labs(title = paste0("Defensive draft prospect visits, by team (as of 4/23/25)"),
       subtitle = NULL,
       caption = "Figure: @SamHoppen | Data: Walter Football, Jack Lichtenstein",
       y = "",
       x = "Consensus Big Board Rank") +
  theme(axis.text.y = element_text(size = 4),
        axis.text.x = element_text(size = 4),
        # axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.spacing.y = unit(0.5, 'lines'),
        legend.position = "none",
        strip.text = element_nfl_wordmark())


brand_nfl_plot(def_visits_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/top_30_visits_defense_4_23.png"))

