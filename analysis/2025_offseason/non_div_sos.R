source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")


win_totals <- read_csv(file.path(data_path, "NFL Win Totals-export-2025-08-25.csv")) %>% 
  rename(team = `Team...1`,
         win_total = `Adj. Total`) %>% 
  mutate(team = nflreadr::clean_team_abbrs(team))

.clear_cache()
# test <- nflseedR::simulate_nfl(nfl_season = 2023, simulations = 10)

sched <- nflreadr::load_schedules() %>% 
  filter(season == 2025, div_game != 1) %>% 
  filter(game_type == "REG") %>% 
  select(week, away_team, home_team) %>% 
  unique() %>% 
  nflreadr::clean_homeaway() %>% 
  left_join(win_totals,
            by = c("opponent" = "team")) %>% 
  mutate(loss_total = 17-win_total)


sos <- sched %>% 
  group_by(team) %>% 
  summarize(opp_win_pct = sum(win_total, na.rm = T)/(sum(win_total, na.rm = T)+sum(loss_total, na.rm = T)))

win_chart <- ggplot(data = sos) +
  geom_col(aes(x = reorder(team, -opp_win_pct), y = opp_win_pct, fill = team, color = team)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team, x = reorder(team, -opp_win_pct), y = opp_win_pct), width = 0.03, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     breaks = scales::pretty_breaks(n=8),
                     expand = expansion(mult = c(0, 0.01)),
                     limits = c(0.4, 0.6), oob=rescale_none) + 
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  theme_FE +
  labs(title = paste0("Team strength of schedule for non-divisional opponents based on implied win %"),
       subtitle = "Implied win percent adjusted for over/under prices",
       caption = "Figure: @SamHoppen | Data: nfelo",
       x = "",
       y = "Implied Opponent Win %") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

brand_nfl_plot(win_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/non_div_sos.png"))
