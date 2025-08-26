source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')


trade_values <- read_csv("~/Documents/Data/Fitzgerald_Spielberger.csv")

draft_picks_25 <- read_csv("~/Documents/Data/Draft_Picks_2025.csv") %>% 
  select(team = team_abbr, pick = pick_num, round) %>% 
  mutate(season = 2025)

drafts <- nflreadr::load_draft_picks() %>% 
  filter(season >= 2016) %>% 
  select(season, round, pick, team) %>% 
  rbind(draft_picks_25) %>% 
  mutate(team = nflreadr::clean_team_abbrs(team),
         day_pick = case_when(round == 1 ~ "Day 1",
                              between(round, 2, 3) ~ "Day 2",
                              TRUE ~ "Day 3"))

draft_capital <- drafts %>% 
  left_join(trade_values) %>% 
  group_by(team, season, day_pick) %>% 
  summarize(draft_value = sum(value, na.rm = T))

all_combinations <- expand.grid(
  team = unique(draft_capital$team),
  season = unique(draft_capital$season),
  day_pick = unique(draft_capital$day_pick),
  stringsAsFactors = FALSE
)

# Convert to tibble if needed for cleaner printing
all_combinations <- as_tibble(all_combinations)

# Join with original data
draft_capital <- all_combinations %>%
  left_join(draft_capital, by = c("team", "season", "day_pick")) %>%
  mutate(draft_value = if_else(is.na(draft_value), 0, draft_value))

draft_capital$team <- factor(draft_capital$team, level = nfl_teams)

draft_chart <- ggplot(data = draft_capital) + 
  geom_area(aes(x = season, y = draft_value, fill = day_pick),
            linewidth = 0.05, position = 'stack', alpha = 0.8, color = 'black')+
  scale_x_continuous(breaks = c(2016:2025)) +
  scale_y_continuous(breaks = scales::pretty_breaks(8))+
  facet_wrap(. ~ team, nrow = 4) +
  theme_FE + 
  labs(title = paste0("Total draft capital for each team, by season"),
       subtitle = "Draft capital measured using Fitzgerald-Spielberger trade chart",
       caption = "Figure: @SamHoppen | Data: nflreadR",
       y = "Draft Capital",
       x = NULL,
       fill = NULL) +
  theme(axis.text.x = element_text(size = 4, angle = 90, vjust = 0.5), 
        axis.text.y = element_text(size = 4),
        panel.spacing.y = unit(0.5, 'lines'),
        legend.key.size = unit(0.25, 'cm'),
        legend.position = "right",
        strip.text = element_nfl_wordmark(size = 0.5),
        legend.title.align = 0.5)

brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/draft_capital_season.png"))
