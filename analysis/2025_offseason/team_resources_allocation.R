source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")

pick_values <- read_csv(file.path(data_path, "Fitzgerald_Spielberger.csv"))

drafts <- nflreadr::load_draft_picks() %>% filter(season == 2025)

pos <- drafts %>% select(position) %>% unique()

off_pos <- c('QB', 'WR', 'OT', 'RB', 'OL', 'TE', 'C', 'OG', 'LT', 'RG', 'LG', 'RT')

draft_capital <- drafts %>% 
  left_join(pick_values,
            by = c("pick")) %>% 
  mutate(team = nflreadr::clean_team_abbrs(team)) %>% 
  group_by(team) %>% 
  summarize(total_dc = sum(value, na.rm = T),
            off_dc = sum(value[position %in% off_pos], na.rm = T)) %>% 
  mutate(off_dc_pct = off_dc/total_dc)


fa_table = read_csv(file.path(data_path, "Free_Agents_2025.csv"))

pos <- fa_table %>% select(pos) %>% unique()
pos <- fa_table %>% select(type) %>% unique()

fa_capital <- fa_table %>% 
  # filter(type %in% c("Signed", "Trade", "Franchise", "UFA", "SFA")) %>% 
  filter(team_abbr_24 != team_abbr_25) %>%
  group_by(team_abbr_25) %>% 
  summarize(tot_apy = sum(current_apy, na.rm = T),
            off_apy = sum(current_apy[pos %in% off_pos], na.rm = T)) %>% 
  mutate(off_apy_pct = off_apy/tot_apy,
         team_abbr_25 = nflreadr::clean_team_abbrs(team_abbr_25))

fin_data <- draft_capital %>% left_join(fa_capital, by = c("team" = "team_abbr_25"))

draft_chart <- ggplot(data = fin_data) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team, x = off_dc_pct, y = off_apy_pct), width = 0.04, alpha = 0.8) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=8),
                     labels = scales::percent_format()) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     labels = scales::percent_format()) + 
  theme_FE +
  labs(title = paste0("How did teams spend their resources in the 2025 offseason?"),
       subtitle = "Draft capital measured by Fitzgerald-Spielberger trade value chart | Free agent signings do not include re-signings or extensions",
       caption = "Figure: @SamHoppen | Data: nflreadr, Over the Cap",
       x = "Percent of Draft Capital Spent on Offense",
       y = "Percent of Free Agent APY Signings on Offense") #+
  # theme(panel.grid.major = element_blank(),
  #       plot.title = element_text(size = 12),
  #       plot.subtitle = element_text(size = 6),
  #       plot.caption = element_text(size = 6),
  #       axis.text = element_text(size = 6),
  #       axis.title = element_text(size = 6))


brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/resource_allocation.png"))
