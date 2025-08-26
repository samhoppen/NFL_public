source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")


pick_values <- read_csv(file.path(data_path, "Fitzgerald_Spielberger.csv"))

big_boards <- data.frame()

for(szn in c(2016:2025)){
  temp_df <- read_csv(file.path(data_path, paste0("big_board_",szn,".csv"))) %>% 
    select(-std_dev) %>% 
    mutate(season = szn)
  
  big_boards <- bind_rows(big_boards, temp_df)
}

top_100_rbs <- big_boards %>% 
  filter(position == "RB", rank <= 100) %>% 
  left_join(pick_values,
            by = c("rank" = "pick"))

prospect_values <- top_100_rbs %>% 
  group_by(season) %>% 
  summarize(prospect_value = sum(value),
            prospect_count = n())

draft_picks <- nflreadr::load_draft_picks() %>% 
  filter(season >= 2016)

rb_draft_values <- draft_picks %>% 
  left_join(pick_values,
            by = c("pick")) %>% 
  filter(position == 'RB', pick <= 100) %>% 
  group_by(season) %>% 
  summarize(draft_value = sum(value, na.rm = T),
            draft_count = n())

rb_values <- prospect_values %>% 
  left_join(rb_draft_values)%>%
  pivot_longer(
    cols = -season,
    names_to = c("type", "measure"),
    names_pattern = "(.*)_(.*)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>% 
  mutate(type = case_when(type == "prospect" ~ "Implied Prospect Value",
                          type == "draft" ~ "Draft Value"))

### 40-yard dash charts ###
draft_chart <- ggplot(data = rb_values) + 
  geom_line(aes(x = season, y = value, color = type), size = 2)+
  # geom_point(aes(x = season, y = value, color = type, size = count), shape = 15)+
  scale_x_continuous(breaks = c(2016:2025))+
  theme_FE + 
  # theme(panel.grid.major = element_blank()) +
  labs(title = "Measuring the strength of running back classes over the past decade",
       subtitle = "Values based on Fitzgerald-Spielberger trade value chart",
       caption = "Figure: @SamHoppen | Data: nflreadr",
       x = NULL,
       y = "Total Draft Value",
       color = NULL)

brand_nfl_plot(draft_chart,
               logo = F,
               # asp = 1,
               save_name = paste0("~/Documents/Charts/rb_draft_strength.png"))

  