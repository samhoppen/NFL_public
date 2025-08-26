source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')

week_num <- 18

pbp <- nflfastR::load_pbp(2024)

off_pbp <- pbp %>% 
  filter(week <= 18) %>% 
  filter(!is.na(epa), !is.na(posteam)) %>% 
  filter(pass == 1 | rush == 1) %>% 
  filter(!is.na(down)) %>% 
  arrange(posteam, week, play_id) %>% 
  group_by(posteam) %>% 
  mutate(play_count = 1,
         plays = n(),
         play_num = cumsum(play_count),
         cum_epa = cumsum(epa))

off_pbp$posteam <- factor(off_pbp$posteam, levels = nfl_teams)

def_pbp <- pbp %>% 
  filter(week <= 18) %>% 
  filter(!is.na(epa), !is.na(defteam)) %>% 
  filter(pass == 1 | rush == 1) %>% 
  filter(!is.na(down)) %>% 
  arrange(defteam, week, play_id) %>% 
  group_by(defteam) %>% 
  mutate(epa = epa*-1,
         play_count = 1,
         plays = n(),
         play_num = cumsum(play_count),
         cum_epa = cumsum(epa)) %>% 
  select(-posteam) %>% 
  dplyr::rename(posteam = defteam)

def_pbp$posteam <- factor(def_pbp$posteam, levels = nfl_teams)


cov_chart <- ggplot() + 
  geom_line(data = off_pbp, aes(x = play_num, y = cum_epa), color = "forestgreen", alpha = 0.6)+
  geom_line(data = def_pbp, aes(x = play_num, y = cum_epa), color = "blue", alpha = 0.6)+
  # geom_line(data = st_pbp, aes(x = play_num, y = cum_epa), color = "orange")+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_continuous(expand = expansion(mult = c(0,0))) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  facet_wrap(. ~ posteam, nrow = 4) +
  nflplotR::scale_color_nfl(type = "primary") +
  theme_FE + 
  labs(title = paste0("Cumulative EPA throughout the 2024 regular season"),
       subtitle = "<span style = 'color: forestgreen;'>Green=OFF </span>|<span style = 'color: blue;'>Blue=DEF</span>", #
       caption = "Figure: @SamHoppen | Data: @nlflfastR",
       y = "Cumulative EPA",
       x = "Play Number") +
  theme(axis.text.x = element_text(size = 4), 
        axis.text.y = element_text(size = 4),
        panel.spacing.y = unit(0.5, 'lines'),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        strip.text = element_nfl_wordmark(),
        legend.key.size = unit(0.25, 'cm'),
        plot.subtitle = ggtext::element_markdown())


brand_nfl_plot(cov_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/cumulative_epa_24.png"))
