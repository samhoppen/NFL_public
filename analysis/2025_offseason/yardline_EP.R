source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')

pbp <- nflfastR::load_pbp(2024)

touchback <- pbp %>% 
  filter(down == 1, ydstogo == 10, half_seconds_remaining>=120) %>% 
  group_by(yardline_100, down, ydstogo) %>% 
  summarize(avg_ep = mean(ep, na.rm = T)) %>% 
  mutate(yardline_100 = 100-yardline_100) %>% 
  filter(yardline_100 <= 50)

touchback <- pbp %>% 
  filter(down == 1, ydstogo == 10, half_seconds_remaining>=120, !is.na(ep)) %>% 
  mutate(yardline_100 = 100-yardline_100) %>% 
  filter(yardline_100 <= 50)

### 40-yard dash charts ###
touchback_chart <- ggplot(data = touchback)+
  geom_jitter(aes(x = yardline_100, y = ep), alpha = 0.05)+
  geom_smooth(aes(x = yardline_100, y = ep))+
  geom_vline(xintercept = 30, linetype = 'dashed', size = 1.5, color = 'purple')+
  geom_vline(xintercept = 35, linetype = 'dashed', color = 'forestgreen', size = 1.5)+
  geom_segment(aes(x = 30, y = 3.05, xend = 35, yend = 3.05),
               arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
               color = "forestgreen",
               size = 1
  ) +
  # geom_line(aes(x = yardline_100, y = avg_ep))+
  theme_FE + 
  labs(title = "A touchback at the 35 would add ~0.2 expected points compared to the 30",
       subtitle = "Expected Points on 1st & 10 outside of 2-min warning in 2024",
       caption = "Figure: @SamHoppen | Data: nflfastR",
       x = "Yardline",
       y = "Expected Points")

brand_nfl_plot(touchback_chart,
               logo = F,
               asp = 16/9,
               save_name = paste0("~/Documents/Charts/yardline_ep.png"))
