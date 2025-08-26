source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)
library(rvest)
library(stringr)
library(httr2)

# draft_link <- "https://overthecap.com/rookie-classes-by-apy/2016"
# 
# draft_node <- draft_link %>%
#   read_html() %>%
#   # html_element("table") %>%
#   html_table()
# 
# tot_value <- draft_node %>% pluck(1)

team_abbr_df <- nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LAR"))) %>% 
  select(team_abbr, team_nick)

value_data <- data.frame()

for(szn in c(2011:2020)){
  draft_link <- paste0("https://overthecap.com/rookie-classes-by-apy/",szn)
  
  draft_node <- draft_link %>%
    read_html() %>%
    html_table()
  
  tot_value <- draft_node %>% pluck(1) %>% select(tot_rank = Rank, team_name = Team, total_value = Index)
  relative_value <- draft_node %>% pluck(2) %>% select(relative_rank = Rank, team_name = Team, relative_value = Index)
  
  team_value <- tot_value %>% left_join(relative_value) %>% mutate(season = szn)
  
  value_data <- rbind(value_data, team_value)
  
  print(paste0(szn, " done"))
  Sys.sleep(5)
}

value_data_fin <- value_data %>% 
  left_join(team_abbr_df,
            by = c("team_name" = "team_nick"))
  
value_data_fin$team_abbr <- factor(value_data_fin$team_abbr, levels = nfl_teams)

draft_chart <- ggplot(data = value_data_fin) + 
  geom_line(aes(x = season, y = relative_value, color = team_abbr), alpha = 0.7, linewidth = 0.5, linetype = '22') +
  geom_line(aes(x = season, y = total_value, color = team_abbr), alpha = 0.7, linewidth = 0.5) +
  scale_x_continuous(breaks = c(2011:2020))+
  nflplotR::scale_color_nfl(type = "primary") +
  facet_wrap(. ~ team_abbr, nrow = 4) +
  theme_FE + 
  labs(title = paste0("How teams have drafted in terms of total value (solid) and relative value (dashed)"),
       subtitle = "Using Over the Cap's rookie class by APY methodology",
       caption = "Figure: @SamHoppen | Data: Over the Cap",
       y = "Draft Value",
       x = "Season") +
  theme(axis.text.x = element_text(size = 4, angle = 90, vjust = 0.5), 
        axis.text.y = element_text(size = 4),
        panel.spacing.y = unit(0.5, 'lines'),
        legend.position = "none",
        strip.text = nflplotR::element_nfl_wordmark(),
        panel.grid.major.x = element_blank())


brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/draft_value_otc_chart.png"))
