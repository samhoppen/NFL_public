source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(dplyr)
library(tidyverse)
library(nflreadr)

combine_2025 <- read_csv("~/Documents/Data/combine_2025.csv")

combine_2025$drills_participated <- 6-rowSums(is.na(combine_2025[ , 7:12]))

sum(combine_2025$drills_participated>=3)

forty_times <- nflreadr::load_combine() %>% 
  rbind(combine_2025, fill = T) %>% 
  filter(season >= 2009) %>% 
  filter(!(pos %in% c("K", "P", "LS", 'FB'))) %>% 
  mutate(pos = case_when(pos %in% c('S', 'SAF', 'DB', 'CB') ~ 'DB',
                         pos %in% c('DE', 'EDGE') ~ 'DE',
                         pos %in% c('DT', 'DL') ~ 'iDL',
                         pos %in% c("OG", "G", "OL", "OT", "C") ~ "OL",
                         pos %in% c("ILB", "LB", "OLB") ~ "LB",
                         TRUE ~pos)) %>% 
  group_by(season, pos) %>% 
  mutate(szn_pos_count = n())


head(forty_times %>% select(season, player_name, pos, forty))
# pos <- forty_times %>% 
#   select(pos) %>% unique()

### 40-yard dash charts ###
forty_chart <- ggplot(data = forty_times %>% filter(!is.na(forty)), aes(x = forty, y = ..scaled..)) + 
  geom_density_ridges_gradient(aes(y = as.character(season), fill = stat(x)),
                               calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 2,
                               scale = 1.25)+
  scale_fill_viridis_c(option = "C") +  # Use a continuous scale
  theme_FE + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0),
        panel.grid.major = element_blank()) +
  labs(title = "40-yard dash times since 2009",
       subtitle = "Vertical line indicates median time for that season",
       caption = "Figure: @SamHoppen | Data: Pro-Football-Reference",
       x = "40-yard Dash Times",
       y = NULL) +
  coord_cartesian(xlim = c(4.2, 6))

brand_nfl_plot(forty_chart,
               logo = F,
               asp = 1,
               save_name = paste0("~/Documents/Charts/forty_times_total.png"))

forty_times$pos <- factor(forty_times$pos,
                          levels = c('QB', 'RB', 'WR', 'TE', 'OL', 'DE', 'iDL', 'LB', 'DB'))

forty_chart <- ggplot(data = forty_times %>% filter(!is.na(forty), !is.na(pos)), aes(x = forty, y = ..scaled..)) + 
  geom_density_ridges_gradient(aes(y = as.character(season), fill = stat(x)),
                               calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 2,
                               scale = 1.25)+
  scale_fill_viridis_c(option = "C") +
  facet_wrap(.~pos,
             nrow = 3,
             scales = "free_x")+
  theme_FE + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0, size = 4),
        axis.text.x = element_text(size = 8),
        panel.grid.major = element_blank()) +
  labs(title = "40-yard dash times since 2009",
       subtitle = "Vertical line indicates median time for that season",
       caption = "Figure: @SamHoppen | Data: Pro-Football-Reference",
       x = "40-yard Dash Times",
       y = NULL)

brand_nfl_plot(forty_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/forty_times_position.png"))

forty_chart <- ggplot(data = forty_times %>% filter(!is.na(forty), !is.na(pos))) + 
  geom_jitter(aes(x = season, y = forty), alpha = 0.1)+
  geom_smooth(aes(x = season, y = forty))+
  # scale_fill_viridis_c(option = "C") +
  facet_wrap(.~pos,
             nrow = 3,
             scales = "free_y")+
  theme_FE + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0, size = 6),
        axis.text.x = element_text(size = 8),
        panel.grid.major.x = element_blank()) +
  labs(title = "40-yard dash times since 2009, split by position",
       # subtitle = "Vertical line indicates median time for that season",
       caption = "Figure: @SamHoppen | Data: Pro-Football-Reference",
       x = "Season",
       y = "40-yard Dash Times")

brand_nfl_plot(forty_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/forty_times_position.png"))

szn_pos = forty_times %>% 
  group_by(season, pos) %>% 
  summarize(count = n(),
            med_time = median(forty, na.rm = T),
            avg_time = mean(forty, na.rm = T)) %>% 
  arrange(desc(season))

szn_tot = forty_times %>% 
  group_by(season) %>% 
  summarize(med_time = median(forty, na.rm = T),
            avg_time = mean(forty, na.rm = T)) %>% 
  arrange(desc(season))

forty_table <- szn_tot %>% 
  gt() %>% 
  tab_header(title = html("<strong>How 40-yard dash times<br>have changed at the combine</strong>")) %>% 
  cols_label(med_time = "Median Time",
             avg_time = "Average Time",
             season = "Season") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  fmt_number(columns = c(med_time, avg_time),
             decimals = 2) %>% 
  data_color(
    columns = c(med_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  data_color(
    columns = c(avg_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  # tab_spanner(
  #   label = html("Players"),
  #   columns = c(`Josh Allen`, `Lamar Jackson`, `Saquon Barkley`, `Patrick Mahomes`,
  #               `Jared Goff`, `Joe Burrow`, `Jayden Daniels`, `Baker Mayfield`, 
  #               `Ja'Marr Chase`, `Justin Herbert`, `Sam Darnold`)
  # ) %>%
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Pro-Football-Reference"
  )

gtExtras::gtsave_extra(forty_table, filename = paste0("~/Documents/Charts/forty_times_table.png"),
                       vwidth = 250)


# Define the current year
current_year <- 2025  # Update this to the actual year of interest

# Split the dataset
this_year <- forty_times %>% filter(season == current_year) %>% pull(forty)
past_years <- forty_times %>% filter(between(season, 2022, 2024)) %>% pull(forty)

# Check normality assumption
shapiro.test(this_year)  # If p-value < 0.05, data is not normal
shapiro.test(past_years)

# # Perform two-sample t-test if normal
# t_test_result <- t.test(this_year, past_years, var.equal = FALSE)  # Welch’s t-test
# print(t_test_result)

# If non-normal, use Mann-Whitney U test
wilcox_test_result <- wilcox.test(this_year, past_years)
print(wilcox_test_result)

# Check normality within groups
forty_times %>%
  group_by(season) %>%
  summarise(p_value = shapiro.test(forty)$p.value)

# Perform ANOVA
anova_result <- aov(forty ~ as.factor(season), data = forty_times)
summary(anova_result)

# If ANOVA is significant, perform Tukey’s post-hoc test
tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

# If normality assumption is violated, use Kruskal-Wallis test
kruskal_test_result <- kruskal.test(forty ~ as.factor(season), data = forty_times)
print(kruskal_test_result)

### cone/shuttle charts ###
cone_chart <- ggplot(data = forty_times %>% filter(!is.na(cone)), aes(x = cone, y = ..scaled..)) + 
  geom_density_ridges_gradient(aes(y = as.character(season), fill = stat(x)),
                               calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 2,
                               scale = 1.25)+
  scale_fill_viridis_c(option = "C") +  # Use a continuous scale
  theme_FE + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0),
        panel.grid.major = element_blank()) +
  labs(title = "3-cone drill times since 2009",
       subtitle = "Vertical line indicates median time for that season",
       caption = "Figure: @SamHoppen | Data: Pro-Football-Reference",
       x = "40-yard Dash Times",
       y = NULL)

brand_nfl_plot(cone_chart,
               logo = F,
               asp = 1,
               save_name = paste0("~/Documents/Charts/cone_times_total.png"))

shuttle_chart <- ggplot(data = forty_times %>% filter(!is.na(shuttle)), aes(x = shuttle, y = ..scaled..)) + 
  geom_density_ridges_gradient(aes(y = as.character(season), fill = stat(x)),
                               calc_ecdf = TRUE, quantile_lines = TRUE, quantiles = 2,
                               scale = 1.25)+
  scale_fill_viridis_c(option = "C") +  # Use a continuous scale
  theme_FE + 
  theme(legend.position = "none",
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(vjust = 0),
        panel.grid.major = element_blank()) +
  labs(title = "Shuttle drill times since 2009",
       subtitle = "Vertical line indicates median time for that season",
       caption = "Figure: @SamHoppen | Data: Pro-Football-Reference",
       x = "40-yard Dash Times",
       y = NULL)

brand_nfl_plot(shuttle_chart,
               logo = F,
               asp = 1,
               save_name = paste0("~/Documents/Charts/shuttle_times_total.png"))

szn_tot_oth = forty_times %>% 
  group_by(season) %>% 
  summarize(med_cone_time = median(cone, na.rm = T),
            avg_cone_time = mean(cone, na.rm = T),
            med_shuttle_time = median(shuttle, na.rm = T),
            avg_shuttle_time = mean(shuttle, na.rm = T)) %>% 
  arrange(desc(season))

agility_table <- szn_tot_oth %>% 
  gt() %>% 
  tab_header(title = html("<strong>How 3-cone drill and shuttle<br>times have changed at the combine</strong>")) %>% 
  cols_label(med_shuttle_time = "Median",
             avg_shuttle_time = "Average",
             med_cone_time = "Median",
             avg_cone_time = "Average",
             season = "Season") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  fmt_number(columns = c(med_shuttle_time, avg_shuttle_time, med_cone_time, avg_cone_time),
             decimals = 2) %>% 
  data_color(
    columns = c(med_shuttle_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  data_color(
    columns = c(avg_shuttle_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  data_color(
    columns = c(med_cone_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  data_color(
    columns = c(avg_cone_time),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = NULL,
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  tab_spanner(
    label = html("3-cone drill times"),
    columns = c(avg_cone_time, med_cone_time)
  ) %>%
  tab_spanner(
    label = html("Shuttle drill times"),
    columns = c(avg_shuttle_time, med_shuttle_time)
  ) %>%
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Pro-Football-Reference"
  )

gtExtras::gtsave_extra(agility_table, filename = paste0("~/Documents/Charts/agility_times_table.png"),
                       vwidth = 500)


agility_ct = forty_times %>% 
  group_by(season, pos) %>% 
  summarize(ct = sum(!is.na(shuttle)),
            ct_cone = sum(!is.na(cone))) %>% 
  arrange(desc(season))

