source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyr)
library(tidyverse)
library(dplyr)
library(datapasta)
library(ggplot2)
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")

# tribble_paste()

future_picks <- tibble::tribble(
                                                                                      ~round, ~future_pick,
                                                                                          1L,          48L,
                                                                                          2L,          80L,
                                                                                          3L,         112L,
                                                                                          4L,         144L,
                                                                                          5L,         176L,
                                                                                          6L,         208L,
                                                                                          7L,         240L
                                                                                      )

team_abbr <- nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "SD", "OAK", "LAR"))) %>% 
  select(team_nick, team_abbr)

execs <- read_csv(file.path(data_path, "nfl_executives.csv"))

execs_yearly <- read_csv(file.path(data_path, "nfl_executives_yearly.csv"))

drafters <- execs_yearly %>% 
  left_join(execs %>% select(name, team, from, to, position, position_group)) %>% 
  filter(between(year, from, to)) %>% 
  mutate(job_tenure = to-from,
         first_season = if_else(from == year, 1, 0),
         final_season = if_else(to == year, 1, 0)) %>%
  filter(draft_control == 1) %>%
  left_join(team_abbr,
            by = c("team" = "team_nick"))

pick_values <- read_csv(file.path(data_path, "draft_pick_values.csv"))

trades = nflreadr::load_trades()

# trades_old <- read_csv(url("https://raw.githubusercontent.com/arjunmenon10/NFL-Graphs/main/draftpickstrades.csv"))

players <- nflreadr::load_draft_picks() %>% 
  select(pfr_id = pfr_player_id, position) %>% 
  filter(!is.na(pfr_id)) %>% 
  unique()

fin_trades <- trades %>% 
  filter(season >= 2000) %>% 
  group_by(trade_id, season) %>% 
  mutate(players_involved = sum(is.na(pick_season)),
         trade_up_team = if_else(pick_number == min(pick_number, na.rm = T) & pick_season == min(pick_season, na.rm = T), 1, 0)) %>% 
  ungroup() %>% 
  group_by(trade_id, season, received) %>% 
  mutate(trade_up_team = sum(trade_up_team)) %>% 
  ungroup() %>% 
  filter(players_involved == 0) %>% 
  left_join(players) %>% 
  group_by(trade_id, season, received) %>% 
  mutate(future_years = pick_season - season,
         qbs_drafted = sum(position == "QB", na.rm = T),
         new_pick_round = pick_round+future_years-1) %>% 
  ungroup() %>% 
  # filter(trade_id == 458) %>% 
  mutate(qb_trade = if_else(qbs_drafted == 1 & trade_up_team == 1, "QB Trade", "Non-QB Trade")) %>% 
  # ungroup() %>% 
  left_join(future_picks,
            by = c("new_pick_round" = "round")) %>% 
  mutate(fin_pick_number = if_else(future_years > 0, future_pick, pick_number))
  

trade_values <- fin_trades %>% 
  left_join(pick_values,
            by = c("fin_pick_number" = "ovr_pick")) %>% 
  mutate(jimmy_johnson_new = jimmy_johnson*(0.5^future_years),
         fitzgerald_spielberger_new = fitzgerald_spielberger*(0.5^future_years),
         rich_hill_new = rich_hill*(0.5^future_years),
         baldwin_new = baldwin*(0.5^future_years)) %>% 
  group_by(trade_id, trade_up_team, qb_trade, season, received) %>% 
  summarize(jj_value_received = sum(jimmy_johnson_new),
            fs_value_received = sum(fitzgerald_spielberger_new),
            rh_value_received = sum(rich_hill_new),
            bald_value_received = sum(baldwin_new)) %>% 
  ungroup() %>% 
  group_by(trade_id, season) %>% 
  mutate(jj_value_traded = if_else(row_number() == 1, jj_value_received[2], jj_value_received[1]),
         fs_value_traded = if_else(row_number() == 1, fs_value_received[2], fs_value_received[1]),
         rh_value_traded = if_else(row_number() == 1, rh_value_received[2], rh_value_received[1]),
         bald_value_traded = if_else(row_number() == 1, bald_value_received[2], bald_value_received[1])) %>% 
  ungroup() %>% 
  mutate(jj_value_diff = jj_value_received-jj_value_traded,
         jj_value_diff_pct = jj_value_diff/jj_value_traded,
         fs_value_diff = fs_value_received-fs_value_traded,
         fs_value_diff_pct = fs_value_diff/fs_value_traded,
         rh_value_diff = rh_value_received-rh_value_traded,
         rh_value_diff_pct = rh_value_diff/rh_value_traded,
         bald_value_diff = bald_value_received-bald_value_traded,
         bald_value_diff_pct = bald_value_diff/bald_value_traded)

trade_eval <- trade_values %>% 
  select(-c(jj_value_received, fs_value_received, rh_value_received, bald_value_received,
            jj_value_traded, fs_value_traded, rh_value_traded, bald_value_traded)) %>% 
  left_join(drafters,
            by = c("season" = "year",
                   "received" = "team_abbr")) %>% 
  mutate(#final_season = as.character(final_season),
         #first_season = as.character(first_season),
         season_type = case_when(first_season == 1 ~ "First Year",
                                 final_season == 1 & first_season !=1 ~ "Final Year",
                                 TRUE ~ "During Tenure"),
         trade_type = case_when(trade_up_team == 1 ~ "Traded Up",
                                TRUE ~ "Traded Down")) %>% 
  filter(job_tenure >= 3)

fs_data <- trade_eval %>% filter(season >= 2011)

fs_data$trade_type <- factor(fs_data$trade_type, levels = c("Traded Down", "Traded Up"))
fs_data$season_type <- factor(fs_data$season_type, levels = c("First Year", "During Tenure", "Final Year"))


draft_chart <- ggplot(data = fs_data) +
  geom_density(aes(x = fs_value_diff, fill = qb_trade),
               alpha = 0.7)+
  facet_grid(season_type~trade_type)+
  # scale_y_continuous(labels = scales::percent_format(accuracy=1),
  #                    expand = expansion(mult = c(0,0.05))) + 
  theme_FE +
  labs(title = paste0("GMs consistently overpay to trade up in the draft"),
       subtitle = "Draft-pick exclusive trades since 2011 | Only considers GMs with tenure of at least 3 years | Year N+1 picks valued as year N Round+1 middle pick value",
       caption = "Figure: @SamHoppen | Data: @nflreadr",
       x = "Fitzgerald-Spielberger Draft Value Difference",
       y = "Density",
       fill = "Trade-up Type") +
  theme(panel.grid.minor = element_blank(),
        legend.title.align = 0.5)

brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/GM trade diff FS.png"))

fs_grouping <- fs_data %>% 
  group_by(season_type, trade_type) %>% 
  summarize(count = n(),
            median_diff = median(fs_value_diff, na.rm = T),
            # avg_diff = mean(fs_value_diff, na.rm = T)
  ) %>% 
  pivot_wider(names_from = trade_type,
              values_from = c(count, median_diff))

full_val_range <- fs_grouping %>% 
  ungroup %>%
  select(c(`median_diff_Traded Down`, `median_diff_Traded Up`)) %>% 
  range


draft_table <- fs_grouping %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = md("**Trade tendencies of GMs in their<br>first and final seasons**"),
             subtitle = paste0("Draft-pick trades since 2011")) %>% 
  cols_label(season_type = "GM Season",
             `count_Traded Down` = "# Trades",
             `count_Traded Up` = "# Trades",
             `median_diff_Traded Down` = "Median Diff",
             `median_diff_Traded Up` = "Median Diff") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(100)) %>% 
  data_color(
    columns = c(`median_diff_Traded Down`, `median_diff_Traded Up`),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = full_val_range
    )
  ) %>%
  tab_spanner(
    label = html("Traded Up"),
    columns = c(`count_Traded Up`, `median_diff_Traded Up`)
  ) %>%
  tab_spanner(
    label = html("Traded Down"),
    columns = c(`count_Traded Down`, `median_diff_Traded Down`)
  ) %>%
  tab_footnote(
    locations = cells_column_labels(c(`median_diff_Traded Down`, `median_diff_Traded Up`)),
    footnote = "Using Fitzgerald-Spielberger trade value chart"
  ) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: nflreadR"
  )

gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/FS GM Trade Eval.png"),
                       vwidth = 550)

jj_data <- trade_eval %>% filter(season >= 2011,
                                 between(jj_value_diff, -400, 400))

jj_data$trade_type <- factor(jj_data$trade_type, levels = c("Traded Down", "Traded Up"))
jj_data$season_type <- factor(jj_data$season_type, levels = c("First Year", "During Tenure", "Final Year"))

min(jj_data$season)
draft_chart <- ggplot(data = jj_data) +
  geom_density(aes(x = jj_value_diff, fill = qb_trade),
               alpha = 0.7)+
  facet_grid(season_type~trade_type)+
  # scale_y_continuous(labels = scales::percent_format(accuracy=1),
  #                    expand = expansion(mult = c(0,0.05))) + 
  theme_FE +
  labs(title = paste0("When using the Jimmy Johnson trade chart, trade differences appear more even"),
       subtitle = "Draft-pick exclusive trades since 2011 | Only considers GMs with tenure of at least 3 years | Year N+1 picks valued as year N Round+1 middle pick value",
       caption = "Figure: @SamHoppen | Data: @nflreadr",
       x = "Jimmy Johnson Draft Value Difference",
       y = "Density",
       fill = "Trade-up Type") +
  theme(panel.grid.minor = element_blank(),
        legend.title.align = 0.5)

brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/GM trade diff JJ.png"))

jj_grouping <- jj_data %>% 
  group_by(season_type, trade_type) %>% 
  summarize(count = n(),
            median_diff = median(jj_value_diff, na.rm = T),
            # avg_diff = mean(fs_value_diff, na.rm = T)
  ) %>% 
  pivot_wider(names_from = trade_type,
              values_from = c(count, median_diff))

full_val_range <- jj_grouping %>% 
  ungroup %>%
  select(c(`median_diff_Traded Down`, `median_diff_Traded Up`)) %>% 
  range


draft_table <- jj_grouping %>% 
  ungroup() %>% 
  gt() %>% 
  tab_header(title = md("**Trade tendencies of GMs in their<br>first and final seasons**"),
             subtitle = paste0("Draft-pick trades since 2011")) %>% 
  cols_label(season_type = "GM Season",
             `count_Traded Down` = "# Trades",
             `count_Traded Up` = "# Trades",
             `median_diff_Traded Down` = "Median Diff",
             `median_diff_Traded Up` = "Median Diff") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(100)) %>% 
  data_color(
    columns = c(`median_diff_Traded Down`, `median_diff_Traded Up`),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::PRGn"
      ) %>% as.character(),
      domain = full_val_range
    )
  ) %>%
  tab_spanner(
    label = html("Traded Up"),
    columns = c(`count_Traded Up`, `median_diff_Traded Up`)
  ) %>%
  tab_spanner(
    label = html("Traded Down"),
    columns = c(`count_Traded Down`, `median_diff_Traded Down`)
  ) %>%
  tab_footnote(
    locations = cells_column_labels(c(`median_diff_Traded Down`, `median_diff_Traded Up`)),
    footnote = "Using Jimmy Johnson trade value chart"
  ) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: nflreadR"
  )

gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/JJ GM Trade Eval.png"),
                       vwidth = 550)


