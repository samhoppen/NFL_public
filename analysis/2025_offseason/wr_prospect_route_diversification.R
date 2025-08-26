source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')

big_board <- read_csv("~/Documents/Data/big_board_2025.csv") %>% 
  mutate(player = paste0(first_name, " ", last_name),
         player = nflreadr::clean_player_names(player))

wr_route_locations <- read_csv("~/Documents/Data/WRLocationDiversification.csv")
location_diversification_career <- wr_route_locations %>%
  filter((position == "WR" | player == "Travis Hunter"), Routes >= 50) %>%
  mutate(other_routes = Routes-(RWR_Routes+RslotWR_Routes+LWR_Routes+LslotWR_Routes),
         # other_routes = if_else(other_routes == 0, 0.000001,other_routes),
         Routes = (RWR_Routes+RslotWR_Routes+LWR_Routes+LslotWR_Routes),
         RWR_Routes = RWR_Routes/Routes,
         RslotWR_Routes = RslotWR_Routes/Routes,
         LWR_Routes = LWR_Routes/Routes,
         LslotWR_Routes = LslotWR_Routes/Routes,
         RWR_Routes = if_else(RWR_Routes == 0, 0.000001,RWR_Routes),
         RslotWR_Routes = if_else(RslotWR_Routes == 0, 0.000001,RslotWR_Routes),
         LWR_Routes = if_else(LWR_Routes == 0, 0.000001,LWR_Routes),
         LslotWR_Routes = if_else(LslotWR_Routes == 0, 0.000001,LslotWR_Routes),
         shannon_entropy = -(RWR_Routes*log(RWR_Routes, 2))-(RslotWR_Routes*log(RslotWR_Routes, 2))-(LWR_Routes*log(LWR_Routes, 2))-(LslotWR_Routes*log(LslotWR_Routes, 2)),
         shannon_pct = (shannon_entropy-min(shannon_entropy))/(max(shannon_entropy)-min(shannon_entropy)),
         player = nflreadr::clean_player_names(player))

wr_route_locations <- read_csv("~/Documents/Data/WRLocationDiversificationYearly.csv")
location_diversification_final <- wr_route_locations %>% 
  filter((position == "WR" | player == "Travis Hunter"), Routes >= 50) %>% 
  group_by(playerId) %>%
  filter(season == max(season)) %>%
  ungroup() %>%
  mutate(other_routes = Routes-(RWR_Routes+RslotWR_Routes+LWR_Routes+LslotWR_Routes),
         other_routes = if_else(other_routes == 0, 0.000001,other_routes),
         Routes = (RWR_Routes+RslotWR_Routes+LWR_Routes+LslotWR_Routes),
         RWR_Routes = RWR_Routes/Routes,
         RslotWR_Routes = RslotWR_Routes/Routes,
         LWR_Routes = LWR_Routes/Routes,
         LslotWR_Routes = LslotWR_Routes/Routes,
         RWR_Routes = if_else(RWR_Routes == 0, 0.000001,RWR_Routes),
         RslotWR_Routes = if_else(RslotWR_Routes == 0, 0.000001,RslotWR_Routes),
         LWR_Routes = if_else(LWR_Routes == 0, 0.000001,LWR_Routes),
         LslotWR_Routes = if_else(LslotWR_Routes == 0, 0.000001,LslotWR_Routes),
         shannon_entropy = -(RWR_Routes*log(RWR_Routes, 2))-(RslotWR_Routes*log(RslotWR_Routes, 2))-(LWR_Routes*log(LWR_Routes, 2))-(LslotWR_Routes*log(LslotWR_Routes, 2)),
         shannon_pct = (shannon_entropy-min(shannon_entropy))/(max(shannon_entropy)-min(shannon_entropy)),
         player = nflreadr::clean_player_names(player))

location_diversification <- location_diversification_career %>%
  select(player, playerId, Routes, LWR_Routes, LslotWR_Routes, RslotWR_Routes, RWR_Routes, shannon_entropy, shannon_pct) %>%
  left_join(location_diversification_final %>% select(player, playerId, Routes, LWR_Routes, LslotWR_Routes, RslotWR_Routes, RWR_Routes, shannon_entropy, shannon_pct) ,
            by = c("player", "playerId"),
            suffix = c("_car", "_fy"))

fin_location_data <- big_board %>% 
  filter((position == "WR" | player == "Travis Hunter"), rank <= 250) %>% 
  select(player, rank, headshot, school) %>% 
  left_join(location_diversification,
            by = c("player" = "player"))


location_table <- fin_location_data %>%
  arrange(rank) %>%
  select(-Routes_car, -Routes_fy, -playerId, -shannon_entropy_car, -shannon_entropy_fy) %>% 
  gt() %>%
  tab_header(title = md("**Calculating wide receiver prospect location diversification using Shannon Entropy**"),
             subtitle = paste0("Percentiles calculated among college WR seasons since 2019")) %>%
  cols_label(rank = "Rank",
             player = "Prospect",
             headshot = "",
             school = "School",
             RWR_Routes_car = "Right %",
             RslotWR_Routes_car = html("Right<br>Slot %"),
             LWR_Routes_car = "Left %",
             LslotWR_Routes_car = html("Left<br>Slot %"),
             shannon_pct_car = html("Entropy<br>%ile"),
             RWR_Routes_fy = "Right %",
             RslotWR_Routes_fy = html("Right<br>Slot %"),
             LWR_Routes_fy = "Left %",
             LslotWR_Routes_fy = html("Left<br>Slot %"),
             shannon_pct_fy = html("Entropy<br>%ile")) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_move(columns = c(player, headshot, school),
            after = c(rank)) %>%
  cols_width(everything() ~ px(75)) %>%
  cols_width(c(rank) ~ px(60)) %>%
  cols_width(c(player) ~ px(140)) %>%
  cols_hide(columns = c(headshot)) %>% 
  fmt_percent(columns = c(RWR_Routes_car, RslotWR_Routes_car, LslotWR_Routes_car, LWR_Routes_car, shannon_pct_car,
                          RWR_Routes_fy, RslotWR_Routes_fy, LslotWR_Routes_fy, LWR_Routes_fy, shannon_pct_fy),
              decimals = 1) %>%
  tab_options(data_row.padding = px(2)) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(LWR_Routes_car, LWR_Routes_fy)
      )
    )
  ) %>%
  data_color(
    columns = c(shannon_pct_car, shannon_pct_fy),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::GnBu"
      ) %>% as.character(),
      domain = c(0,1)
    )
  ) %>%
  tab_spanner(
    label = html("Career Alignment Frequencies"),
    columns = c(LWR_Routes_car, LslotWR_Routes_car, RslotWR_Routes_car, RWR_Routes_car, shannon_pct_car)
  ) %>%
  tab_spanner(
    label = html("Final Season Alignment Frequencies"),
    columns = c(LWR_Routes_fy, LslotWR_Routes_fy, RslotWR_Routes_fy, RWR_Routes_fy, shannon_pct_fy)
  ) %>%
  text_transform(locations = cells_body(c(school)),
                 fn = function(x) web_image(url = paste0(x),
                                            height = 25)) %>%
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Trumedia"
  ) %>%
  tab_footnote(
    locations = cells_column_labels(c(rank)),
    footnote = "Consensus Big Board rank from https://jacklich10.com/bigboard/nfl/"
  )


# nflplotR::gt_nfl_logos(locations = cells_body(c(team_abbr)))
gtExtras::gtsave_extra(location_table, filename = paste0("~/Documents/Charts/wr_location_diversification.png"),
                       vwidth = 1075)


wr_route_frequencies <- read_csv("~/Documents/Data/RouteFrequencyCareer.csv")
route_diversification_career <- wr_route_frequencies %>%
  filter((position == "WR" | player == "Travis Hunter"), Routes >= 50) %>%
  mutate(wr_screen_pct = if_else(wr_screen_pct == 0, 0.000001, wr_screen_pct),
         cross_pct = if_else(cross_pct == 0, 0.000001, cross_pct),
         # flare_pct = if_else(flare_pct == 0, 0.000001, flare_pct),
         slant_pct = if_else(slant_pct == 0, 0.000001, slant_pct),
         in_pct = if_else(in_pct == 0, 0.000001, in_pct),
         out_pct = if_else(out_pct == 0, 0.000001, out_pct),
         hitch_pct = if_else(hitch_pct == 0, 0.000001, hitch_pct),
         corner_pct = if_else(corner_pct == 0, 0.000001, corner_pct),
         post_pct = if_else(post_pct == 0, 0.000001, post_pct),
         go_pct = if_else(go_pct == 0, 0.000001, go_pct),
         shannon_entropy = -(wr_screen_pct*log(wr_screen_pct, 2))-(cross_pct*log(cross_pct, 2))-(slant_pct*log(slant_pct, 2))-
           (in_pct*log(in_pct, 2))-(out_pct*log(out_pct, 2))-(hitch_pct*log(hitch_pct, 2))-(corner_pct*log(corner_pct, 2))-(post_pct*log(post_pct, 2))-(go_pct*log(go_pct, 2)),
         shannon_pct = (shannon_entropy-min(shannon_entropy))/(max(shannon_entropy)-min(shannon_entropy)),
         player = nflreadr::clean_player_names(player))

wr_route_frequencies <- read_csv("~/Documents/Data/RouteFrequencySzn.csv")
route_diversification_final <- wr_route_frequencies %>% 
  filter((position == "WR" | player == "Travis Hunter"), Routes >= 50) %>% 
  group_by(playerId) %>%
  filter(season == max(season)) %>%
  ungroup()%>%
  mutate(wr_screen_pct = if_else(wr_screen_pct == 0, 0.000001, wr_screen_pct),
         cross_pct = if_else(cross_pct == 0, 0.000001, cross_pct),
         # flare_pct = if_else(flare_pct == 0, 0.000001, flare_pct),
         slant_pct = if_else(slant_pct == 0, 0.000001, slant_pct),
         in_pct = if_else(in_pct == 0, 0.000001, in_pct),
         out_pct = if_else(out_pct == 0, 0.000001, out_pct),
         hitch_pct = if_else(hitch_pct == 0, 0.000001, hitch_pct),
         corner_pct = if_else(corner_pct == 0, 0.000001, corner_pct),
         post_pct = if_else(post_pct == 0, 0.000001, post_pct),
         go_pct = if_else(go_pct == 0, 0.000001, go_pct),
         shannon_entropy = -(wr_screen_pct*log(wr_screen_pct, 2))-(cross_pct*log(cross_pct, 2))-(slant_pct*log(slant_pct, 2))-
           (in_pct*log(in_pct, 2))-(out_pct*log(out_pct, 2))-(hitch_pct*log(hitch_pct, 2))-(corner_pct*log(corner_pct, 2))-(post_pct*log(post_pct, 2))-(go_pct*log(go_pct, 2)),
         shannon_pct = (shannon_entropy-min(shannon_entropy))/(max(shannon_entropy)-min(shannon_entropy)),
         player = nflreadr::clean_player_names(player))

route_diversification <- route_diversification_career %>%
  select(player, playerId, wr_screen_pct, cross_pct, slant_pct, in_pct, out_pct, hitch_pct, corner_pct, post_pct, go_pct, shannon_entropy, shannon_pct) %>%
  left_join(route_diversification_final %>% select(player, playerId, wr_screen_pct, cross_pct, slant_pct, in_pct, out_pct, hitch_pct, corner_pct, post_pct, go_pct, shannon_entropy, shannon_pct) ,
            by = c("player", "playerId"),
            suffix = c("_car", "_fy"))

fin_route_data <- big_board %>% 
  filter((position == "WR" | player == "Travis Hunter"), rank <= 250) %>% 
  select(player, rank, headshot, school) %>% 
  left_join(route_diversification,
            by = c("player" = "player"))


route_table <- fin_route_data %>%
  arrange(rank) %>%
  select(-playerId, -shannon_entropy_car, -shannon_entropy_fy, - headshot) %>% 
  gt() %>%
  tab_header(title = md("**Calculating wide receiver prospect route type diversification using Shannon Entropy**"),
             subtitle = paste0("Percentiles calculated among college WR seasons since 2019")) %>%
  cols_label(rank = "Rank",
             player = "Prospect",
             school = "School",
             wr_screen_pct_car = "WR Screen",
             cross_pct_car = "Cross",
             # flare_pct_car = "Flare",
             slant_pct_car = "Slant",
             in_pct_car = "In",
             out_pct_car = "Out",
             hitch_pct_car = "Hitch",
             corner_pct_car = "Corner",
             post_pct_car = "Post",
             go_pct_car = "Go",
             shannon_pct_car = html("Entropy<br>%ile"),
             wr_screen_pct_fy = "WR Screen",
             cross_pct_fy = "Cross",
             # flare_pct_fy = "Flare",
             slant_pct_fy = "Slant",
             in_pct_fy = "In",
             out_pct_fy = "Out",
             hitch_pct_fy = "Hitch",
             corner_pct_fy = "Corner",
             post_pct_fy = "Post",
             go_pct_fy = "Go",
             shannon_pct_fy = html("Entropy<br>%ile")) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_move(columns = c(player, school),
            after = c(rank)) %>%
  cols_width(everything() ~ px(60)) %>%
  cols_width(c(school) ~ px(75)) %>% 
  cols_width(c(player) ~ px(140)) %>%
  fmt_percent(columns = c(wr_screen_pct_car, cross_pct_car, slant_pct_car, in_pct_car, out_pct_car, hitch_pct_car, corner_pct_car, post_pct_car, go_pct_car, shannon_pct_car,
                          wr_screen_pct_fy, cross_pct_fy, slant_pct_fy, in_pct_fy, out_pct_fy, hitch_pct_fy, corner_pct_fy, post_pct_fy, go_pct_fy, shannon_pct_fy),
              decimals = 1) %>%
  tab_options(data_row.padding = px(2)) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "black",
        weight = px(3)
      )
    ),
    locations = list(
      cells_body(
        columns = c(wr_screen_pct_car, wr_screen_pct_fy)
      )
    )
  ) %>%
  data_color(
    columns = c(shannon_pct_car, shannon_pct_fy),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::GnBu"
      ) %>% as.character(),
      domain = c(0,1)
    )
  ) %>%
  tab_spanner(
    label = html("Career Route Type Frequencies"),
    columns = c(wr_screen_pct_car, cross_pct_car, slant_pct_car, in_pct_car, out_pct_car, hitch_pct_car, corner_pct_car, post_pct_car, go_pct_car, shannon_pct_car)
  ) %>%
  tab_spanner(
    label = html("Final Season Route Type Frequencies"),
    columns = c(wr_screen_pct_fy, cross_pct_fy, slant_pct_fy, in_pct_fy, out_pct_fy, hitch_pct_fy, corner_pct_fy, post_pct_fy, go_pct_fy, shannon_pct_fy)
  ) %>%
  text_transform(locations = cells_body(c(school)),
                 fn = function(x) web_image(url = paste0(x),
                                            height = 25)) %>%
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Trumedia"
  ) %>%
  tab_footnote(
    locations = cells_column_labels(c(rank)),
    footnote = "Consensus Big Board rank from https://jacklich10.com/bigboard/nfl/"
  )


# nflplotR::gt_nfl_logos(locations = cells_body(c(team_abbr)))
gtExtras::gtsave_extra(route_table, filename = paste0("~/Documents/Charts/wr_route_diversification.png"),
                       vwidth = 1525)
