source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')

drafts <- nflreadr::load_draft_picks() %>% 
  # filter(team != "PHO") %>%
  mutate(team = nflreadr::clean_team_abbrs(team),
         team = case_when(team == 'HOU' & season < 2002 ~ "TEN",
                          team == "PHO" ~ "ARI",
                          TRUE ~ team))

# old_roster_positions <- read_csv("~/Documents/Data/pfr_final_player_positions.csv") %>% 
#   mutate(player = nflreadr::clean_player_names(player))
# 
# new_pos <- load_rosters(seasons = 2024) %>% 
#   filter(!is.na(pfr_id)) %>% 
#   mutate(player = nflreadr::clean_player_names(full_name)) %>% 
#   select(player, pfr_id, fin_pos = position)
# 
# roster_positions <- old_roster_positions %>% 
#   rbind(new_pos) %>% 
#   group_by(pfr_id) %>% 
#   filter(row_number() == 1) %>% 
#   ungroup()
# 
pos <- drafts %>% select(position) %>% unique()

fin_drafts <- drafts %>% 
  mutate(position = case_when(position %in% c("DT", "DE", "NT") ~ "DL",
                              position %in% c("LB", "OLB", "ILB") ~ "LB",
                              position %in% c("T", "C", "G", "OL", "OT", "OG") ~ "OL",
                              position %in% c("DB", "CB", "S", "SAF", "FS") ~ "DB",
                              TRUE ~ position)) %>% 
  # mutate(position = case_when(fin_pos == "S" & position == "DB" ~ "S",
  #                             fin_pos == "S" & position == "SAF" ~ "S",
  #                             position == "SAF" | position == "FS" ~ "S",
  #                             position == "OG" ~ "OL",
  #                             position == "OL" & fin_pos %in% c("C", "G") ~ "OL",
  #                             position == "OL" & fin_pos == "OT" ~ "OL",
  #                             position == "DB" ~ "CB",
  #                             position %in% c("ILB", "OLB") ~ "LB",
  #                             position %in% c("C", "G", "OL") ~ "OL",
  #                             position %in% c("DT", "NT") ~ "DL",
  #                             position == "DL" ~ "DL",
  #                             position == "T" ~ "OL",
  #                             position == "DE" ~ "DL",
  #                             TRUE ~ position)) %>% 
  filter(!(position %in% c("P", "K", "FB", "LS", "KR")))

pos <- fin_drafts %>% select(position) %>% unique()

pos_table <- fin_drafts %>% 
  filter(round <= 1) %>% 
  group_by(team, position) %>% 
  summarize(last_drafted = max(season, na.rm = T)) %>% 
  pivot_wider(names_from = position, values_from = last_drafted) %>%
  select(Team = team, QB, RB, WR, TE, OL, DL, LB, DB)
  
draft_table <- pos_table %>% 
  ungroup() %>% 
  arrange(Team) %>% 
  gt() %>% 
  tab_header(title = md("**The last time each team drafted a ___ in the first round**"),
             subtitle = paste0("Drafts since 1980 | Based on position listed in the draft on Pro-Football-Reference")) %>% 
  # cols_label(team_abbr = "Team") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(60)) %>% 
  cols_width(Team ~ 125) %>% 
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
        columns = c(QB, DL)
      )
    )
  ) %>% 
  data_color(
    columns = c(QB, RB, WR, TE, OL, DL, LB, DB),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_spanner(
    label = html("Offense"),
    columns = c(QB, RB, WR, TE, OL)
  ) %>%
  tab_spanner(
    label = html("Defense"),
    columns = c(DL, LB, DB)
  ) %>%
  nflplotR::gt_nfl_wordmarks(locations = cells_body(c(Team))) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: nflreadr"
  )

#
# Note: these are based on PFR's position designations, some of which were generalized so I made some assumptions on what they played if I didn't have their official NFL position in my database. 
gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/first_round_pos_picks.png"),
                       vwidth = 650)

pos_table <- fin_drafts %>% 
  filter(round <= 2) %>% 
  group_by(team, position) %>% 
  summarize(last_drafted = max(season, na.rm = T)) %>% 
  pivot_wider(names_from = position, values_from = last_drafted) %>%
  select(Team = team, QB, RB, WR, TE, OL, DL, LB, DB)

draft_table <- pos_table %>% 
  ungroup() %>% 
  arrange(Team) %>% 
  gt() %>% 
  tab_header(title = md("**The last time each team drafted a ___ in the first two rounds**"),
             subtitle = paste0("Drafts since 1980 | Based on position listed in the draft on Pro-Football-Reference")) %>% 
  # cols_label(team_abbr = "Team") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(60)) %>% 
  cols_width(Team ~ 125) %>% 
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
        columns = c(QB, DL)
      )
    )
  ) %>% 
  data_color(
    columns = c(QB, RB, WR, TE, OL, DL, LB, DB),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_spanner(
    label = html("Offense"),
    columns = c(QB, RB, WR, TE, OL)
  ) %>%
  tab_spanner(
    label = html("Defense"),
    columns = c(DL, LB, DB)
  ) %>%
  nflplotR::gt_nfl_wordmarks(locations = cells_body(c(Team))) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: nflreadr"
  )

#
# Note: these are based on PFR's position designations, some of which were generalized so I made some assumptions on what they played if I didn't have their official NFL position in my database. 
gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/second_round_pos_picks.png"),
                       vwidth = 650)


pos_table <- fin_drafts %>% 
  filter(round <= 3) %>% 
  group_by(team, position) %>% 
  summarize(last_drafted = max(season, na.rm = T)) %>% 
  pivot_wider(names_from = position, values_from = last_drafted) %>%
  select(Team = team, QB, RB, WR, TE, iOL, OT, DE, iDL, LB, CB, S)

draft_table <- pos_table %>% 
  ungroup() %>% 
  arrange(Team) %>% 
  gt() %>% 
  tab_header(title = md("**The last time each team drafted a ___ in the first three rounds**"),
             subtitle = paste0("Drafts since 1980 only | PFR position designation changes: OL = iOL | DL = DE | DB = CB")) %>% 
  # cols_label(team_abbr = "Team") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(50)) %>% 
  cols_width(Team ~ 125) %>% 
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
        columns = c(QB, DE)
      )
    )
  ) %>% 
  data_color(
    columns = c(QB, RB, WR, TE, iOL, OT, DE, iDL, LB, CB, S),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::YlGn"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  tab_spanner(
    label = html("Offense"),
    columns = c(QB, RB, WR, TE, iOL, OT)
  ) %>%
  tab_spanner(
    label = html("Defense"),
    columns = c(DE, iDL, LB, CB, S)
  ) %>%
  nflplotR::gt_nfl_wordmarks(locations = cells_body(c(Team))) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: nflreadr"
  )

#
# Note: these are based on PFR's position designations, some of which were generalized so I made some assumptions on what they played if I didn't have their official NFL position in my database. 
gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/third_round_pos_picks.png"),
                       vwidth = 710)
