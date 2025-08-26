source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)
library(rvest)
library(stringr)
library(httr2)
dotenv::load_dot_env()
data_path <- Sys.getenv("LOCAL_DATA_PATH")

team_abbr <- nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_nick)

draft_link <- "https://overthecap.com/draft"

draft_node <- draft_link %>%
  read_html() %>%
  html_element("table") %>%
  html_table()

picks_col <- draft_node[c(2:1330),c(1:5)]

colnames(picks_col) <- c('year', 'base', 'bonus', 'cap_num', 'ovr_pick')

picks_df <- picks_col %>%
  mutate(ovr_pick = as.numeric(zoo::na.locf(ovr_pick))) %>%
  filter(!grepl("Total Picks", year),
         ovr_pick <= 32,
         base != "Base Salary") %>%
  group_by(ovr_pick) %>%
  mutate(row = row_number()) %>%
  ungroup() %>%
  filter(row <= 4)

picks_df$cap_value <- as.numeric(gsub("[\\$,]", "", picks_df$cap_num))

first_value <- picks_df %>%
  group_by(ovr_pick) %>%
  summarize(apy = mean(cap_value))


positions <- c('quarterback', 'running-back', 'wide-receiver', 'tight-end',
         'left-tackle', 'left-guard', 'center', 'right-guard', 'right-tackle',
         'interior-defensive-line', 'edge-rusher', 'linebacker', 'cornerback', 'safety')

pos <- positions[1]

all_contracts <- data.frame()

for(pos in positions){
  pos_link <- paste0("https://overthecap.com/position/", pos)
  temp_df <- pos_link %>%
    read_html() %>%
    html_element("table") %>%
    html_table() %>%
    mutate(position = pos,
           pos_rank = row_number())

  all_contracts <- rbind(all_contracts, temp_df)

  print(paste0(pos, " done"))
  Sys.sleep(2)
}


all_contracts$apy <- as.numeric(gsub("[\\$,]", "", all_contracts$`Avg./Year`))

all_contracts <- all_contracts %>%
  mutate(position = case_when(position %in% c('left-guard', 'right-guard')~ "G",
                              position == 'quarterback' ~ "QB",
                              position == 'running-back' ~ "RB",
                              position == "wide-receiver" ~ "WR",
                              position == "tight-end" ~ "TE",
                              position == 'left-tackle' ~ "LT",
                              position == 'right-tackle' ~ "RT",
                              position == 'center' ~ "C",
                              position == 'edge-rusher' ~ "EDGE",
                              position == 'interior-defensive-line' ~ "IDL",
                              position == 'linebacker' ~ "LB",
                              position == 'cornerback' ~ "CB",
                              position == 'safety' ~ "S"),
         starter = case_when(position %in% c("QB", "LT", "RT", "C", "TE", "RB") & pos_rank <= 32 ~ 1,
                             position %in% c("G", "EDGE", "IDL", "S") & pos_rank <= 64 ~ 1,
                             position %in% c("WR", "LB", "CB") & pos_rank <= 92 ~ 1,
                             TRUE ~ 0))

fin_contracts <- all_contracts %>%
  select(position, pos_rank, apy)

final_df <- data.frame()

for(i in c(1:32)){

  
  temp_apy <- first_value %>% filter(ovr_pick == i) %>% .$apy
  print(paste0("APY: ", temp_apy))
  temp_pos_ranks <- all_contracts %>%
    group_by(position) %>%
    summarize(num = max(pos_rank[apy>=temp_apy])+1) %>% 
    ungroup()

  temp_pct_ranks <- all_contracts %>%
    filter(starter == 1) %>%
    group_by(position) %>%
    mutate(apy_pct_rank = percent_rank(apy)) %>%
    ungroup() %>%
    group_by(position) %>%
    summarize(pct = min(apy_pct_rank[apy>=temp_apy], na.rm = T)) %>% 
    ungroup()


  temp_df <- temp_pos_ranks %>% left_join(temp_pct_ranks) %>%
    mutate(ovr_pick = i,
           apy = temp_apy)

  final_df <- rbind(final_df, temp_df)
  print(paste0(i, " done"))
  # Sys.sleep(5)
  

}


fin_df <- final_df %>%
  pivot_wider(names_from = position,
              values_from = c(num, pct),
              names_glue = "{position}_{.value}") %>%
  select(c(ovr_pick, apy, QB_num, QB_pct, RB_num, RB_pct, WR_num, WR_pct, TE_num, TE_pct, RT_num, RT_pct,
           LT_num, LT_pct, G_num, G_pct, C_num, C_pct,
           EDGE_num, EDGE_pct, IDL_num, IDL_pct, LB_num, LB_pct, CB_num, CB_pct, S_num, S_pct))

# write_csv(fin_df, file.path(data_path, "rookie_contract_percentiles.csv"))
# # write_csv(first_value, file.path(data_path, "rookie_contract_percentiles.csv"))
# 
# fin_df <- read_csv(file.path(data_path, "rookie_contract_percentiles.csv"))

# first_value <- read_csv(file.path(data_path, "rookie_salaries_2025.csv")) %>%
#   group_by(ovr_pick) %>%
#   summarize(apy = mean(cap_value))

draft_picks <- read_csv(file.path(data_path, "Draft_Picks_2025.csv")) %>% 
  filter(round == 1) %>% 
  select(ovr_pick = pick_num, team_abbr) %>% 
  # left_join(first_value) %>% 
  left_join(fin_df)

full_val_range <- draft_picks %>% 
  select(-ovr_pick) %>% 
  ungroup %>%
  select(c(QB_pct, RB_pct, WR_pct, TE_pct, RT_pct, LT_pct, G_pct, C_pct,
           EDGE_pct, IDL_pct, LB_pct, CB_pct, S_pct)) %>% 
  range

draft_table <- draft_picks %>% 
  arrange(ovr_pick) %>% 
  gt() %>% 
  tab_header(title = md("**How all of the 1st round draft pick salaries will rank (in the # column) within the current market at each position**"),
             subtitle = paste0("%ile is percentile among starters")) %>% 
  cols_label(ovr_pick = "Pick",
             team_abbr = "Team",
             apy = "Est. APY",
             QB_num = "#",
             QB_pct = "%ile",
             RB_num = "#",
             RB_pct = "%ile",
             WR_num = "#",
             WR_pct = "%ile",
             TE_num = "#",
             TE_pct = "%ile",
             LT_num = "#",
             LT_pct = "%ile",
             RT_num = "#",
             RT_pct = "%ile",
             G_num = "#",
             G_pct = "%ile",
             C_num = "#",
             C_pct = "%ile",
             EDGE_num = "#",
             EDGE_pct = "%ile",
             IDL_num = "#",
             IDL_pct = "%ile",
             LB_num = "#",
             LB_pct = "%ile",
             CB_num = "#",
             CB_pct = "%ile",
             S_num = "#",
             S_pct = "%ile") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  cols_width(everything() ~ px(50)) %>% 
  cols_width(team_abbr ~ 125) %>% 
  cols_width(ends_with("pct") ~ px(60)) %>% 
  cols_width(ends_with("num") ~ px(30)) %>% 
  cols_width(apy ~ px(110)) %>% 
  fmt_currency(columns = c(apy),
               currency = "USD",
               decimals = 0) %>% 
  fmt_percent(columns = c(QB_pct, RB_pct, WR_pct, TE_pct, RT_pct, LT_pct, G_pct, C_pct,
                          EDGE_pct, IDL_pct, LB_pct, CB_pct, S_pct),
              decimals = 1) %>% 
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
        columns = c(QB_num, EDGE_num)
      )
    )
  ) %>% 
  data_color(
    columns = c(QB_pct, RB_pct, WR_pct, TE_pct, RT_pct, LT_pct, G_pct, C_pct,
                EDGE_pct, IDL_pct, LB_pct, CB_pct, S_pct),
    alpha = 0.7,
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::GnBu"
      ) %>% as.character(),
      domain = full_val_range
    )
  ) %>%
  tab_spanner(
    label = html("QB"),
    columns = c(QB_num, QB_pct)
  ) %>%
  tab_spanner(
    label = html("RB"),
    columns = c(RB_num, RB_pct)
  ) %>%
  tab_spanner(
    label = html("WR"),
    columns = c(WR_num, WR_pct)
  ) %>%
  tab_spanner(
    label = html("TE"),
    columns = c(TE_num, TE_pct)
  ) %>%
  tab_spanner(
    label = html("RT"),
    columns = c(RT_num, RT_pct)
  ) %>%
  tab_spanner(
    label = html("LT"),
    columns = c(LT_num, LT_pct)
  ) %>%
  tab_spanner(
    label = html("G"),
    columns = c(G_num, G_pct)
  ) %>%
  tab_spanner(
    label = html("C"),
    columns = c(C_num, C_pct)
  ) %>%
  tab_spanner(
    label = html("EDGE"),
    columns = c(EDGE_num, EDGE_pct)
  ) %>%
  tab_spanner(
    label = html("IDL"),
    columns = c(IDL_num, IDL_pct)
  ) %>%
  tab_spanner(
    label = html("LB"),
    columns = c(LB_num, LB_pct)
  ) %>%
  tab_spanner(
    label = html("CB"),
    columns = c(CB_num, CB_pct)
  ) %>%
  tab_spanner(
    label = html("S"),
    columns = c(S_num, S_pct)
  ) %>% 
  nflplotR::gt_nfl_wordmarks(locations = cells_body(c(team_abbr))) %>% 
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Over The Cap"
  )


# nflplotR::gt_nfl_logos(locations = cells_body(c(team_abbr)))
gtExtras::gtsave_extra(draft_table, filename = paste0("~/Documents/Charts/draft_pick_comp_25_fin.png"),
                       vwidth = 1500)
# gtExtras::gtsave_extra(draft_table, filename = paste0("C:/Users/sphop/OneDrive/FantasyPros/Charts/Draft position compensation 2024_new.png"),
#                        vwidth = 1500)
