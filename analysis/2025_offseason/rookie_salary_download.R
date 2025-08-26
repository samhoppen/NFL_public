source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)
library(rvest)
library(stringr)
library(httr2)

team_abbr <- nflfastR::teams_colors_logos %>% 
  filter(!(team_abbr %in% c("STL", "OAK", "SD", "LA"))) %>% 
  select(team_abbr, team_nick)

draft_link <- "https://overthecap.com/draft"

draft_node <- draft_link %>%
  read_html() %>%
  # html_element("table") %>%
  html_table()

# test <- draft_node %>% pluck(2)

all_picks <- c(1, 33, 65, 103, 139, 177, 217, 
               2, 34, 66, 104, 140, 178, 218, 
               3, 35, 67, 105, 141, 179, 219, 
               4, 36, 68, 106, 142, 180, 220, 
               5, 37, 69, 107, 143, 181, 221, 
               6, 38, 70, 108, 144, 182, 222, 
               7, 39, 71, 109, 145, 183, 223, 
               8, 40, 72, 110, 146, 184, 224, 
               9, 41, 73, 111, 147, 185, 225, 
               10, 42, 74, 112, 148, 186, 226, 
               11, 43, 75, 113, 187, 227, 
               12, 44, 76, 114, 149, 188, 228, 
               13, 45, 77, 115, 150, 189, 229, 
               14, 46, 78, 116, 151, 190, 230, 
               15, 47, 79, 117, 191, 231, 
               16, 48, 80, 118, 152, 192, 232, 
               17, 49, 81, 119, 153, 193, 233, 
               18, 50, 82, 120, 154, 194, 234, 
               19, 51, 83, 121, 155, 195, 235, 
               20, 52, 84, 122, 156, 196, 236, 
               21, 53, 85, 123, 157, 197, 237, 
               22, 54, 86, 124, 158, 198, 238, 
               23, 55, 87, 125, 159, 199, 239, 
               24, 56, 88, 126, 160, 200, 240, 
               25, 57, 89, 127, 161, 201, 241, 
               26, 58, 90, 128, 162, 202, 242, 
               27, 59, 91, 129, 163, 203, 243, 
               28, 60, 92, 130, 164, 204, 244, 
               29, 61, 93, 131, 165, 205, 245, 
               30, 62, 94, 132, 166, 206, 246, 
               31, 63, 95, 133, 167, 207, 247, 
               32, 64, 96, 134, 168, 208, 248, 
               97, 135, 169, 209, 249, 
               98, 136, 170, 210, 250, 
               99, 137, 171, 211, 251, 
               100, 138, 172, 212, 252, 
               101, 173, 213, 253, 
               102, 174, 214, 254, 
               175, 215, 255, 
               176, 216, 256, 
               257)


# all_picks[[4]]
picks_df <- data.frame()

for(i in c(2:258)){
  
  pick_num <- all_picks[[i-1]]
  
  temp_salary <- draft_node %>% pluck(i) %>% 
    dplyr::mutate(ovr_pick = pick_num,
                  node_num = i-1,
                  row = row_number())
  
  colnames(temp_salary) <- c('year', 'base', 'bonus', 'cap_num', 'ovr_pick', 'node_num', 'row')
  
  picks_df <- bind_rows(picks_df, temp_salary)
  
}



picks_df$cap_value <- as.numeric(gsub("[\\$,]", "", picks_df$cap_num))
picks_df$base <- as.numeric(gsub("[\\$,]", "", picks_df$base))
picks_df$bonus <- as.numeric(gsub("[\\$,]", "", picks_df$bonus))
picks_df$cap_num <- as.numeric(gsub("[\\$,]", "", picks_df$cap_num))

write_csv(picks_df, "~/Documents/Data/rookie_salaries_2025.csv")

first_value <- picks_df %>% 
  group_by(ovr_pick) %>% 
  summarize(apy = mean(cap_value))

positions <- c('quarterback', 'running-back', 'wide-receiver', 'tight-end',
               'left-tackle', 'left-guard', 'center', 'right-guard', 'right-tackle',
               'interior-defensive-line', 'edge-rusher', 'linebacker', 'cornerback', 'safety')

# pos <- positions[1]

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
all_contracts$total_value <- as.numeric(gsub("[\\$,]", "", all_contracts$`Total Value`))
all_contracts$total_guaranteed <- as.numeric(gsub("[\\$,]", "", all_contracts$`Total Guaranteed`))
all_contracts$fully_guaranteed <- as.numeric(gsub("[\\$,]", "", all_contracts$`Fully Guaranteed`))

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

write_csv(all_contracts, "~/Documents/Data/latest_contracts.csv")

fin_contracts <- all_contracts %>% 
  select(position, pos_rank, apy)

final_df <- data.frame()

for(i in c(1:257)){
  
  temp_apy <- first_value %>% filter(ovr_pick == i) %>% .$apy
  
  temp_pos_ranks <- all_contracts %>% 
    group_by(position) %>% 
    summarize(num = max(pos_rank[apy>=temp_apy]))
  
  temp_pct_ranks <- all_contracts %>% 
    filter(starter == 1) %>% 
    group_by(position) %>% 
    mutate(apy_pct_rank = percent_rank(apy)) %>%
    ungroup() %>% 
    group_by(position) %>% 
    summarize(pct = min(apy_pct_rank[apy>=temp_apy], na.rm = T))
  
  
  temp_df <- temp_pos_ranks %>% left_join(temp_pct_ranks) %>% 
    mutate(ovr_pick = i)
  
  final_df <- rbind(final_df, temp_df)
  print(paste0(i, " done"))
  
}


fin_df <- final_df %>% 
  left_join(first_value) %>% 
  pivot_wider(names_from = position,
              values_from = c(num, pct),
              names_glue = "{position}_{.value}") %>% 
  select(c(ovr_pick, apy, QB_num, QB_pct, RB_num, RB_pct, WR_num, WR_pct, TE_num, TE_pct, RT_num, RT_pct,
           LT_num, LT_pct, G_num, G_pct, C_num, C_pct,
           EDGE_num, EDGE_pct, IDL_num, IDL_pct, LB_num, LB_pct, CB_num, CB_pct, S_num, S_pct))

write_csv(fin_df, "~/Documents/Data/rookie_contract_percentiles.csv")

# write_csv(picks_df %>% select(-cap_value, -row, -node_num), "~/Documents/Data/rookie_contract_values.csv")

