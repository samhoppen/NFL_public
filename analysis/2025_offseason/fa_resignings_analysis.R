source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(tidyverse)

ext_df = read_csv("~/Documents/Data/spotrac_extensions.csv") %>% 
  mutate(player = nflreadr::clean_player_names(player)) %>% 
  unique()

szn <- ext_df %>% 
  group_by(offseason) %>% 
  summarize(count = n())

ft_df = read_csv("~/Documents/Data/spotrac_franchise_tags.csv") %>% 
  mutate(player = nflreadr::clean_player_names(player)) %>% 
  unique()
ufa_df = read_csv("~/Documents/Data/spotrac_ufa.csv") %>% 
  mutate(player = nflreadr::clean_player_names(player)) %>% 
  filter(years != 0) %>% 
  unique()  %>% 
  group_by(player, offseason) %>% 
  mutate(count = row_number()) %>% 
  filter(count == 1)

otc_df = read_csv("~/Documents/Data/otc_fa_contracts.csv") %>% 
  filter(type %in% c('Signed', 'UFA', 'Franchise','Transition')) %>% 
  filter(snaps != '0.0%')


fa_df <- otc_df %>% 
  mutate(player = nflreadr::clean_player_names(player)) %>% 
  left_join(ufa_df,
            by = c("player", "offseason"))

missing <- fa_df %>% filter(is.na(count) & new_team!=old_team)
dupes <- fa_df %>% 
  group_by(player, offseason) %>% 
  mutate(count = n()) %>% 
  filter(count >= 2)

otc_df %>% select(type) %>% unique()

void = otc_df %>% filter(type == 'Void')
