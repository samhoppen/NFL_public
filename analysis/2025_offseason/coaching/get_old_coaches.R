library(tidyverse)
library(dplyr)
library(rvest)
library(httr2)

coaches <- read_csv("~/Documents/Data/yearly_coaching_history.csv")

data.frame(
  abv = c(
    'crd',
    'atl',
    'rav',
    'buf',
    'car',
    'chi',
    'cin',
    'cle',
    'dal',
    'den',
    'det',
    'gnb',
    'htx',
    'clt',
    'jax',
    'kan',
    'rai',
    'sdg',
    'ram',
    'mia',
    'min',
    'nwe',
    'nor',
    'nyg',
    'nyj',
    'phi',
    'pit',
    'sfo',
    'sea',
    'tam',
    'oti',
    'was'
  ),
  name = c(
    "Arizona Cardinals",
    "Atlanta Falcons",
    "Baltimore Ravens",
    "Buffalo Bills",
    "Carolina Panthers",
    "Chicago Bears",
    "Cincinnati Bengals",
    "Cleveland Browns",
    "Dallas Cowboys",
    "Denver Broncos",
    "Detroit Lions",
    "Green Bay Packers",
    "Houston Texans",
    "Indianapolis Colts",
    "Jacksonville Jaguars",
    "Kansas City Chiefs",
    "Las Vegas Raiders",
    "Los Angeles Chargers",
    "Los Angeles Rams",
    "Miami Dolphins",
    "Minnesota Vikings",
    "New England Patriots",
    "New Orleans Saints",
    "New York Giants",
    "New York Jets",
    "Philadelphia Eagles",
    "Pittsburgh Steelers",
    "San Francisco 49ers",
    "Seattle Seahawks",
    "Tampa Bay Buccaneers",
    "Tennessee Titans",
    "Washington Commanders"
  )
) -> team_cross




get_PFR_coaches <- function(team = 'Cincinnati Bengals', season = 2022) {
  Sys.sleep(2)
  message(paste("Scraping", season, team, "coaches"))
  team_data <- paste0(
    'https://www.pro-football-reference.com/teams/',
    team_cross$abv[team_cross$name == team],
    '/',
    season,
    '.htm'
  ) %>%
    read_html() %>%
    html_nodes('p')
  
  team_year_df <- data.frame()
  
  item_count = length(team_data)
  off_scheme <- ""
  def_scheme <- ""
  
  for(i in c(1:item_count)){
    # print(i)
    title = team_data[[i]] %>% html_nodes("strong") %>% html_text()
    
    
    
    if(length(title) > 0 && any(grepl("Coach", title, ignore.case = TRUE) |
                                grepl("Asst", title, ignore.case = TRUE) |
                                grepl("Coordinator", title, ignore.case = TRUE))){
      temp_list <- list(type = team_data[[i]] %>% html_nodes("strong") %>% html_text(),
                        coach = team_data[[i]] %>% html_nodes("a") %>% html_text(),
                        coach_id = team_data[[i]] %>% html_nodes("a") %>% html_attr("href"))
      
      temp_df <- data.frame(temp_list)
      
      team_year_df <- bind_rows(team_year_df, temp_df)
    }
    if(length(title) > 0 && any(grepl("Scheme", title, ignore.case = TRUE))){
      off_scheme <- team_data[[i]] %>% html_text()
    }
    
    if(length(title) > 0 && any(grepl("Alignment", title, ignore.case = TRUE))){
      def_scheme <- team_data[[i]] %>% html_text()
    }
    
  }
  
  team_year_df <- team_year_df %>% 
    mutate(team_name = team,
           season = season,
           type = gsub(":", "", type),
           coach_id = gsub(".htm", "", gsub("/coaches/", "", coach_id)),
           off_scheme = gsub("Offensive Scheme: ", "", off_scheme),
           def_scheme = gsub("Defensive Alignment: ", "", def_scheme))
  
  return(team_year_df)
}

new_coaches <- data.frame()

for(i in c(2024:2025)){
  map2_dfr(
    .y = rep(i, 32),
    .x = rep(team_cross$name) %>% sort,
    .f = possibly(get_PFR_coaches, otherwise = tibble::tibble())
  ) -> temp_coaches
  
  new_coaches <- bind_rows(new_coaches, temp_coaches)
}

fin_coaches <- rbind(coaches, new_coaches) %>% 
  unique()

write_csv(fin_coaches, "~/Documents/Data/yearly_coaching_history.csv")

coaches <- read_csv("~/Documents/Data/yearly_coaching_history.csv") %>%
  mutate(coach_id = gsub("/executives/", "", coach_id))

all_coaches <- coaches %>% 
  filter(season >= 2024) %>%
  select(coach_id) %>% unique() %>% pluck(1)

get_coaching_history <- function(coach_id) {
  Sys.sleep(2)
  message(paste("Scraping", coach_id, "history"))
  coach_link <- paste0("https://www.pro-football-reference.com/coaches/",coach_id,".htm") %>%
    request() %>%
    req_retry(max_tries = 3) %>%
    req_user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:107.0) Gecko/20100101 Firefox/107.0') %>%
    req_perform() %>%
    resp_body_html()
  
  coach_html <- coach_link %>%
    gsub("<!--", "", .) %>%
    gsub("-->", "", .) %>%
    read_html()
  
  coaching_history <- coach_html %>%
    html_element('#all_coaching_history')
  
  if(length(coaching_history)>0){
    history_table <- coaching_history %>% 
      html_table() %>% 
      mutate(coach_id = coach_id)
    
  }else{
    history_table = data.frame()
  }
  
  worked_for <- coach_html %>%
    html_element('#all_worked_for')
  
  if(length(worked_for)>0){
    worked_for_table <- worked_for %>% 
      html_table() %>% 
      mutate(type = "Worked For",
             coach_id = coach_id)
    
  }
  
  employed <- coach_html %>%
    html_element('#all_employed')
  
  if(length(employed)>0){
    employed_table <- employed %>% 
      html_table() %>% 
      mutate(type = "Employed",
             coach_id = coach_id)
    
  }
  
  if(length(worked_for)>0 & length(employed)>0){
    coach_tree <- bind_rows(worked_for_table, employed_table)
  }else if(length(worked_for)>0){
    coach_tree <- worked_for_table
  }else if(length(employed)>0){
    coach_tree <- employed_table
  }
  
  
  
  return(list(coaching_history = history_table, coaching_tree = coach_tree))
}

temp_df <- get_coaching_history("MoraJi1")
tree = temp_df$coaching_tree
coaching_history <- data.frame()
coaching_trees <- data.frame()


for(coach in all_coaches){
  temp_df <- get_coaching_history(coach)
  
  coaching_history <- bind_rows(coaching_history, temp_df$coaching_history)
  coaching_trees <- bind_rows(coaching_trees, temp_df$coaching_tree)
}

all_coaching_history <- read_csv("~/Documents/Data/coaching_histories.csv")
all_coaching_trees <- read_csv("~/Documents/Data/coaching_trees.csv")

# coaches_completed <- all_coaching_history %>% select(coach_id) %>% unique() %>% pluck(1)
# 
# all_coaches <- coaches %>% filter(!(coach_id %in% coaches_completed)) %>% select(coach_id) %>% unique() %>% pluck(1)

fin_coaching_history <- rbind(all_coaching_history, coaching_history) %>% unique()
fin_coaching_trees <- rbind(all_coaching_trees, coaching_trees) %>% unique()

write_csv(fin_coaching_history, "~/Documents/Data/coaching_histories.csv")
write_csv(fin_coaching_trees, "~/Documents/Data/coaching_trees.csv")

coach_ids <- coaches %>% 
  mutate(coach_id = gsub("/executives/", "", coach_id)) %>% 
  select(coach, coach_id) %>% 
  unique() %>% 
  group_by(coach) %>% 
  mutate(dupes = n())
