library(tidyverse)
library(rvest)
library(stringr)
library(httr2)

years <- c(2015:2025)

ufa_df = data.frame()
ext_df = data.frame()
ft_df = data.frame()

otc_df = data.frame()

for(yr in years){
  print(paste0("Collecting UFAs for ", yr))
  
  ufa_link <- paste0("https://www.spotrac.com/nfl/free-agents/_/year/",yr,"/type/ufa/sort/contract_value")
  
  ufa_table <- ufa_link %>%
    read_html() %>%
    html_element("table") %>%
    html_table()
  
  ufa_table <- ufa_table[,c(2:7)]
  
  ufa_col_names <- c('player', 'pos', 'years', 'value', 'apy', 'guaranteed')
  colnames(ufa_table) <- ufa_col_names
  
  ufa_df <- bind_rows(ufa_df, ufa_table %>% mutate(offseason = yr))
  
  print(paste0("Collecting extensions for ", yr))
  
  ext_link <- paste0("https://www.spotrac.com/nfl/contracts/extensions/_/year/",yr,"/type/pending-free-agent-extension/sort/value")
  
  ext_table <- ext_link %>%
    read_html() %>%
    html_element("table") %>%
    html_table()
  
  ext_table <- ext_table[,c(2:7, 11)]
  
  ext_col_names <- c('player', 'pos', 'team', 'age', 'years', 'value', 'apy', 'contract_type')
  colnames(ext_table) <- ext_col_names
  
  ext_df <- bind_rows(ext_df, ext_table %>% mutate(offseason = yr))
  
  print(paste0("Collecting franchise tags for ", yr))
  
  ft_link <- paste0("https://www.spotrac.com/nfl/contracts/franchise-tag/_/year/",yr,"/sort/value")
  
  ft_table <- ft_link %>%
    read_html() %>%
    html_element("table") %>%
    html_table()
  
  ft_table <- ft_table[,c(1:5,7:8)]
  
  ft_col_names <- c('player', 'pos', 'team', 'age', 'length', 'value', 'decision')
  colnames(ft_table) <- ext_col_names
  
  ft_df <- bind_rows(ft_df, ft_table %>% mutate(offseason = yr))
  
  print(paste0("Collecting OTC data for ", yr))
  resp <- request("https://overthecap.com/wp-admin/admin-ajax.php") %>% 
    req_body_form(
      action = "get_free_agents",
      season = yr) %>%
    req_perform()
  
  otc_table <- resp %>% resp_body_string() %>% 
    str_glue(
      paste0(
        '<table class="controls-table" id="table',yr,'" cellspacing="0" align="center">
    <thead>
        <tr>
            <th class="sortable">Player</th>
            <th class="sortable sorttable_numeric">Pos.</th>
            <th class="sortable">',yr,' Team</th>
            <th class="sortable">',yr+1,' Team</th>
            <th class="sortable">Type</th>
            <th class="sortable">Snaps</th>
            <th class="sortable">Age</th>
            <th class="sortable">Current APY</th>
            <th class="sortable mobile_drop">Guarantees</th>
        </tr>
    </thead>
    <tbody>{.}</tbody>')) %>% 
    # turn it into valid html
    minimal_html() %>% 
    html_element("table") %>% 
    html_table()
  
  otc_names <- c('player', 'pos', 'old_team', 'new_team', 'type', 'snaps', 'age', 'current_apy', 'guarantees')
  
  colnames(otc_table) <- otc_names
  
  otc_df <- bind_rows(otc_df, otc_table %>% mutate(offseason = yr))
  print(paste0(yr, " done"))
  Sys.sleep(3)
}


write_csv(ext_df, "~/Documents/Data/spotrac_extensions.csv")
write_csv(ft_df, "~/Documents/Data/spotrac_franchise_tags.csv")
write_csv(ufa_df, "~/Documents/Data/spotrac_ufa.csv")
write_csv(otc_df, "~/Documents/Data/otc_fa_contracts.csv")

yr=2024

ufa_link <- paste0("https://www.spotrac.com/nfl/free-agents/_/year/",yr,"/type/ufa/sort/contract_value")

ufa_table <- ufa_link %>%
  read_html() %>%
  # html_element("table") %>%
  html_table() %>% 
  pluck(2)


ufa_table <- ufa_link %>%
  read_html() %>%
  # html_element("table") %>%
  html_table() %>% 
  pluck(2)
