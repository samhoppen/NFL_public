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

picks_col <- draft_node[,1]

draft_picks <- picks_col %>%
  filter(grepl("Value", `Click a team to highlight picks`)) %>%
  rename(pick_info = `Click a team to highlight picks`) %>%
  separate_rows(pick_info, sep = "Total Picks") %>% 
  separate_rows(pick_info, sep = " - ") %>% 
  separate(pick_info, sep = "\\)", into = c(NA, "round"), remove = F) %>% 
  mutate(pick_num = gsub("#", "", gsub( " .*$", "", pick_info)),
         round = lag(round),
         team = if_else(pick_num == pick_info, pick_num, NA)) %>%
  fill(team) %>% 
  filter(!is.na(round), round != "") %>% 
  mutate(round = as.integer(substr(round, 6,7)),
         pick_num = as.integer(pick_num),
         team = str_trim(team)) %>% 
  left_join(team_abbr,
            by = c("team" = "team_nick")) %>% 
  select(-pick_info)

write_csv(draft_picks, file.path(data_path, "Draft_Picks_2025.csv"))


cap_link <- "https://overthecap.com/salary-cap-space"

cap_table <- cap_link %>% 
  read_html() %>% 
  html_element("table") %>% 
  html_table() %>% 
  left_join(team_abbr,
            by = c("Team" = "team_nick")) %>% 
  mutate(`Cap Space` = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Cap Space`))),
         `Effective Cap Space` = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Effective Cap Space`))),
         `Active Cap Spending` = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Active Cap Spending`))),
         `Dead Money` = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", `Dead Money`)))) %>% 
  select(team_abbr, cap_space = `Cap Space`, eff_cap_space = `Effective Cap Space`, 
         active_cap_spending = `Active Cap Spending`, dead_money = `Dead Money`)

write_csv(cap_table, file.path(data_path, "Cap_Space_2025.csv"))

# make a POST request with action=get_free_agents&season=2023 in form data
resp <- request("https://overthecap.com/wp-admin/admin-ajax.php") %>% 
  req_body_form(
    action = "get_free_agents",
    season = 2025) %>%
  req_perform()

# check response content
# resp %>% resp_body_string() %>% str_trunc(80)
#> [1] "\t\t\t\t<tr class=\"sortable\" data-old-team=\"TB\" data-new-team=\"\" data-position=\"Q..."

# response includes table rows
# fit those into table template from https://overthecap.com/free-agency source,
# "{.}" in "<tbody>{.}</tbody>" corresponds to resp_body_string() output
fa_table <- resp %>% resp_body_string() %>% 
  str_glue(
    '<table class="controls-table" id="table2025" cellspacing="0" align="center">
    <thead>
        <tr>
            <th class="sortable">Player</th>
            <th class="sortable sorttable_numeric">Pos.</th>
            <th class="sortable">2024 Team</th>
            <th class="sortable">2025 Team</th>
            <th class="sortable">Type</th>
            <th class="sortable">Snaps</th>
            <th class="sortable">Age</th>
            <th class="sortable">Current APY</th>
            <th class="sortable mobile_drop">Guarantees</th>
        </tr>
    </thead>
    <tbody>{.}</tbody>') %>% 
  # turn it into valid html
  minimal_html() %>% 
  html_element("table") %>% 
  html_table() %>% 
  select(player = Player, pos = `Pos.`, team_2024 = `2024 Team`, team_2025 = `2025 Team`,
         type = Type, snaps = Snaps, age = Age, current_apy = `Current APY`, guarantees = Guarantees) %>% 
  left_join(team_abbr,
            by = c("team_2024" = "team_nick")) %>% 
  rename(team_abbr_24 = team_abbr) %>% 
  left_join(team_abbr,
            by = c("team_2025" = "team_nick")) %>% 
  rename(team_abbr_25 = team_abbr) %>% 
  mutate(current_apy = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", current_apy))),
         guarantees = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", current_apy))),
         snaps = as.numeric(gsub("%","", snaps))/100)

write_csv(fa_table, file.path(data_path, "Free_Agents_2025.csv"))


# url <- "https://www.pff.com/api/nfl/grades?league=nfl&position=QB&season=2022"
# cookie <- '_sharedID=547ec3e4-97dc-4a46-ab70-6631e128ee60; _sharedID_cst=zix7LPQsHA%3D%3D; _ga=GA1.1.1911505820.1738795457; _cc_id=e92f48eeba0e07d8e87eab644d497a17; panoramaId_expiry=1739400257270; panoramaId=14478aa2bad8550ea6f65c47bca416d539383f01a3a46797ce91a8e46dfda37b; panoramaIdType=panoIndiv; _ScCbts=%5B%22570%3Bchrome.2%3A2%3A5%22%5D; FPID=FPID2.2.G%2BIcad3C23B7hJX10VuozYzV7P4Sd1E1QhUY0yLPhTc%3D.1738795457; FPAU=1.2.99632523.1738795457; _gtmeec=e30%3D; _fbp=fb.1.1738795457233.1851222021; _sharedid=c63e73db-9c8c-4b0c-8a88-ed47db4f4016; _sharedid_cst=zix7LPQsHA%3D%3D; cto_bundle=C_7buF8xeHdWRE5kM3d1cHhESjlYTGxSaSUyRndWVW54d3BtTkQwbkFBVmhYNFBrZTVEVUhlM2lVZiUyQmd1S0RvYXFDQ1I1RnlEcWpXekVYMFZUWGRrY3RvNVRKWjlpVlhjc1RUJTJGaGZRY3AlMkZzSGd6RFlyQlhJOXk5ZSUyRlR4dXprSW52dmxwJTJCNEo4d2hzUGZPTk9xMDZOZllyM21NSSUyQkJpeGx3Z3pVakFLeWtqeXF1elI1RSUzRA; cto_bidid=jNUvcF9iQ2Q4anRwbkpRVm8lMkZIMVZ0aEV4OTM1Qkd0UHRxWE1sZTJqejlrSE1jMmtqdiUyQkJoNnNnOWJXaUdsSyUyQmdZRXhiMlBRNUtUQkdIMm9IMnlmWGRuRGM5d1M1aWlUeWVuV0VqYTJJMXlpeDJTbyUzRA; _scid=kmIjKCbf6WGChxutPdny6JL376QLVkYP-MEPJw; c_groot_access_token=bi5FJTJ0f5u6dPYM7GQNnoPDeyBub9O6jhBsLyF8JIKiuKTFAXCqmwHdrEVIut32; c_groot_access_ts=2025-02-12T13:42:17Z; c_groot_refresh_token=op8LKni0GKZBsae0NHipEwrv8gdHZmJ0NpptNQ-gRlCE3-HLHZBsKdreYaZAFYUt; _hp2_ses_props.2100373990=%7B%22ts%22%3A1739367737735%2C%22d%22%3A%22www.pff.com%22%2C%22h%22%3A%22%2F%22%7D; FPLC=K21cF0C%2FUIrr2HXl1lL6N%2FuHQ0Ra%2Bqds9CLqe%2BzgFHsNdjcCnN8HnUpVge0de3DvA%2FXaU9ZqoNjrxgUjfL3sJlX59pHGXVyoEg0h6qq4GQyIWQb5PMFuMK%2FabahNtQ%3D%3D; _sctr=1%7C1739340000000; _merlin_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYczUxMmRkblpkZWRvNm9Yb2psSXFKY3A4bQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAjpleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTNNemt6TnpFek16Y3NJbWxoZENJNk1UY3pPVE0yTnpjek55d2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pWldFMk5USTBNV1F0WmpWa015MDBNVGcxTFRneVlXTXRPR1JoT0dVd01USmpOREkzSWl3aWJtSm1Jam94TnpNNU16WTNOek0yTENKd1pXMGlPbnNpY0hKbGJXbDFiU0k2TVgwc0luTjFZaUk2SW50Y0ltVnRZV2xzWENJNlhDSmlaM0psZEdOb1FHZHRZV2xzTG1OdmJWd2lMRndpWm1WaGRIVnlaWE5jSWpwYlhTeGNJbVpwY25OMFgyNWhiV1ZjSWpwdWRXeHNMRndpYkdGemRGOXVZVzFsWENJNmJuVnNiQ3hjSW5WcFpGd2lPbHdpTVRFNU1XTm1OVGt0WVRsaU5DMDBZamd5TFRrNU5ESXRNR0UxT1Roak56ZzFPV1kxWENJc1hDSjJaWEowYVdOaGJGd2lPbHdpUTI5dWMzVnRaWEpjSW4waUxDSjBlWEFpT2lKaFkyTmxjM01pZlEuWHBlNENCQlRiMVQtV3NvOEMwWnotUUN2RWNyaU1objduOHltb0kyQmdKcUNYU3ZESzV3bURpd1FtMHdhRExQMGlSWUl0UW0yTlBrZlNscDItZGNLendtAAAAGGxhdW5jaF9kYXJrbHlfc2Vzc2lvbl9pZG0AAAAkNjNjYWI0OGYtNWM5Yy00NjkwLWExN2ItYmJkNjYxOGY5MmU2bQAAABpsYXVuY2hfZGFya2x5X3VzZXJfY29udGV4dHQAAAADZAAKYXR0cmlidXRlc3QAAAAHZAACaXBtAAAAJjI2MDE6MjQ5Ojk0ODA6MTgwMDo1OTViOmYwNjU6M2NmMzpmM2UybQAAAAlhbm9ueW1vdXNkAAVmYWxzZW0AAAALcGZmSW50ZXJuYWxkAAVmYWxzZW0AAAAJcGZmVXNlcklkbQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVtAAAAB3ByZW1pdW1kAAR0cnVlbQAAAAlzZXNzaW9uSWRtAAAAJDYzY2FiNDhmLTVjOWMtNDY5MC1hMTdiLWJiZDY2MThmOTJlNm0AAAAIc2lnbmVkSW5kAAR0cnVlZAADa2V5bQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVkAARraW5kbQAAAAR1c2VybQAAAAlyZXR1cm5fdG9tAAAAFy9uZmwvZ3JhZGVzL3Bvc2l0aW9uL3Fi.IZzv7MHXoBZqBuZckvWSfB3OqxpOsPHgGZTEdgzL_4Q; __kla_id=eyJjaWQiOiJPV00yWmpGa01UZ3ROelExTWkwMFpqZGxMVGxtTnpRdFlqa3dZamd6WXpNNE0ySmgiLCIkZXhjaGFuZ2VfaWQiOiJCaTh0NmtldGJsd1VXMGJMclMweVBvd3FhMmRXT3pzSUpWcGw3Z0MzdWZ3LlU3TWhoRyJ9; _scid_r=nGIjKCbf6WGChxutPdny6JL376QLVkYP-MEPIQ; AWSALB=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; AWSALBCORS=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; _ga_123456789=GS1.1.1739367737.2.1.1739369395.0.0.868918702; _hp2_id.2100373990=%7B%22userId%22%3A%226993897955886911%22%2C%22pageviewId%22%3A%226292591763098925%22%2C%22sessionId%22%3A%222522677639990233%22%2C%22identity%22%3A%221191cf59-a9b4-4b82-9942-0a598c7859f5%22%2C%22trackerVersion%22%3A%224.0%22%2C%22identityField%22%3Anull%2C%22isIdentified%22%3A1%7D'
# temp <- GET(
#   url,
#   add_headers(cookie = cookie)
# ) %>%
#   content(as = 'text') %>%
#   fromJSON %>%
#   .$players
# 
# pff_test <- temp %>%
#   select(name, offense)

off_pos <- c('QB', 'WR', 'HB', 'FB', 'TE',
             'C', 'G', 'T')

def_pos <- c('CB', 'S', 'LB', 'DI', 'ED')

grades_df <- data.frame()
for(p in off_pos){
  url <- paste0("https://www.pff.com/api/nfl/grades?league=nfl&position=",p,"&season=2024")
  cookie <- '_sharedID=547ec3e4-97dc-4a46-ab70-6631e128ee60; _sharedID_cst=zix7LPQsHA%3D%3D; _ga=GA1.1.1911505820.1738795457; _cc_id=e92f48eeba0e07d8e87eab644d497a17; panoramaId_expiry=1739400257270; panoramaId=14478aa2bad8550ea6f65c47bca416d539383f01a3a46797ce91a8e46dfda37b; panoramaIdType=panoIndiv; _ScCbts=%5B%22570%3Bchrome.2%3A2%3A5%22%5D; FPID=FPID2.2.G%2BIcad3C23B7hJX10VuozYzV7P4Sd1E1QhUY0yLPhTc%3D.1738795457; FPAU=1.2.99632523.1738795457; _gtmeec=e30%3D; _fbp=fb.1.1738795457233.1851222021; _sharedid=c63e73db-9c8c-4b0c-8a88-ed47db4f4016; _sharedid_cst=zix7LPQsHA%3D%3D; cto_bundle=C_7buF8xeHdWRE5kM3d1cHhESjlYTGxSaSUyRndWVW54d3BtTkQwbkFBVmhYNFBrZTVEVUhlM2lVZiUyQmd1S0RvYXFDQ1I1RnlEcWpXekVYMFZUWGRrY3RvNVRKWjlpVlhjc1RUJTJGaGZRY3AlMkZzSGd6RFlyQlhJOXk5ZSUyRlR4dXprSW52dmxwJTJCNEo4d2hzUGZPTk9xMDZOZllyM21NSSUyQkJpeGx3Z3pVakFLeWtqeXF1elI1RSUzRA; cto_bidid=jNUvcF9iQ2Q4anRwbkpRVm8lMkZIMVZ0aEV4OTM1Qkd0UHRxWE1sZTJqejlrSE1jMmtqdiUyQkJoNnNnOWJXaUdsSyUyQmdZRXhiMlBRNUtUQkdIMm9IMnlmWGRuRGM5d1M1aWlUeWVuV0VqYTJJMXlpeDJTbyUzRA; _scid=kmIjKCbf6WGChxutPdny6JL376QLVkYP-MEPJw; c_groot_access_token=bi5FJTJ0f5u6dPYM7GQNnoPDeyBub9O6jhBsLyF8JIKiuKTFAXCqmwHdrEVIut32; c_groot_access_ts=2025-02-12T13:42:17Z; c_groot_refresh_token=op8LKni0GKZBsae0NHipEwrv8gdHZmJ0NpptNQ-gRlCE3-HLHZBsKdreYaZAFYUt; _hp2_ses_props.2100373990=%7B%22ts%22%3A1739367737735%2C%22d%22%3A%22www.pff.com%22%2C%22h%22%3A%22%2F%22%7D; FPLC=K21cF0C%2FUIrr2HXl1lL6N%2FuHQ0Ra%2Bqds9CLqe%2BzgFHsNdjcCnN8HnUpVge0de3DvA%2FXaU9ZqoNjrxgUjfL3sJlX59pHGXVyoEg0h6qq4GQyIWQb5PMFuMK%2FabahNtQ%3D%3D; _sctr=1%7C1739340000000; _merlin_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYczUxMmRkblpkZWRvNm9Yb2psSXFKY3A4bQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAjpleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTNNemt6TnpFek16Y3NJbWxoZENJNk1UY3pPVE0yTnpjek55d2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pWldFMk5USTBNV1F0WmpWa015MDBNVGcxTFRneVlXTXRPR1JoT0dVd01USmpOREkzSWl3aWJtSm1Jam94TnpNNU16WTNOek0yTENKd1pXMGlPbnNpY0hKbGJXbDFiU0k2TVgwc0luTjFZaUk2SW50Y0ltVnRZV2xzWENJNlhDSmlaM0psZEdOb1FHZHRZV2xzTG1OdmJWd2lMRndpWm1WaGRIVnlaWE5jSWpwYlhTeGNJbVpwY25OMFgyNWhiV1ZjSWpwdWRXeHNMRndpYkdGemRGOXVZVzFsWENJNmJuVnNiQ3hjSW5WcFpGd2lPbHdpTVRFNU1XTm1OVGt0WVRsaU5DMDBZamd5TFRrNU5ESXRNR0UxT1Roak56ZzFPV1kxWENJc1hDSjJaWEowYVdOaGJGd2lPbHdpUTI5dWMzVnRaWEpjSW4waUxDSjBlWEFpT2lKaFkyTmxjM01pZlEuWHBlNENCQlRiMVQtV3NvOEMwWnotUUN2RWNyaU1objduOHltb0kyQmdKcUNYU3ZESzV3bURpd1FtMHdhRExQMGlSWUl0UW0yTlBrZlNscDItZGNLendtAAAAGGxhdW5jaF9kYXJrbHlfc2Vzc2lvbl9pZG0AAAAkNjNjYWI0OGYtNWM5Yy00NjkwLWExN2ItYmJkNjYxOGY5MmU2bQAAABpsYXVuY2hfZGFya2x5X3VzZXJfY29udGV4dHQAAAADZAAKYXR0cmlidXRlc3QAAAAHZAACaXBtAAAAJjI2MDE6MjQ5Ojk0ODA6MTgwMDo1OTViOmYwNjU6M2NmMzpmM2UybQAAAAlhbm9ueW1vdXNkAAVmYWxzZW0AAAALcGZmSW50ZXJuYWxkAAVmYWxzZW0AAAAJcGZmVXNlcklkbQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVtAAAAB3ByZW1pdW1kAAR0cnVlbQAAAAlzZXNzaW9uSWRtAAAAJDYzY2FiNDhmLTVjOWMtNDY5MC1hMTdiLWJiZDY2MThmOTJlNm0AAAAIc2lnbmVkSW5kAAR0cnVlZAADa2V5bQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVkAARraW5kbQAAAAR1c2VybQAAAAlyZXR1cm5fdG9tAAAAFy9uZmwvZ3JhZGVzL3Bvc2l0aW9uL3Fi.IZzv7MHXoBZqBuZckvWSfB3OqxpOsPHgGZTEdgzL_4Q; __kla_id=eyJjaWQiOiJPV00yWmpGa01UZ3ROelExTWkwMFpqZGxMVGxtTnpRdFlqa3dZamd6WXpNNE0ySmgiLCIkZXhjaGFuZ2VfaWQiOiJCaTh0NmtldGJsd1VXMGJMclMweVBvd3FhMmRXT3pzSUpWcGw3Z0MzdWZ3LlU3TWhoRyJ9; _scid_r=nGIjKCbf6WGChxutPdny6JL376QLVkYP-MEPIQ; AWSALB=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; AWSALBCORS=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; _ga_123456789=GS1.1.1739367737.2.1.1739369395.0.0.868918702; _hp2_id.2100373990=%7B%22userId%22%3A%226993897955886911%22%2C%22pageviewId%22%3A%226292591763098925%22%2C%22sessionId%22%3A%222522677639990233%22%2C%22identity%22%3A%221191cf59-a9b4-4b82-9942-0a598c7859f5%22%2C%22trackerVersion%22%3A%224.0%22%2C%22identityField%22%3Anull%2C%22isIdentified%22%3A1%7D'
  temp <- GET(
    url,
    add_headers(cookie = cookie)
  ) %>%
    content(as = 'text') %>%
    fromJSON %>%
    .$players
  
  temp <- temp %>% 
    select(name, grade = offense, team_name)
  grades_df <- rbind(grades_df, temp)
  Sys.sleep(2)
}

for(d in def_pos){
  url <- paste0("https://www.pff.com/api/nfl/grades?league=nfl&position=",d,"&season=2024")
  cookie <- '_sharedID=547ec3e4-97dc-4a46-ab70-6631e128ee60; _sharedID_cst=zix7LPQsHA%3D%3D; _ga=GA1.1.1911505820.1738795457; _cc_id=e92f48eeba0e07d8e87eab644d497a17; panoramaId_expiry=1739400257270; panoramaId=14478aa2bad8550ea6f65c47bca416d539383f01a3a46797ce91a8e46dfda37b; panoramaIdType=panoIndiv; _ScCbts=%5B%22570%3Bchrome.2%3A2%3A5%22%5D; FPID=FPID2.2.G%2BIcad3C23B7hJX10VuozYzV7P4Sd1E1QhUY0yLPhTc%3D.1738795457; FPAU=1.2.99632523.1738795457; _gtmeec=e30%3D; _fbp=fb.1.1738795457233.1851222021; _sharedid=c63e73db-9c8c-4b0c-8a88-ed47db4f4016; _sharedid_cst=zix7LPQsHA%3D%3D; cto_bundle=C_7buF8xeHdWRE5kM3d1cHhESjlYTGxSaSUyRndWVW54d3BtTkQwbkFBVmhYNFBrZTVEVUhlM2lVZiUyQmd1S0RvYXFDQ1I1RnlEcWpXekVYMFZUWGRrY3RvNVRKWjlpVlhjc1RUJTJGaGZRY3AlMkZzSGd6RFlyQlhJOXk5ZSUyRlR4dXprSW52dmxwJTJCNEo4d2hzUGZPTk9xMDZOZllyM21NSSUyQkJpeGx3Z3pVakFLeWtqeXF1elI1RSUzRA; cto_bidid=jNUvcF9iQ2Q4anRwbkpRVm8lMkZIMVZ0aEV4OTM1Qkd0UHRxWE1sZTJqejlrSE1jMmtqdiUyQkJoNnNnOWJXaUdsSyUyQmdZRXhiMlBRNUtUQkdIMm9IMnlmWGRuRGM5d1M1aWlUeWVuV0VqYTJJMXlpeDJTbyUzRA; _scid=kmIjKCbf6WGChxutPdny6JL376QLVkYP-MEPJw; c_groot_access_token=bi5FJTJ0f5u6dPYM7GQNnoPDeyBub9O6jhBsLyF8JIKiuKTFAXCqmwHdrEVIut32; c_groot_access_ts=2025-02-12T13:42:17Z; c_groot_refresh_token=op8LKni0GKZBsae0NHipEwrv8gdHZmJ0NpptNQ-gRlCE3-HLHZBsKdreYaZAFYUt; _hp2_ses_props.2100373990=%7B%22ts%22%3A1739367737735%2C%22d%22%3A%22www.pff.com%22%2C%22h%22%3A%22%2F%22%7D; FPLC=K21cF0C%2FUIrr2HXl1lL6N%2FuHQ0Ra%2Bqds9CLqe%2BzgFHsNdjcCnN8HnUpVge0de3DvA%2FXaU9ZqoNjrxgUjfL3sJlX59pHGXVyoEg0h6qq4GQyIWQb5PMFuMK%2FabahNtQ%3D%3D; _sctr=1%7C1739340000000; _merlin_key=SFMyNTY.g3QAAAAFbQAAAAtfY3NyZl90b2tlbm0AAAAYczUxMmRkblpkZWRvNm9Yb2psSXFKY3A4bQAAABZndWFyZGlhbl9kZWZhdWx0X3Rva2VubQAAAjpleUpoYkdjaU9pSklVelV4TWlJc0luUjVjQ0k2SWtwWFZDSjkuZXlKaGRXUWlPaUpOWlhKc2FXNGlMQ0psZUhBaU9qRTNNemt6TnpFek16Y3NJbWxoZENJNk1UY3pPVE0yTnpjek55d2lhWE56SWpvaVRXVnliR2x1SWl3aWFuUnBJam9pWldFMk5USTBNV1F0WmpWa015MDBNVGcxTFRneVlXTXRPR1JoT0dVd01USmpOREkzSWl3aWJtSm1Jam94TnpNNU16WTNOek0yTENKd1pXMGlPbnNpY0hKbGJXbDFiU0k2TVgwc0luTjFZaUk2SW50Y0ltVnRZV2xzWENJNlhDSmlaM0psZEdOb1FHZHRZV2xzTG1OdmJWd2lMRndpWm1WaGRIVnlaWE5jSWpwYlhTeGNJbVpwY25OMFgyNWhiV1ZjSWpwdWRXeHNMRndpYkdGemRGOXVZVzFsWENJNmJuVnNiQ3hjSW5WcFpGd2lPbHdpTVRFNU1XTm1OVGt0WVRsaU5DMDBZamd5TFRrNU5ESXRNR0UxT1Roak56ZzFPV1kxWENJc1hDSjJaWEowYVdOaGJGd2lPbHdpUTI5dWMzVnRaWEpjSW4waUxDSjBlWEFpT2lKaFkyTmxjM01pZlEuWHBlNENCQlRiMVQtV3NvOEMwWnotUUN2RWNyaU1objduOHltb0kyQmdKcUNYU3ZESzV3bURpd1FtMHdhRExQMGlSWUl0UW0yTlBrZlNscDItZGNLendtAAAAGGxhdW5jaF9kYXJrbHlfc2Vzc2lvbl9pZG0AAAAkNjNjYWI0OGYtNWM5Yy00NjkwLWExN2ItYmJkNjYxOGY5MmU2bQAAABpsYXVuY2hfZGFya2x5X3VzZXJfY29udGV4dHQAAAADZAAKYXR0cmlidXRlc3QAAAAHZAACaXBtAAAAJjI2MDE6MjQ5Ojk0ODA6MTgwMDo1OTViOmYwNjU6M2NmMzpmM2UybQAAAAlhbm9ueW1vdXNkAAVmYWxzZW0AAAALcGZmSW50ZXJuYWxkAAVmYWxzZW0AAAAJcGZmVXNlcklkbQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVtAAAAB3ByZW1pdW1kAAR0cnVlbQAAAAlzZXNzaW9uSWRtAAAAJDYzY2FiNDhmLTVjOWMtNDY5MC1hMTdiLWJiZDY2MThmOTJlNm0AAAAIc2lnbmVkSW5kAAR0cnVlZAADa2V5bQAAACQxMTkxY2Y1OS1hOWI0LTRiODItOTk0Mi0wYTU5OGM3ODU5ZjVkAARraW5kbQAAAAR1c2VybQAAAAlyZXR1cm5fdG9tAAAAFy9uZmwvZ3JhZGVzL3Bvc2l0aW9uL3Fi.IZzv7MHXoBZqBuZckvWSfB3OqxpOsPHgGZTEdgzL_4Q; __kla_id=eyJjaWQiOiJPV00yWmpGa01UZ3ROelExTWkwMFpqZGxMVGxtTnpRdFlqa3dZamd6WXpNNE0ySmgiLCIkZXhjaGFuZ2VfaWQiOiJCaTh0NmtldGJsd1VXMGJMclMweVBvd3FhMmRXT3pzSUpWcGw3Z0MzdWZ3LlU3TWhoRyJ9; _scid_r=nGIjKCbf6WGChxutPdny6JL376QLVkYP-MEPIQ; AWSALB=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; AWSALBCORS=Z/eUIEVPn6IyxFFOqPOS7r1nRAUu2LEa5R40KtmZyPbSB6nFQtVAH1GoQQ3lwkj8EH0KnOIZoMb5bmnZ9QADkFRG0ypPnVlSK85rDrWH9aKMQrOer3mUhCI0BNDT; _ga_123456789=GS1.1.1739367737.2.1.1739369395.0.0.868918702; _hp2_id.2100373990=%7B%22userId%22%3A%226993897955886911%22%2C%22pageviewId%22%3A%226292591763098925%22%2C%22sessionId%22%3A%222522677639990233%22%2C%22identity%22%3A%221191cf59-a9b4-4b82-9942-0a598c7859f5%22%2C%22trackerVersion%22%3A%224.0%22%2C%22identityField%22%3Anull%2C%22isIdentified%22%3A1%7D'
  temp <- GET(
    url,
    add_headers(cookie = cookie)
  ) %>%
    content(as = 'text') %>%
    fromJSON %>%
    .$players
  
  temp <- temp %>% 
    select(name, grade = defense, team_name)
  grades_df <- rbind(grades_df, temp)
  Sys.sleep(2)
  print(paste0(d, " done"))
}

fin_grades_df <- grades_df %>% 
  mutate(name = nflreadr::clean_player_names(name),
         team_name = nflreadr::clean_team_abbrs(team_name))

write_csv(fin_grades_df, "~/Documents/Data/PFF_Grades_2024.csv")





# restructure_link <- "https://overthecap.com/restructure"
# 
# restructure_table <- restructure_link %>% 
#   read_html() %>% 
#   html_element("table") %>% 
#   html_table()
# 
# colnames(restructure_table)[5] = "cap_space_simple_restructure"
# colnames(restructure_table)[8] = "cap_space_max_restructure"
# 
# fin_restructure_table <- restructure_table %>% 
#   select(team = Team, simple_resctructure = `Simple Restructure`,
#          cap_space_simple_restructure, max_restructure = `Maximum Restructure`, cap_space_max_restructure) %>% 
#   mutate(simple_resctructure = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", simple_resctructure))),
#          cap_space_simple_restructure = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", cap_space_simple_restructure))),
#          max_restructure = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", max_restructure))),
#          cap_space_max_restructure = as.numeric(gsub("\\(", "-", gsub("\\)|\\$|,", "", cap_space_max_restructure))))  %>% 
#   left_join(team_abbr,
#             by = c("team" = "team_nick")) %>% 
#   select(-team)


# fa_link <- "https://overthecap.com/free-agency"
# 
# fa_table <- fa_link %>% 
#   read_html() %>% 
#   html_element("table") %>% 
#   html_table()
# 
# 
# free_agents <- read_csv("C:/Users/sphop/OneDrive/Betsperts/R/Data/FA 2023.csv") %>% 
#   left_join(team_abbr,
#             by = c("team_2022" = "team_nick")) %>% 
#   rename(team_abbr_22 = team_abbr) %>% 
#   left_join(team_abbr,
#             by = c("team_2023" = "team_nick")) %>% 
#   rename(team_abbr_23 = team_abbr)
# 
# write_csv(free_agents, "C:/Users/sphop/OneDrive/Betsperts/R/Data/Free_Agents_2023.csv")
