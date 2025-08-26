library(tidyverse)
library(dplyr)
library(datapasta)
library(gt)
library(RColorBrewer)

# datapasta::tribble_paste()
mvp <- tibble::tribble(
               ~voter,        ~place_1,         ~place_2,          ~place_3,          ~place_4,          ~place_5,
         "Pat Kirwan",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley", "Patrick Mahomes",      "Jared Goff",
    "Tom Silverstein", "Lamar Jackson",     "Joe Burrow",      "Josh Allen",  "Saquon Barkley",      "Jared Goff",
         "Mike Sando", "Lamar Jackson",     "Josh Allen",      "Joe Burrow", "Patrick Mahomes",  "Saquon Barkley",
         "Mike Jones", "Lamar Jackson",     "Josh Allen", "Patrick Mahomes",  "Saquon Barkley",      "Joe Burrow",
        "Doug Farrar", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley", "Patrick Mahomes",
   "Armando Salguero", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",      "Jared Goff",      "Joe Burrow",
         "Nate Davis", "Lamar Jackson",     "Josh Allen",      "Jared Goff",  "Jayden Daniels",  "Baker Mayfield",
   "Charean Williams",    "Josh Allen",  "Lamar Jackson",      "Jared Goff",      "Joe Burrow",  "Saquon Barkley",
        "Rich Gannon",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
      "Lindsay Jones",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
         "Mark Craig",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",  "Jayden Daniels",      "Jared Goff",
       "Jarrett Bell", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley", "Patrick Mahomes",      "Joe Burrow",
        "Chris Simms",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",   "Ja'Marr Chase",
        "Laura Okmin",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
         "Dan Pompei", "Lamar Jackson",     "Josh Allen",      "Joe Burrow", "Patrick Mahomes",      "Jared Goff",
        "Adam Schein",    "Josh Allen",  "Lamar Jackson",      "Joe Burrow",  "Saquon Barkley",      "Jared Goff",
       "Dave Birkett", "Lamar Jackson",     "Josh Allen",      "Jared Goff",  "Saquon Barkley", "Patrick Mahomes",
        "Vic Carucci",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
      "Lorenzo Reyes", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley",      "Jared Goff",
   "Anthony DiGuilio",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
     "Jonathan Jones",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Joe Burrow", "Patrick Mahomes",
       "Jori Epstein",    "Josh Allen",  "Lamar Jackson",      "Joe Burrow",  "Saquon Barkley",      "Jared Goff",
        "Mike Florio", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",      "Jared Goff",      "Joe Burrow",
   "Charles Robinson",    "Josh Allen",  "Lamar Jackson",      "Joe Burrow",  "Saquon Barkley",   "Ja'Marr Chase",
     "Michael Silver",    "Josh Allen",  "Lamar Jackson",      "Jared Goff",  "Saquon Barkley", "Patrick Mahomes",
    "Nora Princiotti",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",  "Justin Herbert",      "Joe Burrow",
        "Mike Tirico", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
        "Pete Prisco",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Jared Goff",      "Joe Burrow",
      "Nick Pavlatos",    "Josh Allen",  "Lamar Jackson",      "Joe Burrow",  "Saquon Barkley",      "Jared Goff",
       "Aaron Schatz", "Lamar Jackson",     "Josh Allen",      "Joe Burrow", "Patrick Mahomes",      "Jared Goff",
         "Tony Dungy", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",  "Justin Herbert",  "Jayden Daniels",
          "Ben Volin",    "Josh Allen",  "Lamar Jackson",      "Jared Goff",  "Saquon Barkley",     "Sam Darnold",
         "Diante Lee", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",  "Justin Herbert",  "Jayden Daniels",
       "Reuben Frank", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",      "Joe Burrow",      "Jared Goff",
         "Jim Miller",    "Josh Allen", "Saquon Barkley",      "Joe Burrow",   "Lamar Jackson", "Patrick Mahomes",
       "Bruce Murray",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Jared Goff",      "Joe Burrow",
     "Dianna Russini", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley",      "Jared Goff",
          "Kay Adams", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley", "Patrick Mahomes",      "Joe Burrow",
        "Ira Kaufman",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley", "Patrick Mahomes",      "Joe Burrow",
        "Gary Meyers",    "Josh Allen",  "Lamar Jackson",      "Jared Goff",  "Saquon Barkley",      "Joe Burrow",
  "Aditi Kinkhabwala",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley",      "Jared Goff",      "Joe Burrow",
         "Sam Monson", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley",     "Sam Darnold",
          "Tom Brady", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",   "Ja'Marr Chase",      "Joe Burrow",
       "Tedy Bruschi", "Lamar Jackson",     "Josh Allen",  "Saquon Barkley",   "Ja'Marr Chase", "Patrick Mahomes",
      "John McMullen",    "Josh Allen",  "Lamar Jackson",      "Joe Burrow",  "Saquon Barkley",     "Sam Darnold",
         "Greg Auman", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley",  "Baker Mayfield",
         "Mina Kimes", "Lamar Jackson",     "Josh Allen",      "Joe Burrow",  "Saquon Barkley",  "Jayden Daniels",
         "Tom Curran",    "Josh Allen",  "Lamar Jackson", "Patrick Mahomes",  "Saquon Barkley",      "Jared Goff",
       "Dan Orlovsky",    "Josh Allen",  "Lamar Jackson",  "Saquon Barkley", "Patrick Mahomes",  "Jayden Daniels",
      "Emmanuel Acho",    "Josh Allen",  "Lamar Jackson", "Patrick Mahomes",  "Saquon Barkley",      "Jared Goff"
  )

reshaped_mvp <- mvp %>%
  pivot_longer(
    cols = starts_with("place"),
    names_to = "place",
    values_to = "player"
  ) %>%
  mutate(value = place) %>%
  select(voter, player, value)

# Pivot wider to have players as columns
fin_mvp <- reshaped_mvp %>%
  pivot_wider(
    names_from = player,
    values_from = value,
    values_fill = NA
  ) %>%
  mutate(across(-voter, ~ as.integer(gsub("place_", "", .))))

mvp_table <- fin_mvp %>% 
  gt() %>% 
  tab_header(title = md("**2024 NFL MVP Voting**")) %>% 
  cols_label(voter = "MVP Voter") %>% 
  cols_align(align = "center",
             columns = everything()) %>%
  data_color(
    columns = c(`Josh Allen`, `Lamar Jackson`, `Saquon Barkley`, `Patrick Mahomes`,
                `Jared Goff`, `Joe Burrow`, `Jayden Daniels`, `Baker Mayfield`, 
                `Ja'Marr Chase`, `Justin Herbert`, `Sam Darnold`),
    colors = scales::col_numeric(
      paletteer::paletteer_d(
        palette = "RColorBrewer::GnBu"
      ) %>% as.character(),
      domain = c(1,5),
      reverse = T
    ),
    alpha = 0.8
  ) %>% 
  sub_missing(missing_text = "-") %>% 
  tab_spanner(
    label = html("Players"),
    columns = c(`Josh Allen`, `Lamar Jackson`, `Saquon Barkley`, `Patrick Mahomes`,
                `Jared Goff`, `Joe Burrow`, `Jayden Daniels`, `Baker Mayfield`, 
                `Ja'Marr Chase`, `Justin Herbert`, `Sam Darnold`)
  ) %>%
  tab_source_note(
    source_note = "Table: @SamHoppen | Data: Associated Press"
  )

gtExtras::gtsave_extra(mvp_table, filename = paste0("~/Documents/Charts/MVP_voting.png"),
                       vwidth = 750)
?scales::col_numeric
