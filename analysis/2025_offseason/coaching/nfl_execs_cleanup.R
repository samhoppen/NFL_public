library(tidyr)
library(tidyverse)
library(dplyr)

# manually-updated file
# sourced from: https://www.pro-football-reference.com/executives/
execs <- read_csv("~/Documents/Data/nfl_executives.csv")

new_execs <- execs %>%
  mutate(year = map2(from, to, ~seq(.x, .y))) %>%
  unnest(year) %>% 
  select(-c(to, from)) %>%
  group_by(team, year) %>% 
  mutate(gm_count = sum(grepl("General Manager", position_group))) %>% 
  ungroup() %>% 
  mutate(draft_control = if_else(grepl("General Manager", position_group) & gm_count == 1, 1, 0)) %>% 
  mutate(draft_control = case_when(name == "George Halas" & (between(year, 1946,1962) | year %in% c(1942,1974)) ~ 1,
                                   name == "James Breuil" & year %in% c(1948, 1949) ~ 1,
                                   name == "Hugh Culverhouse" & year %in% c(1992, 1993) ~ 1,
                                   name == "Duke Tobin" ~ 1,
                                   name == "Mike Brown" & year >= 2002 ~ 0,
                                   name == "Sashi Brown" & year %in% c(2016, 2017) ~ 1,
                                   name == "Jim Goodman" & year == 2008 ~ 1,
                                   name == "Walter Wolfner" & between(year, 1952, 1961) ~ 1,
                                   name == "Bob Ferguson" & year %in% c(1996, 1997, 1998) ~ 1,
                                   name == "Steve Keim" & year == 2022 ~ 1,
                                   name == "George Richards" & year == 1939 ~ 1,
                                   name == "Fred Mandel" & year == 1940 ~ 1,
                                   name == "Fred Mandel" & between(year, 1943, 1947) ~ 1,
                                   name == "Tom Telesco" & year == 2023 ~ 1,
                                   name == "Harry Hulmes" & year == 1967 ~ 1,
                                   name == "Don Kellett" & year == 1966 ~ 1,
                                   name == "Bill Polian" & year == 2009 ~ 1,
                                   # name == "Lamar Hunt" & year == 1960 ~ 1,
                                   name == "Dennis Shea" & year %in% c(1942, 1943) ~ 1,
                                   name == "Howie Roseman" & year == 2015 ~ 1,
                                   name == "Tom Heckert" & team == "Eagles" ~ 0,
                                   name == "Andy Reid" & between(year, 2006, 2009) ~ 1,
                                   name == "Ken Herock" & between(year, 1987, 1996) ~ 1,
                                   name == "Tim Mara" & year <= 1946 ~ 1,
                                   name == "Dick Haley" & year %in% c(2001, 2002) ~ 1,
                                   name == "James Harris" & between(year, 2003, 2008) ~ 1,
                                   name == "Martin Mayhew" & year == 2015 ~ 1,
                                   name == "Matt Millen" & year == 2008 ~ 1,
                                   name == "Bill Polian" & year == 1995 ~ 1,
                                   name == "Dave Gettleman" & between(year, 2013, 2017) ~ 1,
                                   name == "Matt Rhule" & between(year, 2020, 2022) ~ 1,
                                   name == "Peter Hadhazy" & year == 1972 ~ 1,
                                   name == "Chuck Fairbanks" & year == 1973 ~ 1,
                                   name == "Joe Mendes" & year == 1991 ~ 1,
                                   name == "Charley Armey" & year == 1992 ~ 1,
                                   name == "Bobby Grier" & year %in% c(1997, 1998,1999) ~ 1,
                                   name == "Dave Ziegler" & year == 2023 ~ 1,
                                   name == "Tom Lipscomb" & year == 1938 ~ 1,
                                   name == "Dan Reeves" & year == 1944 ~ 1,
                                   name == "John Thompson" & year == 1982 ~ 1,
                                   name == "Randy Mueller" & year %in% c(1997, 1998) ~ 1,
                                   name == "Art Rooney" & year <= 1944 ~ 1,
                                   name == "Jon Robinson" & year == 2022 ~ 1,
                                   name == "Rob Brzezinski" & between(year, 2002, 2005) ~ 1,
                                   name == "Fran Foley" & year == 2006 ~ 1,
                                   team == "Packers" & position == "President" & year <= 1935 ~ 1,
                                   name == "Dick Corrick" & between(year, 1981, 1983) ~ 1,
                                   name == "Tom Braatz" & between(year, 1987, 1991) ~ 1,
                                   name == "George Preston Marshall" & between(year, 1944, 1946) ~ 1,
                                   name == "Vince Lombardi" & between(year, 1969, 1970) ~ 1,
                                   name == "Vinny Cerrato" & between(year, 2000, 2009) ~ 1,
                                   name == "Mike Shanahan" & between(year, 2010, 2013) ~ 1,
                                   name == "Bruce Allen" & between(year, 2017, 2019) ~ 1,
                                   TRUE ~ draft_control)) %>% 
  group_by(team, year) %>% 
  mutate(draft_confirmed = sum(draft_control)) %>% 
  arrange(team, year, position_group) %>% 
  ungroup() %>% 
  select(-c(gm_count, draft_confirmed))

write_csv(new_execs, "~/Documents/Data/nfl_executives_yearly.csv")


