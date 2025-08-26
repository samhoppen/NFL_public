source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(datapasta)
library(ggplot2)

datapasta::tribble_paste()
draft_picks <- tibble::tribble(
  ~overall,             ~player,   ~pos, ~pick, ~team, ~prospect_value, ~draft_value,
        1L,     "Travis Hunter",   "CB",    2L, "JAX",        3000L,           2649L,
        2L,      "Abdul Carter",   "ED",    3L, "NYG",        2649L,           2443L,
        3L,     "Ashton Jeanty",   "RB",    6L,  "LV",        2443L,           2092L,
        4L,      "Mason Graham", "DL3T",    5L, "CLE",        2297L,           2184L,
        5L,     "Will Campbell",   "OT",    4L,  "NE",        2184L,           2297L,
        6L,     "Armand Membou",   "OT",    7L, "NYJ",        2092L,           2014L,
        7L,      "Tyler Warren",   "TE",   14L, "IND",        2014L,           1663L,
        8L,          "Cam Ward",   "QB",    1L, "TEN",        1946L,           3000L,
       10L, "Tetairoa McMillan",   "WR",    8L, "CAR",        1833L,           1946L,
       11L,      "Jalon Walker",   "ED",   15L, "ATL",        1785L,           1628L,
       12L,  "Colston Loveland",   "TE",   10L, "CHI",        1741L,           1833L,
       13L,     "Jahdae Barron",   "CB",   20L, "DEN",        1700L,           1482L,
       14L,   "Jihaad Campbell",   "LB",   31L, "PHI",        1663L,           1260L,
       15L,  "Kelvin Banks Jr.",   "OT",    9L,  "NO",        1628L,           1887L,
       16L,     "Malaki Starks",    "S",   27L, "BAL",        1595L,           1330L,
       18L,    "Mykel Williams",   "ED",   11L,  "SF",        1535L,           1785L,
       19L,    "Shemar Stewart",   "ED",   17L, "CIN",        1508L,           1564L,
       20L,      "Walter Nolen", "DL3T",   16L, "ARI",        1482L,           1595L,
       21L,      "Josh Simmons",   "OT",   32L,  "KC",        1457L,           1244L,
       23L,     "Kenneth Grant", "DL1T",   13L, "MIA",        1411L,           1700L,
       24L,    "Matthew Golden",   "WR",   23L,  "GB",        1389L,           1411L,
       25L,      "Emeka Egbuka",   "WR",   19L,  "TB",        1369L,           1508L,
       26L,  "James Pearce Jr.",   "ED",   26L, "ATL",        1349L,           1349L,
       27L,   "Omarion Hampton",   "RB",   22L, "LAC",        1330L,           1434L,
       28L,    "Derrick Harmon", "DL3T",   21L, "PIT",        1311L,           1457L,
       31L,      "Tyler Booker",   "OG",   12L, "DAL",        1260L,           1741L,
       33L,        "Grey Zabel",   "OG",   18L, "SEA",        1228L,           1535L,
       34L,  "Josh Conerly Jr.",   "OT",   29L, "WAS",        1213L,           1294L,
       36L,   "Donovan Jackson",   "OG",   24L, "MIN",        1184L,           1389L,
       39L,  "Maxwell Hairston",   "CB",   30L, "BUF",        1143L,           1276L,
       41L,   "Tyleik Williams", "DL1T",   28L, "DET",        1118L,           1311L,
       49L,       "Jaxson Dart",   "QB",   25L, "NYG",        1028L,           1369L
  )
log_trans <- function(base = exp(1)) {
  # Define the transformation function as the natural logarithm (or log to a specified base)
  trans <- function(x) log(x, base)
  
  # Define the inverse transformation function as exponentiation to the base
  inv <- function(x) base^x
  
  # Use the trans_new function to create a new transformation
  # with the specified name, transformation, inverse transformation,
  # appropriate breaks, and domain.
  trans_new(paste0("log-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}
x_midpoint <- (max(draft_picks$draft_value) + min(draft_picks$draft_value)) / 2
x_high <- (max(draft_picks$draft_value) + x_midpoint)*0.525
x_low <- (min(draft_picks$draft_value) + x_midpoint)*0.475

y_midpoint <- (max(draft_picks$prospect_value) + min(draft_picks$prospect_value)) / 2
y_high <- (max(draft_picks$prospect_value) + y_midpoint)*0.525
y_low <- (min(draft_picks$prospect_value) + y_midpoint)*0.475

ints <- seq(-2,2, by = 0.05)

chart_max = max(draft_picks$prospect_value, draft_picks$draft_value)+10
chart_min = min(draft_picks$prospect_value, draft_picks$draft_value)-10

draft_chart <- ggplot(data = draft_picks) +
  annotate("text", x = x_high, y = y_low, label = "Perceived\ndraft reach", fontface = 2, alpha = 0.8, size = 2.25)+
  annotate("text", x = x_low, y = y_high, label = "Perceived\ndraft value", fontface = 2, alpha = 0.8, size = 2.25)+
  geom_abline(slope = 1, intercept = 0)+
  nflplotR::geom_nfl_logos(aes(team_abbr = team, x = draft_value, y = prospect_value), width = 0.02, alpha = 0.8) +
  geom_text_repel(aes(x = draft_value, y = prospect_value, label = player), size = 2) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=8),
                     limits = c(chart_min, chart_max),
                     trans=log_trans(10)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n=8),
                     limits = c(chart_min, chart_max),
                     trans=log_trans(10)) + 
  theme_FE +
  labs(title = paste0("Using prospect big board ranks to identify draft values and reaches"),
       subtitle = "Pick values based on Fitzgerald-Spielberger trade value chart",
       caption = "Figure: @SamHoppen | Data: Arif Hasan",
       x = "Draft Capital Spent",
       y = "Implied Prospect Draft Capital") +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 6),
        plot.caption = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 6))


brand_nfl_plot(draft_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/rd1_picks.png"))
d