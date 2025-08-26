source('https://raw.githubusercontent.com/samhoppen/Fantasy-Evaluator/main/Code/Image%20themes.R')
library(datapasta)

# tribble_paste()
pressures <- tibble::tribble(
  ~defteam, ~orig_pressure_rate,  ~new_pressure_rate,       ~pressure_diff,          ~full_name, ~pressures,
     "ARI",   0.301458670988655,   0.280388978930308,   0.0210696920583469,     "Zaven Collins",         31,
     "ATL",   0.284615384615385,   0.247692307692308,   0.0369230769230769,      "Kaden Elliss",         42,
     "BAL",   0.332460732984293,   0.297120418848168,   0.0353403141361257,  "Nnamdi Madubuike",         59,
     "BUF",   0.334705075445816,   0.285322359396433,   0.0493827160493827,     "Greg Rousseau",         70,
     "CAR",                0.25,    0.20777027027027,   0.0422297297297297,  "Jadeveon Clowney",         44,
     "CHI",    0.37037037037037,   0.328042328042328,   0.0423280423280423,      "Montez Sweat",         49,
     "CIN",   0.314329738058552,   0.234206471494607,   0.0801232665639446,  "Trey Hendrickson",         83,
     "CLE",   0.415944540727903,   0.334488734835355,   0.0814558058925476,     "Myles Garrett",         83,
     "DAL",    0.36241610738255,   0.305369127516779,   0.0570469798657718,     "Micah Parsons",         70,
     "DEN",   0.398907103825137,   0.342896174863388,   0.0560109289617486,        "Zach Allen",         81,
     "DET",   0.369806094182826,   0.344875346260388,   0.0249307479224377,   "Levi Onwuzurike",         46,
      "GB",   0.355322338830585,   0.313343328335832,   0.0419790104947527,       "Rashan Gary",         49,
     "HOU",   0.347578347578348,   0.283475783475783,   0.0641025641025642,   "Danielle Hunter",         74,
     "IND",   0.291325695581015,   0.243862520458265,   0.0474631751227496,    "Dayo Odeyingbo",         42,
     "JAX",   0.296352583586626,   0.246200607902736,   0.0501519756838906,  "Josh Hines-Allen",         63,
      "KC",   0.370904325032765,   0.306684141546527,   0.0642201834862385,       "Chris Jones",         88,
      "LA",   0.366136034732272,   0.296671490593343,   0.0694645441389291,       "Jared Verse",         89,
     "LAC",   0.339622641509434,   0.290275761973875,   0.0493468795355588,       "Khalil Mack",         52,
      "LV",    0.29951690821256,   0.246376811594203,   0.0531400966183575,       "Maxx Crosby",         54,
     "MIA",   0.349358974358974,   0.299679487179487,   0.0496794871794872,     "Chop Robinson",         56,
     "MIN",   0.359078590785908,   0.292682926829268,   0.0663956639566396, "Jonathan Greenard",         84,
      "NE",   0.279661016949153,   0.230508474576271,   0.0491525423728813,       "Keion White",         45,
      "NO",   0.336309523809524,    0.27827380952381,   0.0580357142857143,       "Chase Young",         66,
     "NYG",   0.347222222222222,   0.279513888888889,   0.0677083333333333,       "Brian Burns",         61,
     "NYJ",   0.374789915966387,   0.312605042016807,   0.0621848739495798,  "Will McDonald IV",         61,
     "PHI",   0.368289637952559,    0.32334581772784,   0.0449438202247191,      "Jalen Carter",         74,
     "PIT",   0.359090909090909,   0.303030303030303,    0.056060606060606,   "Cameron Heyward",         60,
     "SEA",   0.378294573643411,   0.344186046511628,   0.0341085271317829,  "Leonard Williams",         55,
      "SF",   0.327464788732394,   0.253521126760563,    0.073943661971831,         "Nick Bosa",         69,
      "TB",   0.374331550802139,    0.31283422459893,   0.0614973262032086,        "Yaya Diaby",         70,
     "TEN",   0.286764705882353,   0.237132352941176,   0.0496323529411765,   "Jeffery Simmons",         45,
     "WAS",    0.36036036036036,    0.31981981981982,   0.0405405405405405, "Dorance Armstrong",         51
  ) %>% 
  mutate(player_label = paste0(full_name, " (", pressures,")"))

# pressure_df <- pressures %>% 
#   pivot_longer(cols = c('orig_pressure_rate'), values_to = "rate", names_to = "type")
# 
# off_exp$type <- factor(off_exp$type, levels = c('exp_pass_rate', 'exp_run_rate'))

pressures_chart <- ggplot(data = pressures) + 
  geom_col(aes(x = orig_pressure_rate, y = reorder(defteam, orig_pressure_rate), fill = defteam, color = defteam), alpha = 0.4, width = 0.75, size = 0.25) +
  geom_col(aes(x = new_pressure_rate, y = reorder(defteam, orig_pressure_rate), fill = defteam, color = defteam), width = 0.75, size = 0.25) +
  geom_text(aes(x = orig_pressure_rate+0.001, y = reorder(defteam, orig_pressure_rate), label = player_label), size =2,
            hjust = 0)+
  scale_x_continuous(labels = scales::percent_format(accuracy=1),
                     expand = expansion(mult = c(0,0.05))) + 
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(type = "primary") +
  theme_FE +
  labs(title = paste0("How pressure rates change when you remove pressures (transparent bar) from each team's top pass rusher"),
       subtitle = "Each team's top pass rusher determined by total number of pressures on the season",
       caption = "Figure: @SamHoppen | Data: Trumedia",
       x = "Pressure Rate",
       y = NULL) +
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(size = 12),
        strip.background = element_rect(fill = 'gray90', color = 'black'),
        axis.text.y = element_nfl_logo(size = 0.35),
        panel.grid.major = element_blank(),
        legend.position = "none")+
  coord_cartesian(xlim = c(0.15, 0.5))

brand_nfl_plot(pressures_chart,
               logo = F,
               save_name = paste0("~/Documents/Charts/team_top_pressures.png"))
