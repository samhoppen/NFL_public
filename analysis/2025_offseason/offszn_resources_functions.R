sysfonts::font_add_google("Encode Sans Condensed", "encode", regular.wt = 400, bold.wt = 600)
sysfonts::font_add_google("Inconsolata", "incon")
showtext::showtext_auto()


brand_nfl_plot <- function(orig_plot, save_name, asp = 16/9, base_size = 5, tm_wordmarks = F, logo = F, 
                           logo_fp = F, logo_bp = F, logo_loc) {
  
  ## start by adding team wordmarks
  if (tm_wordmarks) {
    orig_plot_bld <- ggplot_gtable(ggplot_build(orig_plot))
    grob_strip_index <- which(sapply(orig_plot_bld$grob, function(x) x$name)=='strip')
    facet_id <- sapply(grob_strip_index, function(grb) {
      orig_plot_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
    })
    
    orig_plot_bld$layout$z[grob_strip_index] <- 0
    
    for (i in 1:length(facet_id)) {
      team_wd <- rasterGrob(image = image_read(nfl_wordmark_url(facet_id[i])), vp = viewport(height = 0.6))
      tot_tree <- grobTree(team_wd)
      
      orig_plot_bld$grobs[[grob_strip_index[i]]] <- tot_tree
    }
    orig_plot <- ggdraw(orig_plot_bld)
  }
  
  # aesthetics for various logos used
  if (logo_fp){
    logo_file <- magick::image_read("C:/Users/sphop/OneDrive/FantasyPros/Logos/fp_logo.jpg")
    logo_width <- 0.09*(5/base_size)
    logo_height <- 0.09*(5/base_size)
    logo_x <- 0.925
    logo_y <- 0.91+(0.01*(base_size-5))
  }
  if (logo_bp){
    logo_file <- magick::image_read("C:/Users/sphop/OneDrive/FantasyPros/Logos/bp_logo.jpg")
    logo_width <- 0.09*(5/base_size)
    logo_height <- 0.09*(5/base_size)
    logo_x <- 0.925
    logo_y <- 0.91+(0.01*(base_size-5))
  }
  
  
  if (logo) {
    if (tm_wordmarks){
      logo_x <- logo_x
      logo_y <- 0.92
    }
    
    orig_plot <- ggdraw(orig_plot) + draw_image(logo_file, x = logo_x, y = logo_y, hjust = 0, vjust = 0, height = logo_height, width = logo_width)
  }
  else{
    orig_plot <- ggdraw(orig_plot)
  }
  ggsave(save_name, orig_plot, dpi = 480, height = base_size, width = base_size * (asp))
  
}



tm_recap_chart_fn <- function(chart_team){
  
  if(chart_team == "LAR"){
    team_logo_url <- paste0("https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/LA.png")
  } else{
    team_logo_url <- paste0("https://raw.githubusercontent.com/ajreinhard/data-viz/master/wordmark/",chart_team,".png")
  }
  
  team_record <- records %>% 
    filter(team == chart_team)
  
  team_record <- team_record[[1,2]]
  
  df <- team_names %>% 
    filter(team_abbr == chart_team)
  
  full_team_name <- df[[1,2]]
  
  cap_space_chart <- cap_space %>% 
    left_join(team_colors,
              by = c("team_abbr" = "team_abbr")) %>% 
    mutate(color = ifelse(team_abbr == chart_team, NA, "b/w"),
           col_color = ifelse(team_abbr == chart_team, team_color2, "gray28"),
           col_fill = ifelse(team_abbr == chart_team, team_color, "gray87"),
           alpha = ifelse(team_abbr == chart_team, 0.9, 0.8),
           logo_size = ifelse(team_abbr == chart_team, 0.025, 0))
  
  cap_chart <- ggplot(data = cap_space_chart) + 
    geom_col(aes(x = cap_space, y = reorder(team_abbr, cap_space), fill = I(col_fill), color = I(col_color)), #, alpha = I(alpha)
             alpha = 0.9, linewidth = 0.1) +
    nflplotR::geom_nfl_logos(aes(team_abbr = team_abbr, x = cap_space, y = reorder(team_abbr, cap_space),
                                 color = I(color), alpha = I(alpha), width = I(logo_size)), alpha = 0.8) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02)),
                       breaks = scales::pretty_breaks(n=8),
                       labels = scales::dollar_format(accuracy = 1, scale = 1/1000000)) +
    labs(title = "Total cap space available ($M)",
         x =  NULL,
         y = NULL) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
          text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
          panel.border = element_blank(),     #makes the panel around the plotting area the color black
          panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
          axis.line = element_line(color = 'black', size = 0.5),
          axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
          axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
          axis.title = element_text(size = 24, margin = margin(t = 0, r = 0, b = 0, l = 0)),     #changes the size of the axis titles, if any
          axis.text = element_text(size = 24, color = 'black', margin = margin(t = 0, r = 0, b = 0, l = 0)),     #changes the size of the axis labels
          plot.title = element_text(size = 36, face = "bold", margin = margin(0,0,5,0), family = "encode"),     #changes the size of the title
          plot.subtitle = element_text(size = 18, margin = margin(0,0,5,0), family = "encode"),     #changes the size of the subtitle
          plot.caption = element_text(size = 18, family = "encode"),     #changes the size of the caption , family = "encode"
          legend.background = element_blank(),     #makes background of the legend to be grey
          legend.key = element_blank(),     #removes the legend key
          panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(size = 0.5, color = "gray"),
          axis.title.y = element_text(angle = 90, vjust = 0.5),     #changes the size of the axis labels
          strip.background = element_blank(),
          strip.text = element_text(size = 8, color = 'black'),
          panel.spacing.y = unit(0, 'lines'),
          panel.spacing.x = unit(0.5, 'lines'),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 6))
  
  draft_new <- draft_new %>% 
    left_join(team_colors,
              by = c("team" = "team_abbr")) %>% 
    mutate(tm_color = ifelse(team == chart_team, team_color2, "black"),
           tm_fill = ifelse(team == chart_team, team_color, "gray"),
           alpha = ifelse(team == chart_team, 0.9, 0.4))
  
  draft_chart <- ggplot(data = draft_new) + 
    geom_tile(aes(x = round_pick, y = round, fill = I(tm_fill)))+
    geom_text(aes(x = round_pick, y = round, label = ovr_pick, color = I(tm_color)),
              size = 6, fontface = "bold")+
    scale_x_continuous(expand = expansion(mult = c(0, 0))) +
    scale_y_reverse(breaks = c(1:7),
                    expand = expansion(mult = c(0, 0))) +
    labs(title = "Draft picks",
         x = NULL,
         y = "Round") +
    theme(
      panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      line = element_line(lineend = 'round', color='black'),     #rounds the edges of all lines; makes the color black
      text = element_text(color='black'),     #uses the Incon text format for all text; makes the color black
      panel.border = element_blank(),     #makes the panel around the plotting area the color black
      panel.background = element_rect(fill = 'white', color = 'transparent'),     #background of the non-plotting area is white
      axis.line = element_line(color = 'black', size = 0.5),
      axis.ticks = element_line(color = 'black', size = 0.5),     #changes the size (width) of the x-axis ticks
      axis.ticks.length = unit(0.15, 'lines'),     #changes the length of the axis ticks
      axis.title = element_text(size = 18),     #changes the size of the axis titles, if any
      axis.text = element_text(size = 18, color = 'black'),     #changes the size of the axis labels
      plot.title = element_text(size = 36, face = "bold", margin = margin(0,0,5,0), family = "encode"),     #changes the size of the title
      plot.subtitle = element_text(size = 18, margin = margin(0,0,5,0), family = "encode"),     #changes the size of the subtitle
      plot.caption = element_text(size = 18, family = "encode"),     #changes the size of the caption , family = "encode"
      legend.background = element_blank(),     #makes background of the legend to be grey
      legend.key = element_blank(),     #removes the legend key
      panel.grid.minor = element_blank(),     #removes the lines on the plot between the ticks
      panel.grid.major = element_blank(),
      axis.title.y = element_text(angle = 90, vjust = 0.5),     #changes the size of the axis labels
      strip.background = element_blank(),
      strip.text = element_text(size = 8, color = 'black'),
      panel.spacing.y = unit(0, 'lines'),
      panel.spacing.x = unit(0.5, 'lines'),
      legend.text = element_text(size = 6),
      legend.title = element_text(size = 6)
    )
  
  
  team_free_agents <- free_agents %>% 
    filter(team_abbr == chart_team) %>% 
    select(Player = player, Position = pos, Age = age, `Snap %` = snap_rate,
           `Current APY` = current_apy, `2024 PFF Grade` = grade) #, `Current Guarantees` = guarantees
  
  tot_fas <- nrow(team_free_agents)
  
  t1 <- ttheme_default(
    colhead=list(fg_params = list(fontsize = 25)),
    core=list(
      fg_params=list(fontsize = 25),
      bg_params = list(fill=c(rep(c("grey95", "grey90"),length.out=tot_fas)),
                       alpha = rep(c(1,0.5), each=5))
    ))

  fa_table <- gridExtra::tableGrob(team_free_agents,
                                   rows = NULL,
                                   theme = t1)
  
  
  fa_widths <- rep(grid::unit(1.75, "cm"), length(fa_table$widths))
  fa_widths[1] <- grid::unit(2.1, "cm")
  fa_table$widths <- fa_widths
  
  fin_plot <- ggdraw(
    xlim = c(0, 160),
    ylim = c(0, 90)
  ) +
    draw_image(
      image = team_logo_url,
      x = 4.5,
      y = 73,
      height = 10,
      width = 10.5 * 3
    ) +
    draw_text(
      text = paste0(full_team_name, " offseason resource dashboard"),
      x = 4.5,
      y = 87.5,
      size = 70,
      hjust = 0,
      vjust = 1,
      family = "encode", fontface = "bold", color = "#313131"
    ) +
    draw_text(
      text = paste0("2024 record: ", team_record),
      x = 4.5,
      y = 72.5,
      size = 45,
      hjust = 0,
      vjust = 1
    ) +
    draw_text(
      text = "Key Free Agents",
      x = 30,
      y = 63,
      size = 55,
      hjust = 0,
      vjust = 1
    ) +
    draw_grob(# BOTTOM LEFT CHART
      fa_table,
      x = 40,
      hjust = 0.5,
      width = 90,
      y = 29.5+(1.95*(15-tot_fas)),
      # height = 80,
      vjust = 1
    ) +
    draw_plot(# TOP RIGHT CHART 
      cap_chart,
      x = 120,
      hjust = 0.5,
      width = (80),
      y = 60,
      height = (50),
      vjust = 0.5
    ) +
    draw_plot(# BOTTOM RIGHT CHART 
      draft_chart,
      x = 120,
      hjust = 0.5,
      width = (80),
      y = 17.5, # 22.5
      height = (35), #45
      vjust = 0.5
    ) +
    draw_text(
      text = "Sources: Over the Cap, nflfastR, PFF | Figure: @SamHoppen",
      x = 130,
      y = 1,
      size = 20,
      hjust = 0,
      vjust = 1
    )
  
  brand_nfl_plot(fin_plot,
                 logo = F,
                 save_name = paste0("~/Documents/Charts/team_recap_charts/", chart_team," season recap.png"))
  
  
}
