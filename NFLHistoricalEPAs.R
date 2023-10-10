#Team EPA Histories

#packages
library(nflverse)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(ggthemes)

#My favorite theme (only in ggthemes)
theme_set(theme_fivethirtyeight())

colors <- teams_colors_logos %>%
  select(team_color)

#Creates rush epa horizontal analysis for each team
repa <- load_pbp(2012:2022) |>
  filter(rush == 1, !is.na(epa)) |>
  select(season, desc, rusher, defteam, yards_gained, epa) |>
  group_by(season) |>
  transmute(mean_repa = aggregate(x = epa,
                                  by = list(season),
                                  FUN = mean)) |>
    distinct()

for(posteam in repa) {
  rush <- ggplot(repa, aes(x = season, y = mean_repa$x)) +
    geom_point(color = ) +
    geom_smooth(method = 'loess', color = 'black') +
    scale_x_continuous(name = 'Season', breaks = 2012:2022) +
    scale_y_continuous(name = 'EPA/Play') +
    geom_hline(yintercept = mean(repa$mean_repa$x), linetype = 2) +
    labs(title = "The Last 11 Seasons of " + posteam + " Rushing with EPA/Play",
         subtitle = "2012-2022 Seasons")
  
  return(rush)
}