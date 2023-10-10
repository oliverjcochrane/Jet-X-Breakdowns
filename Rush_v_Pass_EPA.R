library(nflverse)
library(tidyverse)
library(ggpubr)
library(ggimage)
library(ggrepel)
library(ggthemes)

theme_set(theme_fivethirtyeight())

reff <- load_pbp(2022) |>
  filter(rush == 1, !is.na(epa)) |>
  group_by(posteam) |>
  summarize(rushes = n(),
            rush_epa = mean(epa))

peff <- load_pbp(2022) |>
  filter(week == ifelse(posteam == 'NYJ', c(12:14, 17), c(1:13)), pass == 1, !is.na(epa)) |>
  group_by(posteam) |>
  summarize(passes = n(),
            pass_epa = mean(epa))

teameff <- reff |>
  left_join(peff, by = 'posteam')

teameff <- teameff |>
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

ggplot(teameff, aes(rush_epa, pass_epa)) +
  geom_hline(yintercept = mean(teameff$pass_epa, color = 'black', linetype = 2)) +
  geom_vline(xintercept = mean(teameff$rush_epa, color = 'black', linetype = 2)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  labs(title = 'Rush Efficiency vs Pass Efficiency Among NFL Teams',
       subtitle = 'Weeks 12-13 (Jets: White Starts), 2022 Season',
       x = 'Rush EPA per Play',
       y = 'Pass EPA per Play',
       caption = 'Visualized by Oliver Cochrane (@OliverJCochrane)')  +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())