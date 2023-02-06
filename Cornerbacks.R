library(tidyverse)
library(nflverse)
library(ggrepel)
library(ggimage)
library(ggthemes)

theme_set(theme_fivethirtyeight())

#Import PFF Data
plot_cb <- read_csv('~/Desktop/DS/csv/PFF/PFF CBs (2022).csv')

plot_cb <- plot_cb |>
  filter(position == 'CB', snap_counts_coverage >= 400) |>
  mutate(
    interception_per_cover_snap = interceptions/snap_counts_coverage,
    forced_incompletion_rate = pass_break_ups/targets,
    int_per_target = interceptions/targets,
    targets_per_cover_snap = targets/snap_counts_coverage,
    yards_per_coverage_snap = yards/snap_counts_coverage) |>
  arrange(-grades_coverage_defense) |>
  slice(1:50)

#Create Uniform Team Abbreviations (Must do BEFORE joining dfs)
plot_cb[plot_cb == 'HST'] <- 'HOU'
plot_cb[plot_cb == 'ARZ'] <- 'ARI'
plot_cb[plot_cb == 'CLV'] <- 'CLE'
plot_cb[plot_cb == 'BLT'] <- 'BAL'

plot_cb <- plot_cb |>
  left_join(teams_colors_logos, by = c('team_name' = 'team_abbr'))

#Isolate Jets players
jets <- plot_cb %>% 
  filter(player == 'Sauce Gardner' | 
           player == 'D.J. Reed Jr.' | 
           player == 'Michael Carter II')

#Forced Incompletion Rate vs INTs per Target of all qualifiers 
ggplot(plot_cb, aes(forced_incompletion_rate, int_per_target, label = player))+
  geom_hline(yintercept = mean(plot_cb$int_per_target), color = "darkblue", linetype = 'dashed') +
  geom_vline(xintercept = mean(plot_cb$forced_incompletion_rate), color = "darkblue", linetype = 'dashed') +
  geom_label_repel(size = 4, max.overlaps = 50, color = ifelse(plot_cb$team_name == 'NYJ', 'darkgreen', plot_cb$team_color)) +
  labs(title = "Forced Incompletion Rate vs. INTs/Target",
       subtitle = "50 Highest-Graded CBs in Coverage with 400+ Coverage Snaps, 2022 Regular Season",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "Forced Incompletion Rate",
       y = "Interceptions per Target") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())

#Targets/Cover Snap vs Yards/Cover Snap of all qualifiers
ggplot(plot_cb, aes(targets_per_cover_snap, yards_per_coverage_snap, label = player)) +
  geom_smooth(method = 'lm', color = 'white', alpha = 0.1) +
  geom_hline(yintercept = mean(plot_cb$yards_per_coverage_snap), color = "darkblue", linetype = 'dashed') +
  geom_vline(xintercept = mean(plot_cb$targets_per_cover_snap), color = "darkblue", linetype = 'dashed') +
  geom_label_repel(size = 4, max.overlaps = 50, color = ifelse(plot_cb$team_name == 'NYJ', 'darkgreen', plot_cb$team_color)) +
  labs(title = "Targets/Cover Snap vs. Yards/Cover Snap",
       subtitle = "50 Highest-Graded CBs in Coverage with 400+ Coverage Snaps, 2022 Season",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "Targets per Cover Snap",
       y = "Yards per Cover Snap") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())

#INT/Target vs Yards/Cover Snap
ggplot(plot_cb, aes(yards_per_coverage_snap, int_per_target, label = player))+
  geom_hline(yintercept = mean(plot_cb$int_per_target), color = "darkblue", linetype = 'dashed') +
  geom_vline(xintercept = mean(plot_cb$yards_per_coverage_snap), color = "darkblue", linetype = 'dashed') +
  geom_label_repel(size = 4, max.overlaps = 40, color = ifelse(plot_cb$team_name == 'NYJ', 'darkgreen', plot_cb$team_color)) +
  labs(title = "Ball-Hawks vs Cover Corners in the NFL",
       subtitle = "50 Highest-Graded CBs in Coverage with 400+ Coverage Snaps, 2022 Regular Season",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "Yards per Cover Snap",
       y = "Interceptions per Target") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())

