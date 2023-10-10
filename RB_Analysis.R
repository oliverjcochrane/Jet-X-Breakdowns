library(nflverse)
library(tidyverse)
library(ggpubr)
library(ggrepel)
library(ggthemes)

#My favorite theme (only in ggthemes)
theme_set(theme_fivethirtyeight())

rbs <- load_pbp(2023) %>%
  filter(rusher_jersey_number == 32 | rusher_jersey_number == 20 | rusher_jersey_number == 33,
         posteam == "NYJ", !is.na(run_location)) %>%
  mutate(rb = rusher) %>%
  group_by(run_location) %>%
  select(rusher, run_location, yards_gained, desc)

rs <- ggplot(rbs, aes(y = yards_gained, x = rusher, fill = run_location)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Rush Yards Accumulated by Running Direction",
       subtitle = "2023 Season",
       y = "Rush Yards",
       x = "Rusher",
       caption = "Visualized by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastr",
       fill = "Rush Location") +
  scale_fill_manual(values = c("right" = "#69CE64",
                               "middle" = "#10CE10",
                               "left" = "#2B9011")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))

#Now for Passing
pass_spread <- load_pbp(2023) %>%
  filter(receiver_jersey_number == 17 | receiver_jersey_number == 10 | 
           receiver_jersey_number == 18 |receiver_jersey_number == 83,
         posteam == "NYJ", !is.na(pass_location)) %>%
  mutate(wr = receiver) %>%
  group_by(pass_location) %>%
  select(receiver, pass_location, yards_gained, desc)

ps <- ggplot(pass_spread, aes(y = yards_gained, x = receiver, fill = pass_location)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Receiving Yards Accumulated by Passing Location",
       subtitle = "2023 Season",
       y = "Receiving Yards",
       x = "Receiver",
       caption = "Visualized by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastr",
       fill = "Pass Location") +
  scale_fill_manual(values = c("right" = "#69CE64",
                               "middle" = "#10CE10",
                               "left" = "#2B9011")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1))

gg1 <- ggarrange(rs,ps,ncol = 1,nrow = 2)

#PFF RB Data for 2022
pff <- read_csv("~/Desktop/DS/csv/PFF/PFF RBs (Updated W5).csv") #PFF Premium HB Rushing Data

tcl <- teams_colors_logos

#Be mindful of qualifiers here:
pfflg <- pff |>
  filter(attempts >= 25, receptions >= 5)

#PFF has different abbreviations for Browns, Texans, and Cardinals. Make them uniform.
pfflg[pfflg == "CLV"] <- 'CLE'
pfflg[pfflg == "HST"] <- 'HOU'
pfflg[pfflg == "ARZ"] <- 'ARI'

pfflg <- pfflg |>  
  left_join(tcl, by = c('team_name' = 'team_abbr'))

#YAC Viz
gg2 <- ggplot(pfflg, aes(yco_attempt, yards_after_contact, label = player)) +
  annotate('text', x = 6, y = 100, label = 'Unproductive & Efficient' ) +
  annotate('text', x = 2.5, y = 500, label = 'Productive & Inefficient') +
  annotate('text', x = 2.5, y = 100, label = 'Unproductive & Inefficient') +
  annotate('text', x = 6, y = 500, label = 'Productive & Efficient') +
  geom_smooth(method = 'lm', color = 'white', alpha = 0.1) +
  geom_hline(yintercept = mean(pfflg$yards_after_contact), color = "darkgreen", linetype = 'dashed') +
  geom_vline(xintercept = mean(pfflg$yco_attempt), color = "darkblue", linetype = 'dashed') +
  geom_point(size = (pfflg$attempts)*0.05, color = pfflg$team_color) +
  geom_label_repel(size = 3, max.overlaps = 37) +
  labs(title = "Yards After Contact: Efficiency vs Production of NFL RBs",
       subtitle = "2023 Season",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "YAC Efficiency",
       y = "Total YAC") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())

#General RB Production
gg3 <- ggplot(pfflg, aes(ypa, yprr, label = player)) +
  geom_smooth(method = 'lm', color = 'white', alpha = 0.1) +
  annotate('text', x = 9, y = 0.5, label = 'Pure Rusher' ) +
  annotate('text', x = 3.75, y = 1.8, label = 'Receiving Back') +
  annotate('text', x = 3.75, y = 0.5, label = 'Overall Unimpressive') +
  annotate('text', x = 9, y = 1.8, label = 'Complete Back') +
  geom_hline(yintercept = mean(pfflg$yprr), color = "darkgreen", linetype = 'dashed') +
  geom_vline(xintercept = mean(pfflg$ypa), color = "darkblue", linetype = 'dashed') +
  geom_point(size = (pfflg$attempts)*0.05, color = pfflg$team_color) +
  geom_label_repel(size = 3) +
  labs(title = "Efficiency Metrics: Receiving vs Rushing of NFL RBs",
       subtitle = "2023 Season",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "Yards per Carry",
       y = "Yards per Route Run") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())