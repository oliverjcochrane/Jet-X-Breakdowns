#WR Breakdown!
library(tidyverse)
library(nflverse)
library(ggplot2)
library(ggimage)
library(ggrepel)
library(ggthemes)

theme_set(theme_fivethirtyeight())

#Import passing data from NFLfastR
wrs <- load_pbp(2022) %>% 
  filter(week <= 18, pass == 1, down <= 4, 
         !is.na(receiver_player_id), !is.na(air_yards), !is.na(cpoe))

#Filter out the Jets' receivers in a separate data frame
jetwrs <- wrs %>%
  filter(posteam == "NYJ", receiver_player_name %in% 
           c("G.Wilson", "C.Davis", "E.Moore", "D.Mims", "B.Berrios")) %>% 
  summarize(avg_air_yards = mean(jetwrs$air_yards),
            avg_cpoe = mean(jetwrs$cpoe))

#Store individual receiver sets to find averages
gw <- jetwrs %>%
  filter(receiver == "G.Wilson")
gw <- gw %>%
  mutate(
    avg_air_yards = mean(gw$air_yards),
    avg_cpoe = mean(gw$cpoe)
  ) %>%
  as_tibble()

cd <- jetwrs %>%
  filter(receiver == "C.Davis")
cd <- cd %>%
  mutate(
    avg_air_yards = mean(cd$air_yards),
    avg_cpoe = mean(cd$cpoe)
  ) %>%
  as_tibble()

em <- jetwrs %>%
  filter(receiver == "E.Moore")
em <- em %>%
  mutate(
    avg_air_yards = mean(em$air_yards),
    avg_cpoe = mean(em$cpoe)
  ) %>%
  as_tibble()

dm <- jetwrs %>%
  filter(receiver == "D.Mims")
dm <- dm %>%
  mutate(
    avg_air_yards = mean(dm$air_yards),
    avg_cpoe = mean(dm$cpoe)
  ) %>%
  as_tibble()

bb <- jetwrs %>%
  filter(receiver == "B.Berrios")
bb <- bb %>%
  mutate(
    avg_air_yards = mean(bb$air_yards),
    avg_cpoe = mean(bb$cpoe)
  ) %>%
  as_tibble()

#Now combine rows into a single data frame
data1 <- rbind(gw, cd, em, dm, bb) %>%
  select(receiver, avg_air_yards, avg_cpoe) %>%
  distinct()

# First plot - Average Air Yards vs CPOE
ggplot(data1, aes(avg_air_yards, avg_cpoe, label = receiver))+
  geom_point(size = 0.5) +
  geom_label_repel(size = 4) +
  geom_hline(yintercept = mean(wrs$cpoe), color = "darkblue", linetype = 2) +
  geom_vline(xintercept = mean(wrs$air_yards), color = "darkblue", linetype = 2) +
  labs(title = "Air Yards vs CPOE of Jets' WRs (2022)",
       subtitle = "Jets' Top-5 Receivers by Receiving Yards",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @NFLfastR",
       x = "Average Air Yards",
       y = "Average CPOE") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  xlim(0,20) +
  ylim(-25,25)

#The next plot will also draw on data from Pro Football Focus (PFF)
wrspff <- read.csv("~/Desktop/DS/csv/PFF/PFF WRs (Updated W17).csv")

#Set qualifications for dataset
wrspff <- wrspff %>%
  filter(targets >= 100, position == "WR", !is.na(contested_catch_rate))

#Filter a separate version of just Jets' receivers
jetpff <- wrspff %>%
  filter(team_name == "NYJ")

#Second Plot - Contested Catch Rate vs YAC per Reception
ggplot(wrspff, aes(contested_catch_rate, yards_after_catch_per_reception, label = player)) +
  geom_hline(yintercept = mean(wrspff$yards_after_catch_per_reception), color = "darkgreen", linetype = 2) +
  geom_vline(xintercept = mean(wrspff$contested_catch_rate), color = "darkblue", linetype = 2) +
  geom_point(size = 0.5) +
  geom_label_repel(size = 4) +
  labs(title = "Contested Catch Rate vs YAC per Reception of Jets' WRs (2022)",
       subtitle = "Jets' Receivers with 100+ Targets",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) | Data via @PFF",
       x = "Contested Catch Rate",
       y = "YAC per Reception") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text())

#Third Plot - PFF Offense Grade vs QB Rating when Targeted
ggplot(wrspff, aes(grades_offense, targeted_qb_rating, label = player)) +
  geom_hline(yintercept = mean(wrspff$targeted_qb_rating), color = "darkgreen", linetype = 2) +
  geom_vline(xintercept = mean(wrspff$grades_offense), color = "darkblue", linetype = 2) +
  geom_point(size = 0.5) +
  geom_label_repel(size = 3) +
  labs(title = "PFF Offense Grade vs QB Rating when Targeted of WRs (2022)",
       subtitle = "Receivers with 30+ Targets",
       caption = "Built by Oliver Cochrane (@OliverJCochrane) of Jets X-Factor (@jetsxfactor) and NYJ Stats Hub (@nyjetsbyR) | Data via @PFF",
       x = "PFF Offensive Grade",
       y = "QB Rating when Targeted") +
  theme(axis.title.x = element_text(),
        axis.title.y = element_text()) +
  xlim(0,100) +
  ylim(0,158.3)