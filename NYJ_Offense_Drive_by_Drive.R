library(tidyverse)
library(nflverse)
library(ggplot2)
library(ggrepel)
library(lubridate)

#Load data, create a new column with drive time in seconds (instead of mm:ss).
data <- load_pbp(2021) %>%
  mutate(drive_time = period_to_seconds(ms(drive_time_of_possession)))

#Isolate Jets data.
jdata <- data %>%
  filter(posteam == "NYJ", season_type == "REG")

#Separate into each drive within each game for each week.
drive1 <- jdata %>%
  filter(drive == 1 | drive == 2) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive2 <- jdata %>%
  filter(drive == 3 | drive == 4) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive3 <- jdata %>%
  filter(drive == 5 | drive == 6) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive4 <- jdata %>%
  filter(drive == 7 | drive == 8) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive5 <- jdata %>%
  filter(drive == 9 | drive == 10) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive6 <- jdata %>%
  filter(drive == 11 | drive == 12) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive7 <- jdata %>%
  filter(drive == 13 | drive == 14) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

drive8 <- jdata %>%
  filter(drive == 15 | drive == 16) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

#Not every game has the same number of drives, so isolating final drives will
#take some extra work.
fdrive <- jdata %>%
  filter(drive == 17:26) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

#Drive Time Each Quarter by Week
start <- jdata %>%
  filter(qtr == 1 & qb_kneel == 0) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

second <- jdata %>%
  filter(qtr == 2 & qb_kneel == 0) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

third <- jdata %>%
  filter(qtr == 3 & qb_kneel == 0) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

clutch <- jdata %>%
  filter(qtr == 4 & qb_kneel == 0) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

#Since the number of drives varies each game, remove all non-final drive rows.
fdrive <- fdrive[-c(1:3,5:7,10,13,15,16,18,19,21:23,25,27,30,35,36), ]


#Opening Drives by number of plays.
open <- ggplot(drive1, aes(week,drive_play_count,fill = drive_play_count))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  scale_y_continuous(name = "Number of Plays",breaks = 1:10)+
  labs(title = "Plays Run on Opening Drives by Week", subtitle = "2021 Jets Offense", 
       x = "Week", y = "Number of Plays", fill = "Plays",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Final Drives by number of plays.
final <- ggplot(fdrive, aes(week,drive_play_count,fill = drive_play_count))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  scale_y_continuous(name = "Number of Plays",breaks = 1:15)+
  labs(title = "Plays Run on Final Drives by Week", subtitle = "2021 Jets Offense", 
       x = "Week", y = "Number of Plays", fill = "Plays",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Q1 Drives (All are by time of possession)
q1 <- ggplot(start, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q1 Time of Possession in Seconds by Week", subtitle = "2021 
       Jets Offense", x = "Week", y = "Q1 Time of Possession (seconds)", 
       fill = "Q1 TOP", caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Q2 Drives
q2 <- ggplot(second, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q2 Time of Possession in Seconds by Week", subtitle = "2021 
       Jets Offense", x = "Week", y = "Q2 Time of Possession (seconds)", 
       fill = "Q2 TOP", caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Q3 Drives
q3 <- ggplot(third, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q3 Time of Possession in Seconds by Week", subtitle = "2021 
       Jets Offense", x = "Week", y = "Q3 Time of Possession (seconds)", 
       fill = "Q3 TOP", caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Q4 Drives
q4 <- ggplot(clutch, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q4 Time of Possession in Seconds by Week", subtitle = "2021 
       Jets Offense", x = "Week", y = "Q4 Time of Possession (seconds)", 
       fill = "Q4 TOP", caption = "Created by Oliver Cochrane (@OliverJCochrane) for Jets X-Factor (@jetsxfactor) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

#Combine Q1-4 plots into single figure
ggarrange(q1, q2, q3, q4, 
          ncol = 2, nrow = 2)

#Combine opening and final drive plots together too.
ggarrange(open,final,
          ncol = 1,nrow = 2)

#Jets vs League Offensive Averages
mppd <- mean(jdata$drive_play_count)
mdt <-mean(jdata$drive_time)
nfl_mppd <- mean(data$drive_play_count,na.rm = TRUE)
nfl_mdt <- mean(data$drive_time,na.rm = TRUE)
fdpd <- mean(fdrive$drive_first_downs)
