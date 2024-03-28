# Imports
library(tidyverse)
library(nflverse)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(ggpubr)

# Load data, create a new column with drive time in seconds (instead of mm:ss).
data <- load_pbp(2023) %>%
  mutate(drive_time = period_to_seconds(ms(drive_time_of_possession)))

# Isolate Jets data.
jdata <- data %>%
  filter(posteam == "NYJ", season_type == "REG", week != 7, !is.na(drive_time))

# Isolate opening drive data
drive1 <- jdata %>%
  filter(drive == 1 | drive == 2) %>%
  select(week, drive_time, drive_play_count, drive_first_downs) %>%
  distinct()

# Not every game has the same number of drives, so isolating final drives will take some extra work.
fdrive <- jdata %>%
  filter(drive >= 17 & qb_kneel == 0) %>% # View to find range of total num of drives
  select(week, drive_time, drive_play_count, drive_first_downs, touchdown, qb_kneel) %>%
  distinct()

# Since the number of drives varies each game, remove all non-final drive rows (some data error too).
fdrive <- fdrive[-c(1,2,4,5,6,8:14,16,17,19:21,23:25,27:35,37:40,42:44,46:48,50,
                    51,53:56,58:62,64:66,68:75,77:82,84:89,91,92), ]

# Another collection error to manually drop
drive1 <- drive1[-6,]

# Drive Time Each Quarter by Week
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

# Opening Drives by number of plays.
open <- ggplot(drive1, aes(week,drive_play_count,fill = drive_play_count))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  scale_y_continuous(name = "Number of Plays",breaks = 1:10)+
  labs(title = "Plays Run on Opening Drives by Week", subtitle = "2023 Jets Offense", 
       x = "Week", y = "Number of Plays", fill = "Plays",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Final Drives by number of plays.
final <- ggplot(fdrive, aes(week,drive_play_count,fill = drive_play_count))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  scale_y_continuous(name = "Number of Plays",breaks = 1:15)+
  labs(title = "Plays Run on Final Drives by Week", subtitle = "2023 Jets Offense", 
       x = "Week", y = "Number of Plays", fill = "Plays",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Q1 Drives (All are by time of possession)
q1 <- ggplot(start, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q1 Time of Possession in Seconds by Week", subtitle = "2023 
       Jets Offense", x = "Week", y = "Q1 Time of Possession (seconds)", 
       fill = "Q1 TOP") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Q2 Drives
q2 <- ggplot(second, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q2 Time of Possession in Seconds by Week", subtitle = "2023 
       Jets Offense", x = "Week", y = "Q2 Time of Possession (seconds)", 
       fill = "Q2 TOP") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Q3 Drives
q3 <- ggplot(third, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q3 Time of Possession in Seconds by Week", subtitle = "2023 
       Jets Offense", x = "Week", y = "Q3 Time of Possession (seconds)", 
       fill = "Q3 TOP") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Q4 Drives
q4 <- ggplot(clutch, aes(week,drive_time,fill = drive_time))+
  geom_col(color = "black")+
  geom_smooth(method = loess, color = "darkgreen")+
  scale_x_continuous(name = "Week",breaks = 1:18)+
  labs(title = "Q4 Time of Possession in Seconds by Week", subtitle = "2023 
       Jets Offense", x = "Week", y = "Q4 Time of Possession (seconds)", 
       fill = "Q4 TOP", caption = "Created by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastR") + 
  scale_fill_distiller(palette = "Greens")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Combine Q1-4 plots into single figure
ggarrange(q1, q2, q3, q4, 
          ncol = 2, nrow = 2)

# Combine opening and final drive plots together too.
ggarrange(open,final,
          ncol = 1,nrow = 2)

# Jets vs League Offensive Averages
mppd <- mean(jdata$drive_play_count)
mdt <-mean(jdata$drive_time)
nfl_mppd <- mean(data$drive_play_count, na.rm = TRUE)
nfl_mdt <- mean(data$drive_time, na.rm = TRUE)
fdpd <- mean(data$drive_first_downs)

# Visualize Jets vs League ppd
df <- data.frame(
  groups = c("NYJ", "NFL"),
  ppd = c(mppd, nfl_mppd)
)

# Plot the grouped column chart
stat1 <- ggplot(df, aes(x = groups, y = ppd, fill = ppd)) +
  geom_col(color = 'black') +
  labs(title = "NYJ Average Plays per Drive vs. NFL Average (2023)",
       x = "",
       y = "Average Plays per Drive",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastR") +
  scale_fill_distiller(palette = "Greens") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Visualize Jets vs League drive time
df2 <- data.frame(
  groups = c("NYJ", "NFL"),
  mdt = c(mdt, nfl_mdt)
)

# Plot the grouped column chart
stat2 <- ggplot(df2, aes(x = groups, y = mdt, fill = mdt)) +
  geom_col(color = 'black') +
  labs(title = "NYJ Average Drive Time vs. NFL Average (2023)",
       x = "",
       y = "Average Drive Time (seconds)",
       caption = "Created by Oliver Cochrane (@OliverJCochrane) | Data via @nflfastR") +
  scale_fill_distiller(palette = "Greens") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text(face = "bold"))

# Combine charts into single figure
avgs <- ggarrange(stat1, stat2, ncol = 2, nrow = 1)
# ggsave("FirstLastDrives_2023.png", device = 'png', height = 10, width = 14, units = "in", dpi = 'retina')

# Seeing how the Jets stack up against the entire NFL
mean_drive_time <- aggregate(drive_time ~ posteam, data = data, FUN = mean) %>% 
  arrange(desc(drive_time))

mean_drive_plays <- aggregate(drive_play_count ~ posteam, data = data, FUN = mean) %>% 
  arrange(desc(drive_play_count))

mean_first_downs <- aggregate(drive_first_downs ~ posteam, data = data, FUN = mean) %>% 
  arrange(desc(drive_first_downs))
