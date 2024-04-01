# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

library(tidyverse)
library(styler)
library(ggthemes)
library(dplyr)

#Used data from rotowire (they provided data in csv form)
pitcher_data <- read.csv("mlb-player-stats-P.csv")

# -------------------------------------------------------------------------
#Creates a variable indicating how many games a pitcher starts
pitcher_data <- pitcher_data |> 
                  filter(IP > 100) |> 
                  mutate(gstart_ratio = GS / G)

#For pitchers win percentage
pitcher_data <- pitcher_data |> 
                  mutate(win_loss_percentage = (W/(W + L)))

#Creates a variable in which a pitcher does not acquire a win or loss
pitcher_data <- pitcher_data |> 
                  mutate(No_decision = (G - (W + L)))

#Creates a percentage of games in which a pitcher gets a no decision
pitcher_data <- pitcher_data |> 
                  mutate(No_dec_percentage = No_decision/G)

#Creates a percentage in which a pitcher wins or gets a no decision
pitcher_data <- pitcher_data |> 
                  mutate(WND_percentage = (No_decision + W)/G)

#Creates Age category (Could be used as a categorical variable)
pitcher_data <- pitcher_data |> 
                  mutate(a_range = ifelse(Age <= 25, "25 or Younger", ifelse(Age <= 30, "26 - 30", ifelse(Age <= 35, "31 - 35", ifelse(Age <= 40, "36 - 40", "41+")))))

#Creates Division variable (to show data differences among divisions)
pitcher_data <- pitcher_data |> 
                  mutate(div = ifelse(Team == "ATL"|Team == "WAS"|Team == "NYM"|Team == "PHI"|Team == "MIA", "NL East", ifelse(Team == "CHC"|Team == "CIN"|Team == "PIT"|Team == "STL"|Team == "MIL", "NL Central", ifelse(Team == "SF"|Team == "SD"|Team == "LAD"|Team == "COL"|Team == "ARI", "NL West", ifelse(Team == "NYY"|Team == "TOR"|Team == "BOS"|Team == "BAL"|Team == "TB", "AL East", ifelse(Team == "CWS"|Team == "KC"|Team == "DET"|Team == "MIN"|Team == "CLE", "AL Central", "AL West"))))))

#Eliminates possible field players 
cleaner_pitcher_data <- pitcher_data |> 
                          filter( ERA < 11 & ERA > 0 & G >= 5 & W + L > 0 & IP > 20)

#Starting pitcher data
starters <- pitcher_data |> 
              filter(gstart_ratio >= 0.5 & GS >= 5 & G >= 10)

#Relief pitchers 
relievers <- pitcher_data |> 
              filter(GS < 10 & gstart_ratio < 0.5 & G > 10 & SV < 7)

#Closers
closers <- pitcher_data |> 
            filter(SV > 6 & HLD < 10)

# -------------------------------------------------------------------------

#Creates a scatterplot using a modified data set, provides a single regression line for all pitchers with 100+ IP (non-smoothed)
ggplot(cleaner_pitcher_data, aes(x = win_loss_percentage, y = ERA)) +
  geom_point(aes(color = div)) +
  geom_smooth(method = "lm") +
  labs(x = "Win-Loss Percentage", 
       y = "Earned Run Average", color = "Division") +
  theme(plot.title = element_text(hjust = 0.5, size = 24), 
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14))


#Creates a scatterplot using a modified data set, provides a single regression line for all pitchers with 100+ IP (smoothed)
ggplot(starters, aes(x = win_loss_percentage, y = ERA)) + 
  geom_point(aes(color = div)) +
  geom_smooth(se = FALSE) +
  labs(x = "Win-Loss Percentage", y = "Earned Run Average", color = "Division") +
  theme(plot.title = element_text(hjust = 0.5, size = 24), 
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14))


#Creates a bar graph of pitchers wins and shows the differences in division
ggplot(starters, aes(W, fill = div)) +
  geom_bar() +
  labs(title = "Win distribution among pitchers", subtitle = "Among starting pitchers", x = "Wins", y = "Amount of Pitchers", fill = "Division") +
  theme(plot.title = element_text(hjust = 0.5, size = 24), 
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14))

# ERA range distribution among starting pitchers
era_ranges <- c(2.01, 3.25, 4.25, 5.00, Inf)

starters <- starters %>%
  mutate(ERA_Range = cut(ERA, breaks = era_ranges,
                         labels = c("2.01-3.25", "3.26-4.25", "4.26-5.00", "5.00+"),
                         include.lowest = TRUE))

ggplot(starters, aes(x = ERA_Range, fill = div)) +
  geom_bar() +
  labs(title = "ERA Range Distribution",
       subtitle = "Among starting pitchers",
       x = "ERA Range Category",
       y = "Amount of pitchers",
       fill = "Division") +
  theme(plot.title = element_text(hjust = 0.5, size = 24), 
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14))


#Boxplot with wins and ERA as grouped by IP intervals
custom_labels <- c("(0, 100]" = "0-100 IP",
                   "(100, 150]" = "100-150 IP",
                   "(150, 200]" = "150-200 IP",
                   "(200, Inf]" = "200+ IP")

player_data <- cleaner_pitcher_data |>
  mutate(IP_interval = cut(IP, breaks = c(0, 100, 150, 200, Inf), labels = custom_labels)) |> 
    group_by(IP_interval)
  
ggplot(player_data, aes(x = ERA, y = W, fill = IP_interval)) +
  geom_boxplot() +
  facet_wrap(~ IP_interval) +
  labs(title = "ERA/W-L Correlation as grouped by IP intervals",
       subtitle = "Pitchers with 100+ IP",
       x = "ERA",
       y = "Number of Wins",
       fill = "IP Interval") +
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title = element_text(size = 14),
      strip.text = element_text(size = 20))

# Boxplot with W% and ERA as grouped by IP intervals

custom_labels <- c("(0, 100]" = "0-100 IP",
                   "(100, 150]" = "100-150 IP",
                   "(150, 200]" = "150-200 IP",
                   "(200, Inf]" = "200+ IP")

player_data <- cleaner_pitcher_data |>
  mutate(IP_interval = cut(IP, breaks = c(0, 100, 150, 200, Inf), labels = custom_labels)) |> 
  group_by(IP_interval)

ggplot(player_data, aes(x = ERA, y = win_loss_percentage, fill = IP_interval)) +
  geom_boxplot() +
  facet_wrap(~ IP_interval) +
  labs(title = "ERA/W-L Correlation as grouped by IP intervals",
       subtitle = "Pitchers with 100+ IP",
       x = "ERA",
       y = "Win-Loss Percantage",
       fill = "IP Interval") +
  theme(axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 20))

