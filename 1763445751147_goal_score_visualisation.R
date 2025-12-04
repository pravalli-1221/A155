getwd()

df <- read.csv("goalscorers.csv")
install.packages("ggplot2")
install.packages("dplyr")

library(ggplot2)
library(dplyr)


# Count goals by team type
goal_counts <- df %>%
  mutate(team_type = ifelse(team == home_team, "Home", 
                            ifelse(team == away_team, "Away", NA))) %>%
  group_by(team_type) %>%
  summarise(total_goals = n())

ggplot(goal_counts, aes(x = team_type, y = total_goals, fill = team_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Goals Scored: Home vs Away Teams",
       x = "Team Type",
       y = "Number of Goals") +
  theme_minimal()
#Boxplot: Distribution of Goal Minutes (Home vs Away)

goal_minutes <- df %>%
  mutate(team_type = ifelse(team == home_team, "Home",
                            ifelse(team == away_team, "Away", NA)))

ggplot(goal_minutes, aes(x = team_type, y = minute, fill = team_type)) +
  geom_boxplot() +
  labs(title = "Distribution of Goal Scoring Minutes (Home vs Away)",
       x = "Team Type",
       y = "Goal Minute") +
  theme_minimal()
#Side-by-Side Bar Plot per Match
install.packages("tidyr")
library(tidyr)

match_goals <- df %>%
  mutate(team_type = ifelse(team == home_team, "Home", "Away")) %>%
  group_by(date, home_team, away_team, team_type) %>%
  summarise(goals = n()) %>%
  tidyr::pivot_wider(names_from = team_type, values_from = goals, values_fill = 0)

ggplot(match_goals, aes(x = Home, y = Away)) +
  geom_point() +
  labs(title = "Goals Scored per Match (Home vs Away)",
       x = "Home Team Goals",
       y = "Away Team Goals") +
  theme_minimal()
#Histogram of Goal Timing (Home vs Away)
ggplot(goal_minutes, aes(x = minute, fill = team_type)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 30) +
  labs(title = "Goal Timing Distribution (Home vs Away)",
       x = "Match Minute",
       y = "Count of Goals") +
  theme_minimal()


#Goals by Team (Top 10 Teams)
top_teams <- df %>%
  group_by(team) %>%
  summarise(total_goals = n()) %>%
  arrange(desc(total_goals)) %>%
  slice(1:10)

ggplot(top_teams, aes(x = reorder(team, total_goals), y = total_goals)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Goal-Scoring Teams",
       x = "Team",
       y = "Total Goals") +
  theme_minimal()
#Goals by Year

df$date <- as.Date(df$date)

df$year <- format(df$date, "%Y")

goals_by_year <- df %>%
  group_by(year) %>%
  summarise(total_goals = n())

ggplot(goals_by_year, aes(x = year, y = total_goals, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Goals per Year",
       x = "Year",
       y = "Total Goals") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Timeline of Goals Within Matches
df$minute_bin <- cut(df$minute,
                     breaks=c(0,15,30,45,60,75,90),
                     labels=c("1–15","16–30","31–45","46–60","61–75","76–90"))

ggplot(df, aes(x = minute_bin, fill = minute_bin)) +
  geom_bar() +
  labs(title = "Goals by Match Time Segments",
       x = "Match Time Segment",
       y = "Number of Goals") +
  theme_minimal()
#Statistical test
install.packages("readr")
library(readr)
library(dplyr)
library(tidyr)

# Load data
df <- read_csv("C:/Users/OneDrive/Desktop\A155_women_goalscoreswomens_goalscorers.csv")
match_goals <- df %>%
  # Mark each goal as Home or Away
  mutate(goal_for = ifelse(team == home_team, "Home",
                           ifelse(team == away_team, "Away", NA))) %>%
  # Count goals per match and type
  group_by(date, home_team, away_team) %>%
  summarise(
    home_goals = sum(goal_for == "Home", na.rm = TRUE),
    away_goals = sum(goal_for == "Away", na.rm = TRUE),
    .groups = "drop"
  )
cat("\n=== HOME ADVANTAGE STATISTICAL TEST ===\n")
cat("Number of matches analysed:", nrow(match_goals), "\n")
cat("Mean home goals per match :", round(mean(match_goals$home_goals), 3), "\n")
cat("Mean away goals per match :", round(mean(match_goals$away_goals), 3), "\n\n")
# Paired t-test
t_result <- t.test(match_goals$home_goals, match_goals$away_goals, paired = TRUE)
cat("Paired t-test:\n")
cat("   t =", round(t_result$statistic, 3),
    "  df =", t_result$parameter,
    "  p-value =", format.pval(t_result$p.value, digits = 4), "\n")

# Non-parametric Wilcoxon signed-rank test (more robust)
w_result <- wilcox.test(match_goals$home_goals, match_goals$away_goals, paired = TRUE)
cat("Wilcoxon signed-rank test:\n")
cat("   V =", w_result$statistic,
    "  p-value =", format.pval(w_result$p.value, digits = 4), "\n")

if(t_result$p.value < 0.001) cat("\n→ Very strong evidence of home advantage!\n")
