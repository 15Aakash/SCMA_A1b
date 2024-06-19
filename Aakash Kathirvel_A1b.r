
# Set the working directory and verify it
setwd('C:/Users/Aakash/Desktop/SCMA')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA", "glue","fitdistrplus")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("C:/Users/Aakash/Desktop/SCMA/IPL_ball_by_ball_updated till 2024.csv")
# Display the first few rows of the data
head(data)
tail(data)

df <- read_excel("C:/Users/Aakash/Desktop/SCMA/IPL SALARIES 2024.xlsx", sheet = 1)
head(df)
tail(df)

# Convert Salary to numeric (handle 'lakh' and 'crore')
salary_data <- df %>%
  mutate(
    Salary = case_when(
      grepl("lakh", Salary) ~ as.numeric(gsub(" lakh", "", Salary)) * 1e5,
      grepl("crore", Salary) ~ as.numeric(gsub(" crore", "", Salary)) * 1e7,
      TRUE ~ as.numeric(Salary)
    )
  )

# Clean column names to remove any leading/trailing spaces
colnames(data) <- trimws(colnames(data))
colnames(df) <- trimws(colnames(df))

# Rename the columns 
data <- data %>%
  rename(
    Match_id = `Match.id`,
    Batting_team = `Batting.team`,
    Bowling_team = `Bowling.team`,
    Innings_No = `Innings.No`,
    Ball_No = `Ball.No`
  )

# Ensure player names are in a consistent format
data <- data %>%
  mutate(Striker = trimws(Striker))

df <- df %>%
  mutate(Player = trimws(Player))

# Arrange the data IPL round-wise and batsman, ball, runs, and wickets per player per match
df_roundwise <- data %>%
  arrange(Season,Match_id)
head(df_roundwise)

#Arrange the data for each batsman
batsman_data=df_roundwise%>%
  group_by(Match_id, Date,Striker) %>%
  summarise(
    Total_Balls_Faced =n(), 
    Total_Runs_Scored=sum(runs_scored),
    .groups='drop'
  )
str(batsman_data)

#Arrange the data for each bowler
bowler_data=df_roundwise%>%
  filter(wicket_confirmation==1) %>%
  group_by(Match_id, Date,Bowler) %>%
  summarise(
    Total_Wickets_Taken=n(),
    .groups='drop'
  )
str(bowler_data)
str(df_roundwise)

# Top three run-getters and wicket-takers in each IPL round
top_performers <- df_roundwise %>%
  group_by(Season, Batting_team, Striker) %>%
  summarize(total_runs = sum(runs_scored), .groups = 'drop') %>%
  arrange(desc(total_runs)) %>%
  top_n(3, total_runs)

top_bowlers <- df_roundwise %>%
  group_by(Season, Bowling_team, Bowler) %>%
  summarize(total_wickets = sum(wicket_confirmation), .groups = 'drop') %>%
  arrange(desc(total_wickets)) %>%
  top_n(3, total_wickets)

# Fit the most appropriate distribution for the top three batsmen and bowlers in the last three IPL tournaments
last_three_seasons <- df_roundwise %>% filter(Season %in% tail(unique(Season), 3))

# Fit distributions for top batsmen
top_batsmen <- last_three_seasons %>%
  filter(Striker %in% unique(top_performers$Striker)) %>%
  group_by(Striker) %>%
  summarize(total_runs = sum(runs_scored), .groups = 'drop')

top_batsmen_dist <- fitdist(top_batsmen$total_runs, "norm")

# Fit distributions for top bowlers
top_bowlers <- last_three_seasons %>%
  filter(Bowler %in% unique(top_bowlers$Bowler)) %>%
  group_by(Bowler) %>%
  summarize(total_wickets = sum(wicket_confirmation), .groups = 'drop')

top_bowlers_dist <- fitdist(top_bowlers$total_wickets, "pois")

# Fitting distribution for Harpreet Brar 
harpreet_brar_runs <- last_three_seasons %>%
  filter(Striker == "Harpreet Brar") %>%
  dplyr::select(runs_scored)
harpreet_brar_runs

# Check if the resulting runs are numeric and have more than one element
if (is.numeric(harpreet_brar_runs$runs_scored) && length(harpreet_brar_runs$runs_scored) > 1) {
  harpreet_brar_dist <- fitdist(harpreet_brar_runs$runs_scored, "norm")
  print(summary(harpreet_brar_dist))
  
  
  # Data visualization (optional)
  hist(harpreet_brar_runs$runs_scored, breaks = 10, freq = FALSE, main = "Histogram of Harpreet Brar's Runs as Striker")
  lines(density(harpreet_brar_runs$runs_scored), col = "green")
  
  # Merge performance data with salary data 
  performance_salary <- left_join(df_roundwise, salary_data, by = c("Striker" = "Player"))
  
  # Check for missing salaries after the join
  missing_salaries <- performance_salary %>%
    filter(is.na(Salary))
  
  # Print missing salaries to debug
  print("Players with missing salaries:")
  print(missing_salaries)
  
  # Summarize total runs and wickets with salary
  performance_summary <- performance_salary %>%
    filter(!is.na(Salary)) %>%
    group_by(Striker, Salary) %>%
    summarize(total_runs = sum(runs_scored), total_wickets = sum(wicket_confirmation), .groups = 'drop')
  
  # Filter the last three seasons
  last_three_seasons_salary <- last_three_seasons %>%
    left_join(df, by = c("Striker" = "Player"))
  
  # Summarize the performance with latest salary
  performance_with_salary <- last_three_seasons_salary %>%
    filter(!is.na(Salary)) %>%
    group_by(Striker) %>%
    summarize(total_runs = sum(runs_scored), total_wickets = sum(wicket_confirmation), latest_salary = max(Salary), .groups = 'drop')
  
  # Top 10 batsmen and bowlers
  top_10_batsmen <- performance_summary %>%
    arrange(desc(total_runs)) %>%
    head(10)
  print(top_10_batsmen)
  
  top_10_bowlers <- performance_summary %>%
    arrange(desc(total_wickets)) %>%
    head(10)
  print(top_10_bowlers)
  
  # Perform t-test
  t_test_result <- t.test(top_10_batsmen$Salary, top_10_bowlers$Salary)
  
  # Display results
  t_test_result
  
  
