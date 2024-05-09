library(ggplot2)
library(dplyr)
library(car)
library(readr)

# Here is a function to run a regression model and perform a hypothesis test
run_model_and_test <- function(data, formula, constraints) {
  model <- lm(formula, data = data)
  summary(model)
  linearHypothesis(model, constraints)
}

# A function to create scatter plots for visualizing relationships
create_scatter_plot <- function(data, x_col, y_col, title, xlab, ylab, color) {
  ggplot(data, aes(x = !!sym(x_col), y = !!sym(y_col))) +
    geom_point(color = color, pch = 16) +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal()
}

# Load and inspect the dataset
NCAAB <- read_csv("621 - Gambling/Module2/Data/NCAAB_updated_3_24_24.csv")
glimpse(NCAAB)

# Model 1: Full Dataset
formula1 <- scorediff ~ (0 + intercept) + closingline
constraints1 <- c("intercept = 0", "closingline = 1")
run_model_and_test(NCAAB, formula1, constraints1)

create_scatter_plot(
  NCAAB, "closingline", "scorediff",
  "Closing Line vs. Score Differential", "Closing Line", "Score Differential", "blue"
)

# Subset road favorites
NCAAB_roadfav <- filter(NCAAB, closeroadfav == 1)

# Model 2: Road Favorites
formula2 <- scorediff ~ (0 + intercept) + closingline
constraints2 <- c("intercept = 0", "closingline = 1")
run_model_and_test(NCAAB_roadfav, formula2, constraints2)

create_scatter_plot(
  NCAAB_roadfav, "closingline", "scorediff",
  "Away Favorite Closing Line vs. Score Differential", "Closing Line", "Score Differential", "orange"
)

# Model 3: Tight Games for Road Favorites
NCAAB_roadfav_tightgames <- filter(NCAAB_roadfav, scorediff >= -3 & scorediff <= 3)
run_model_and_test(NCAAB_roadfav_tightgames, formula2, constraints2)

create_scatter_plot(
  NCAAB_roadfav_tightgames, "closingline", "scorediff",
  "Away Team Favorite - Tight Games Closing Line vs. Score Differential",
  "Closing Line", "Score Differential", "red"
)

# Model 4: Very Tight Games and Tight Score Differentials for Road Favorites
NCAAB_roadfav_verytight <- filter(NCAAB_roadfav, scorediff >= -3 & scorediff <= 3, closingline >= -5 & closingline <= 5)
run_model_and_test(NCAAB_roadfav_verytight, formula2, constraints2)

create_scatter_plot(
  NCAAB_roadfav_verytight, "closingline", "scorediff",
  "Away Team Favorite - Real Tight Closing Line vs. Score Differential",
  "Closing Line", "Score Differential", "black"
)

# Subset home favorites
NCAAB_homefav <- filter(NCAAB, closeroadfav == 0)

# Model 5: Home Favorites
run_model_and_test(NCAAB_homefav, formula2, constraints2)

create_scatter_plot(
  NCAAB_homefav, "closingline", "scorediff",
  "Home Favorite Closing Line vs. Score Differential",
  "Closing Line", "Score Differential", "purple"
)

# Model 6: Tight Games for Home Favorites
NCAAB_homefav_tightgames <- filter(NCAAB_homefav, scorediff >= -3 & scorediff <= 3)
run_model_and_test(NCAAB_homefav_tightgames, formula2, constraints2)

create_scatter_plot(
  NCAAB_homefav_tightgames, "closingline", "scorediff",
  "Home Team Favorite - Tight Closing Line vs. Score Differential",
  "Closing Line", "Score Differential", "pink")
