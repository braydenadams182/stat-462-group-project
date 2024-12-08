# Load necessary library
library(glmnet)

# Define player stats (including Furkan Korkmaz)
players <- data.frame(
  name = c("Nikola Jokic", "Luka Doncic", "Joel Embiid", "Giannis Antetokounmpo", "Shai Gilgeous-Alexander", 
           "Anthony Davis", "LeBron James", "Kevin Durant", "Jayson Tatum", "Stephen Curry", "Killian Hayes", 
           "Patrick Beverley", "Jay Huff", "Nicolas Claxton", "Desmond Bane", "Tobias Harris", "Paolo Banchero", 
           "Myles Turner", "Austin Reaves", "D'Angelo Russell", "Ja Morant", "Furkan Korkmaz"),
  points_per_game = c(26.4, 33.9, 34.7, 30.4, 30.1, 24.7, 25.7, 27.1, 28.4, 26.4, 6.9, 6.2, 3.5, 12.6, 21.5, 18.0, 
                      20.0, 15.0, 13.0, 17.0, 25.1, 6.8),
  defensive_rating = c(107, 110, 107, 102, 108, 104, 106, 109, 107, 111, 110, 110, 103, 101, 106, 105, 
                       108, 103, 107, 108, 105, 110),
  assists_per_game = c(9.0, 9.8, 5.6, 6.5, 6.2, 3.5, 8.3, 5.0, 5.6, 5.1, 4.9, 2.9, 1.0, 1.5, 4.4, 
                       2.5, 3.7, 1.2, 3.4, 6.1, 8.1, 1.2),
  per = c(32.1, 23.5, 28.3, 29.5, 27.8, 26.4, 25.7, 27.1, 26.9, 28.5, 10.5, 12.2, 15.0, 20.3, 19.8, 17.5, 
          16.0, 18.0, 14.0, 16.5, 20.8, 11.2),
  win_shares_per_48 = c(0.301, 0.250, 0.270, 0.280, 0.240, 0.220, 0.230, 0.250, 0.260, 0.270, 0.050, 0.050, 
                        0.100, 0.150, 0.200, 0.180, 0.150, 0.200, 0.120, 0.140, 0.180, 0.050),
  bpm = c(8.5, 7.2, 8.0, 7.8, 6.5, 6.9, 7.5, 7.3, 7.0, 7.6, 2.5, 2.8, 1.0, 4.5, 5.0, 4.0, 3.8, 4.2, 3.2, 4.1, 6.8, 1.0),
  vorp = c(7.0, 6.5, 6.8, 6.7, 5.5, 5.8, 6.2, 6.0, 5.9, 6.3, 1.5, 1.8, 0.5, 3.5, 4.0, 3.0, 3.5, 4.0, 3.0, 3.5, 6.2, 0.5),
  rating_2k = c(98, 95, 96, 96, 93, 93, 96, 96, 95, 96, 75, 76, 67, 84, 84, 82, 84, 83, 80, 83, 92, 70)  # Provided 2K ratings
)

# Adjusted normalization function to scale to a range of 67 to 99
adjusted_norm <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)) * (99 - 67) + 67)  # Scale to a range that ensures the lowest rating is 67
}

# Apply adjusted normalization to each criterion
players$scoring <- adjusted_norm(players$points_per_game)
players$defense <- adjusted_norm(max(players$defensive_rating) - players$defensive_rating)  # Invert because lower defensive rating is better
players$playmaking <- adjusted_norm(players$assists_per_game)
players$efficiency <- adjusted_norm(players$per)
players$impact <- adjusted_norm(players$win_shares_per_48)
players$bpm_norm <- adjusted_norm(players$bpm)
players$vorp_norm <- adjusted_norm(players$vorp)

# Calculate final rating
players$final_rating_formula <- rowMeans(players[, c("scoring", "defense", "playmaking", "efficiency", "impact")])

# Prepare data for ridge regression
x <- as.matrix(players[, c("points_per_game", "defensive_rating", "assists_per_game", "per", "win_shares_per_48", "bpm", "vorp")])
y <- players$rating_2k

# Fit ridge regression model
ridge_model <- cv.glmnet(x, y, alpha = 0)

# Predict ratings using the ridge regression model
players$predicted_rating_ridge <- predict(ridge_model, s = "lambda.min", newx = x)

# Calculate correlation coefficient between predicted_rating and 2K rating
correlation_coefficient_formula <- cor(players$final_rating_formula, players$rating_2k)
correlation_coefficient_ridge <- cor(players$predicted_rating_ridge, players$rating_2k)

# Rank players by final_rating in descending order
ranked_players_formula <- players[order(-players$final_rating_formula), ]
ranked_players_ridge <- players[order(-players$predicted_rating_ridge), ]

# Display final ratings and correlation coefficients
print("Ranked Players by Formula-Based Ratings:")
print(ranked_players_formula[, c("name", "final_rating_formula", "rating_2k")])
print(paste("The correlation coefficient between the formula ratings and the actual ratings is", correlation_coefficient_formula))

print("\nRanked Players by Ridge Regression-Based Ratings:")
print(ranked_players_ridge[, c("name", "predicted_rating_ridge", "rating_2k")])
print("The correlation coefficient between the ridge regression ratings and the actual ratings is")
correlation_coefficient_ridge

library(ggplot2)
ggplot(players, aes(x = rating_2k, y = final_rating_formula)) +
  geom_point(color = 'blue') +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = 'Formula Ratings vs. 2K Ratings', x = '2K Ratings', y = 'Formula Ratings')
ggplot(players, aes(x = rating_2k, y = predicted_rating_ridge)) +
  geom_point(color = 'green') +
  geom_smooth(method = 'lm', color = 'red') +
  labs(title = 'Ridge Regression Ratings vs. 2K Ratings', x = '2K Ratings', y = 'Ridge Ratings')

write.csv(ranked_players_formula, "formula_based_rankings.csv")
write.csv(ranked_players_ridge, "ridge_based_rankings.csv")

