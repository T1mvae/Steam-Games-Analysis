# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(stringr)

# Load the dataset
games <- read_csv("/data_group_b.csv")

# Check for missing values
sum(is.na(games))

# Handle missing values and changing names for more effective work (e.g., remove rows with missing target variable 'rating')
games %>% drop_na("Metacritic score")
games <- games %>% rename("rating" = "Metacritic score")
games <- games %>% rename("genre" = "Genres")
games <- games[games$rating != 0, ]

# Convert Genres from string to an array of genres
games <- games %>% mutate(genre = str_split(genre, ","))

# Extract unique genres
all_genres <- unique(unlist(games$genre))

# Create a binary column for each genre
for (genre in all_genres) {
  genre_col <- paste0("genre_", str_trim(genre))
  games[[genre_col]] <- sapply(games$genre, function(x) as.integer(genre %in% x))
}

games <- games %>%
  gather(key = "Genre", value = "Presence", starts_with("genre_")) %>%
  filter(Presence == 1) %>%
  select(-Presence)

# Exploratory Data Analysis (EDA)
# Distribution of the rating
ggplot(games, aes(x = rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Game Ratings", x = "Rating", y = "Count")

ggplot(games, aes(x = Genre, y = rating)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Rating by Genre", x = "Genre", y = "Rating") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Distribution of game prices
ggplot(games, aes(x = Price)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Game Prices", x = "Price", y = "Count")

# Relationship between price and rating
ggplot(games, aes(x = Price, y = rating)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_minimal() +
  labs(title = "Relationship between Price and Rating", x = "Price", y = "Rating")

# Data Preparation: Extract and preprocess languages
games$languages <- str_extract_all(games$`Supported languages`, "[a-zA-Z]+")
languages_list <- unique(unlist(games$languages))
for (lang in languages_list) {
  games[[paste0("lang_", lang)]] <- sapply(games$languages, function(x) as.integer(lang %in% x))
}

games$num_languages <- sapply(games$languages, length)

rating_stats <- games %>%
  group_by(num_languages) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE),
            sd_rating = sd(rating, na.rm = TRUE),
            n = n())

# Create the bar plot with error bars
ggplot(rating_stats, aes(x = as.factor(num_languages), y = mean_rating)) +
  geom_bar(stat = "identity", fill = "blue", color = "black") +
  geom_errorbar(aes(ymin = mean_rating - sd_rating, ymax = mean_rating + sd_rating), width = 0.2) +
  theme_minimal() +
  labs(title = "Average Rating by Number of Languages Supported", x = "Number of Languages", y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Average rating by Windows support
ggplot(games, aes(x = Windows, y = rating)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Rating by Windows Support", x = "Windows Support", y = "Rating")

# Average rating by MacOS support
ggplot(games, aes(x = Mac, y = rating)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Rating by MacOS Support", x = "MacOS Support", y = "Rating")

# Average rating by Linux support
ggplot(games, aes(x = Linux, y = rating)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Average Rating by Linux Support", x = "Linux Support", y = "Rating")

# Model Specification and Fitting
# Fit a linear regression model with OS support as predictors
model <- lm(rating ~ Windows + Mac + Linux + num_languages + Genre + Price, data = games)

# Summary of the model
summary(model)
  
# Interpret Results and Model Diagnostics
# Genre and Reviews model diagnostics
par(mfrow = c(2, 2))
plot(model)
