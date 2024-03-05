# Load the training data set
train <- read.csv("train.csv")

# Correct invalid values
train[train == "N/A" |
        train == "NA" |
        train == "#N/A" | train == "NULL" | train == ""] <- NA

# Load the testing data set
test <- read.csv("test.csv")

# Correct invalid values
test[test == "N/A" |
       test == "NA" |
       test == "#N/A" | test == "NULL" | test == ""] <- NA

# install.packages("tidyverse")
library(tidyverse)

# Calculate the number of missing values for each column and sort in descending order
missing <-
  train %>% is.na() %>% colSums() %>% sort(decreasing = TRUE)

# Create a data frame for the top 8 columns with the most missing values
top_missing <- data.frame(column = names(missing)[1:8],
                          count = missing[1:8])


# Create the bar plot
ggplot(top_missing, aes(x = reorder(column,-count), y = count)) +
  geom_bar(stat = "identity",
           fill = "skyblue",
           color = "black") +
  labs(x = "Column", y = "Count of Missing Values",
       title = "Top 8 Columns with Missing Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Function for parsing dictionary fields
library(jsonlite)

convert_string_to_dict <- function(string_value) {
  if (is.na(string_value) ||
      is.null(string_value) || string_value == "") {
    return(NA)
  }
  
  # Replace ': None,' with ': "null",'
  string_value <- gsub(': None', ': "null"', string_value)
  
  # Replace single quoted keys & values with double quotes
  cleaned_string <- gsub("\\{'", '\\{"', string_value)
  cleaned_string <- gsub("'\\}", '"\\}', cleaned_string)
  cleaned_string <- gsub(", '", ', "', cleaned_string)
  cleaned_string <- gsub("': ", '": ', cleaned_string)
  cleaned_string <- gsub(": '", ': "', cleaned_string)
  cleaned_string <- gsub("', ", '", ', cleaned_string)
  
  # Replace double quotes inside values with single quotes
  cleaned_string <-
    gsub('([a-zA-Z]) "([a-zA-Z])', "\\1 '\\2", cleaned_string)
  cleaned_string <-
    gsub('([a-zA-Z])" ([a-zA-Z])', "\\1' \\2' ", cleaned_string)
  
  # misc cleaning
  cleaned_string <- gsub('([a-zA-Z])"",', "\\1'\",", cleaned_string)
  cleaned_string <- gsub('""([a-zA-Z])', "\"'\\1", cleaned_string)
  cleaned_string <- gsub("\\\\xa0", '', cleaned_string)
  
  # Convert string to JSON
  json_data <- tryCatch(
    expr = {
      fromJSON(cleaned_string, simplifyVector = FALSE)
    },
    error = function(e) {
      message("Error converting string to JSON: ", e$message)
      return(NA)
    }
  )
  
  return(json_data)
}

# Parsing columns in the training data set
library(dplyr)

train <- train %>%
  rowwise() %>%
  mutate(belongs_to_collection = convert_string_to_dict(belongs_to_collection)) %>%
  ungroup()

train$belongs_to_collection[sapply(train$belongs_to_collection, is.null)] <-
  NA

train <- train %>%
  rowwise() %>%
  mutate(genres = list(convert_string_to_dict(genres))) %>%
  ungroup()

train$genres[sapply(train$genres, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(production_companies = list(convert_string_to_dict(production_companies)))

train$production_companies[sapply(train$production_companies, is.null)] <-
  NA

train <- train %>%
  rowwise() %>%
  mutate(production_countries = list(convert_string_to_dict(production_countries))) %>%
  ungroup()

train$production_countries[sapply(train$production_countries, is.null)] <-
  NA

train <- train %>%
  rowwise() %>%
  mutate(spoken_languages = list(convert_string_to_dict(spoken_languages))) %>%
  ungroup()

train$spoken_languages[sapply(train$spoken_languages, is.null)] <-
  NA

train <- train %>%
  rowwise() %>%
  mutate(Keywords = list(convert_string_to_dict(Keywords))) %>%
  ungroup()

train$Keywords[sapply(train$Keywords, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(cast = list(convert_string_to_dict(cast))) %>%
  ungroup()

train$cast[sapply(train$cast, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(crew = list(convert_string_to_dict(crew))) %>%
  ungroup()

train$crew[sapply(train$crew, is.null)] <- NA

# Number of fields belonging to a collection
train %>%
  mutate(collection_flag = ifelse(!is.na(belongs_to_collection), 1, 0)) %>%
  count(collection_flag)

# Extract all collection names from the data frame
collections <- train %>% filter(!is.na(belongs_to_collection))
collections <- collections$belongs_to_collection
collection_names <- sapply(collections, function(x)
  x[["name"]])

collection_count <- sort(table(collection_names), decreasing = TRUE)

# Find the top collections
top_collections <- head(collection_count, 15)
top_collections <-
  top_collections[order(top_collections, decreasing = FALSE)]

# Generate a bar plot of the top collections
par(mar = c(5, 16, 2, 2), cex.lab = 1.5)
barplot(
  top_collections,
  horiz = TRUE,
  main = "Top 15 Collections",
  xlab = "Count",
  las = 2,
  space = 0.4,
)

# Number of fields with a tagline
train %>%
  mutate(tagline_flag = ifelse(!is.na(tagline), 1, 0)) %>%
  count(tagline_flag)

# install.packages(c('wordcloud', 'tm', 'slam', 'stringr'))
library(wordcloud)
library(tm)
library(slam)
library(stringr)

# Concatenate all taglines into a single string
taglines <-
  paste(train$tagline[!is.na(train$tagline)], collapse = ' ')

# Create a Corpus object for text processing
corpus <- Corpus(VectorSource(taglines))

# Preprocess the text: convert to lowercase, remove punctuation, and remove stop words
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Convert the Corpus object to a character vector
taglines_clean <- unlist(sapply(corpus, as.character))

# Generate the word cloud of top taglines
wordcloud(
  words = taglines_clean,
  max.words = 100,
  random.order = FALSE,
  min.freq = 1,
  colors = brewer.pal(8, "Dark2")
)

# Number of movies with a production company
train %>%
  mutate(flag = ifelse(!is.na(production_companies), 1, 0)) %>%
  count(flag)

# Extract all production companies from the data frame
companies <- train %>% filter(!is.na(production_companies))
companies <- companies$production_companies
company_names <-
  unlist(sapply(companies, function(x)
    sapply(x, function(y)
      y[["name"]])))

company_count <- sort(table(company_names), decreasing = TRUE)

# Find the top production companies
top_companies <- head(company_count, 15)
top_companies <-
  top_companies[order(top_companies, decreasing = FALSE)]

# Generate a bar plot of the production companies
par(mar = c(5, 16, 2, 2), cex.lab = 1.5)
barplot(
  top_companies,
  horiz = TRUE,
  main = "Top 15 Production Companies",
  xlab = "Count",
  las = 2,
  space = 0.4,
)

# Number of movies with a production country
train %>%
  mutate(flag = ifelse(!is.na(production_countries), 1, 0)) %>%
  count(flag)

# Extract all production countries from the data frame
countries <- train %>% filter(!is.na(production_countries))
countries <- countries$production_countries
country_names <-
  unlist(sapply(countries, function(x)
    sapply(x, function(y)
      y[["name"]])))

country_count <- sort(table(country_names), decreasing = TRUE)

# Find the top production countries
top_countries <- head(country_count, 15)
top_countries <-
  top_countries[order(top_countries, decreasing = FALSE)]

# Generate a bar plot of the production countries
par(mar = c(5, 16, 2, 2), cex.lab = 1.5)
barplot(
  top_countries,
  horiz = TRUE,
  main = "Top 15 Production Countries",
  xlab = "Count",
  las = 2,
  space = 0.4,
)

# Number of movies with a spoken language
train %>%
  mutate(flag = ifelse(!is.na(spoken_languages), 1, 0)) %>%
  count(flag)

# Extract all spoken languages from the data frame
languages <- train %>% filter(!is.na(spoken_languages))
languages <- languages$spoken_languages
language_names <-
  unlist(sapply(languages, function(x)
    sapply(x, function(y)
      y[["name"]])))

language_count <- sort(table(language_names), decreasing = TRUE)

# Find the top languages
top_languages <- head(language_count, 10)
top_languages <-
  top_languages[order(top_languages, decreasing = FALSE)]

# Generate a bar plot of the top languages
par(mar = c(5, 8, 2, 2), cex.lab = 1.5)
barplot(
  top_languages,
  horiz = TRUE,
  main = "Top 10 Spoken Languages",
  xlab = "Count",
  las = 2,
  space = 0.4,
)

# Number of movies with a genre
train %>%
  mutate(flag = ifelse(!is.na(genres), 1, 0)) %>%
  count(flag)

# Extract all genres from the data frame
genres <- train %>% filter(!is.na(genres))
genres <- genres$genres
genre_names <-
  unlist(sapply(genres, function(x)
    sapply(x, function(y)
      y[["name"]])))

genre_count <- sort(table(genre_names), decreasing = TRUE)

# Find the top genres
top_genres <- head(genre_count, 10)
top_genres <-
  top_genres[order(top_genres, decreasing = FALSE)]

# Generate a bar plot of the top genres
par(mar = c(5, 8, 2, 2), cex.lab = 1.5)
barplot(
  top_genres,
  horiz = TRUE,
  main = "Top 10 Genres",
  xlab = "Count",
  las = 2,
  space = 0.4,
)

# Parsing columns in the testing data set
test <- test %>%
  rowwise() %>%
  mutate(belongs_to_collection = convert_string_to_dict(belongs_to_collection)) %>%
  ungroup()

test$belongs_to_collection[sapply(test$belongs_to_collection, is.null)] <-
  NA

test <- test %>%
  rowwise() %>%
  mutate(genres = list(convert_string_to_dict(genres))) %>%
  ungroup()

test$genres[sapply(test$genres, is.null)] <- NA

test <- test %>%
  rowwise() %>%
  mutate(production_companies = list(convert_string_to_dict(production_companies)))

test$production_companies[sapply(test$production_companies, is.null)] <-
  NA

test <- test %>%
  rowwise() %>%
  mutate(production_countries = list(convert_string_to_dict(production_countries))) %>%
  ungroup()

test$production_countries[sapply(test$production_countries, is.null)] <-
  NA

test <- test %>%
  rowwise() %>%
  mutate(spoken_languages = list(convert_string_to_dict(spoken_languages))) %>%
  ungroup()

test$spoken_languages[sapply(test$spoken_languages, is.null)] <-
  NA

test <- test %>%
  rowwise() %>%
  mutate(Keywords = list(convert_string_to_dict(Keywords))) %>%
  ungroup()

test$Keywords[sapply(test$Keywords, is.null)] <- NA

test <- test %>%
  rowwise() %>%
  mutate(cast = list(convert_string_to_dict(cast))) %>%
  ungroup()

test$cast[sapply(test$cast, is.null)] <- NA

test <- test %>%
  rowwise() %>%
  mutate(crew = list(convert_string_to_dict(crew))) %>%
  ungroup()

test$crew[sapply(test$crew, is.null)] <- NA

# Calculate the natural log of revenue & budget
train$log_revenue <- log1p(train$revenue)
train$log_budget <- log1p(train$budget)

summary(train$revenue)

# Scatter plot between budget & revenue
ggplot(data = train, aes(x = budget, y = revenue)) + geom_point()

# Scatter plot between log of budget & log of revenue
ggplot(data = train, aes(x = log_budget, y = log_revenue)) + geom_point()

# Histogram of popularity
hist(train$popularity, breaks = 30, col = "red")

# Scatter plot between popularity & revenue
ggplot(data = train, aes(x = popularity, y = revenue)) + geom_point()

# Load necessary packages
library(lubridate)
library(ggplot2)

# Define the date conversion function
date <- function(x) {
  x <- as.character(x)
  year <- strsplit(x, "/")[[1]][3]
  if (as.integer(year) < 19) {
    return(paste0(substr(x, 1, nchar(x) - 2), '20', year))
  } else {
    return(paste0(substr(x, 1, nchar(x) - 2), '19', year))
  }
}

# Apply date conversion function and convert to datetime objects
train$release_date <-
  ifelse(is.na(train$release_date), '1/1/90', train$release_date)
test$release_date <-
  ifelse(is.na(test$release_date), '1/1/90', test$release_date)

train$release_date <- mdy(train$release_date)
test$release_date <- mdy(test$release_date)

# Extract day, month, and year components
train$release_day <- wday(train$release_date)
train$release_month <- month(train$release_date)
train$release_year <- year(train$release_date)

test$release_day <- wday(test$release_date)
test$release_month <- month(test$release_date)
test$release_year <- year(test$release_date)

# Count the number of releases by day of the week
day_counts <- table(train$release_day)

# Create a bar plot
ggplot(data = data.frame(day = names(day_counts), count = as.numeric(day_counts)), aes(x = factor(day, levels = 1:7), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  scale_x_discrete(
    labels = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
  ) +
  labs(x = "Day of the Week", y = "No of releases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a categorical plot (catplot)
ggplot(train, aes(x = factor(release_day), y = revenue)) +
  geom_boxplot() +
  scale_x_discrete(
    labels = c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday"
    )
  ) +
  labs(x = "Release Day", y = "Revenue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Revenue on Different Days of the Week of Release")
