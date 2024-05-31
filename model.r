library(tidyverse)
library(jsonlite)
library(dplyr)
library(wordcloud)
library(tm)
library(slam)
library(stringr)
library(lubridate)
library(caret)

# Load the training data set
train <- read.csv("train.csv")

# Correct invalid values
train[train == "N/A" |
        train == "NA" |
        train == "#N/A" |
        train == "NULL" | train == "" | train == "<NA>"] <- NA

# Load the testing data set
test <- read.csv("test.csv")

# Correct invalid values
test[test == "N/A" |
       test == "NA" |
       test == "#N/A" |
       test == "NULL" | test == "" | test == "<NA>"] <- NA

# Calculate the number of missing values for each column and sort in descending order
missing <-
  train %>% is.na() %>% colSums() %>% sort(decreasing = TRUE)

# Create a data frame for the top 8 columns with the most missing values
top_missing <- data.frame(column = names(missing)[1:8],
                          count = missing[1:8])

# Create the bar plot
ggplot(top_missing, aes(x = reorder(column, -count), y = count)) +
  geom_bar(stat = "identity",
           fill = "skyblue",
           color = "black") +
  labs(x = "Column", y = "Count of Missing Values",
       title = "Top 8 Columns with Missing Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Function for parsing dictionary fields
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
par(mar = c(5, 12, 2, 2), cex.lab = 1.5)
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
# test$log_revenue <- log1p(test$revenue)
test$log_budget <- log1p(test$budget)

summary(train$revenue)

# Scatter plot between budget & revenue
ggplot(data = train, aes(x = budget, y = revenue)) + geom_point()

# Scatter plot between log of budget & log of revenue
ggplot(data = train, aes(x = log_budget, y = log_revenue)) + geom_point()

# Histogram of popularity
hist(train$popularity, breaks = 30, col = "red")

# Scatter plot between popularity & revenue
ggplot(data = train, aes(x = popularity, y = revenue)) + geom_point()

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

# Bar plot of number of releases on different week days
ggplot(data = data.frame(day = names(day_counts), count = as.numeric(day_counts)),
       aes(x = factor(day, levels = 1:7), y = count)) +
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
  labs(x = "Day of the Week", y = "Number of releases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a categorical plot of revenue based on the week day of release
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

# Create a box plot of the run time of movies based on its release week day
ggplot(train, aes(x = factor(release_day), y = runtime)) +
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
  labs(x = "Release Day", y = "Runtime") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Runtime on Different Days of the Week of Release")

# Create a categorical plot of revenue of movies based on its release month
ggplot(train, aes(x = factor(release_month), y = revenue)) +
  geom_boxplot() +
  scale_x_discrete(labels = month.abb) +
  labs(x = "Release Month", y = "Revenue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  ggtitle("Revenue on Different Months of Release")

# Aggregate average revenue per year
yearly <-
  aggregate(revenue ~ release_year, data = train, FUN = mean)

# Create a line plot of mean revenue of movies released every year
ggplot(yearly, aes(x = release_year, y = revenue)) +
  geom_line() +
  labs(x = "Year", y = "Revenue") +
  ggtitle("Average Revenue per Year") +
  theme_minimal()

# ggsave("fig.png", width = 15, height = 8)

# Histogram of film length in hours
ggplot(train, aes(x = runtime, y = ..density..)) +
  geom_histogram(fill = "skyblue", bins = 40) +
  labs(x = "Length of Film (hours)", y = "Density") +
  ggtitle("Distribution of Length of Film in Hours") +
  theme_minimal()

# Scatter plot of film length vs. revenue
ggplot(train, aes(x = runtime, y = revenue)) +
  geom_point(color = "red") +
  labs(x = "Runtime", y = "Revenue") +
  ggtitle("Runtime vs. Revenue") +
  theme_minimal()

# Scatter plot of film length vs. popularity
ggplot(train, aes(x = runtime, y = popularity)) +
  geom_point(color = "green") +
  labs(x = "Runtime", y = "Popularity") +
  ggtitle("Runtime vs. Popularity") +
  theme_minimal()

# Count occurrences of each value in the homepage column
homepage_counts <- table(train$homepage)

top_homepages <- sort(homepage_counts, decreasing = TRUE)[1:5]
top_homepages

# Plotting by genre
genres <-
  train[train$genres %>% lengths() == 1, c('genres', 'revenue', 'budget', 'popularity', 'runtime')]
rownames(genres) <- NULL
genres$genres <- sapply(genres$genres, function(x)
  pluck(x[[1]], "name"))

# Group by 'genres' and calculate the mean
genres <- genres %>%
  group_by(genres) %>%
  summarize(
    popularity = mean(popularity, na.rm = TRUE),
    revenue = mean(revenue, na.rm = TRUE),
    budget = mean(budget, na.rm = TRUE),
    runtime = mean(runtime, na.rm = TRUE)
  )

par(mfrow = c(2, 2))

barplot(
  genres$revenue,
  names.arg = genres$genres,
  main = "Mean Revenue by Genre",
  xlab = "Revenue",
  ylab = "Genres"
)
barplot(
  genres$budget,
  names.arg = genres$genres,
  main = "Mean Budget by Genre",
  xlab = "Budget",
  ylab = "Genres"
)
barplot(
  genres$popularity,
  names.arg = genres$genres,
  main = "Mean Popularity by Genre",
  xlab = "Popularity",
  ylab = "Genres"
)
barplot(
  genres$runtime,
  names.arg = genres$genres,
  main = "Mean Runtime by Genre",
  xlab = "Runtime",
  ylab = "Genres"
)

par(mfrow = c(1, 1))

# Function to extract names from from cast & crew columns
extract_names <- function(x) {
  if (length(x) == 0) {
    return(character(0))
  } else {
    return(sapply(x, function(i)
      pluck(i, "name")))
  }
}

# Top crew members
crew <- lapply(train$crew, function(x)
  extract_names(x))
crew_flattened <- unlist(crew)
crew_name_counts <- table(crew_flattened)
top_15_crew <- head(sort(crew_name_counts, decreasing = TRUE), 15)
print(top_15_crew)

# Top cast members
cast <- lapply(train$cast, function(x)
  extract_names(x))
cast_flattened <- unlist(cast)
cast_name_counts <- table(cast_flattened)
top_15_cast <- head(sort(cast_name_counts, decreasing = TRUE), 15)
print(top_15_cast)

# Pre-processing the data frame for training
prepare_data <- function(df) {
  impute(df$runtime, 0)
  impute(df$budget, 0)
  impute(df$popularity, 0)
  impute(df$original_language, "")
  
  df$budget_runtime_ratio <-
    with(df, ifelse(runtime == 0, 0, budget / runtime))
  
  df$budget_runtime_ratio[is.infinite(df$budget_runtime_ratio) |
                            is.nan(df$budget_runtime_ratio)] <-
    0
  
  df$budget_popularity_ratio <-
    df$budget / df$popularity
  
  df$budget_year_ratio <-
    with(df,
         ifelse(is.na(budget), 0, budget) / (release_year * release_year))
  
  df$releaseYear_popularity_ratio <-
    df$release_year / df$popularity
  
  df$releaseYear_popularity_ratio2 <-
    df$popularity / df$release_year
  
  df$budget <- log1p(df$budget)
  
  df$collection_name <-
    sapply(df$belongs_to_collection, function(x)
      ifelse(class(x) == 'logical' ||
               length(x) == 0, 0, pluck(x, "name")))
  
  df$has_homepage <- 1
  df$has_homepage[is.na(df$homepage)] <- 0
  
  df$isOriginalLanguageEng <-
    sapply(df$original_language, function(x)
      ifelse(x == "en", 1, 0))
  
  labs.coll <- LabelEncoder.fit(df$collection_name)
  df$collection_name <-
    transform(labs.coll, df$collection_name)
  
  labs.lang <- LabelEncoder.fit(df$original_language)
  df$original_language <-
    transform(labs.lang, df$original_language)
  
  df$num_Keywords <- sapply(df$Keywords, length)
  
  df$num_cast <-
    sapply(df$cast, function(x)
      ifelse(class(x) == "logical", 0, length(x)))
  
  df$isbelongto_coll <- 1
  df$isbelongto_coll[is.na(df$belongs_to_collection)] <-
    0
  
  df$isTaglineNA <-
    sapply(df$tagline, function(x)
      ifelse(is.na(x) || length(x) == 0, 1, 0))
  
  df$ismovie_released <-
    sapply(df$status, function(x)
      ifelse(x == "Released", 1, 0))
  
  df$no_spoken_languages <-
    sapply(df$spoken_languages, function(x)
      ifelse(class(x) == 'logical', 0, length(x)))
  
  df$original_title_letter_count <-
    sapply(df$original_title, function(x)
      ifelse(is.na(x), 0, nchar(x)))
  
  df$original_title_word_count <-
    sapply(df$original_title, function(x)
      ifelse(class(x) == 'logical', 0, length(unlist(strsplit(
        x, " "
      )))))
  
  df$title_word_count <-
    sapply(df$title, function(x)
      ifelse(is.na(x), 0, length(unlist(
        strsplit(as.character(x), " ")
      ))))
  
  df$overview_word_count <-
    sapply(df$overview, function(x)
      ifelse(is.na(x), 0, length(unlist(
        strsplit(as.character(x), " ")
      ))))
  
  df$tagline_word_count <-
    sapply(df$tagline, function(x)
      ifelse(is.na(x), 0, length(unlist(
        strsplit(as.character(x), " ")
      ))))
  
  df$collection_id <-
    sapply(df$belongs_to_collection, function(x)
      ifelse(class(x) == 'logical' ||
               length(x) == 0, NA, pluck(x, "id")))
  
  df$production_countries_count <-
    sapply(df$production_countries, length)
  
  df$production_companies_count <-
    sapply(df$production_companies, length)
  
  df$cast_count <-
    sapply(df$cast, function(x)
      ifelse(class(x) == 'logical', 0, length(x)))
  
  df$crew_count <-
    sapply(df$crew, function(x)
      ifelse(class(x) == 'logical', 0, length(x)))
  
  df$genders_0_crew <-
    sapply(df$crew, function(row)
      ifelse(class(row) == 'logical' ||
               length(row) == 0, 0, sum(sapply(row, function(member)
                 ifelse(member[["gender"]] == 0, 1, 0)))))
  
  df$genders_1_crew <-
    sapply(df$crew, function(row)
      ifelse(class(row) == 'logical' ||
               length(row) == 0, 0, sum(sapply(row, function(member)
                 ifelse(member[["gender"]] == 1, 1, 0)))))
  
  df$genders_2_crew <-
    sapply(df$crew, function(row)
      ifelse(class(row) == 'logical' ||
               length(row) == 0, 0, sum(sapply(row, function(member)
                 ifelse(member[["gender"]] == 2, 1, 0)))))
  
  cols <- c('genres',
            'production_countries',
            'spoken_languages',
            'production_companies')
  
  for (col in cols) {
    df[[col]] <-
      sapply(df[[col]], function(row)
        unlist(sapply(row, function(x)
          pluck(x, "name", .default = NA)))) %>%
      sapply(function(row) {
        newRow = list()
        for (i in row) {
          if (i %in% names(train_dict[[col]])) {
            newRow = list(newRow, i)
          } else {
            newRow = list(newRow, as.character(glue::glue("{col}_etc")))
          }
        }
        
        newRow <-
          newRow %>% unlist %>% unique %>% unlist %>% sort %>% unlist
        return(newRow)
      }) %>%
      sapply(paste, collapse = ",")
  }
  
  df <- dummy_columns(df, split = ',', select_columns = cols)
  
  df <- df[,!grepl("genres_etc", names(df))]
  
  cols_to_normalize = c(
    'runtime',
    'popularity',
    'budget',
    'budget_runtime_ratio',
    'budget_year_ratio',
    'budget_popularity_ratio',
    'releaseYear_popularity_ratio',
    'releaseYear_popularity_ratio2',
    'num_Keywords',
    'num_cast',
    'no_spoken_languages',
    'original_title_letter_count',
    'original_title_word_count',
    'title_word_count',
    'overview_word_count',
    'tagline_word_count',
    'production_countries_count',
    'production_companies_count',
    'cast_count',
    'crew_count',
    'genders_0_crew',
    'genders_1_crew',
    'genders_2_crew'
  )
  
  for (col in cols_to_normalize) {
    x_array <- df[[col]]
    x_array <- replace(x_array, is.na(x_array), 0)
    x_array <- sapply(x_array, as.numeric)
    X_norm <- as.list(scale(x_array))
    df[[col]] <- X_norm
  }
  
  df <-
    subset(df, select = !(
      names(df) %in% c(
        'belongs_to_collection',
        'genres',
        'homepage',
        'imdb_id',
        'overview',
        'id',
        'poster_path',
        'production_companies',
        'production_countries',
        'release_date',
        'spoken_languages',
        'status',
        'title',
        'Keywords',
        'cast',
        'crew',
        'original_language',
        'original_title',
        'tagline',
        'collection_id'
      )
    ))  %>%
    replace(is.na(.), 0.0)
  
}

dict_columns = c(
  'belongs_to_collection',
  'genres',
  'production_companies',
  'production_countries',
  'spoken_languages',
  'Keywords',
  'cast',
  'crew'
)

get_json <- function(df) {
  result <- list()
  for (col in dict_columns) {
    d <- list()
    rows <- df[[col]]
    if (col == 'belongs_to_collection') {
      for (i in rows) {
        if (!is.na(i["name"]) & !is.null(i["name"])) {
          if (!(i["name"] %in% names(d))) {
            d[i[["name"]]] <- 0
          } else {
            d[i[["name"]]] <- as.numeric(d[i[["name"]]]) + 1
          }
        }
      }
    } else {
      for (row in rows) {
        for (i in row) {
          if (!is.na(i["name"]) & !is.null(i["name"])) {
            if (!(i["name"] %in% names(d))) {
              d[i[["name"]]] <- 0
            } else {
              if (!is.null(d[[i[["name"]]]]))
                d[i[["name"]]] <- as.numeric(d[i[["name"]]]) + 1
            }
          }
        }
      }
    }
    result[[col]] <- d
  }
  return(result)
}

train_dict = get_json(train)
test_dict = get_json(test)

for (col in dict_columns) {
  remove <- c()
  train_id <- unique(names(train_dict[[col]]))
  test_id <- unique(names(test_dict[[col]]))
  remove <-
    c(remove,
      setdiff(train_id, test_id),
      setdiff(test_id, train_id))
  
  for (i in union(train_id, test_id)[!union(train_id, test_id) %in% remove]) {
    if (train_dict[[col]][[i]] < 10 || i == '') {
      remove <- c(remove, i)
    }
  }
  
  for (i in remove) {
    if (i %in% names(train_dict[[col]])) {
      train_dict[[col]][[i]] <- NULL
    }
    if (i %in% names(test_dict[[col]])) {
      test_dict[[col]][[i]] <- NULL
    }
  }
}

library(plyr)
library(Hmisc)
library(CatEncoders)
library(fastDummies)

test$revenue <- NA

all_data <- bind_rows(train, test)

all_data <- prepare_data(all_data)

all_data <- all_data %>%
  mutate(row_num = 1:nrow(.)) %>%
  select(-row_num)

train <- all_data[1:nrow(train),]
test <- all_data[(nrow(train) + 1):nrow(all_data),]

print(dim(train))

train <- train[, -which(names(train) == "revenue")]
head(all_data)

y <- train$log_revenue
X <- train[, !(names(train) %in% c("log_revenue"))]

set.seed(42)  # Setting seed for reproducibility
train_indices <- createDataPartition(y, p = 0.9, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Create cross-validation iterator
kfold <-
  createFolds(y_train,
              k = 3,
              list = TRUE,
              returnTrain = FALSE)

# Every metric used to measure success of an algorithm
show_metrics <- function(y_test, y_pred) {
  print(paste("Mean Squared Log Error =", mean((
    log(y_pred + 1) - log(y_test + 1)
  ) ^ 2)))
  print(paste("Root Mean Squared Log Error =", sqrt(mean((
    log(y_pred + 1) - log(y_test + 1)
  ) ^ 2))))
  print(paste("Mean Squared Error =", mean((y_pred - y_test) ^ 2)))
  print(paste("Root Mean Squared Error =", sqrt(mean((y_pred - y_test) ^
                                                       2
  ))))
  print(paste("R^2 =", cor(y_pred, y_test) ^ 2))
}

# Linear Regression
X_train <- as.data.frame(lapply(X_train, unlist))
colnames(X_train) <- make.names(colnames(X_train), unique = TRUE)

lr_model <- lm(y_train ~ ., data = X_train)

X_test <- as.data.frame(lapply(X_test, unlist))
colnames(X_test) <- make.names(colnames(X_test), unique = TRUE)

y_pred_lr <- lr_model %>% predict(X_test)
print("Linear Regression Prediction Metrics:")
show_metrics(y_test, y_pred_lr)


# Artificial Neural Network
library(keras)
X_train_matrix <- as.matrix(X_train)

nn_model <- keras_model_sequential() %>%
  layer_dense(
    units = 712,
    activation = "relu",
    input_shape = ncol(X_train_matrix)
  ) %>%
  layer_dense(units = 356, activation = "relu") %>%
  layer_dense(units = 356, activation = "relu") %>%
  layer_dense(units = 256, activation = "relu") %>%
  layer_dense(units = 1)

nn_model %>% compile(
  optimizer = "rmsprop",
  loss = "mean_squared_error",
  metrics = c("mean_squared_error")
)

history <- nn_model %>% fit(X_train_matrix,
                            y_train,
                            epochs = 100, )

X_test_matrix <- as.matrix(X_test)
y_pred_nn <- predict(nn_model, X_test_matrix)
show_metrics(y_test, y_pred_nn)


# Random Forest
library(randomForest)

set.seed(42)

# Base Random Forest Regressor
rf_base <- randomForest(x = X_train, y = y_train)

y_rf_base_pred <- predict(rf_base, newdata = X_test)
print("Base Random Forest Regressor Prediction Metrics:\n")
show_metrics(y_test, y_rf_base_pred)

varImpPlot(rf_base)


# Tuned Random Forest Regressor
n_estimators <- seq(200, 2000, length.out = 10)
max_features <- c('auto', 'sqrt')
max_depth <- seq(10, 110, length.out = 11)
max_depth <-
  c(max_depth, NA)
min_samples_split <- c(2, 5, 10)
min_samples_leaf <- c(1, 2, 4)
bootstrap <- c(TRUE, FALSE)

random_grid <- expand.grid(
  n_estimators = n_estimators,
  max_features = max_features,
  max_depth = max_depth,
  min_samples_split = min_samples_split,
  min_samples_leaf = min_samples_leaf,
  bootstrap = bootstrap
)

train_control <- trainControl(
  method = "cv",
  number = 3,
  search = "random",
  verboseIter = TRUE
)

rf_tuned <- randomForest(
  x = X_train,
  y = y_train,
  trControl = train_control,
  tuneGrid = random_grid,
  nIter = 75
)

y_rf_tuned_pred <- predict(rf_tuned, newdata = X_test)
print("Tuned Random Forest Regressor Prediction Metrics:\n")
show_metrics(y_test, y_rf_tuned_pred)

varImpPlot(rf_tuned)


# Light Gradient Boosting Machine
msle <- function(y_true, y_pred) {
  sqrt(mean((log1p(y_pred) - log1p(y_true)) ^ 2))
}

params <- list(
  num_leaves = 30,
  min_data_in_leaf = 20,
  objective = "regression",
  max_depth = 5,
  learning_rate = 0.01,
  boosting = "gbdt",
  feature_fraction = 0.9,
  bagging_freq = 1,
  bagging_fraction = 0.9,
  bagging_seed = 11,
  metric = "rmse",
  lambda_l1 = 0.2,
  verbosity = -1
)

library(lightgbm)

model_lgb <- lgb.train(
  params = params,
  data = lgb.Dataset(data = as.matrix(X_train), label = y_train),
  nrounds = 20000,
)

prediction_lgb_test <-
  predict(model_lgb, newdata = as.matrix(X_test))

show_metrics(y_test, prediction_lgb_test)


# Extreme Gradient Boosting
library(xgboost)

params <- list(
  objective = "reg:linear",
  eta = 0.01,
  max_depth = 6,
  subsample = 0.6,
  colsample_bytree = 0.7,
  eval_metric = "rmse",
  silent = TRUE
)

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 20000,
  watchlist = list(train = dtrain),
  early_stopping_rounds = 10,
  maximize = FALSE
)

y_xgb_pred <- predict(xgb_model, as.matrix(X_test))
show_metrics(y_test, y_xgb_pred)

#
# # CatBoost Regression
# library(remotes)
# remotes::install_url(
#   'https://github.com/catboost/catboost/releases/download/v1.2.5/catboost-R-windows-x86_64-1.2.5.tgz',
#   INSTALL_opts = c("--no-multiarch", "--no-test-load")
# )
#
#
# library(catboost)
#
# model <- catboost.load_pool(data = X_train,
#                             label = y_train,
#                             cat_features = NULL)
#
# fit_model <- catboost.train(
#   data = model,
#   num_iterations = 10000,
#   learning_rate = 0.004,
#   depth = 5,
#   col_sample_rate = 0.8,
#   random_seed = 2020,
#   bagging_temperature = 0.2,
#   metric_period = NULL
# )
#
# test_pred <-
#   catboost.predict(fit_model, data = X_test, prediction_type = "RawFormulaVal")
#
# show_metrics(y_test, test_pred)


# Elastic-Net
library(glmnet)

elastic_net_model <-
  glmnet(
    x = X_train,
    y = y_train,
    alpha = 0.5,
    lambda = NULL,
    random.state = 42
  )

y_elastic_pred <-
  predict(elastic_net_model, newx = as.matrix(X_test))
show_metrics(y_test, y_elastic_pred)

# ElasticNetCV Model
elastic_netCV_model <-
  cv.glmnet(
    x = as.matrix(X_train),
    y = y_train,
    alpha = 0.5,
    nfolds = 5,
    random.state = 42
  )

y_elastic_CV_pred <-
  predict(elastic_netCV_model, newx = as.matrix(X_test), s = "lambda.min")
show_metrics(y_test, y_elastic_CV_pred)


# Ridge Regression
ridged_model <-
  glmnet(as.matrix(X_train),
         as.matrix(y_train),
         alpha = 0,
         lambda = 0)
y_ridge_pred <- predict(ridged_model, newx = as.matrix(X_test))
show_metrics(y_test, y_ridge_pred)

# Ridge Regression with Cross-Validation
lambdas <- c(0.0005, 0.001, 0.00125, 0.0015, 0.00175, 0.002)
ridgeCV_model <-
  cv.glmnet(as.matrix(X_train),
            as.matrix(y_train),
            alpha = 0,
            lambda = lambdas)

optimal_lambda <- ridgeCV_model$lambda.min

y_ridgeCV_pred <-
  predict(ridgeCV_model, newx = as.matrix(X_test), s = optimal_lambda)
show_metrics(y_test, y_ridgeCV_pred)

cat("Optimal lambda:", optimal_lambda, "\n")


# Support Vector Regression
library(e1071)

svr_model <- svm(
  y_train ~ .,
  data = X_train,
  kernel = "radial",
  cost = 100,
  epsilon = 1
)

y_pred_svr <- predict(svr_model, X_test)
show_metrics(y_test, y_pred_svr)


# Perform grid search with cross-validation
params_selection <- expand.grid(C = seq(10, 200, length.out = 10),
                                epsilon = seq(0.02, 6, length.out = 10))

# Function for cross-validation
cv_svr <- function(params, X_train, y_train) {
  n <- nrow(X_train)
  folds <- sample(rep(1:5, length.out = n))  # Create 5 folds
  
  cv_scores <- numeric(5)
  for (i in 1:5) {
    test_idx <- which(folds == i)
    train_idx <- which(folds != i)
    
    model <- tune.svm(
      X_train[train_idx, ],
      y_train[train_idx],
      gamma = 1 / dim(X_train)[2],
      cost = params["C"],
      epsilon = params["epsilon"],
      kernel = "radial"
    )
    
    print(model)
    
    y_pred <- predict(model$best.model, X_train[test_idx, ])
    cv_scores[i] <- mean((y_pred - y_train[test_idx]) ^ 2)  # MSE
  }
  
  return(mean(cv_scores))
}

# Gridsearch to find the best params
best_params <-
  params_selection[which.min(apply(params_selection, 1, cv_svr, X_train, y_train)), ]

print(paste0(
  "Best Selected Model with params C = ",
  best_params["C"],
  ", epsilon = ",
  best_params["epsilon"]
))

# Train the model with best parameters
best_model <- tune.svm(
  X_train,
  y_train,
  gamma = 1 / dim(X_train)[2],
  cost = best_params["C"],
  epsilon = best_params["epsilon"],
  kernel = "radial"
)$best.model

y_pred <- predict(best_model, X_test)
show_metrics(y_test, y_pred)


# Find top correlated features
cor_mat <- cor(X_train, y_train, method = "pearson")
cor_target <- abs(cor_mat)[, 1]

relevant_feature_names <- names(cor_target[cor_target > 0.19])
relevant_feature_names <-
  relevant_feature_names[relevant_feature_names != "log_revenue"]

top_features <- sort(as.table(cor_target[cor_target > 0.27]))

# Generate a bar plot of the top features
par(mar = c(5, 20, 2, 2), cex.lab = 1.5)
barplot(
  top_features,
  horiz = TRUE,
  main = "Top Features Contributing to revenue",
  xlab = "Pearson's Coefficient",
  las = 2,
  space = 0.4,
)

print(relevant_feature_names)
print(length(relevant_feature_names))

minimized_X_train <- X_train[, relevant_feature_names]
minimized_X_test <- X_test[, relevant_feature_names]

# Training with top features
svr_pearson <- svm(
  y_train ~ .,
  data = minimized_X_train,
  kernel = "radial",
  cost = 200,
  epsilon = 1.67
)

y_pred_svr_pearson <-
  predict(svr_pearson, newdata = minimized_X_test)
show_metrics(y_test, y_pred_svr_pearson)


# Applying Principal Component Analysis
library(stats)

pca <- prcomp(X_train, scale = TRUE)

pca_X_train <- pca$x[, 1:20]
pca_X_test <- predict(pca, newdata = X_test)[, 1:20]

# Print the explained variance ratio
print(summary(pca)$importance[3, 1:20])

# Training with top PCA features
svr_pca <- svm(
  y_train ~ .,
  data = pca_X_train,
  kernel = "radial",
  cost = 200,
  epsilon = 1.67
)

y_pred_svr_pca <- predict(svr_pca, newdata = pca_X_test)
show_metrics(y_test, y_pred_svr_pca)


# Stacked Model
X_train_meta <-
  bind_cols(predict(xgb_model, as.matrix(X_train)),
            predict(svr_model, X_train)) %>% as.matrix()
y_train_meta <- y_train %>% as.matrix()

X_test_meta <-
  bind_cols(predict(xgb_model, as.matrix(X_test)), predict(svr_model, X_test)) %>% as.matrix()
y_test_meta <- y_test

meta_nn <- keras_model_sequential() %>%
  layer_dense(
    units = 8,
    activation = "gelu",
    kernel_regularizer = regularizer_l1_l2(l1 = 0.01, l2 = 0.01),
    input_shape = ncol(X_train_meta)
  ) %>%
  layer_dense(units = 1)

meta_nn %>% compile(
  optimizer = "adam",
  loss = "mean_squared_error",
  metrics = c("mean_squared_error")
)

hist_meta <- meta_nn %>% fit(X_train_meta,
                             y_train_meta,
                             epochs = 150)

y_pred_meta <- predict(meta_nn, X_test_meta)
show_metrics(y_test_meta, y_pred_meta)
