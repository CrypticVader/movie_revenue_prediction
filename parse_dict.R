train <- read.csv("train.csv")
train[train == "N/A" |
        train == "NA" |
        train == "#N/A" | train == "NULL" | train == ""] <- NA

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

# Parsing columns
library(dplyr)

train <- train %>%
  rowwise() %>%
  mutate(belongs_to_collection = convert_string_to_dict(belongs_to_collection)) %>%
  ungroup()

train$belongs_to_collection[sapply(train$belongs_to_collection, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(genres = list(convert_string_to_dict(genres))) %>%
  ungroup()

train$genres[sapply(train$genres, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(production_companies = list(convert_string_to_dict(production_companies)))

train$production_companies[sapply(train$production_companies, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(production_countries = list(convert_string_to_dict(production_countries))) %>%
  ungroup()

train$production_countries[sapply(train$production_countries, is.null)] <- NA

train <- train %>%
  rowwise() %>%
  mutate(spoken_languages = list(convert_string_to_dict(spoken_languages))) %>%
  ungroup()

train$spoken_languages[sapply(train$spoken_languages, is.null)] <- NA

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
