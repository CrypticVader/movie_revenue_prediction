library(tidyverse)

# install.packages("Hmisc")
library(Hmisc)

# install.packages("CatEncoders")
library(CatEncoders)

# install.packages("fastDummies")
library(fastDummies)

impute(df$runtime, 0)
impute(df$budget, 0)
impute(df$popularity, 0)
impute(df$collection_name, "")
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

df <- df[, !grepl("genres_etc", names(df))]

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
