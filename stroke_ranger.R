# Using ranger for Random Forest classification

library(tidyverse)
library(fastDummies)
library(ranger)
library(rsample)

# set working directory
setwd("/Users/jeremiahhamilton/code/stroke_prediction")

# read in data
df <- read.csv("healthcare-dataset-stroke-data.csv")

df <- dummy_cols(df, select_columns = c('smoking_status', 'ever_married','work_type', 'Residence_type', 'gender'))

df %>% select(-ever_married, -work_type, -Residence_type, -gender, -smoking_status, -id, -avg_glucose_level) -> df

# split into train/test groups
set.seed(22)
df_split <- initial_split(df, prop = .7, strata = "stroke")
train_df <- training(df_split)
test_df <- testing(df_split)

n_features <- length(setdiff(names(train_df), "stroke"))

stroke_rf1 <- ranger (
  stroke ~ age + hypertension + heart_disease + bmi + smoking_status_smokes,
  data = train_df,
  respect.unordered.factors = "order",
  seed = 123
)

(default_rmse <- sqrt(stroke_rf1$prediction.error))

