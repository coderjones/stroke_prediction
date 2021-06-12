# Stroke prediction in R  Really this should've been a classification solution not regression.
library(tidyverse)
library(fastDummies)
library(recipes)
library(rsample)
library(caret)

# set working directory
setwd("/Users/jeremiahhamilton/code/stroke_prediction")

# read in data
df <- read.csv("healthcare-dataset-stroke-data.csv")

# create dummy variables for factors

# bin bmi values into normal, obese, morbidly obese
#df$bmi = cut(as.numeric(df$bmi), breaks=c(0,25,30,40,100), labels=c("normal","over_weight", "obese", "morbid"),
#             include.lowest=T)

df <- dummy_cols(df, select_columns = c('smoking_status', 'ever_married','work_type', 'Residence_type', 'gender'))

df %>% select(-ever_married, -work_type, -Residence_type, -gender, -smoking_status) -> df

# split into train/test groups
set.seed(22)
df_split <- initial_split(df, prop = .7, strata = "stroke")
train_df <- training(df_split)
test_df <- testing(df_split)


blueprint <- recipe(stroke ~ ., data= train_df) %>%
  step_nzv(all_nominal()) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes()) %>%
  step_pca(all_numeric(), -all_outcomes())

prepare <- prep(blueprint, training = train_df)

baked_train <- bake(prepare, new_data = train_df)
baked_test <- bake(prepare, new_data = test_df)

cv_model1 <- train(
  form= stroke ~ .,
  data = train_df,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

cv_model_pls <- train(
  form= stroke ~ .,
  data = train_df,
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv","center","scale"),
  tuneLength = 20
)

ggplot(cv_model_pls)