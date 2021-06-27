# Using gbm for gradient boosting classification

library(tidyverse)
library(fastDummies)
library(rsample)
library(gbm)

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


stroke_gbm1 <- gbm(
  formula = stroke ~ .,
  data = train_df,
  distribution = "gaussian",
  n.trees = 5000,
  shrinkage = 0.1,
  interaction.depth = 3,
  n.minobsinnode = 10,
  cv.folds = 10
)

# find index fornumber trees with minimum cv error
best <- which.min(stroke_gbm1$cv.error)

#get RMSE
sqrt(stroke_gbm1$cv.error[best])

# plot error curve
gbm.perf(stroke_gbm1, method = "cv")


