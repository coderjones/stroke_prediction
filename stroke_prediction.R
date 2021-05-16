# Stroke prediction in R
library(tidyverse)
library(fastDummies)

# set working directory
setwd("/Users/jeremiahhamilton/code/stroke_prediction")

# read in data
df <- read.csv("healthcare-dataset-stroke-data.csv")

# create dummy variables for factors

# bin bmi values into normal, obese, morbidly obese
df$bmi = cut(as.numeric(df$bmi), breaks=c(0,25,30,40,100), labels=c("normal","over_weight", "obese", "morbid"),
             include.lowest=T)

df <- dummy_cols(df, select_columns = c('smoking_status','bmi'))

s_lm <- lm(stroke ~ age + bmi_over_weight + bmi_obese + bmi_morbid + hypertension + smoking_status_smokes, data = df, na.action = na.omit)
summary(s_lm)

# From the following linear model we can see age and hypertension are much more significant
# than smoking or bmi