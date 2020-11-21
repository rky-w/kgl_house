

library(tidyverse)
library(janitor)


## Data Loading ----
train <- read.csv("./data/raw/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("./data/raw/test.csv", header = TRUE, stringsAsFactors = FALSE)


## Data Checks ----

dim(train)
length(unique(train$Id))

str(train)



## Data Cleaning ----

# Wrap as function to re-apply against test

df <- train

# Ensure character variables are correctly represented as factors
df$MSSubClass <- as.character(df$MSSubClass)

dfc <- df %>% 
  mutate(across(where(is.character), as.factor))

# Check missing values

apply(dfc, 2, function(x) sum(is.na(x)))










