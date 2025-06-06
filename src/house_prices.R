
## KAGGLE: House Prices competition

library(tidyverse)
library(tidymodels)
library(tictoc)
library(doParallel)
library(vip)



## Data Loading ----
rawtrain <- read.csv("./data/raw/train.csv", header = TRUE, stringsAsFactors = FALSE)
rawtest <- read.csv("./data/raw/test.csv", header = TRUE, stringsAsFactors = FALSE)

## Data Cleaning ----

str(rawtrain)
str(rawtest)

rawtrain$set <- "Train"
rawtest$SalePrice <- NA
rawtest$set <- "Test"

comb <- rbind(rawtrain, rawtest)

str(comb)



# Choose between datasets to assess
df <- comb
dfclean <- df



# Check missing values ----
misstab <- apply(df, 2, function(x) sum(is.na(x)))
missvals <- which(misstab > 0)
missvars <- names(missvals)

sort(missvals / nrow(df))

summary(df[missvars])


vartypes <- unlist(lapply(df[missvars], class))


charcheck <- function(var, impNA = FALSE, impNAval = "None") {
  print(table(var, useNA = "always"))
  
  if (impNA) {
    var[is.na(var)] <- impNAval
    return(var)
  }
}

numcheck <- function(var, impNA = FALSE, impNAval = 0) {
  hist(var)
  print(prop.table(table(is.na(var))))
  print(summary(var))
  
  if (impNA) {
    var[is.na(var)] <- impNAval
    hist(var)
    return(var)
  } 
}

vartypes

dfclean$MSZoning <- charcheck(df$MSZoning, impNA = TRUE, impNAval = "RL")

dfclean$LotFrontage <- numcheck(df$LotFrontage, impNA = TRUE, impNAval = median(rawtrain$LotFrontage, na.rm = TRUE))

dfclean$Alley <- charcheck(df$Alley, impNA = TRUE, impNAval = "None")

dfclean$Utilities <- charcheck(df$Utilities, impNA = TRUE, impNAval = "AllPub")

dfclean$Exterior1st <- charcheck(df$Exterior1st, impNA = TRUE, impNAval = "VinylSd")

dfclean$Exterior2nd <- charcheck(df$Exterior2nd, impNA = TRUE, impNAval = "VinylSd")

dfclean$MasVnrType <- charcheck(df$MasVnrType, impNA = TRUE, impNAval = "None")

dfclean$MasVnrArea <- numcheck(df$MasVnrArea, impNA = TRUE, impNAval = 0)

dfclean$BsmtQual <- charcheck(df$BsmtQual, impNA = TRUE, impNAval = "TA")

dfclean$BsmtCond <- charcheck(df$BsmtCond, impNA = TRUE, impNAval = "TA")

dfclean$BsmtExposure <- charcheck(df$BsmtExposure, impNA = TRUE, impNAval = "No")

dfclean$BsmtFinType1 <- charcheck(df$BsmtFinType1, impNA = TRUE, impNAval = "None")

dfclean$BsmtFinType2 <- charcheck(df$BsmtFinType2, impNA = TRUE, impNAval = "None")

dfclean$BsmtFinSF1 <- numcheck(df$BsmtFinSF1, impNA = TRUE, impNAval = median(rawtrain$BsmtFinSF1, na.rm = TRUE))

dfclean$BsmtFinSF2 <- numcheck(df$BsmtFinSF2, impNA = TRUE, impNAval = median(rawtrain$BsmtFinSF2, na.rm = TRUE))

dfclean$BsmtUnfSF <- numcheck(df$BsmtUnfSF, impNA = TRUE, impNAval = median(rawtrain$BsmtUnfSF, na.rm = TRUE))

dfclean$TotalBsmtSF <- numcheck(df$TotalBsmtSF, impNA = TRUE, impNAval = median(rawtrain$TotalBsmtSF, na.rm = TRUE))

dfclean$BsmtFullBath <- numcheck(df$BsmtFullBath, impNA = TRUE, impNAval = median(rawtrain$BsmtFullBath, na.rm = TRUE))

dfclean$BsmtHalfBath <- numcheck(df$BsmtHalfBath, impNA = TRUE, impNAval = median(rawtrain$BsmtHalfBath, na.rm = TRUE))

dfclean$Electrical <- charcheck(df$Electrical, impNA = TRUE, impNAval = "SBrkr")

dfclean$KitchenQual <- charcheck(df$KitchenQual, impNA = TRUE, impNAval = "TA")

dfclean$Functional <- charcheck(df$Functional, impNA = TRUE, impNAval = "Typ")

dfclean$FireplaceQu <- charcheck(df$FireplaceQu, impNA = TRUE, impNAval = "TA")

dfclean$GarageType <- charcheck(df$GarageType, impNA = TRUE, impNAval = "Attchd")

df$GarageYrBlt[df$GarageYrBlt > 2020] <- NA
numcheck(df$GarageYrBlt)
df$GarageYrBlt[is.na(df$GarageYrBlt)] <- df$YearBuilt[is.na(df$GarageYrBlt)]
dfclean$GarageYrBlt <- df$GarageYrBlt
numcheck(dfclean$GarageYrBlt)

dfclean$GarageFinish <- charcheck(df$GarageFinish, impNA = TRUE, impNAval = "Unf")

dfclean$GarageCars <- numcheck(df$GarageCars, impNA = TRUE, impNAval = round(median(df$GarageCars, na.rm = TRUE)))

dfclean$GarageArea <- numcheck(df$GarageArea, impNA = TRUE, impNAval = median(df$GarageAre, na.rm = TRUE))

dfclean$GarageQual <- charcheck(df$GarageQual, impNA = TRUE, impNAval = "TA")

dfclean$GarageCond <- charcheck(df$GarageCond, impNA = TRUE, impNAval = "TA")

dfclean$PoolQC <- charcheck(df$PoolQC, impNA = TRUE, impNAval = "None")

dfclean$Fence <- charcheck(df$Fence, impNA = TRUE, impNAval = "None")

dfclean$MiscFeature <- charcheck(df$MiscFeature, impNA = TRUE, impNAval = "None")

dfclean$SaleType <- charcheck(df$SaleType, impNA = TRUE, impNAval = "WD")


# Any remaining missing?
misstab <- apply(dfclean, 2, function(x) sum(is.na(x)))
missvals <- which(misstab > 0)
missvars <- names(missvals)

sort(missvals / nrow(df))

summary(df[missvars])
# Yes, just the ones we didn't impute


# Create new variables
dfclean$TotSF <- dfclean$TotalBsmtSF + dfclean$X1stFlrSF + dfclean$X2ndFlrSF

# Ensure character variables are correctly represented
dfclean$MSSubClass <- as.character(dfclean$MSSubClass)

## Get variable types
dfvartypes <- unlist(lapply(dfclean, class))

charvars <- names(dfvartypes[which(dfvartypes %in% c('factor', 'character'))])
numvars <- names(dfvartypes[which(dfvartypes %in% c('integer', 'numeric') & 
                                    !(names(dfvartypes) %in% c('Id', "SalePrice")))])

# Ensure character levels are represent across both train and test ----
apply(dfclean[charvars], 2, table, dfclean$set)


dfclean <- dfclean %>% 
  mutate(
    MSSubClass = case_when(
      MSSubClass == '150' ~ '120',
      MSSubClass == '180' ~ '160',
      MSSubClass == '40' ~ '50',
      TRUE ~ MSSubClass),
    Condition1 = case_when(
      Condition1 == 'RRNe' ~ 'RRAe',
      Condition1 == 'RRNn' ~ 'RRAn',
      Condition1 == 'PosA' ~ 'PosN',
      TRUE ~ Condition1),
    HouseStyle = case_when(
      HouseStyle == '1.5Unf' ~ '1.5Fin',
      HouseStyle == '2.5Fin' ~ '2.5Unf',
      TRUE ~ HouseStyle),
    RoofStyle = case_when(
      RoofStyle == 'Shed' ~ 'Flat',
      RoofStyle == 'Mansard' ~ 'Gambrel',
      TRUE ~ RoofStyle),
    Asbestos = ifelse(Exterior1st == 'AsbShng' | Exterior2nd == 'AsbShng', 'Asbestos', 'None'),
    Exterior1st = case_when(
      Exterior1st %in% c('AsbShng', 'AsphShn', 'WdShing', 'Wd Shng') ~ 'Shingle',
      Exterior1st %in% c('ImStucc', 'Stucco') ~ 'Stucco',
      Exterior1st %in% c('HdBoard', 'Plywood', 'Wd Sdng') ~ 'Wood',
      Exterior1st %in% c('BrkComm', 'Brk Cmn', 'BrkFace') ~ 'Brick',
      Exterior1st %in% c('CBlock', 'CemntBd', 'CmentBd') ~ 'Cement',
      Exterior1st %in% c('VinylSd') ~ 'VinylSd',
      Exterior1st %in% c('MetalSd') ~ 'MetalSd',
      TRUE ~ 'Other'),
    Exterior2nd = case_when(
      Exterior2nd %in% c('AsbShng', 'AsphShn', 'WdShing', 'Wd Shng') ~ 'Shingle',
      Exterior2nd %in% c('ImStucc', 'Stucco') ~ 'Stucco',
      Exterior2nd %in% c('HdBoard', 'Plywood', 'Wd Sdng') ~ 'Wood',
      Exterior2nd %in% c('BrkComm', 'Brk Cmn', 'BrkFace') ~ 'Brick',
      Exterior2nd %in% c('CBlock', 'CemntBd', 'CmentBd') ~ 'Cement',
      Exterior2nd %in% c('VinylSd') ~ 'VinylSd',
      Exterior2nd %in% c('MetalSd') ~ 'MetalSd',
      TRUE ~ 'Other'),
    ExterCond = case_when(
      ExterCond %in% c('Ex', 'Gd') ~ 'Gd',
      ExterCond %in% c('Fa', 'Po') ~ 'Po',
      TRUE ~ ExterCond),
    Foundation = case_when(
      Foundation %in% c('Stone', 'Wood') ~ 'Other',
      TRUE ~ Foundation),
    BsmtCond = case_when(
      BsmtCond == 'Po' ~ 'Fa',
      TRUE ~ BsmtCond),
    HeatingQC = case_when(
      HeatingQC == 'Po' ~ 'Fa',
      TRUE ~ HeatingQC),
    Functional = case_when(
      Functional %in% c('Maj1', 'Maj2', 'Sev') ~ 'Maj',
      Functional %in% c('Min1', 'Min2', 'Mod') ~ 'Min',
      TRUE ~ Functional),
    GarageQual = case_when(
      GarageQual %in% c('Ex', 'Gd') ~ 'Gd',
      GarageQual %in% c('Fa', 'Po') ~ 'Fa',
      TRUE ~ GarageQual),
    GarageCond = case_when(
      GarageCond %in% c('Ex', 'Gd') ~ 'Gd',
      GarageCond %in% c('Fa', 'Po') ~ 'Fa',
      TRUE ~ GarageCond),
    Fence = case_when(
      Fence %in% c('GdPrv', 'GdWo') ~ 'Good',
      Fence %in% c('MnPrv', 'MnWw') ~ 'Mini',
      TRUE ~ 'None'),
    SaleType = case_when(
      SaleType == 'WD' ~ 'WD',
      SaleType == 'New' ~ 'New',
      SaleType == 'COD' ~ 'COD',
      SaleType %in% c('Con', 'ConLD', 'ConLI', 'ConLw') ~ 'Con',
      TRUE ~ 'Other'),
    SaleCondition = case_when(
      SaleCondition == 'Normal' ~ 'Normal',
      SaleCondition == 'Partial' ~ 'Partial',
      SaleCondition == 'Abnorml' ~ 'Abnorml',
      TRUE ~ 'Other')
    )


dfclean$Utilities <- NULL
dfclean$Condition2 <- NULL

dfclean$LotFrontage <- ifelse(dfclean$LotFrontage %in% c('FR2', 'FR3'), 'FR', dfclean$LotFrontage)

dfclean$RoofMatl <- ifelse(dfclean$RoofMatl == 'CompShg', 'CompShg', 'Other')

dfclean$Heating <- ifelse(dfclean$Heating == 'GasA', 'GasA', 'Other')

dfclean$Electrical <- ifelse(dfclean$Electrical %in% c('FuseF', 'FuseP', 'Mix'), 'Poor', dfclean$Electrical)

dfclean$GarageType <- ifelse(dfclean$GarageType %in% c('2Types', 'CarPort'), 'Other', dfclean$GarageType)

dfclean$PoolQC <- ifelse(dfclean$PoolQC == 'None', 'None', 'Pool')

dfclean$MiscFeature <- ifelse(dfclean$MiscFeature == 'None', 'None', 'Feat')

dfclean$MultiKitchen <- ifelse(dfclean$KitchenAbvGr > 1, 'Multi', 'Single')



# Create factors from character variables
dfcleanf <- dfclean %>% 
  mutate(across(where(is.character), as.factor))


str(dfcleanf)


# Ensure numeric variables have similar distributions across both train and test ----

for (var in numvars) {
  print(var)
  boxplot(dfcleanf[[var]] ~ dfcleanf$set, main = var)
  print(summary(dfcleanf[dfclean$set == 'Test', var]))
  print(summary(dfcleanf[dfclean$set == 'Train', var]))
  
  var <- sym(var)
  
  p <- ggplot(dfcleanf, aes(!!var, colour = set)) +
    geom_density()
  print(p)
}



# Transformations and create some new variables

dfcleanf$MiscVal <- log10(dfcleanf$MiscVal + 1)
dfcleanf$PoolArea <- log10(dfcleanf$PoolArea + 1)
dfcleanf$ScreenPorch <- log10(dfcleanf$ScreenPorch + 1)
dfcleanf$X3SsnPorch <- log10(dfcleanf$X3SsnPorch + 1)
dfcleanf$EnclosedPorch <- log10(dfcleanf$EnclosedPorch + 1)

dfcleanf$LotArea <- log10(dfcleanf$LotArea + 1)

dfcleanf$TotFullBaths <- dfcleanf$FullBath + dfcleanf$BsmtFullBath
dfcleanf$TotHalfBaths <- dfcleanf$HalfBath + dfcleanf$BsmtHalfBath



# Split back into training and submission datasets ----
training_data <- dfcleanf[dfcleanf$set == 'Train', -which(names(dfcleanf) == 'set')]

submission_data <- dfcleanf[dfcleanf$set == 'Test', -which(names(dfcleanf) %in% c('set', 'SalePrice'))]



# Data splits for modelling ----

house_split <- initial_split(training_data, strata = SalePrice, breaks = 10, prop = 3/4)

house_train <- training(house_split)

house_test <- testing(house_split)



# Preprocessing steps ----

house_recipe <- house_train %>% 
  recipe(SalePrice ~ .) %>% 
  update_role(Id, new_role = "ID") %>% 
  step_corr(all_numeric(), -all_outcomes(), threshold = .9) %>% 
  step_zv(all_predictors())
  

# Define model spec and tuning parameters ----

rf_spec <- rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>% 
  set_engine("ranger", importance = "impurity")


rf_grid <- grid_regular(
  mtry(c(5, 50)),
  min_n(c(1, 5)),
  levels = 10
)

dim(rf_grid)
sapply(rf_grid, unique)



# Build model and cross-validate ----

set.seed(345)

cv_data <- vfold_cv(house_train, v = 8, repeats = 8, strata = SalePrice, breaks = 10)

house_wf <- workflow() %>%
  add_recipe(house_recipe) %>% 
  add_model(rf_spec)


# Set up parallel processing
ncores <- parallel::detectCores()
cl <- makePSOCKcluster(ncores)
registerDoParallel(cl)

# Tune models
tic()
tree_res <- 
  house_wf %>% 
  tune_grid(
    resamples = cv_data,
    grid = rf_grid
  )
toc()
stopCluster(cl)
registerDoSEQ()
getDoParWorkers()


# Assess tuning results
tree_res %>% 
  collect_metrics() %>% 
  # filter(.metric == 'rmse') %>% 
  ggplot(aes(mtry, mean, colour = as.factor(min_n))) +
  geom_point() +
  geom_line() + 
  facet_wrap(~ .metric, ncol = 1, scales = "free_y") +
  theme(legend.position = "bottom") +
  labs(title = "Tuning results")



tree_res %>% 
  show_best("rmse")


best_mod <- tree_res %>% 
  select_best("rmse")


# Finalize model ----

final_wf <- house_wf %>% 
  finalize_workflow(best_mod)

final_fit <- final_wf %>% 
  fit(data = training_data)

final_fit


# Variable importance
final_fit %>% pull_workflow_fit() %>% 
  vip()


# Check on test set ----
predout <- cbind(house_test$Id, predict(final_fit, house_test))
names(predout) <- c('Id', 'PredPrice')

testset <- house_test %>% 
  select(Id, SalePrice) %>% 
  inner_join(predout, by = "Id")

rmse(testset, truth = SalePrice, estimate = PredPrice)
rsq(testset, truth = SalePrice, estimate = PredPrice)

# Produce final test predictions ----
submission_preds <- cbind(submission_data$Id, predict(final_fit, submission_data))
names(submission_preds) <- c("Id", "SalePrice")

write.csv(submission_preds,
          file = "./data/output/submission.csv",
          row.names = FALSE)

