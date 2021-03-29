
## KAGGLE: House Prices competition


library(tidyverse)
library(janitor)
library(mice)


## Data Loading ----
train <- read.csv("./data/raw/train.csv", header = TRUE, stringsAsFactors = FALSE)
test <- read.csv("./data/raw/test.csv", header = TRUE, stringsAsFactors = FALSE)


## Data Checks ----

dim(train)
length(unique(train$Id))

str(train)

summary(train)



## Data Cleaning ----

str(train)
str(test)

train$set <- "Train"
test$SalePrice <- NA
test$set <- "Test"

comb <- rbind(train, test)

str(comb)



# Choose between datasets to assess
df <- comb
dfclean <- df



# Check missing values
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

dfclean$LotFrontage <- numcheck(df$LotFrontage, impNA = TRUE, impNAval = median(train$LotFrontage, na.rm = TRUE))

dfclean$Alley <- charcheck(df$Alley, impNA = TRUE, impNAval = "None")

dfclean$Utilities <- charcheck(df$Utilities, impNA = TRUE, impNAval = "AllPub")

dfclean$Exterior1st <- charcheck(df$Exterior1st, impNA = TRUE, impNAval = "VinylSd")

dfclean$Exterior2nd <- charcheck(df$Exterior2nd, impNA = TRUE, impNAval = "VinylSd")

dfclean$MasVnrType <- charcheck(df$MasVnrType, impNA = TRUE, impNAval = "None")

dfclean$MasVnrArea <- numcheck(df$MasVnrArea, impNA = TRUE, impNAval = 0)

dfclean$BsmtQual <- charcheck(df$BsmtQual, impNA = TRUE, impNAval = "TA")

dfclean$BsmtCond <- charcheck(df$BsmtCond, impNA = TRUE, impNAval = "TA")

dfclean$BsmtExposure <- charcheck(df$BsmtExposure, impNA = TRUE, impNAval = "No")

dfclean$TotalBsmtSF <- numcheck(df$TotalBsmtSF, impNA = TRUE, impNAval = median(train$TotalBsmtSF, na.rm = TRUE))

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


# Ensure character variables are correctly represented
dfclean$MSSubClass <- as.character(dfclean$MSSubClass)

## Redefine levels
dfvartypes <- unlist(lapply(dfclean, class))

charvars <- names(dfvartypes[which(dfvartypes %in% c('factor', 'character'))])
numvars <- names(dfvartypes[which(dfvartypes %in% c('integer', 'numeric') & 
                                    !(names(dfvartypes) %in% c('Id', "SalePrice")))])

# Ensure character levels are represent across both train and test
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

# Create factors from character variables
dfcleanf <- dfclean %>% 
  mutate(across(where(is.character), as.factor))

# Create new variables
dfcleanf$TotSF <- dfcleanf$TotalBsmtSF + dfcleanf$X1stFlrSF + dfcleanf$X2ndFlrSF




# Ensure numeric variables have similar distributions across both train and test

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




# Define variable lists

str(dfcleanf)



continuous <- c("LotFrontage", 
                "LotArea", 
                "YearBuilt",
                "YearRemodAdd",
                "MasVnrArea",
                "TotalBsmtSF",
                "KitchenQual"
                
)

categorical <- c("MSSubClass",
                 "MSZoning",
                 "Street",
                 "Alley",
                 "LotShape",
                 "LandContour",
                 "utili"
                 )




## Notes
#' Combine Basement vars?
#' Percent 1st floor footage
#' Combine bathroom vars
#' Binary flags for certain categorical?
#' Create total sqr footage




