##----------PREP----------##
library(tidyverse)
library(glmnet)
set.seed(39415)



##----------FUNCTIONS----------##
names_times_x <- function(x, col_names) {
  new_names <- c()
  for(i in 1:x) {
    names_x <- paste0(col_names, i)
    new_names <- c(new_names, names_x)
  }
  return(new_names)
}


model.lasso_cv <- function(df) {
  as.mat <- model.matrix(Regime ~ ., df)[,-1]
  reg <- df$Regime
  
  cv_model <- cv.glmnet(as.mat, reg, alpha = 1, intercept = FALSE)
  best_lambda <- cv_model$lambda.min
  best_model <- glmnet(as.mat, reg, alpha = 1, lambda = best_lambda, 
                       intercept = FALSE)
  
  return(best_model)
}



##----------LOAD DATA----------##
regime.table <- read_rds('Data.rds')

logged.table <- regime.table
logged.table[,names_times_x(10, 'GDPPerCapita')] <- 
  log(logged.table[,names_times_x(10, 'GDPPerCapita')])
logged.table[,names_times_x(10, 'Population')] <- 
  log(logged.table[,names_times_x(10, 'Population')])

logged.table_with_year_and_entity <- logged.table %>% 
  mutate(Regime = as.numeric(Regime))
logged.table <- logged.table %>% select(-c('Year', 'Entity')) %>% 
  mutate(Regime = as.numeric(Regime))



##----------AUTOMATIC FEATURE SELECTION----------##
all_features <- names(logged.table)


# (1) 1,10; (2) 9,10; (3) 8,10; (4) 5,10; (5) 1-10; (6) 1-5
selection.labels <- c('Years 1, 10', 'Years 9, 10', 'Years 8, 10', 
                      'Years 5-10', 'Years 1-10', 'Years 1-5')
selections <- list(c(all_features[grepl('.+(1|10)', all_features)]),
                   c(all_features[grepl('.+(9|10)', all_features)]),
                   c(all_features[grepl('.+(8|10)', all_features)]),
                   c(all_features[grepl('[a-zA-Z](6.)*(5|10)', all_features)]),
                   c(all_features[grepl('.+[0-9]', all_features)]),
                   c(all_features[grepl('.+[1-5]$', all_features)]))
selections <- lapply(selections, function(x) { c(all_features[c(1:2)], x) })


models <- vector("list", length(selections))
for(i in 1:length(selections)) {
  selected <- logged.table[, selections[[i]]]

  rval <- model.lasso_cv(selected)
  models[[i]] <- rval

  all.features <- coef(models[[i]])@Dimnames[[1]]
  non_zero.indices <- !as.numeric(coef(models[[i]]) == 0)
  non_zero.features <- all.features[non_zero.indices]

  print(paste0('-----------', selection.labels[[i]], '-----------'))
  print(sort(non_zero.features))
  print(paste('R-squared:', models[[i]]$dev.ratio))
}



##----------MANUAL FEATURE SELECTION----------##
best.model <- models[[1]]
coef(best.model)
# USED FEATURES AND THEIR COEFFICIENT VALUES:
# DecadePoverty      -0.016002429 x
# GDPPerCapita1       0.705534980 --> 10
# GiniCoef1           0.047931760 x
# GiniCoef10          0.062487982
# InfantMortality1   -0.009768679 x
# InfantMortality10   0.003434244
# LifeSpan1          -0.006112891 x
# LifeSpan10         -0.028662206
# Population1         4.230515465 y
# Population10       -4.066242290 y
# YearsSchooling1.5   1.368990550 y
# YearsSchooling6.10 -0.467806272 y
available.features <- coef(best.model)@Dimnames[[1]]
available.features <- c('Regime', available.features[!grepl('Intercept', 
                                                  available.features)])
data.cols <- logged.table[available.features]


no_poverty <- data.cols %>% select(-c('DecadePoverty'))
no_poverty.model <- lm(Regime ~ . - 1, data = no_poverty)
summary(no_poverty.model)
# RESULT: Poverty not necessary as predictor (probably contained in GDP/Gini)


no_gdp1 <- no_poverty %>% select(-c('GDPPerCapita1'))
no_gdp1.model <- lm(Regime ~ . - 1, data = no_gdp1)
summary(no_gdp1.model)
# RESULT: GDP is redundant, choosing 10th year due to proximity to change


no_gini1 <- no_gdp1 %>% select(-c('GiniCoef1'))
no_gini1.model <- lm(Regime ~ . - 1, data = no_gini1)
summary(no_gini1.model)
# RESULT: Gini is redundant, choosing 10th year


no_infm1 <- no_gini1 %>% select(-c('InfantMortality1'))
no_infm1.model <- lm(Regime ~ . - 1, data = no_infm1)
summary(no_infm1.model)
# RESULT: Infant Mortality is redundant, choosing 10th year


no_lfs1 <- no_infm1 %>% select(-c('LifeSpan1'))
no_lfs1.model <- lm(Regime ~ . - 1, data = no_lfs1)
summary(no_lfs1.model)
# RESULT: LifeSpan is redundant, choosing 10th year


no_ysch1 <- no_lfs1 %>% select(-c('YearsSchooling1.5'))
no_ysch1.model <- lm(Regime ~ . - 1, data = no_ysch1)
summary(no_ysch1.model)
# RESULT: YearsSchooling change gives slight advantage over lack of var


no_pop1 <- no_ysch1 %>% select(-c('Population1'))
no_pop1.model <- lm(Regime ~ . - 1, data = no_pop1)
summary(no_pop1.model)

no_pop <- no_pop1 %>% select(-c('Population10'))
no_pop.model <- lm(Regime ~ . - 1, data = no_pop)
summary(no_pop.model)
# RESULT: Population change gives slight advantage over lack of var



##----------RMSE OF 2017 PREDICTIONS IN FINAL MODEL----------##
final.predictors <- c('GDPPerCapita10', 'GiniCoef10', 
                     'InfantMortality10', 'LifeSpan10', 'YearsSchooling1.5',
                     'YearsSchooling6.10', 'Population1', 'Population10')
final.data <- logged.table_with_year_and_entity[, c('Regime', 'Year', final.predictors)]
final.to_2017 <- final.data[final.data$Year < 2017,] %>% select(-c('Year'))
final.2017 <- final.data[final.data$Year == 2017,] %>% select(-c('Year'))

final.model <- lm(Regime ~ ., 
                  data = final.to_2017)

final.predicted <- predict(final.model, 
                           final.2017)
final.predicted <- floor(final.predicted)

final.distance <- final.predicted - final.2017$Regime
final.rmse <- sqrt(mean(final.distance^2))
final.rmse


