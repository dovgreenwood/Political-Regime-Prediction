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
  
  cv_model <- cv.glmnet(as.mat, reg, alpha = 1)
  best_lambda <- cv_model$lambda.min
  best_model <- glmnet(as.mat, reg, alpha = 1, lambda = best_lambda)
  
  return(list(best_model, best_lambda))
}



##----------LOAD DATA----------##
regime.table <- read_rds('Data.rds')

logged.table <- regime.table
logged.table[,names_times_x(10, 'GDPPerCapita')] <- 
  log(logged.table[,names_times_x(10, 'GDPPerCapita')])
logged.table[,names_times_x(10, 'Population')] <- 
  log(logged.table[,names_times_x(10, 'Population')])
logged.table <- logged.table %>% select(-c('Year')) %>% 
  mutate(Regime = as.numeric(Regime))
# TODO: REMOVE REDUNDANT PREDICTORS BY ANALYZING COEFFICIENT VALUES


##----------SELECT FEATURES----------##
all_features <- names(logged.table)


# (1) 1,10; 2(2) 9,10; (3) 8,10; (4) 5,10; (5) 1-10; (6) 1-5
selections <- list(c(all_features[grepl('.+(1|10)', all_features)]),
                   c(all_features[grepl('.+(9|10)', all_features)]),
                   c(all_features[grepl('.+(8|10)', all_features)]),
                   c(all_features[grepl('[a-zA-Z](6.)*(5|10)', all_features)]),
                   c(all_features[grepl('.+[0-9]', all_features)]),
                   c(all_features[grepl('.+[1-5]$', all_features)]))
selections <- lapply(selections, function(x) { c(all_features[c(1:3)], x) })


models <- vector("list", length(selections))
folds = 5
for(i in 1:length(selections)) {
  selected <- logged.table[, selections[[i]]]

  rval <- model.lasso_cv(selected)
  models[[i]] <- rval[[1]]
  lambda <- rval[[2]]
  
  all.features <- coef(models[[i]], lambda)@Dimnames[[1]]
  non_zero.indices <- !as.numeric(coef(models[[i]], lambda) == 0)
  non_zero.features <- all.features[non_zero.indices]
  simple.features <- c('Entity', non_zero.features[!grepl('(Entity|Intercept)', non_zero.features)])
  
  print(paste0('-----------', i, '-----------'))
  print(sort(simple.features))
  print(paste('R-squared:', models[[i]]$dev.ratio))
}






















