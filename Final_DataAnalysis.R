


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


back_step.cv.lm <- function(folds, features.order, df, train_size = 0.8) {
  rows <- nrow(df)
  prev.RMSE <- Inf
  
  for(feat in features.order) {
    avg.RMSE <- 0
    
    feats  <- features.order[features.order != feat]
    sub.df <- df[c('Regime', feats)] %>% mutate(Regime = as.numeric(Regime))
    
    print(feats)
    
    for(i in 1:folds) {
      print(i)
      
      row.order  <- sample(rows)
      train.rows <- sub.df[ head(row.order, nrow(sub.df)*train_size), ]
      test.rows  <- sub.df[ tail(row.order, -nrow(sub.df)*train_size), ]
      
      curr.model <- lm(Regime ~ ., train.rows)
      
      # Fill out the factor levels that were not used during training
      curr.model$xlevels$Entity <- levels(df$Entity)
      
      predictions <- predict(curr.model, test.rows)
      
      avg.RMSE = avg.RMSE + sqrt(mean((test.rows$Regime - predictions)^2))
    }
    
    avg.RMSE <- avg.RMSE / folds
    
    print(paste('Average:', avg.RMSE))
    if(avg.RMSE < prev.RMSE) {
      features.order <- feats
      prev.RMSE <- avg.RMSE
      print('Updated!')
    }
  }
  
  print('Final features:')
  print(features.order[order(features.order)])
  final.model <- lm(Regime ~ ., data = df[c('Regime', features.order)] %>% 
                      mutate(Regime = as.numeric(Regime)))
  return(final.model)
}


# GLMNET
back_step.cv.lm2 <- function(folds, features.order, df, train_size = 0.8) {
  rows <- nrow(df)
  prev.RMSE <- Inf
  
  for(feat in features.order) {
    avg.RMSE <- 0
    
    feats  <- features.order[features.order != feat]
    sub.df <- df[c('Regime', feats)] %>% mutate(Regime = as.numeric(Regime))
    
    print(feats)
    
    for(i in 1:folds) {
      print(i)
      
      row.order  <- sample(rows)
      train.rows <- sub.df[ head(row.order, nrow(sub.df)*train_size), ]
      train.mat <- model.matrix(Regime ~ ., train.rows)[,-1]
      test.rows  <- sub.df[ tail(row.order, -nrow(sub.df)*train_size), ]
      test.mat <- model.matrix(Regime ~ ., test.rows)[,-1]
      
      curr.model <- glmnet(train.mat, train.rows$Regime, alpha = 1, lambda = 0.01)
      
      # Fill out the factor levels that were not used during training
      curr.model$xlevels$Entity <- levels(df$Entity)
      
      predictions <- predict(curr.model, test.mat)
      
      avg.RMSE = avg.RMSE + sqrt(mean((test.rows$Regime - predictions)^2))
    }
    
    avg.RMSE <- avg.RMSE / folds
    
    print(paste('Average:', avg.RMSE))
    if(avg.RMSE < prev.RMSE) {
      features.order <- feats
      prev.RMSE <- avg.RMSE
      print('Updated!')
    }
  }
  
  print('Final features:')
  print(features.order[order(features.order)])
  final.mat <- model.matrix(Regime ~ ., df)[,-1]
  final.model <- glmnet(final.mat, df$Regime, alpha = 1)
  return(final.model)
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



##----------SELECT FEATURES----------##
all_features <- names(logged.table)


# (1) 1,10; 2(2) 9,10; (3) 8,10; (4) 5,10; (5) 1-10; (6) 1-5
selections <- list(c(all_features[grepl('.+(1|10)', all_features)]),
                   c(all_features[grepl('.+(9|10)', all_features)]),
                   c(all_features[grepl('.+(8|10)', all_features)]),
                   c(all_features[grepl('[a-zA-Z](6.)*(5|10)', all_features)]),
                   c(all_features[grepl('.+[0-9]', all_features)]),
                   c(all_features[grepl('.+[1-5]$', all_features)]))
selections <- lapply(selections, function(x) { c(all_features[c(1:4)], x) })


models <- vector("list", length(selections))
folds = 5
for(i in 1:length(selections)) {
  selected <- logged.table[, selections[[i]]]
  
  features.order <- names(selected)[sample(length(selected))]
  features.order <- c('', features.order[features.order != 'Regime'])
  
  print(paste0('-----------', i, '-----------'))
  models[[i]] <- back_step.cv.lm2(folds, features.order, selected)
}


for(i in 1:length(selections)) {
  print(paste0('-----------', i, '-----------'))
  mdl <- models[[i]]
  all.features <- names(mdl$coefficients)
  simple.features <- c('Entity', all.features[!grepl('(Entity|Intercept)', all.features)])
  print(sort(simple.features))
  print(paste('Adjusted R-sq:', summary(mdl)$adj.r.squared))
}


# ORIGINAL
# for(i in 1:length(selections)) {
#   print(paste0('-----------', i, '-----------'))
#   mdl <- models[[i]]
#   all.features <- names(mdl$coefficients)
#   simple.features <- c('Entity', all.features[!grepl('(Entity|Intercept)', all.features)])
#   print(sort(simple.features))
#   print(paste('Adjusted R-sq:', summary(mdl)$adj.r.squared))
# }

























