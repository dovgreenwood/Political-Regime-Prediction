## LIBRARIES ##
library(tidyverse)
library(gtools)



## FUNCTIONS ##
str_to_int <- function(pop) {
  end <- str_sub(pop, -1)
  start <- as.double( str_sub(pop, 0, -2) )
  
  if(end == 'k')
    as_int <- start * 1000
  else if(end == 'M')
    as_int <- start * 1000000
  else if(end == 'B')
    as_int <- start * 1000000000
  else
    as_int <- as.integer(pop)
  
  return(as_int)
}


supplement_rows <- function(country.df, country) {
  country.df <- ungroup(country.df)

  for(year in year.min:year.max) {
    if(year %in% country.df$Year)
      next

    country.df <- country.df %>% add_row(Year = year)
  }
  
  country.df <- country.df[order(country.df$Year),]
  return(country.df)
}


names_times_x <- function(x, col_names) {
  new_names <- c()
  for(i in 1:x) {
    names_x <- paste0(col_names, i)
    new_names <- c(new_names, names_x)
  }
  return(new_names)
}




## READ IN DATA ##
regime <- read.csv('raw/polity5.csv')

child_mortality <- read.csv('raw/child_mortality_0_5_year_olds_dying_per_1000_born.csv')
life_expectancy <- read.csv('raw/life_expectancy_years.csv')
schooling       <- read.csv('raw/mean-years-of-schooling-long-run.csv')
poverty         <- read.csv('raw/reconstruction-of-historical-poverty-trends-by-country.csv')
gdp             <- read.csv('raw/income_per_person_gdppercapita_ppp_inflation_adjusted.csv')
gini            <- read.csv('raw/gini.csv')
population      <- read.csv('raw/population_total.csv')



## RENAME COLUMNS ##
regime <- regime %>% rename(Entity = country, Year = year, Regime = polity2)

child_mortality <- child_mortality %>% rename(Entity = names(child_mortality)[[1]])
names(child_mortality) <- gsub('X', '', names(child_mortality))

life_expectancy <- life_expectancy %>% rename(Entity = names(life_expectancy)[[1]])
names(life_expectancy) <- gsub('X', '', names(life_expectancy))

population <- population %>% rename(Entity = names(population)[[1]])
names(population) <- gsub('X', '', names(population))

schooling <- schooling %>% rename(YearsSchooling = Average.Total.Years.of.Schooling.for.Adult.Population..Lee.Lee..2016...Barro.Lee..2018..and.UNDP..2018..)

gdp <- gdp %>% rename(Entity = names(gdp)[[1]])
names(gdp) <- gsub('X', '', names(gdp))

gini <- gini %>% rename(Entity = names(gini)[[1]])
names(gini) <- gsub('X', '', names(gini))

poverty <- poverty %>% rename(Poverty = Share.under..5.2.a.day..roughly.equivalent.to.the.international.poverty.line.)



## RESHAPE DATA ##
regime <- regime[c('Entity', 'Year', 'Regime')]
regime[regime == 'United States                   '] <- 'United States'

child_mortality <- child_mortality %>% gather(Year, InfantMortality, '1799':'2099', na.rm = TRUE)
child_mortality$Year <- as.integer(child_mortality$Year)
child_mortality <- child_mortality[order(child_mortality$Entity),]

life_expectancy <- life_expectancy %>% gather(Year, LifeSpan, '1799':'2099', na.rm = TRUE)
life_expectancy$Year <- as.integer(life_expectancy$Year)
life_expectancy <- life_expectancy[order(life_expectancy$Entity),]

population <- population %>% gather(Year, Population, '1799':'2099', na.rm = TRUE)
population$Year <- as.integer(population$Year)
population <- population[order(population$Entity),]
population$Population <- sapply(population$Population, str_to_int)

schooling <- schooling[c('Entity', 'Year', 'YearsSchooling')]

poverty <- poverty[c('Entity', 'Year', 'Poverty')]

gdp <- gdp %>% gather(Year, GDPPerCapita, '1799':'2049', na.rm = TRUE)
gdp$Year <- as.integer(gdp$Year)
gdp <- gdp[order(gdp$Entity),]
gdp$GDPPerCapita <- sapply(gdp$GDPPerCapita, str_to_int)

gini <- gini %>% gather(Year, GiniCoef, '1799':'2049', na.rm = TRUE)
gini$Year <- as.integer(gini$Year)
gini <- gini[order(gini$Entity),]



## SET YEAR BOUNDS ##
year.max <- min(max(child_mortality$Year), max(gdp$Year), max(gini$Year), max(life_expectancy$Year), 
                max(poverty$Year), max(regime$Year), max(schooling$Year))
year.min <- max(min(child_mortality$Year), min(gdp$Year), min(gini$Year), min(life_expectancy$Year), 
                min(poverty$Year), min(regime$Year), min(schooling$Year))

child_mortality <- child_mortality %>% filter(Year <= year.max & Year >= year.min)
gdp             <- gdp %>% filter(Year <= year.max & Year >= year.min)
gini            <- gini %>% filter(Year <= year.max & Year >= year.min)
life_expectancy <- life_expectancy %>% filter(Year <= year.max & Year >= year.min)
poverty         <- poverty %>% filter(Year <= year.max & Year >= year.min)
regime          <- regime %>% filter(Year <= year.max & Year >= year.min)
schooling       <- schooling %>% filter(Year <= year.max & Year >= year.min)
population      <- population %>% filter(Year <= year.max & Year >= year.min)



## FILTER OUT NAs ##
regime <- regime[!is.na(regime$Regime),]



## MERGE PREDICTIVE DATA ##
predictors <- life_expectancy %>% 
  merge(child_mortality, all = TRUE, by = c('Entity', 'Year')) %>%
  merge(gdp, all = TRUE, by = c('Entity', 'Year')) %>%
  merge(gini, all = TRUE, by = c('Entity', 'Year')) %>%
  merge(poverty, all = TRUE, by = c('Entity', 'Year')) %>%
  merge(schooling, all = TRUE, by = c('Entity', 'Year')) %>%
  merge(population, all = TRUE, by = c('Entity', 'Year'))



## PREPARE TARGET DATA ##
regime <- regime %>% filter(Regime >= -10) %>% filter(Year > min(predictors$Year))



## MERGE PREDICTORS AND TARGET DATA ##
merged <- predictors %>% merge(regime, all = TRUE, by = c('Entity', 'Year'))
merged <- merged[!is.na(merged$Regime),]



## PREPARE FOR PANEL DATA ##
merged <- merged %>% group_by(Entity)
filled <- merged %>% group_modify(supplement_rows)



## TRANFORM FROM LONG TO WIDE ##
wide <- data.frame(Entity = '', Year = '', Regime = '')

for(col_nm in names(filled)) {
  if(col_nm == 'Entity' | col_nm == 'Year' | c == 'Regime')
    next
  
  for(new_name in names_times_x(10, col_nm))
    wide[new_name] <- 0
}

row_count <- 1
for(country in unique(filled$Entity)) {
  sub_df <- filled[filled$Entity == country,]
  
  for(i in 1:nrow(sub_df)) {
    if(i + 10 > nrow(sub_df))
      break
    
    for(j in 0:9) {
      for(c in names(filled)) {
        if(c == 'Entity' | c == 'Year' | c == 'Regime') {
          wide[row_count, c] <- sub_df[i + 10, c]
          next
        }
        col_name <- paste0(c, j + 1)
        wide[row_count, col_name] <- sub_df[i + j, c]
      }
    }
    
    row_count <- row_count + 1
  }
}



## MERGE SPARSE COLUMNS ##
wide <- wide %>% mutate(DecadePoverty = coalesce(Poverty1, Poverty2, Poverty3, 
                                                  Poverty4, Poverty5, Poverty6, 
                                                  Poverty7, Poverty8, Poverty9, 
                                                  Poverty10)) %>% 
                  select(-names_times_x(10, c('Poverty')))

wide <- wide %>% mutate(YearsSchooling1.5 = 
                          coalesce(YearsSchooling1, YearsSchooling2, 
                                   YearsSchooling3, YearsSchooling4, 
                                   YearsSchooling5),
                        YearsSchooling6.10 = 
                          coalesce(YearsSchooling6, YearsSchooling7, 
                                   YearsSchooling8, YearsSchooling9, 
                                   YearsSchooling10)) %>%
                  select(-names_times_x(10, c('YearsSchooling')))
#wide <- wide %>% select(-names_times_x(10, c('Regime')))


## REMOVE ALL ROWS WITH NA's##
clean <- wide %>% drop_na()
clean <- clean[, mixedsort(names(clean))] %>% select(Entity, Year, Regime, everything())



## ENSURE TYPES ARE CORRECT ##
clean$Entity <- as.factor(clean$Entity)
clean$Year <- as.integer(clean$Year)
clean$Regime <- as.ordered( as.integer(clean$Regime) )



## WRITE THE DATA TO A CSV AND RDS ##
write.csv(clean, 'Data.csv', row.names = FALSE)
write_rds(clean, 'Data.rds', compress = 'none')











