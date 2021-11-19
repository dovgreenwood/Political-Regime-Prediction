## LIBRARIES ##
library(tidyverse)



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



## READ IN DATA ##
regime <- read.csv('raw/political-regime-updated2016.csv')

child_mortality <- read.csv('raw/child_mortality_0_5_year_olds_dying_per_1000_born.csv')
life_expectancy <- read.csv('raw/life_expectancy_years.csv')
schooling <- read.csv('raw/mean-years-of-schooling-long-run.csv')
pop_density <- read.csv('raw/population-density-3.csv')
poverty <- read.csv('raw/reconstruction-of-historical-poverty-trends-by-country.csv')
gdp_gini <- read.csv('raw/use-of-interpolation-and-extrapolation-on-maddison-gdp-per-capita-data.csv')
population <- read.csv('raw/population_total.csv')



## RENAME COLUMNS ##
child_mortality <- child_mortality %>% rename(Entity = names(child_mortality)[[1]])
names(child_mortality) <- gsub('X', '', names(child_mortality))

life_expectancy <- life_expectancy %>% rename(Entity = names(life_expectancy)[[1]])
names(life_expectancy) <- gsub('X', '', names(life_expectancy))

population <- population %>% rename(Entity = names(population)[[1]])
names(population) <- gsub('X', '', names(population))

regime <- regime %>% rename(Regime = Political.Regime..OWID.based.on.Polity.IV.and.Wimmer...Min.)

pop_density <- pop_density %>% rename(PopulationDensity = Population.Density...HYDE..and.UN.Population.Division..2008.Revision..)

schooling <- schooling %>% rename(YearsSchooling = Average.Total.Years.of.Schooling.for.Adult.Population..Lee.Lee..2016...Barro.Lee..2018..and.UNDP..2018..)

gdp_gini <- gdp_gini %>% rename(GDP = GDP.per.capita,
                      Gini = Baseline.Gini.series..GCIP.income.and.van.Zanden.et.al...2014..)

poverty <- poverty %>% rename(Poverty = Share.under..5.2.a.day..roughly.equivalent.to.the.international.poverty.line.)



## RESHAPE DATA ##
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

gdp_gini <- gdp_gini %>% select(-c('GDP.per.capita_source', 'Total.population..Gapminder..HYDE...UN.'))
poverty <- poverty[c('Entity', 'Code', 'Year', 'Poverty')]



## SET YEAR BOUNDS ##
year.max <- min(max(child_mortality$Year), max(gdp_gini$Year), max(life_expectancy$Year), 
                max(pop_density$Year), max(poverty$Year), max(regime$Year), max(schooling$Year))
year.min <- max(min(child_mortality$Year), min(gdp_gini$Year), min(life_expectancy$Year), 
                min(pop_density$Year), min(poverty$Year), min(regime$Year), min(schooling$Year))

child_mortality <- child_mortality %>% filter(Year <= year.max & Year >= year.min)
gdp_gini <- gdp_gini %>% filter(Year <= year.max & Year >= year.min)
life_expectancy <- life_expectancy %>% filter(Year <= year.max & Year >= year.min)
pop_density <- pop_density %>% filter(Year <= year.max & Year >= year.min)
poverty <- poverty %>% filter(Year <= year.max & Year >= year.min)
regime <- regime %>% filter(Year <= year.max & Year >= year.min)
schooling <- schooling %>% filter(Year <= year.max & Year >= year.min)
population <- population %>% filter(Year <= year.max & Year >= year.min)



## FILTER OUT NAs ##
gdp_gini <- gdp_gini[!is.na(gdp_gini$GDP),]
regime <- regime[!is.na(regime$Regime),]




## MERGE PREDICTIVE DATA ##
predictors <- life_expectancy %>% merge(child_mortality, all = TRUE, by = c('Entity', 'Year')) %>%
                                  merge(gdp_gini, all = TRUE, by = c('Entity', 'Year')) %>%
                                  merge(pop_density, all = TRUE, by = c('Entity', 'Year')) %>%
                                  merge(poverty, all = TRUE, by = c('Entity', 'Year')) %>%
                                  merge(schooling, all = TRUE, by = c('Entity', 'Year'))
# MERGE CODE COLUMNS
#predictors <- rename(predictors, CD = predictors[5]) %>% mutate(predictors, CD = ifelse(is.na(CD), predictors[8], CD))


## PREPARE TARGET DATA ##
# regime <- regime %>% filter(Regime > -20) %>% filter(Year > min(predictors$Year))





############
# Use cross validation to determine ideal window size for time series prediction
# 
############







