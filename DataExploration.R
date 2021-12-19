regime.table <- read_rds('Data.rds')
regime.numeric <- regime.table %>% mutate(Regime = as.numeric(Regime) - 11)


# Year
plot(log(GDPPerCapita10) ~ Year, data = regime.numeric)
abline(reg = lm(log(GDPPerCapita10) ~ Year, data = regime.numeric))

plot(jitter(GiniCoef10) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(GiniCoef10) ~ Year, data = regime.numeric))

plot(jitter(DecadePoverty) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(DecadePoverty) ~ Year, data = regime.numeric))

plot(jitter(InfantMortality10) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(InfantMortality10) ~ Year, data = regime.numeric))

plot(log(Population10) ~ Year, data = regime.numeric)
abline(reg = lm(log(Population10) ~ Year, data = regime.numeric))

plot(log(YearsSchooling6.10) ~ Year, data = regime.numeric)
abline(reg = lm(log(YearsSchooling6.10) ~ Year, data = regime.numeric))


# Regime
plot(jitter(Regime) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ Year, data = regime.numeric))

plot(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric))

plot(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric))

plot(jitter(Regime) ~ DecadePoverty, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ DecadePoverty, data = regime.numeric))

plot(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric))

plot(jitter(Regime) ~ log(Population10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(Population10), data = regime.numeric))

plot(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric))


# Regime > 0
regime.numeric <- regime.table %>% mutate(Regime = as.numeric(Regime) - 11)
regime.numeric <- regime.numeric[regime.numeric$Regime > 0,]

plot(jitter(Regime) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ Year, data = regime.numeric))

plot(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric))

plot(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric))

plot(jitter(Regime) ~ DecadePoverty, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ DecadePoverty, data = regime.numeric))

plot(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric))

plot(jitter(Regime) ~ log(Population10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(Population10), data = regime.numeric))

plot(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric))


# Regime <= 0
regime.numeric <- regime.table %>% mutate(Regime = as.numeric(Regime) - 11)
regime.numeric <- regime.numeric[regime.numeric$Regime <= 0,]

plot(jitter(Regime) ~ Year, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ Year, data = regime.numeric))

plot(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(GDPPerCapita10), data = regime.numeric))

plot(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(GiniCoef10), data = regime.numeric))

plot(jitter(Regime) ~ DecadePoverty, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ DecadePoverty, data = regime.numeric))

plot(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ jitter(InfantMortality10), data = regime.numeric))

plot(jitter(Regime) ~ log(Population10), data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ log(Population10), data = regime.numeric))

plot(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric)
abline(reg = lm(jitter(Regime) ~ YearsSchooling6.10, data = regime.numeric))












