library(ggplot2)
library(tidyverse)
library(statmod)
library(MASS)
library(gridExtra)

# Is there a relationship between the economic indicator GDP and maternal 
# mortality ratio while holding other various socio-economic factors including 
# physicians per thousand, percent of population in the labor force, 
# unemployment rate, and minimum wage constant?

# Is there a relationship between GDP and armed forces size while holding 
# other economic factors constant, including tax rate, gasoline prices, and
# unemployment rate?

worldData <- read.csv('/Users/melitawiles/Desktop/STA712/Data Projects/Project2/world-data-2023.csv', na.string = c("", "NA"))
worldDataNoNA <- worldData[, c(6,11,16,17,23,24,26,27,29,31,32)]
dataClean <- na.omit(worldDataNoNA)
dataClean$Armed.Forces.size <- gsub(",", "", dataClean$Armed.Forces.size)
dataClean$Gasoline.Price <- gsub("\\$", "", dataClean$Gasoline.Price)
dataClean$GDP <- gsub("\\$", "", dataClean$GDP)
dataClean$GDP <- gsub(",", "", dataClean$GDP)
dataClean$Minimum.wage <- gsub("\\$", "", dataClean$Minimum.wage)
dataClean$Out.of.pocket.health.expenditure <- gsub("%", "", dataClean$Out.of.pocket.health.expenditure)
dataClean$Population..Labor.force.participation.... <- gsub("%", "", dataClean$Population..Labor.force.participation....)
dataClean$Total.tax.rate <- gsub("%", "", dataClean$Total.tax.rate)
dataClean$Unemployment.rate <- gsub("%", "", dataClean$Unemployment.rate)
dataNew <- lapply(dataClean, as.numeric)
str(dataNew)

finalData <- data.frame(dataNew)

m1 <- glm(GDP ~ Maternal.mortality.ratio + Population..Labor.force.participation.... 
          + sqrt(Minimum.wage) + Physicians.per.thousand + Unemployment.rate, 
          data = finalData, family = quasipoisson)

plot1a <- data.frame(x = finalData$Maternal.mortality.ratio, resids = qresid(m1)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Maternal mortality ratio", y = "Quantile residuals", title = "Quantile Residual Plots for Model 1") 
plot2a <- data.frame(x = finalData$Population..Labor.force.participation...., resids = qresid(m1)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Population of labor force", y = "Quantile residuals") 
plot3a <- data.frame(x = sqrt(finalData$Minimum.wage), resids = qresid(m1)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Minimum wage", y = "Quantile residuals") 
plot4a <- data.frame(x = finalData$Physicians.per.thousand, resids = qresid(m1)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Physicians per thousand", y = "Quantile residuals") 
plot5a <- data.frame(x = finalData$Unemployment.rate, resids = qresid(m1)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Unemployment rate", y = "Quantile residuals") 

m2 <- glm(GDP ~ Armed.Forces.size + Total.tax.rate 
          + Gasoline.Price + Unemployment.rate + sqrt(Minimum.wage), 
          data = finalData, family = quasipoisson)
plot1b <- data.frame(x = finalData$Armed.Forces.size, resids = qresid(m2)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Armed forces", y = "Quantile residuals", title = "Quantile Residual Plots for Model 2") 
plot2b <- data.frame(x = finalData$Total.tax.rate, resids = qresid(m2)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Tax rate", y = "Quantile residuals") 
plot3b <- data.frame(x = finalData$Gasoline.Price, resids = qresid(m2)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Gas prices", y = "Quantile residuals") 
plot4b <- data.frame(x = finalData$Unemployment.rate, resids = qresid(m2)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Unemployment rate", y = "Quantile residuals") 
plot5b <- data.frame(x = sqrt(finalData$Minimum.wage), resids = qresid(m2)) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Minimum wage", y = "Quantile residuals") 

grid.arrange(plot1a, plot2a, plot3a, plot4a, plot5a, ncol=2)
grid.arrange(plot1b, plot2b, plot3b, plot4b, plot5b, ncol=2)

m1c <- lm(GDP ~ Maternal.mortality.ratio + Population..Labor.force.participation.... 
          + sqrt(Minimum.wage) + Physicians.per.thousand + Unemployment.rate, 
          data = finalData)

plot1c <- data.frame(x = finalData$Maternal.mortality.ratio, resids = m1c$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Maternal mortality ratio", y = "Residuals", title = "Residual Plots for Model 1") 
plot2c <- data.frame(x = finalData$Population..Labor.force.participation...., resids = m1c$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Population of labor force", y = "Residuals") 
plot3c <- data.frame(x = sqrt(finalData$Minimum.wage), resids = m1c$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Minimum wage", y = "Residuals") 
plot4c <- data.frame(x = finalData$Physicians.per.thousand, resids = m1c$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Physicians per thousand", y = "Residuals") 
plot5c <- data.frame(x = finalData$Unemployment.rate, resids = m1c$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Unemployment rate", y = "Residuals") 

m2d <- lm(GDP ~ Armed.Forces.size + Total.tax.rate 
          + Gasoline.Price + Unemployment.rate + sqrt(Minimum.wage), 
          data = finalData)

plot1d <- data.frame(x = finalData$Armed.Forces.size, resids = m2d$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Armed forces", y = "Residuals", title = "Residual Plots for Model 2") 
plot2d <- data.frame(x = finalData$Total.tax.rate, resids = m2d$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Tax rate", y = "Residuals") 
plot3d <- data.frame(x = finalData$Gasoline.Price, resids = m2d$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Gas prices", y = "Residuals") 
plot4d <- data.frame(x = finalData$Unemployment.rate, resids = m2d$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Unemployment rate", y = "Residuals") 
plot5d <- data.frame(x = sqrt(finalData$Minimum.wage), resids = m2d$residuals) |>
  ggplot(aes(x = x, y = resids)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x = "Minimum wage", y = "Residuals") 

grid.arrange(plot1c, plot2c, plot3c, plot4c, plot5c, ncol=2)
grid.arrange(plot1d, plot2d, plot3d, plot4d, plot5d, ncol=2)

summary(m1)
summary(m1c)

summary(m2)
summary(m2d)
