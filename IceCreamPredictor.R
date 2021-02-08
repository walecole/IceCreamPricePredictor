# load necessarry packages
library(tidyverse)
library(statsr) 
library(skimr) 
library(GGally) 
library(stargazer)
library(psych)

#load dataset
ice_cream <- read_csv('http://bit.ly/2OMHgFi')

#Data Summary
glimpse(ice_cream)
str(ice_cream)

#Convert character variables as factor
ice_cream$country <- as.factor(ice_cream$country)
ice_cream$seasons <- as.factor(ice_cream$seasons)

#Check for null values
table(is.na(ice_cream))

#Descriptive statistics of the data
summary(ice_cream)

#EDA Analysis of each variable by ploting graphs
ggplot(data = ice_cream, aes(x=country)) + geom_bar()
ggplot(data = ice_cream, aes(x=seasons)) + geom_bar()
hist(ice_cream$icecream_sales, breaks = 150)
hist(ice_cream$income, breaks = 50)
hist(ice_cream$temperature, breaks = 50)
hist(ice_cream$price, breaks = 50)
boxplot(x = ice_cream$icecream_sales, main='Boxplot for icecream sales') 
boxplot(x = ice_cream$income, main='Boxplot for income') 

#check for outliers
boxplot.stats(ice_cream$icecream_sales)$out
boxplot.stats(ice_cream$income)$out

#Plot continous against categorical
ggplot(data = ice_cream, aes(x=seasons,y=icecream_sales)) + geom_jitter()
ggplot(data = ice_cream, aes(x=country,y=icecream_sales)) + geom_jitter()
ggplot(data = ice_cream, aes(x=seasons,y=price)) + geom_jitter()
ggplot(data = ice_cream, aes(x=icecream_sales,y=temperature, color = seasons)) + geom_point()

#correlation between variables
ice_cream %>% 
  select(icecream_sales, income, price, temperature) %>% 
  cor() %>%
  knitr::kable(digits = 2, caption = "Correlation between numeric variables",
               booktabs = TRUE
  )

#Hypothesis Testing ; Comparing Two means
ggplot(data = ice_cream, aes(x=country,y=income)) + geom_jitter() +
  ggtitle("Average Income Across Country A and Country B") + 
  stat_summary(fun = mean, colour = "darkred", geom = "point", 
               shape = 1, size = 9)

inference(y = income, 
          x = country, 
          data = ice_cream, 
          statistic = c("mean"), #parameter we are interested in
          type = c("ht"), #a confidence interval
          null = 0, #to supply the null value; 0 since the h0 sets the two population means equal to each other
          alternative = c("twosided"), #can be less, greater or twosided
          method = c("theoretical"), #theoretical or simulation based
          conf_level = 0.95#to set the level
)       

#Regression, and finding the R squared
regression <- lm(icecream_sales ~ income + price + seasons + temperature + country, data = ice_cream)
summary(regression)

data_A <- data.frame(income = 13000, country = 'A', price = 2, temperature = 1, seasons = 'Winter')
predict(regression,data_A)

data_B <- data.frame(income = 20000, country = 'B', temperature = 1, seasons = 'Winter')
predict(model,country_data)

data_C <- data.frame(income = 13000, country = 'A', price = 2.75, temperature = 1.5, seasons = 'Winter')
predict(regression,data_C)

data_D <- data.frame(income = 20000, country = 'B', price = 2.75, temperature = 1.5, seasons = 'Winter')
predict(regression,data_D)

anova(regression)


#To define confidence interval
confint(regression, level = 0.99)

#Test of Linearity
plot(regression$residuals ~ ice_cream$income)
abline(h=0)

plot(regression$residuals ~ ice_cream$price)
abline(h=0)

plot(regression$residuals ~ ice_cream$temperature)
abline(h=0)

#Test of Near Normality
hist(regression$residuals)
qqnorm(regression$residuals)
qqline(regression$residuals) 

#Test for constant variability of residuals
plot(regression$residuals ~ regression$fitted)

#Test for independednt variable
plot(regression$residuals)

#Prediction 
predicted_data <- data.frame(income = 20000, country = 'B', price = 2, temperature = 1, seasons = 'Winter')
predict(regression,predicted_data, interval = 'prediction', level = 0.90)
