# packages used
library(rvest)
library(dplyr)
library(stringr)

##### DATA SCCRAPING/CLEANING #####
# scraping html table from website for obesity rates per state data
obesity_site <- 'https://worldpopulationreview.com/state-rankings/obesity-rate-by-state'
obesity_page <- read_html(obesity_site)
obesity_table <- obesity_page %>% html_nodes("table") %>% .[[1]]%>%html_table(fill=TRUE)

# function to select numeric values
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

# data frame with state and obesity rates
obesity.df <- data.frame(States = as.character(obesity_table[[1]]),
                          ObesityRate = as.numeric(numextract(obesity_table[[2]])))

# adding column for state abbreviation
obesity.df$State <- state.abb[match(obesity.df$States,state.name)]

# reading csv file for fast food restaurants across America data
setwd('C:/Users/ammnd/Desktop/BTMA 431/Final Project')
fastfoodData <- read.csv("fastfoodData.csv")

# combining obesity rates with the fast food restaurants data
final.df <- merge(obesity.df, fastfoodData, by=c("State"))

##### DATA ANALYSIS ######
# Question 1
# Are number of fast food restaurants and obesity rates correlated?
cor(final.df$ObesityRate, final.df$Total)

# Are obesity rates and number of restaurants independent?
obesityRateAndTotal.df <- data.frame(final.df$ObesityRate, final.df$Total)
chisq.test(obesityRateAndTotal.df)
# (H0): Obesity rate and total number of fast-food restaurants are independent 
# (H1): Obesity rate and the total number of fast-food restaurants are dependent
# the variables are statistically significantly associated (p-value = 0)

# Question 2
# Can we predict obesity rates using fast food restaurants?
attach(final.df)

# finding the top 25 restaurants to remove some predictors from our model
largest <- sort(colSums(fastfoodData[,3:404]), 
                decreasing = TRUE)[1:25]
names(largest)

# regression model to predict obesity rates using top 25 restaurants
obesityRateRegressionTop25 <- lm(ObesityRate ~ McDonalds + BurgerKing + TacoBell + Wendys + Arbys + KFC + Subway +
                              SonicDriveIn + DominosPizza + Hardees + JackintheBox + JimmyJohns + ChickFilA +
                              BojanglesFamousChickennBiscuits + PizzaHut + DairyQueen + CarlsJr + FiveGuys +
                              Whataburger + TacoJohns + WaffleHouse + PopeyesLouisianaKitchen + Krystal +
                              WhiteCastle + Checkers, data=final.df[-c(1,2,4)])

summary(obesityRateRegressionTop25)

# predicting obesity rates using top 25 model 
final.df$ObesityRateRegTop25 <- predict(obesityRateRegressionTop25, data = final.df)

# regression model to predict obesity rates using top 20 restaurants per capita 
obesityRateRegressionTop20 <- (lm(ObesityRate ~ Subway + McDonalds + BurgerKing + TacoBell + PizzaHut + Wendys + DominosPizza + 
                               KFC + DairyQueen + Arbys + SonicDriveIn + Hardees + JimmyJohns + JackintheBox + ChickFilA + 
                               ChipotleMexicanGrill + PandaExpress + CarlsJr + FiveGuys + Whataburger, data = final.df))

summary(obesityRateRegressionTop20)

# predicting obesity rates using top 20 model 
final.df$ObesityRateRegTop20 <- predict(obesityRateRegressionTop20, data = final.df)

# Question 3
# Can we predict obesity rates using total number of fast food restaurants per state?
obesityRateRegressionTotal <- lm(ObesityRate ~ Total, data = final.df)

summary(obesityRateRegressionTotal)

# predicting obesity rates using total model
final.df$ObesityRateRegTotal <- predict(obesityRateRegressionTotal, data = final.df)

regressionPredictions <- data.frame(ObesityRate, final.df$ObesityRateRegTop25, final.df$ObesityRateRegTop20, final.df$ObesityRateRegTotal)

# calculating residuals for the 3 models
resTop25 <- residuals(lm(ObesityRate ~ McDonalds + BurgerKing + TacoBell + Wendys + Arbys + KFC + Subway +
                                                   SonicDriveIn + DominosPizza + Hardees + JackintheBox + JimmyJohns + ChickFilA +
                                                   BojanglesFamousChickennBiscuits + PizzaHut + DairyQueen + CarlsJr + FiveGuys +
                                                   Whataburger + TacoJohns + WaffleHouse + PopeyesLouisianaKitchen + Krystal +
                                                  WhiteCastle + Checkers, data=final.df[-c(1,2,4)]))
resTop20 <- residuals((lm(ObesityRate ~ Subway + McDonalds + BurgerKing + TacoBell + PizzaHut + Wendys + DominosPizza + 
                                    KFC + DairyQueen + Arbys + SonicDriveIn + Hardees + JimmyJohns + JackintheBox + ChickFilA + 
                                    ChipotleMexicanGrill + PandaExpress + CarlsJr + FiveGuys + Whataburger, data = final.df)))
resTotal <- residuals(lm(ObesityRate ~ Total, data = final.df))

mean(resTop25)
mean(resTop20)
mean(resTotal)

# writing csv file to create visualizations
write.csv(final.df, 'FinalProjectData.csv')