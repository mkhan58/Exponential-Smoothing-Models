#Misha Khan
#BANA 288 Predictive Analytics
#Spring 2021

#Homework #5: Exponential Smoothing Models

################################################################################
#1. Create time series object from the bike sharing data with frequency 7, 
#call the resulting series “cntts”.  Fit the simple exponential smoothing (ses) 
#model to the bike sharing data with a = 0.25 and initial value equal to the 
#first value of count (cntts) in the series.  Compare the result of this model 
#to the ses model with both parameters optimized.  Which is a better fit?  Why?

library("fpp2")

dat <- read.csv("~/Desktop/SPRING 2021/Predictive/Homework 5/hw5_bike_share_day.csv")

cntts <- ts(dat[,14], frequency = 7) #14 is for the cnt col
cntts[1:10]

fit_hs1 <- ses(cntts, alpha = 0.25, initial = "simple") #alpha = smoothing factor
fit_hs2 <- ses(cntts)

MSE_hs_SES1 <- mean((cntts - fit_hs1$fitted)^2)
RMSE_hs_SES1 <- MSE_hs_SES1^0.5
RMSE_hs_SES1

MSE_hs_SES2 <- mean((cntts - fit_hs2$fitted)^2)
RMSE_hs_SES2 <- MSE_hs_SES2^0.5
RMSE_hs_SES2

autoplot(cntts, series = "Bike Count Rental") +
  autolayer(fit_hs1$fitted, series = "SES-0.25") +
  autolayer(fit_hs2$fitted, series = "Optimal") +
  scale_alpha_manual(values = c("Bike Rental" = "blue", 'SES-0.25' = "red", "SES-Optimal" = "green"))

#RMSE are very similar, no significantly better fit which we can see from graph
#Second model is slightly better (964 is lower RMSE than 965), lower ME meaning less deviation from the mean

################################################################################
#2.Fit Holt’s model with the bike sharing data series, cntts.  Compare the 
#results of this model with the best model from question 1.  Which is a better fit?  Why?

fit_holt1 <- holt(cntts)
accuracy(fit_holt1)

RMSE_holt1 <- (mean((cntts-fit_holt1$fitted)^2))^0.5
RMSE_holt1

#Second model in Q1 has a slightly lower RSME (964.51 vs 964.78)
#ME -18.425 -> larger mean error than the second model in Q1 ME so previous model is the best
################################################################################
#3. Fit Holt-Winters’ seasonal method using the additive, multiplicative, and 
#the damped multiplicative methods.  Which of these models is preferred? 
#How did this model compare to the models from questions 1 and 2?  
#Are the results surprising?  Explain.

fit_AS1 <- hw(cntts, seasonal = "additive")
accuracy(fit_AS1)

fit_AS2 <- hw(cntts, seasonal = "multiplicative")
accuracy(fit_AS2)

fit_AS3 <- hw(cntts, seasonal = "multiplicative", damped = TRUE)
accuracy(fit_AS3) 

#Best model is damped multiplicative, low RMSE, low ME
#Does not surprise me because the multiplicative takes into account of the trend
#and damped "flattens" the trend in order to fit better over time

################################################################################
#4. Create forecasts for the preferred model from questions 1-3 for the next 4 weeks.  
#Display the values of the forecasts to the console and plot the forecasts on a time-oriented graph. 

pref.fit <- hw(cntts, seasonal = "multiplicative", damped = TRUE, h = 28)
pref.forecast <- forecast(pref.fit)
autoplot(pref.forecast)

summary(pref.fit)
################################################################################
#5. Built into R package fpp2 are many data sets.  To see all the data sets 
#built into to this suite of packages use the “data()” command.  One way to 
#load a data set “dat1” into the RStudio environment area is to use the 
#“fix(dat1)” command.  To obtain additional information about the data set, 
#“dat1”, use “?dat1”.  Load the Johnson and Johnson data set (JohnsonJohnson).  
#Describe this data.  What is the data range?  How many observations?  What is periodicity of 
#this data set?  Run the “AAA” ETS model on the earnings for Johnson and Johnson. 
#Report the coefficients and the fit of the AAA ETS model. 

fix(JohnsonJohnson)
?JohnsonJohnson 

summary(JohnsonJohnson)

range(JohnsonJohnson)
#Years 1960-1980, data ranges from $0.44 to $16.20 per JJ share

length(JohnsonJohnson)
#84 observations
#Periodicity: Quarterly data, 4 times a year

fit_ets <- ets(JohnsonJohnson, model = "AAA")
summary(fit_ets)
coef(fit_ets) #Coefficients: alpha, beta, gamma, l, b, s0 , s1, s2 
accuracy(fit_ets) #RMSE 0.436
fitted(fit_ets)

autoplot(JohnsonJohnson, series = "Actual Values") +
  autolayer(fitted(fit_ets), series = "AAA")

# Plot the three model components.
autoplot(fit_ets)

plot(fit_ets$fitted, ylab = "Fitted values", xlab = "Year")
plot(fit_ets$residuals, ylab = "Residual values", xlab = "Year")
plot(fit_ets$states)


################################################################################
#6. Compute the best ETS model on the Johnson and Johnson data.  Select the 
#model that is preferred from the models fit questions 5 and 6.  Explain why 
#the chosen model is preferred.

fit_ets1 <- ets(JohnsonJohnson)
fit_ets2 <- ets(JohnsonJohnson, model = "ANN")
fit_ets3 <- ets(JohnsonJohnson, model = "AAN")
fit_ets4 <- ets(JohnsonJohnson, model = "AAA")
fit_ets5 <- ets(JohnsonJohnson, model = "MAA", damped = TRUE)
fit_ets6 <- ets(JohnsonJohnson, model = "MAM", damped = TRUE)

RMSE_ets <- data.frame(matrix(0,6,1))
RMSE_ets[1,1] <- summary(fit_ets1)[,2]
RMSE_ets[2,1] <- summary(fit_ets2)[,2]
RMSE_ets[3,1] <- summary(fit_ets3)[,2]
RMSE_ets[4,1] <- summary(fit_ets4)[,2]
RMSE_ets[5,1] <- summary(fit_ets5)[,2]
RMSE_ets[6,1] <- summary(fit_ets6)[,2]
names(RMSE_ets) <- "RMSE"
rownames(RMSE_ets) <- c('Optimal', 'ANN', 'AAN', 'AAA', 'MAA-Damped', 'MAM-Damped')
RMSE_ets

#Chose model AAA, lowest RMSE 
#OR choose Optimal model, lowest AIC

################################################################################
#7.How is the preferred model selected by the ETS command when the modeler does 
#not specify a certain type of ETS model?  Explain in a few sentences.

#When the certain type of ETS model is not specified, R selects the preferred
#model based on the model with the lowest AICc. AIC is an estimate of a constant
#plus the relative distance between the unknown true likelihood function
#of the data and the fitted likelihood function. A lower AIC indicates closer
#to the truth (better fit of the data).

################################################################################
#8.Compute the best ETS model on the monthly debit card use in Iceland 
#(data set “debitcards”).  What model was chosen?  What are the associated parameters?  
#Display the model components (chart).  Make forecasts with 80% confidence bands
#for the next two years using the chosen model.  Graph the data and the forecasts.
fix(debitcards)
fit_ets1 <- ets(debitcards)
fit_ets2 <- ets(debitcards, model = "ANN")
fit_ets3 <- ets(debitcards, model = "AAN")
fit_ets4 <- ets(debitcards, model = "AAA")
fit_ets5 <- ets(debitcards, model = "MAA", damped = TRUE)
fit_ets6 <- ets(debitcards, model = "MAM", damped = TRUE)

RMSE_ets2 <- data.frame(matrix(0,6,1))
RMSE_ets2[1,1] <- summary(fit_ets1)[,2]
RMSE_ets2[2,1] <- summary(fit_ets2)[,2]
RMSE_ets2[3,1] <- summary(fit_ets3)[,2]
RMSE_ets2[4,1] <- summary(fit_ets4)[,2]
RMSE_ets2[5,1] <- summary(fit_ets5)[,2]
RMSE_ets2[6,1] <- summary(fit_ets6)[,2]
names(RMSE_ets2) <- "RMSE"
rownames(RMSE_ets2) <- c('Optimal', 'ANN', 'AAN', 'AAA', 'MAA-Damped', 'MAM-Damped')
RMSE_ets2

fore_ets1 <- forecast(fit_ets1, h = 24, level = 80)
autoplot(debitcards, series = "Actual Values") +
  autolayer(fore_ets1, series = 'ETS Forecast') +
  ggtitle("Monthly Debit Card Usage in Iceland (2000-2013) with Forecasts") +
  ylab("Debit Card Usage (in million ISK)") +
  xlab("Year")

#Optimal is the best model here because it has the lowest RMSE
################################################################################
#9.Compute the best ETS model on the Google closing price data (data set “goog”).  
#What model was chosen?  What are the associated parameters?  Display the model components.  
#Make forecasts for the next 30 days using the chosen model.  Graph the data and the forecasts.  
fix(goog)
fit_ets1 <- ets(goog)
fit_ets2 <- ets(goog, model = "ANN")
fit_ets3 <- ets(goog, model = "AAN")
#fit_ets4 <- ets(goog, model = "MAM")
#fit_ets5 <- ets(goog, model = "MAA", damped = TRUE)
#fit_ets6 <- ets(goog, model = "MAM", damped = TRUE)

RMSE_ets3 <- data.frame(matrix(0,3,1))
RMSE_ets3[1,1] <- summary(fit_ets1)[,2]
RMSE_ets3[2,1] <- summary(fit_ets2)[,2]
RMSE_ets3[3,1] <- summary(fit_ets3)[,2]
#RMSE_ets3[4,1] <- summary(fit_ets4)[,2]
#RMSE_ets3[5,1] <- summary(fit_ets5)[,2]
#RMSE_ets3[6,1] <- summary(fit_ets6)[,2]

names(RMSE_ets3) <- "RMSE"
rownames(RMSE_ets3) <- c('Optimal', 'ANN', 'AAN')
RMSE_ets3
autoplot(fit_ets2)
fore_ets2 <- forecast(fit_ets2, h = 30, level = FALSE)
autoplot(goog, series = "Actual Values") +
  autolayer(fore_ets2, series = 'ETS Forecast') +
  ggtitle("Daily Closing Stock Prices of Google Inc.") +
  ylab("Closing Stock Price (in $)") +
  xlab("Time")

#AAN is the best model here because it has the lowest RMSE
################################################################################
#10. Compare the results of question 9 with a “best” ARIMA model on data set “goog”.  
#Which technique works better?  What is the chosen model?  Why did you choose that model?  
#What are the model’s parameters?  What is the model error?  Hint:  Use command “auto.arima(goog)” to run the autom

arimamodel<- auto.arima(goog)
summary(arimamodel)
autoplot(forecast(arimamodel))
accuracy(arimamodel)

#RMSE 8.719, lower RMSE than before (8.729) so ARIMA works better
#ARIMA also has a lower AIC 7166.89 than ETS 11250.09 so another reason why ARIMA is a better model.
#The chosen ARIMA model is ARIMA(0,1,0) with drift
#Parameters:
#Coefficients:
#  drift
#0.4213
#s.e.  0.2760
#sigma^2 estimated as 76.19:  log likelihood=-3581.45