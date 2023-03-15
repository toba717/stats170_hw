###
# Anish Dulla, Daniel Neufeldt, Takao Oba, Shoichiro Ueno
# Homework 1
# February 24, 2023
###


### I. Introduction
library(Quandl)
library(dygraphs)
Quandl.api_key("uSPQnqK5PuPzTydvoB_R")

retail_electronic = Quandl(code="FRED/RSEASN",type="ts", collapse="monthly", meta=TRUE)
# Do not include 2020 and 2021 years in our data
electronic = window(retail_electronic, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_hobby = Quandl(code = "FRED/RSSGHBMSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
hobby = window(retail_hobby, end = c(2019, 12))
# > 200 observations & start: 1992 1
start(hobby)

retail_furniture = Quandl(code = "FRED/RSFHFSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
furniture = window(retail_furniture, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_sales <- cbind(electronic, hobby, furniture)
dygraph(retail_sales, main = "Electronic, Hobby, and Furniture Sales Over Time",
        ylab = "Sales (in Millions of Dollars)", xlab = "Time (in years)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector()

retails_electronic_train = electronic %>% 
  window(end = c(2018, 12))

retails_electronic_test = electronic %>% 
  window(start = 2019)

### II. Components Features of the dependent variable
plot(retails_electronic_train)

add_decomp <- (decompose(retails_electronic_train, type='add')) 
mult_decomp <- decompose(retails_electronic_train, type = "mult")

plot(add_decomp)
plot(mult_decomp)

boxplot_elec <- boxplot(retails_electronic_train ~ cycle(retails_electronic_train),
                        main = "Seasonal Boxplot of Retail Electronic Sales (Training)",
                        xlab = "Month",
                        ylab = "Retail Electronic Sales")

### III. Autocorrelation features of the random term of the dependent variable in the training set


mult_decomp_random_term <- mult_decomp$random

par(mfrow=c(2, 1))
acf(window(mult_decomp_random_term, start = c(1992, 7), end = c(2018, 6)), main = "ACF - Multiplicative Decomposition \n of Random Term: Electronic Retail Sales")
pacf(window(mult_decomp_random_term, start = c(1992, 7), end = c(2018, 6)), main = "PACF - Multiplicative Decomposition \n of Random Term: Electronic Retail Sales")

### IV. Exponential smoothing modeling and forecasting

electronic_smooth <- HoltWinters(retails_electronic_train, seasonal = "multiplicative")
plot(electronic_smooth, main = "Exponential Smoothing Model of Electronic Retail Sales", xlab = "Year", ylab = "Retail Sales (in Millions of Dollars)")
legend(2011, 15999, legend=c("Electronic Retail Sales", "Fitted Exp. Smoothing"), col=c("black", "red"), lty=1, cex=0.6)

electronic_pred <- predict(electronic_smooth, n.ahead = 12)
electronic_pred

plot(electronic_smooth, electronic_pred, main = "Exponential Smoothing of Electronic Retail Sales With Forecasting",
     xlab = "Year", ylab = "Retail Sales (in Millions of Dollars)")
lines(retails_electronic_test, col = "blue", lty = 2)
legend(1993, 15900, legend = c("Electronic Retail Sales (Train)", "Electronic Retail Sales (Test)", "Fitted Exp. Smoothing"), col = c("black", "blue", "red"), lty = c(1, 2, 1), cex = 0.6)


par(mfrow = c(1, 2))
plot(electronic_pred, main = "Exponential Smoothing\n Forecast vs Retail \nElectronic Sales", xlab = "Time (2019)", ylab = "Retail Sales (in millions of dollars)", col = "red")
lines(retails_electronic_test, col = "blue", lty = 2)
legend(2019, 10900, legend = c("Electronic Retail Sales", "Fitted Exp. Smoothing") , col = c("blue", "red"), lty = 2:1, cex = 0.6)

residual = as.numeric(retails_electronic_test) - as.numeric(electronic_pred)
plot(residual, main = "Residuals of Exponential\n Smoothing Forecast", xlab = "Time (in months) -- 2019", ylab = "Residuals (in millions of dollars)")
abline(h = 0, col = "red")

### V. Polynomial regression plus seasonal effect modeling and forecasting

mult_decomp_seasonal_term <- mult_decomp$seasonal

regression_df <- data.frame(as.numeric(time(retails_electronic_train)))
regression_df$electronic_sales <- as.numeric(retails_electronic_train)
colnames(regression_df)[1] <- "time"
regression_df$time2 <- regression_df$time ^ 2
regression_df$time3 <- regression_df$time ^ 3
regression_df$time4 <- regression_df$time ^ 4
regression_df$time5 <- regression_df$time ^ 5

head(regression_df)

lin_model <- lm(electronic_sales ~ time, data = regression_df)
quad_model <- lm(electronic_sales ~ time + time2, data = regression_df)
cubic_model <- lm(electronic_sales ~ time + time2 + time3, data = regression_df)
quartic_model <- lm(electronic_sales ~ time + time2 + time3 + time4, data = regression_df)


# library(car)
# vif(cubic_model) # time2 and time3 are perfectly correlated




# just calls time in data section
lin_model_fit <- as.numeric(predict(lin_model, data = regression_df))
quad_model_fit <- as.numeric(predict(quad_model, data = regression_df))
cubic_model_fit <- as.numeric(predict(cubic_model, data = regression_df))
quartic_model_fit <- as.numeric(predict(quartic_model, data = regression_df))

#-60585143 + (60297 * 1992) - (15 * 3968064)

#predict: 3992.539, actual: 3657

seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_lin_model_fit <- lin_model_fit * seasonals
total_quad_model_fit <- quad_model_fit * seasonals
total_cubic_model_fit <- cubic_model_fit * seasonals

lin_residuals <- as.numeric(retails_electronic_train) - total_lin_model_fit
quad_residuals <- as.numeric(retails_electronic_train) - quad_model_fit
cubic_residuals <- as.numeric(retails_electronic_train) - cubic_model_fit
quartic_residuals <- as.numeric(retails_electronic_train) - quartic_model_fit

scaled_lin_residuals <- scale(lin_residuals)
scaled_quad_residuals <- scale(quad_residuals)
scaled_cubic_residuals <- scale(cubic_residuals)
scaled_quartic_residuals <- scale(quartic_residuals)

par(mfrow=c(2,1))

plot(ts(scaled_lin_residuals), main = "Scaled Residual Plot - Linear Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_quad_residuals), main = "Scaled Residual Plot - Quadratic Model", ylab = "Standarized Residuals", xlab = "Time (in months)") # want it in the form of time series
abline(h = 0, col = "red")

plot(ts(scaled_cubic_residuals), main = "Scaled Residual Plot - Cubic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_quartic_residuals), main = "Scaled Residual Plot - Quartic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

testTimeRE <- data.frame(as.numeric(time(retails_electronic_test)))
colnames(testTimeRE)[1] <- "time"
testTimeRE$time2 <- testTimeRE$time ^ 2
testTimeRE$time3 <- testTimeRE$time ^ 3
testTimeRE$time4 <- testTimeRE$time ^ 4

predictions_lin <- predict(lin_model, newdata=testTimeRE)
predictions_quad <- predict(quad_model, newdata=testTimeRE)
predictions_cubic <- predict(cubic_model, newdata=testTimeRE)
predictions_quartic <- predict(quartic_model, newdata=testTimeRE)
 
seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_predictions_lin <- predictions_lin * seasonals
total_predictions_quad <- predictions_quad * seasonals
total_predictions_cubic <- predictions_cubic * seasonals
total_predictions_quartic <- predictions_quartic * seasonals

plot(retails_electronic_test - total_predictions_lin)
abline(h = 0, col = "red")

plot(retails_electronic_test - total_predictions_quad)
abline(h = 0, col = "red")

plot(retails_electronic_test - total_predictions_cubic)
abline(h = 0, col = "red")

plot(retails_electronic_test - total_predictions_quartic)
abline(h = 0, col = "red")

RMSE_lin <- sqrt(mean((retails_electronic_test - total_predictions_lin)^2))
RMSE_quad <- sqrt(mean((retails_electronic_test - total_predictions_quad)^2))
RMSE_cubic <- sqrt(mean((retails_electronic_test - total_predictions_cubic)^2))
RMSE_quartic <- sqrt(mean((retails_electronic_test - total_predictions_quartic)^2))

min(RMSE_lin, RMSE_quad, RMSE_cubic, RMSE_quartic) #RMSE_cubic is smaller

### VI. Conclusion

RMSE_exp_smooth <- sqrt(mean((retails_electronic_test - electronic_pred)^2))

average_forecast <- (total_predictions_quad + electronic_pred) / 2
average_RMSE <- mean(c(RMSE_exp_smooth, RMSE_quad))

### VII. ARIMA modeling and forecasting

y <- retails_electronic_train

par(mfrow=c(3, 1))
plot(y ^ (1/2), main = "Sqrt Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")
plot(y ^ (1/4), main = "Quartic Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")
plot(log(y), main = "Log Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")

y.star <- log(y)

par(mfrow = c(2,1))
acf(y.star, lag = 50, main = "ACF of Electronic Retail Sales")
pacf(y.star, lag = 50, main = "PACF of Electronic Retail Sales")

reg_diff_y.star <- diff(y.star, lag = 1, differences = 1)
seasonal_diff_y.star <- diff(y.star, lag = 12, differences = 1)
reg_seasonal_diff_y.star <- diff(reg_diff_y.star, lag = 12)

par(mfrow=c(2, 1))
acf(reg_diff_y.star, lag = 50, main = "ACF - Regular Differencing: Electronic Retail Sales")
pacf(reg_diff_y.star, lag = 50, main = "PACF - Regular Differencing: Electronic Retail Sales")

par(mfrow=c(2, 1))
acf(seasonal_diff_y.star, lag = 50, main = "ACF - Seasonal Differencing: Electronic Retail Sales")
pacf(seasonal_diff_y.star, lag = 50, main = "PACF - Seasonal Differencing: Electronic Retail Sales")

par(mfrow=c(2, 1))
acf(reg_seasonal_diff_y.star, lag = 50, main = "ACF - Regular and Seasonal Differencing: Electronic Retail Sales")
pacf(reg_seasonal_diff_y.star, lag = 50, main = "PACF - Regular and Seasonal Differencing: Electronic Retail Sales")

y.star.star <- reg_seasonal_diff_y.star

#library("forecast") -- REMOVE LATER
#auto.arima(y.star)
#ARIMA(0,1,1)(1,1,1)[12] -- REMOVE LATER

electronic_arima_model1 <- arima(y.star, order = c(0, 1, 1), seas = list(order = c(1, 1, 1), 12))

par(mfrow=c(2,1))
acf(electronic_arima_model1$residuals, lag = 50, main = "ACF - Residuals of ARIMA(0,1,1)(1,1,1)[12]: Electronic Retail Sales")
pacf(electronic_arima_model1$residuals, lag = 50, main = "PACF - Residuals of ARIMA(0,1,1)(1,1,1)[12]: Electronic Retail Sales")
Box.test(electronic_arima_model1$residuals, lag = 12, type="Ljung") # lag ? 12 or 20
electronic_arima_model1$aic

electronic_arima_model2 <- arima(y.star, order = c(0, 1, 1), seas = list(order = c(0, 1, 2), 12))

par(mfrow=c(2,1))
acf(electronic_arima_model2$residuals, lag = 50, main = "ACF - Residuals of ARIMA(0,1,1)(0,1,2)[12]: Electronic Retail Sales")
pacf(electronic_arima_model2$residuals, lag = 50, main = "PACF - Residuals of ARIMA(0,1,1)(0,1,2)[12]: Electronic Retail Sales")
Box.test(electronic_arima_model2$residuals, lag = 12, type="Ljung") # lag ? 12 or 20
electronic_arima_model2$aic


# AR part
Mod(polyroot(c(1,-0.1843))) # greater than 1
# AR is stationary

# MA part
Mod(polyroot(c(1,-0.3745))) # greater than 1
Mod(polyroot(c(1,-0.5782))) # greater than 1
# MA is invertible

all(c(Mod(polyroot(c(1, -0.3745))), Mod(polyroot(c(1, -0.1843))),Mod(polyroot(c(1, -0.5782)))) > 1)
# Thus, AR is stationary and MA is invertible

# T-Test Statistics for Parameters
arima_coefficients_model1 <- electronic_arima_model1$coef
arima_se_model1 <- sqrt(diag(vcov(electronic_arima_model1)))
t_test_arima_model1 <- arima_coefficients_model1 / arima_se_model1
t_test_arima_model1
abs(t_test_arima_model1) > 2

forecast=predict(electronic_arima_model1, n.ahead = 12)

forecast.value = ts((forecast$pred), start=start(retails_electronic_test), freq=12)
ci.low= ts((forecast$pred-1.96*forecast$se),
           start=start(retails_electronic_test),freq=12)
ci.high=ts((forecast$pred+1.96*forecast$se),
           start=start(retails_electronic_test),freq=12)

# since we did log transformation
df.forecast = data.frame(retails_electronic_test, exp(forecast.value), 
                         exp(ci.low), exp(ci.high), forecast$se)

df.forecast

ts.plot(df.forecast, 
        col = c("black","red","blue","blue"),
        lty = c(1, 1, 2, 2),
        main = "ARIMA Forecast vs Retail Electronic Sales",
        xlab = "Time (in months) -- 2019",
        ylab = "Retail Sales (in millions of dollars)",
        ylim = c(5000, 12000))
legend(1, 11500,
       lty=c(1, 1, 2, 2), 
       text.col=c("black","red","blue","blue"), 
       legend=c("Electronic Retail Sales (Test)",
                "ARIMA Forecast", "Confidence Interval - Low", "Confidence Interval - Low"),text.font=1, cex=0.5)

ts.plot(cbind(retails_electronic_train, 
              exp(ci.low), exp(forecast.value),
              retails_electronic_test, exp(ci.high)),
        col=c("black", "blue", "red", "black" ,"blue"),
        lty = c(1,3,1,1,2),
        main="Electronic Retail Sales with ARIMA Forecast",
        ylab = "Retail Sales (in Millions of Dollars)",
        xlab = "Time (in years)")
abline(v = 2019.1, col = "black", lty = 2)
legend(x=1993.12, y=15000, lty=c(1,1,3),
       col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), 
       legend=c("Electronic Retail Sales", 
                "ARIMA Forecast", "Confidence Interval"),text.font=1, cex=0.5)

RMSE_ARIMA <- sqrt(mean((df.forecast$retails_electronic_test - df.forecast$exp.forecast.value)^2))
RMSE_ARIMA






# splitting the independent variables into train and test
retails_hobby_train = hobby %>% 
  window(end = c(2018, 12))

retails_hobby_test = hobby %>% 
  window(start = 2019)

retails_furniture_train = furniture %>% 
  window(end = c(2018, 12))

retails_furniture_test = furniture %>% 
  window(start = 2019)

# pretransforming independent and dependent variable
x1_star <- log(retails_hobby_train)
x2_star <- log(retails_furniture_train)
y <- retails_electronic_train
y_star <- log(y)


# multiple linear regression
mlr_residuals_star <- lm(y_star ~ x1_star + x2_star)$residuals


# play with residuals
resmean = mean(mlr_residuals_star)
ressd = sd(mlr_residuals_star)
sdresiduals = (mlr_residuals_star)/ressd


resid.ts = ts(sdresiduals, start = start(retails_electronic_test), frequency = 12)
mlr_residuals_star_star = diff(diff(resid.ts, 1), 12) # seasonal difference the regular difference
plot(ts(mlr_residuals_star))
plot(mlr_residuals_star_star) 
abline(h = 0, col = "red") # we can see that this is mean stationary around 0



# look for degree of ARIMA
par(mfrow = c(1,2))
acf(mlr_residuals_star_star, lag.max = 50)
pacf(mlr_residuals_star_star, lag.max = 50)


auto.arima(resid.ts)
am.resid = arima(resid.ts, order = c(0,1,2), seas = list(order = c(0,1,1), 12))



par(mfrow = c(1, 2))
acf(resid(am.resid), lag.max = 50)
pacf(resid(am.resid), lag.max = 50)

Box.test(resid(am.resid), lag = 12, type = "Ljung")


library(nlme)
modelgls=gls(y_star~x1_star+x2_star, correlation=corARMA(c(-0.4746, -0.0803), q=2))
acf(residuals(modelgls, type = "normalized"))

df_future = data.frame(x1_star = retails_hobby_test, 
                       x2_star = retails_furniture_test)


gls_forecast = predict(modelgls, df_future)

gls_forecast

df = data.frame(retails_electronic_test, gls_forecast)
ts.plot(df, col = c("black", "red"))
legend(x=2, y=10000, lty=c(1,1,3),
       col=c("black", "red"),
       text.col=c("black", "red"), 
       legend=c("real_data","forecast"),text.font=1, cex=0.5)

ts.plot(cbind(retails_electronic_train,
              ts(gls_forecast[1:12], start = start(retails_electronic_test), 
                 frequency = 12),
              retails_electronic_test),
        col = c("black", "red"))
abline(v = 2019.1, col = "green", lwd = 1.5)
legend(x=1993.12, y=15000, lty=c(1,1,3),
       col=c("black", "red"),
       text.col=c("black", "red"), 
       legend=c("real_data","forecast"),text.font=1, cex=0.5)












####################################################################################



length(retails_electronic_train)
length(retails_furniture_train)
length(retails_hobby_train)


plot.ts(mts.data,lwd=1.5,cex=1.5)
acf(mts.data)



train_star <- cbind(y_star, x1_star, x2_star)
plot.ts(train_star,lwd=1.5,cex=1.5,
        main="We must make data stationary prior to analysis")
acf(train_star)

# generate the multiple time series
mts.data <- cbind(retails_electronic_train, retails_furniture_train, retails_hobby_train)
colnames(mts.data) = c("electronic", "hobby", "furniture")

# multiple time series of pre-transformed data
mts.data_star = cbind(log(retails_electronic_train), sqrt(retails_hobby_train), log(retails_furniture_train))
colnames(mts.data_star) = c("electronic*", "hobby*", "furniture*")
plot(mts.data_star)

# view the acf of the multiple time series of pre-transformed data
acf(mts.data_star, lag.max = 50)

# multiple time series of differenced, pre-transformed data
mts.data_star_star = diff(diff(mts.data_star, 1), 12)
colnames(mts.data_star_star) = c("electronic**", "hobby**", "furniture**")
acf(mts.data_star_star, lag.max = 50, lwd=1.3) # approximately white noise


# Find the ccf
acf(mts.data_star_star, lag = 50)

# determined that p = 11 from the ccf plot
var.retail = VAR(mts.data, p = 11)

# extract significant coefficients using the summary function
summary(var.retail)

# determine if the roots are all less than 1
roots(var.retail, modulus = TRUE)

# constructing the impulse response function for three variables
irf_electronics <- irf(var.retail, impulse = "electronic", response = c("electronic", "hobby", "furniture"), n.ahead = 12)
irf_furniture <- irf(var.retail, impulse = "furniture", response = c("electronic", "hobby", "furniture"), n.ahead = 12)
irf_hobbies <- irf(var.retail, impulse = "hobby", response = c("electronic", "hobby", "furniture"), n.ahead = 12)

# plotting the imf for all three variables
plot(irf_electronics)
plot(irf_furniture)
plot(irf_hobbies)

# generate a prediction 
var.pred = predict(var.retail, n.ahead = 12, ci = 0.95)
var.pred.electronic = ts(var.pred$fcst$electronic[, 1], 
                         start = start(retails_electronic_test), frequency = 12)

# lower of the CI
var.pred.cilower = ts(var.pred$fcst$electronic[, 2], 
                      start = start(retails_electronic_test), frequency = 12)
# upper of the CI
var.pred.ciupper = ts(var.pred$fcst$electronic[, 3], 
                      start = start(retails_electronic_test), frequency = 12)

# this will be used to plot in the next step
var_df = cbind(retails_electronic_test, var.pred.electronic, 
               var.pred.cilower, var.pred.ciupper)

# close up of the testing variable
ts.plot(var_df, 
        col = c("black", "red", "blue", "blue"), 
        lty = c(1, 1, 3, 3),
        main = "VAR of Just the Testing Data (p = 11)")

legend(x=2019.0, y=12000, lty=c(1,1,3),
       col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), 
       legend=c("real_data", "VAR(p = 11)", "CI"),
       text.font=1, cex=1)

# overall time series
ts.plot(cbind(retails_electronic_train,
              ts(var_df[, c(2, 3, 4)], start = start(retails_electronic_test), 
                 frequency = 12),
              retails_electronic_test),
        col = c("black", "red", "blue", "blue"),
        lty = c(1, 1, 2, 2), main = "VAR Including Training Data (p = 11)")
abline(v = 2019.1, col = "green", lwd = 1.5)
legend(x=1993.12, y=15000, lty=c(1,1,3),
       col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), 
       legend=c("real_data","VAR(p = 11)", "CI"),text.font=1, cex=1)

# RMSE
sqrt(mean((var_df[, 1] - var_df[, 2])^2))


# 
# 
# 
# 
# ##########
# # Forecast
# ##########
# 
# # Predict 20 quarters out of sample .
# 
# VAR.pred <- predict(VAR.train, n.ahead=20)
# VAR.pred
# e.pred <- ts(VAR.pred$fcst$e[,1],st=c(1992,1),fr=4)
# e.pred.low <-ts(VAR.pred$fcst$e[,2],st=c(1992,1),fr=4)
# e.pred.upper<- ts(VAR.pred$fcst$e[,3],st=c(1992,1),fr=4)
# 
# prod.pred <-ts(VAR.pred$fcst$prod[,1],st=c(1996,1),fr=4)
# prod.pred.low <-ts(VAR.pred$fcst$prod[,2],st=c(1996,1),fr=4)
# prod.pred.upper<- ts(VAR.pred$fcst$prod[,3],st=c(1996,1),fr=4)
# 
# rw.pred <-ts(VAR.pred$fcst$rw[,1],st=c(1996,1),fr=4)
# rw.pred.low <-ts(VAR.pred$fcst$rw[,2],st=c(1996,1),fr=4)
# rw.pred.upper<- ts(VAR.pred$fcst$rw[,3],st=c(1996,1),fr=4)
# 
# U.pred <-ts(VAR.pred$fcst$U[,1],st=c(1996,1),fr=4)
# U.pred.low <-ts(VAR.pred$fcst$U[,2],st=c(1996,1),fr=4)
# U.pred.upper<- ts(VAR.pred$fcst$U[,3],st=c(1996,1),fr=4)
# 
# 
# ## Students will calculate the root mean square error of the forecast
# 
# class(Canada[,1])
# start(Canada[,1]); end(Canada[,1])
# start(e.pred); end(e.pred)
# 
# # Put predicted values in plot
# 
# 
# par(mfrow = c(2,2),
#     font.axis = 2,
#     font.main = 2,
#     font.lab = 2,
#     mar = c(5, 5, 4, 4))
# 
# ts.plot(cbind(diff(Canada[,1]), e.pred, e.pred.low, e.pred.upper,diff(test[,1],1)),
#         lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
#         main = "Forecast of change in e with VAR(2)",ylab="e",ylim=c(-2,3))
# #legend(1980,3,c("e","forecast", "CIL", "CIU","test"),lty=c(1,2,3,3,4),col=c("black","red", "blue","blue","black"))
# 
# ts.plot(cbind(diff(Canada[,2],1), prod.pred, prod.pred.low, prod.pred.upper,diff(test[,2],1)),
#         lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
#         main = "Forecast of change in Prod with VAR(2)",ylab="prod",ylim=c(-2,3))
# #legend(1980,3,c("prod","forecast", "CIL", "CIU","test"),lty=c(1,2,3,3,4),col=c("black","red", "blue","blue","black"))
# 
# ts.plot(cbind(diff(Canada[,3],1), rw.pred, rw.pred.low, rw.pred.upper,diff(test[,3],1)),
#         lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
#         main="Forecast of rw with VAR(2)", ylab="rw",ylim=c(-2,6))
# #legend(1985,6,c("rw","forecast", "CIL", "CIU","test"),lty=c(1,2,3,3,4),col=c("black","red", "blue","blue","black"))
# 
# ts.plot(cbind(diff(Canada[,4],1), U.pred, U.pred.low, U.pred.upper,diff(test[,4],1)),
#         lty=c(1,2,3,3,4), col=c("black","red", "blue","blue","black"), lwd=c(2,2,2,2,2),
#         main = "Forecast of U with VAR(2)",  ylab="U",ylim=c(-2,2))
# #legend(1980,-1,c("U","forecast", "CIL", "CIU","test"),lty=c(1,2,3,3,4),col=c("black","red", "blue","blue","black"))
# 
# dev.off()
# ##### Impulse response functions
# 
# ## You can calculate the impulse response functions.
# 
# 
# par(mfrow = c(1,1),
#     font.axis = 2,
#     font.main = 2,
#     font.lab = 2,
#     mar = c(5, 5, 4, 4))
# 
# irf=irf(VAR.Canada, impulse = "e", response = c("e","prod", "rw", "U"), boot =
#           FALSE,n.ahead=40,lwd=2)
# ## plot the four impulse response variables
# plot(irf)
# 
# dev.off()
# 
# irf=irf(VAR.Canada, impulse = "prod", response = c("e","prod", "rw", "U"), boot =
#           FALSE,n.ahead=40)
# plot(irf)
# 
# par(mfrow = c(1,1),
#     font.axis = 2,
#     font.main = 2,
#     font.lab = 2,
#     mar = c(5, 5, 4, 4))
# 
# 
# 
# par(mfrow = c(1,1),
#     font.axis = 2,
#     font.main = 2,
#     font.lab = 2,
#     mar = c(5, 5, 4, 4))
# 
# irf=irf(VAR.Canada, impulse = "rw", response = c("e","prod", "rw", "U"), boot =
#           FALSE,n.ahead=40,lwd=2)
# ## plot the four impulse response variables
# plot(irf)
# 
# dev.off()
# 
# 
# 
# irf=irf(VAR.Canada, impulse = "U", response = c("e","prod", "rw", "U"), boot =
#           FALSE,n.ahead=40)
# plot(irf)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
