###
# Anish Dulla, Daniel Neufeldt, Takao Oba, Shoichiro Ueno
# Homework 1
# February 24, 2023
###


### I. Introduction
library(Quandl)
library(dygraphs)
Quandl.api_key("Rtc_Eon27jXhwyGeEfyr")

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
lines(retails_elecronic_test, col = "blue", lty = 2)
legend(1993, 15900, legend = c("Electronic Retail Sales (Train)", "Electronic Retail Sales (Test)", "Fitted Exp. Smoothing"), col = c("black", "blue", "red"), lty = c(1, 2, 1), cex = 0.6)


par(mfrow = c(1, 2))
plot(electronic_pred, main = "Exponential Smoothing\n Forecast vs Retail \nElectronic Sales", xlab = "Time (2019)", ylab = "Retail Sales (in millions of dollars)", col = "red")
lines(retails_elecronic_test, col = "blue", lty = 2)
legend(2019, 10900, legend = c("Electronic Retail Sales", "Fitted Exp. Smoothing") , col = c("blue", "red"), lty = 2:1, cex = 0.6)

residual = as.numeric(retails_elecronic_test) - as.numeric(electronic_pred)
plot(residual, main = "Residuals of Exponential\n Smoothing Forecast", xlab = "Time (in months) -- 2019", ylab = "Residuals (in millions of dollars)")
abline(h = 0, col = "red")

### V. Polynomial regression plus seasonal effect modeling and forecasting

mult_decomp_seasonal_term <- mult_decomp$seasonal

regression_df <- data.frame(as.numeric(time(retails_electronic_train)))
regression_df$electronic_sales <- as.numeric(retails_electronic_train)
colnames(regression_df)[1] <- "time"
regression_df$time2 <- regression_df$time ^ 2
regression_df$time3 <- regression_df$time ^ 3

head(regression_df)

lin_model <- lm(electronic_sales ~ time, data = regression_df)
quad_model <- lm(electronic_sales ~ time + time2, data = regression_df)
cubic_model <- lm(electronic_sales ~ time + time2 + time3, data = regression_df)

lin_model_fit <- as.numeric(predict(lin_model, data = regression_df))
quad_model_fit <- as.numeric(predict(quad_model, data = regression_df))
cubic_model_fit <- as.numeric(predict(cubic_model, data = regression_df))

seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_lin_model_fit <- lin_model_fit * seasonals
total_quad_model_fit <- quad_model_fit * seasonals
total_cubic_model_fit <- cubic_model_fit * seasonals

lin_residuals <- as.numeric(retails_electronic_train) - total_lin_model_fit
quad_residuals <- as.numeric(retails_electronic_train) - total_quad_model_fit
cubic_residuals <- as.numeric(retails_electronic_train) - total_cubic_model_fit

scaled_lin_residuals <- scale(lin_residuals)
scaled_quad_residuals <- scale(quad_residuals)
scaled_cubic_residuals <- scale(cubic_residuals)

par(mfrow=c(1,3))

plot(ts(scaled_lin_residuals), main = "Scaled Residual Plot - Linear Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_quad_residuals), main = "Scaled Residual Plot - Quadratic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_cubic_residuals), main = "Scaled Residual Plot - Cubic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

testTimeRE <- data.frame(as.numeric(time(retails_elecronic_test)))
colnames(testTimeRE)[1] <- "time"
testTimeRE$time2 <- testTimeRE$time ^ 2
testTimeRE$time3 <- testTimeRE$time ^ 3

predictions_lin <- predict(lin_model, newdata=testTimeRE)
predictions_quad <- predict(quad_model, newdata=testTimeRE)
predictions_cubic <- predict(cubic_model, newdata=testTimeRE)

seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_predictions_lin <- predictions_lin * seasonals
total_predictions_quad <- predictions_quad * seasonals
total_predictions_cubic <- predictions_cubic * seasonals

plot(retails_elecronic_test - total_predictions_lin)
abline(h = 0, col = "red")

plot(retails_elecronic_test - total_predictions_quad)
abline(h = 0, col = "red")

plot(retails_elecronic_test - total_predictions_cubic)
abline(h = 0, col = "red")

RMSE_lin <- sqrt(mean((retails_elecronic_test - total_predictions_lin)^2))
RMSE_quad <- sqrt(mean((retails_elecronic_test - total_predictions_quad)^2))
RMSE_cubic <- sqrt(mean((retails_elecronic_test - total_predictions_cubic)^2))

min(RMSE_lin, RMSE_quad, RMSE_cubic) #RMSE_quad is smallest

### VI. Conclusion

RMSE_exp_smooth <- sqrt(mean((retails_elecronic_test - electronic_pred)^2))

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

Mod(polyroot(c(1, -0.1843))) # Stationary

Mod(polyroot(c(1, -0.3745))) # Invertibility
Mod(polyroot(c(1, -0.5782))) # Invertibility

all(c(Mod(polyroot(c(1, -0.3745))), Mod(polyroot(c(1, -0.1843))),Mod(polyroot(c(1, -0.5782)))) > 1)

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
