library(stats)
library(forecast)

A1 <- read.csv("Discharge/discharge_A1.csv")
A2 <- read.csv("Discharge/discharge_A2.csv")

# IMPORTANT: Check on map and translate direction of flow relative to the basin of interest
# In this case, negative flow in the y-direction (i.e. south) from our A1 file corresponds to positive flow into the basin A
A1 <- -A1$flow
A2 <- A2$flow

# Time series plot
plot(seq (2016,2025-1/48,(1/48)), A1, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from A1 (2016-2024)" )
plot(seq (2016,2025-1/48,(1/48)), A2, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from A2 (2016-2024)" )


# Reserve last year for test data
A1_train <- A1[1:384]
A1_test <- A1[385:432]

A2_train <- A2[1:384]
A2_test <- A2[385:432]


## ARIMA model with linear regression on input series ##

# Fetch river names
file_names <- list.files(path = "Rivers_csv/")
if(length(file_names) == 0){
  stop("Empty or non-existing file_path")
}
file_paths <- file.path("Rivers_csv", file_names)

# First try: one river (Fyrisån)
rf1 <- read.csv(file_paths[6])
rf1_train <- rf1$flow[1:384]

# Check cross correlations between input rivers and flow

# De-season output and input as season dominates both otherwise
ts_out <- ts(A1_train, frequency=48)
ts_in1 <- ts(rf1_train, frequency=48)

stl_out <- stl(ts_out, s.window="periodic")
stl_in1 <- stl(ts_in1, s.window="periodic")

y_deseason <- stl_out$time.series[,3]
x1_deseason <- stl_in1$time.series[,3]

# Pre-whiten input and output to see true CCFs
# (I might change this part in the future, not entirely sure on this)
fit_x1 <- auto.arima(x1_deseason)
x1_filt <- residuals(fit_x1)
y_filt  <- residuals(Arima(y_deseason, model=fit_x1))
ccf(x1_filt, y_filt, main="Prewhitened CCF: Fyrisån inflow vs A1 flow")

#' How to interpret ccf(x,y) plot: 
#' Blue lines shows statistical significance, if it is below it is probably random
#' If there is spike at some lag -k, this means that y_t might depend on x_(t-k)
#' Spikes at positive lags are probably other patterns which we won't use (it wouldn't make sense that future flows from rivers would impact current interface flow.)
#' Most series has some correlation at lag 0 (the water flow in the region as a whole is probably connected to common factors) so check on how strong the correlation at 0 is. 

# Regression model
# Function to extract lags
make_lags <- function(x, maxlag=6) {
  lagmat <- sapply(1:maxlag, function(k) dplyr::lag(x, k))
  colnames(lagmat) <- paste0("lag", 1:maxlag)
  return(lagmat)
}
# Use package for regression
lags <- 1
X1 <- make_lags(rf1_train, lags)
# Add unlagged time series before the lags
X <- cbind(rf1_train, X1)
colnames(X)[1] <- 'lag0'
# Remove first lag number of datapoints which doesn't have lag values
X <- na.omit(X)
# Remove same number of datapoints from the beginning of target output
y <- A1_train[(lags+1):length(A1_train)]

# One can add more features or time-series as input, just put the resulting dataframe X into xreg
# Fit ARIMA automatically
fit_sarimax <- auto.arima(y, xreg=X, seasonal=TRUE)
# Compare different models by checking the AIC score (smaller AIC = better) 
# and if the coefficients are statistically significant (2*s.e. away from 0)
# If a lot of coefficients are insignificant, might need fewer parameters (but also weigh against AIC, it can still be a better model)
summary(fit_sarimax)

# For predictions, we need lag number of elements before the first prediction
rf1_test <- rf1$flow[(385-lags):432]

# Make lags and bind to original series as before
future_X1 <- make_lags(rf1_test, lags)
future_X <- cbind(rf1_test,future_X1)
future_X <- na.omit(future_X)
# Name must match labels in training data
colnames(future_X)[1] <- 'lag0'
# Forecast h steps ahead
fc_ds <- forecast(fit_sarimax, xreg = future_X, h = 48)

# plot actual data for A1 together with predictions
plot(seq (2024,2025-1/48,(1/48)), A1_test, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Flow m3/s",  main = "ARIMA Regression model: Actual vs. predicted flow of A1 (using fyrisån as input)")
lines(seq (2024,2025-1/48,(1/48)), fc_ds$mean,type="l", lwd =1.5, col="red")
lines(seq (2024,2025-1/48,(1/48)), fc_ds$upper[,2],lty="dashed", lwd =1.5, col="blue")
lines(seq (2024,2025-1/48,(1/48)), fc_ds$lower[,2],lty="dashed", lwd =1.5, col="blue")
legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions", "95% Confidence interval"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)


# Do the same for A2
# Check through rivers of interest to find noticeable CCFs
rf2 <- read.csv(file_paths[9])
rf2_train <- rf2$flow[1:384]


# De-season output and input as season dominates both
ts_out <- ts(A2_train, frequency=48)
ts_in1 <- ts(rf2_train, frequency=48)

stl_out <- stl(ts_out, s.window="periodic")
stl_in1 <- stl(ts_in1, s.window="periodic")

y_deseason <- stl_out$time.series[,3]
x1_deseason <- stl_in1$time.series[,3]

# Pre-whiten input and output to see true CCFs
fit_x1 <- auto.arima(x1_deseason) #This one might take some time, probably because ARIMA is a bad fit for some rivers, but I'll check into it
x1_filt <- residuals(fit_x1)
y_filt  <- residuals(Arima(y_deseason, model=fit_x1))
ccf(x1_filt, y_filt, main="Prewhitened CCF: River vs A2 flow")

#' Noticeable correlations:
#' 2: Bällstaån, 0 and 2 lags
#' 3: Brobäcen 0 and 2 lags
#' 4: Enköpingsån, 0 and 2 lags
#' 5: Eskilstunaån: lag 0
#' 7: lag 0
#' 9: Köpingsån: lag 0 >0.3
#' 11: lag 0
#' 15: lag 0 >0.3
#' 16: lag 0 >0.3
#' 17: lag 0 >0.3
#'
#' Checking for relevant rivers might be easier when we can choose rivers from Jorrits map and sum neighboring rivers.

# Chosing 3 rivers as a try
rf2 <- read.csv(file_paths[4])
rf3 <- read.csv(file_paths[5])
rf4 <- read.csv(file_paths[9])
rf2_train <- rf2$flow[1:384]
rf3_train <- rf3$flow[1:384]
rf4_train <- rf4$flow[1:384]
 

lags2 <- 2
X1 <- make_lags(rf2_train, lags2)
X2 <- make_lags(rf3_train, lags2)
X3 <- make_lags(rf4_train, lags2)
# Remember to add original time series (this can probably be automated if we improve the make_lags function)
X <- cbind( rf2_train, X1, rf3_train, X2, rf4_train, X3)
# Name the new columns
colnames(X)[1] <- 'lag0'
colnames(X)[1+lags2+1] <- 'lag0.1'
colnames(X)[1+(lags2+1)*2] <- 'lag0.2'
X <- na.omit(X)
# Match y to lag
y <- A2_train[(lags2+1):length(A2_train)]

# Fit model
fit_sarimax2 <- auto.arima(y, xreg=X, seasonal=TRUE)
summary(fit_sarimax2)
# You can see that this model actually has a lot of statistically insignificant parameters, room for improvement by choosing better input etc.
# We later see that the predictions are alot more unsure for this model

# Try it on test data
rf2_test <- rf2$flow[(385-lags2):432]
rf3_test <- rf3$flow[(385-lags2):432]
rf4_test <- rf4$flow[(385-lags2):432]

future_X1 <- make_lags(rf2_test, lags2)
future_X2 <- make_lags(rf3_test, lags2)
future_X3 <- make_lags(rf4_test, lags2)
future_X <- cbind(rf2_test,future_X1, rf3_test, future_X2, rf4_test,future_X3)
#future_X <- cbind(rf4$flow[383:432],future_X3)
future_X <- na.omit(future_X)
colnames(future_X)[1] <- 'lag0'
colnames(future_X)[1+lags2+1] <- 'lag0.1'
colnames(future_X)[1+(lags2+1)*2] <- 'lag0.2'

#Make forecasts
fc_ds_2 <- forecast(fit_sarimax2, xreg = future_X, h = 48)

# plot actual data for A2 together with predictions seq (2016,2025-1/48,(1/48))
plot(seq (2024,2025-1/48,(1/48)), A2_test, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Flow m3/s",  main = "ARIMA Regression model: Actual vs. predicted flow of A2 (using several rivers)")
lines(seq (2024,2025-1/48,(1/48)), fc_ds_2$mean,type="l", lwd =1.5, col="red")
lines(seq (2024,2025-1/48,(1/48)), fc_ds_2$upper[,2],lty="dashed", lwd =1.5, col="blue")
lines(seq (2024,2025-1/48,(1/48)), fc_ds_2$lower[,2],lty="dashed", lwd =1.5, col="blue")
legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions", "95% Confidence interval"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)



# Finally, compare against real percentage of inflow
# If flow through interface is negative, set it to 0 as we want percentage of inflow
A1_test[A1_test < 0] <- 0
A2_test[A2_test < 0] <- 0
# Calculate percentage for interface of interest
A2_percentage <- A2_test/(A1_test+A2_test)
# If both are 0, replace resulting NaN with 0
A2_percentage[is.nan(A2_percentage)] <- 0

# Do the same for our predictions
pred1 <- as.numeric(fc_ds$mean)
pred2 <- as.numeric(fc_ds_2$mean)
pred1[pred1<0] <- 0
pred2[pred2<0] <- 0
pred_percentage <- pred2/(pred1+pred2)
pred_percentage[is.nan(pred_percentage)] <- 0

# Plot (I do not know what the credible intervals should be after division, we might need to research)
plot(seq (2024,2025-1/48,(1/48)), A2_percentage, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Percentage of flow",  main = "Regression model: Predicted and actual percentage of flow from A2 2024")
lines(seq (2024,2025-1/48,(1/48)), pred_percentage,type="l", lwd =1.5, col="red")

legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c("black","red"),           # Line colors
       lwd = 2)


# Plot for percentage on all data to see model fit on both training and test
# Combine both fitted and predicted data
A1_model <- c(fit_sarimax$fitted, fc_ds$mean)
A2_model <- c(fit_sarimax2$fitted, fc_ds_2$mean)
A1_model[A1_model < 0] <- 0
A2_model[A2_model < 0] <- 0

A2_model_percentage <- A2_model/(A1_model[2:length(A1_model)] + A2_model)
A2_model_percentage[is.nan(A2_model_percentage)] <- 0

A1_true <- A1
A2_true <- A2
A1_true[A1_true < 0] <- 0
A2_true[A2_true < 0] <- 0
A2_true_percentage <- A2_true/(A1_true+A2_true)
# If both are 0, replace resulting NaN with 0
A2_true_percentage[is.nan(A2_true_percentage)] <- 0

plot(seq (2016+2/48,2025-1/48,(1/48)), A2_true_percentage[3:length(A2_true_percentage)], type="l", lwd =1.5, xlim=c(2016,2025), xlab = "Time" , ylab = "Percentage of flow",  main = "Regression model: Actual and modelled percentage of flow from A2")
lines(seq (2016+2/48,2025-1/48,(1/48)), A2_model_percentage,type="l", lwd =1.5, col="red")
abline(v = 2024, col = "blue", lwd = 1.5, lty = 2)

legend(x = "bottom",          # Position
       legend = c("Actual data", "Model", "train/test split"),  # Legend texts
       lty = c(1, 1, 2),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)

# TODO: Maybe add some metrics like standard deviance or mean absolute distance from true data, as a measure of model






## Old model: Simple ARIMA ###
# Only check if curious, the other one is better.  
# ARIMA models can also be fitted automatically with packages such as auto.arima so this manual approach is a bit unnecessary)

# Check if stationary
library("tseries")
adf_test <- adf.test(A1_train)
print(adf_test)
adf_test_2 <- adf.test(A2_train)
print(adf_test_2)
# p-values very low -> Stationary with high certainty

## ARIMA for A1 ##
# ACF and PACF of time series
par(mfrow=c(2,1))
acf(A1_train, 100, main=" ACF for A1")
pacf(A1_train, 100, main=" PACF for A1")

# ACF confirms yearly seasonal trend, start with seasonal difference (AIC = 3146.71 for reference)
m=arima(A1_train,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=48))
m

# Plot ACF and PACF for residuals
acf(m$residuals,100)
pacf(m$residuals,100)

# ACF tails off, PACF cuts off after lag 3, try AR(3) (AIC = 2729.06)
m2=arima(A1_train,order=c(3,0,0),seasonal=list(order=c(0,1,0),period=48))
m2

# Plot ACF and PACF for residuals
acf(m2$residuals,100)
pacf(m2$residuals,100)
# ACF and PACF looks good except some spikes. Try fine tuning parameters
# This one worked best (statistically significant coefficients and AIC = 2607.97)
m3=arima(A1_train,order=c(3,0,0),seasonal=list(order=c(0,1,1),period=48))
m3

# Plot ACF and PACF for residuals
acf(m3$residuals,100)
pacf(m3$residuals,100)
# Very good ACF and PACF, check residual diagnostics

# Histogram and QQ-plot for residuals
hist(m3$residuals)
qqnorm(m3$residuals)
# residuals kind of skewed, some outliers
tsdiag(m3,100)
# P-values good, only problem is residual outliers and skewed to positive (probably as flow has 0 as "bottom" but varies much upwards)

## ARIMA for A2 ##
# ACF and PACF of time series
acf(A2_train, 100, main=" ACF for A2")
pacf(A2_train, 100, main=" PACF for A2")

# Seasonal behaviour here too, start with empty seasonal difference (AIC = 3730.2 for reference)
m4=arima(A2_train,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=48))
m4

# Plot ACF and PACF for residuals
acf(m4$residuals,100)
pacf(m4$residuals,100)

# AR(3) looks reasonable here too (AIC = 3592.04)
m5=arima(A2_train,order=c(3,0,0),seasonal=list(order=c(0,1,0),period=48))
m5

# Plot ACF and PACF for residuals
acf(m5$residuals,100)
pacf(m5$residuals,100)

# Looks ok, fine tune
# This one worked best (statistically significant coefficients and AIC = 3450.72)
m6=arima(A2_train,order=c(3,0,0),seasonal=list(order=c(0,1,1),period=48))
m6

acf(m6$residuals,100)
pacf(m6$residuals,100)
# Very good ACF and PACF, check residual diagnostics

# Histogram and QQ-plot for residuals
hist(m6$residuals)
qqnorm(m6$residuals)
# Very good residuals
tsdiag(m6,100)
# P-values very good

## Predictions on unseen data##
# A1
par(mfrow=c(1,1))
fore_1=predict(m3,n.ahead=48)
# plot actual data for A1 together with predictions seq (2016,2025-1/48,(1/48))
plot(seq (2024,2025-1/48,(1/48)), A1_test, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Flow m3/s",  main = "ARIMA model: Actual and predicted flow for A1 2024")
lines(seq (2024,2025-1/48,(1/48)), fore_1$pred,type="l", lwd =1.5, col="red")
lines(seq (2024,2025-1/48,(1/48)), fore_1$pred+2*fore_1$se,lty="dashed", lwd =1.5, col="blue")
lines(seq (2024,2025-1/48,(1/48)), fore_1$pred-2*fore_1$se,lty="dashed", lwd =1.5, col="blue")
legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions", "Confidence interval"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)
# Not very good, can't capture the assymetric peaks

# A2
par(mfrow=c(1,1))
fore_2=predict(m6,n.ahead=48)
# plot actual data for A1 together with predictions seq (2016,2025-1/48,(1/48))
plot(seq (2024,2025-1/48,(1/48)), A2_test, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Flow m3/s",  main = "ARIMA model: Actual and predicted flow for A2 2024")
lines(seq (2024,2025-1/48,(1/48)), fore_2$pred,type="l", lwd =1.5, col="red")
lines(seq (2024,2025-1/48,(1/48)), fore_2$pred+2*fore_2$se,lty="dashed", lwd =1.5, col="blue")
lines(seq (2024,2025-1/48,(1/48)), fore_2$pred-2*fore_2$se,lty="dashed", lwd =1.5, col="blue")
legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions", "Confidence interval"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)
# Not very good for the same reason


# Try predicting the percentage of A1
A1_test[A1_test < 0] <- 0
A2_test[A2_test < 0] <- 0
A2_percentage <- A2_test/(A1_test+A2_test)
# If both are 0, replace resulting NaN with 0
A2_percentage[is.nan(A2_percentage)] <- 0

pred1 <- fore_1$pred
pred2 <- fore_2$pred
pred1[pred1<0] <- 0
pred2[pred2<0] <- 0
pred_percentage <- pred2/(pred1+pred2)
pred_percentage[is.nan(pred_percentage)] <- 0

plot(seq (2024,2025-1/48,(1/48)), A2_percentage, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Percentage of flow",  main = "ARIMA model: Actual and predicted percentage of flow from A2 2024")
lines(seq (2024,2025-1/48,(1/48)), pred_percentage,type="l", lwd =1.5, col="red")

legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c("black","red"),           # Line colors
       lwd = 2)


