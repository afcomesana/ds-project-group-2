library(corrplot)
library(Hmisc)

# ------------------------
# --- Data Preparation ---
# ------------------------
A1 <- read.csv("Discharge/discharge_A1.csv")
A2 <- read.csv("Discharge/discharge_A2.csv")

# IMPORTANT: Check on map and translate direction of flow relative to the basin of interest
# In this case, negative flow in the y-direction (i.e. south) from our A1 file corresponds to positive flow into the basin A
A1 <- -A1$flow
A2 <- A2$flow

# Percentage target
A1_p <- A1
A2_p <- A2
# If flow through interface is negative, set it to 0 as we want percentage of inflow
A1_p[A1_p < 0] <- 0
A2_p[A2_p < 0] <- 0
# Calculate percentage for interface of interest
A2_percentage <- A2_p/(A1_p+A2_p)
# If both are 0, replace resulting NaN with 0
A2_percentage[is.nan(A2_percentage)] <- 0


# Initiate data frame for each model
df_A1 <- data.frame(A1)
df_A2 <- data.frame(A2)
df_A2p <- data.frame(A2_percentage) 

# Fetch river names
file_names <- list.files(path = "Rivers_csv/")
if(length(file_names) == 0){
  stop("Empty or non-existing file_path")
}
file_paths <- file.path("./Rivers_csv", file_names)

# Add river flows as input to all data-frames
for (path in file_paths){
  R <- read.csv(path)
  df_A1 <- cbind(df_A1, R[2])
  df_A2 <- cbind(df_A2, R[2])
  df_A2p <- cbind(df_A2p, R[2])
}

# Rename to river names
river_names <- c("Arboga", 
                 "Ballsta", 
                 "Brobacken", 
                 "Enkoping", 
                 "Eskilstuna", 
                 "Fyris", 
                 "Hedstrommen", 
                 "Kolback", 
                 "Koping", 
                 "Lovsta", 
                 "Marsta", 
                 "Orsund", 
                 "Oxund", 
                 "Racksta",
                 "Sagan",
                 "Sava",
                 "Svart")
names(df_A1) <- c("A1", river_names)
names(df_A2) <- c("A2", river_names)
names(df_A2p) <- c("A2p", river_names)

# Add sum of northern and western rivers 
# IMPORTANT: The rivers probably needs different splits for different interfaces
north <- c("Fyris", "Orsund", "Sava", "Lovsta", "Marsta", "Oxund")
west <- c("Koping", "Hedstrommen", "Arboga", "Kolback","Eskilstuna", "Svart")
df_A1$north <- rowSums(df_A1[, north])
df_A1$west <- rowSums(df_A1[, west])

df_A2$north <- rowSums(df_A1[, north])
df_A2$west <- rowSums(df_A1[, west])

df_A2p$north <- rowSums(df_A1[, north])
df_A2p$west <- rowSums(df_A1[, west])

# Add meteorological data (Interfaces are close in space so can use the same data)
meteo <- read.csv("Meteo_csv/A1_meteo.csv")

df_A1 <- cbind(df_A1, meteo[2:4])
df_A2 <- cbind(df_A2, meteo[2:4])
df_A2p <- cbind(df_A2p, meteo[2:4])


# ---------------
# --- A1 flow ---
# ---------------

# Split into training and test data
# We could use random samples, but let's use the last two years as test to compare with time series model
A1_train <- df_A1[1:1192,]
A1_test <- df_A1[1193:1296,]

#Correlations and P-values
rcorr(as.matrix(A1_train))
#Correlation matrix
corrplot(cor(A1_train), method='color')
# Flow and rivers overall are very interlinked with strong correlations and low p-values (probably due to seasonal behavior)
# Northern rivers sum is perfect correlation to A1
# Precipitation at interface is almost no correlation and bad p-values (precipitation is probably more important at river watersheds)
# Wind speeds are low p value and some correlation, wind direction not as much (maybe makes sense to use anyways)

# Try model with just fyrisån
m1 <- lm(A1 ~ Fyris, data = A1_train)
summary(m1)
# Good p-values for coefficents
# R-value 0.981
# RSE 3.218

A1_prediction <- predict(m1, newdata = A1_test, interval = "confidence")

x = seq (1193,1296)

plot_pred(A1_prediction, A1_test$A1, x, "Predicted vs. actual flow for A1 (only Fyrisån)")


# Add wind speed and direction
m2 <- lm(A1 ~ Fyris + wind_speed + wind_dir, data = A1_train)
summary(m2)
# R-value 0.981
# RSE 3.212
# Wind is not helping much


A1_prediction <- predict(m2, newdata = A1_test, interval = "confidence")

plot_pred(A1_prediction, A1_test$A1, x, "Predicted vs. actual flow for A1 (Fyrisån + wind)")


# Northern rivers gives good model
m3 <- lm(A1 ~ north , data = A1_train)
summary(m3)
# R-value 0.99
# RSE 2.067

A1_prediction <- predict(m3, newdata = A1_test, interval = "confidence")

plot_pred(A1_prediction, A1_test$A1, x, "Predicted vs. actual flow for A1 (Northern rivers)")


# ---------------
# --- A2 flow ---
# ---------------
# Split into training and test data
# We could use random samples, but let's use the last two years as test to compare with time series model
A2_train <- df_A2[1:1192,]
A2_test <- df_A2[1193:1296,]

#Correlations and P-values
rcorr(as.matrix(A2_train))
#Correlation matrix
corrplot(cor(A2_train), method='color')

# Here western rivers and wind seems important

# Try model with west rivers + wind
m4 <- lm(A2 ~ west + wind_dir + wind_speed, data = A2_train)
summary(m4)
# Good p-values for coefficents
# R-value 0.7228
# RSE 27.11

A2_prediction <- predict(m4, newdata = A2_test, interval = "confidence")

x = seq (1193,1296)

plot_pred(A2_prediction, A2_test$A2, x, "Predicted vs. actual flow for A2 (west rivers + wind)")

# Try adding nortern rivers
m5 <- lm(A2 ~ west + north + wind_dir + wind_speed  , data = A2_train)
summary(m5)
# Good p-values for coefficents
# R-value 0.7499
# RSE 25.8
# Interesting observation: Flow from the north reduces flow into A2

A2_prediction <- predict(m5, newdata = A2_test, interval = "confidence")

x = seq (1193,1296)

plot_pred(A2_prediction, A2_test$A2, x, "Predicted vs. actual flow for A2 (west and north rivers + wind)")

# Try seperate rivers, remove until only stat. sign. coeff
m6 <- lm(A2 ~ Fyris + Orsund + Sava + Lovsta + Marsta  + Arboga + Kolback + Eskilstuna  + Brobacken + Racksta  + Sagan + wind_dir + wind_speed  , data = A2_train)
summary(m6)
# Good p-values for coefficents
# R-value 0.753
# RSE 25.66
# Not really better than using north and west sums

A2_prediction <- predict(m6, newdata = A2_test, interval = "confidence")

x = seq (1193,1296)

plot_pred(A2_prediction, A2_test$A2, x, "Predicted vs. actual flow for A2 (west and north rivers + wind)")


# ------------------
# --- Percentage ---
# ------------------
A2p_train <- df_A2p[1:1192,]
A2p_test <- df_A2p[1193:1296,]

# Plot predicted and real percentage
pred1 <- A1_prediction[,1]
pred2 <- A2_prediction[,1]
pred1[pred1<0] <- 0
pred2[pred2<0] <- 0
pred_percentage <- pred2/(pred1+pred2)
pred_percentage[is.nan(pred_percentage)] <- 0

plot(x, A2p_test$A2p, type="l", lwd =1.5, xlab = "Time" , ylab = "Percentage of flow",  main = "Predicted and actual percentage of flow from A2")
lines(x, pred_percentage,type="l", lwd =1.5, col="red")

legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions"),  # Legend texts
       lty = c(1, 1),           # Line types
       col = c("black","red"),           # Line colors
       lwd = 2)

# ------------------------------
# --- Predicting A2 directly ---
# ------------------------------

#Correlations and P-values
rcorr(as.matrix(A2p_train))
#Correlation matrix
corrplot(cor(A2p_train), method='color')

# Here western rivers and wind seems important

# Try model with west rivers + wind
m7 <- lm(A2p ~ west + north + Brobacken  + wind_dir + wind_speed, data = A2p_train)
summary(m7)
# Good p-values for coefficents
# R-value 0.1497
# RSE 0.2206

A2p_prediction <- predict(m7, newdata = A2p_test, interval = "confidence")

x = seq (1193,1296)

plot_pred(A2p_prediction, A2p_test$A2p, x, "Predicted vs. actual flow for A2 percentage directly")

# This one didn't work as well
# Getting closer to an average only


# ------------------------
# --- Helper functions ---
# ------------------------

plot_pred = function(preds, real, x, title){
  plot(x, real, type="l", lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = title)
  lines(x, preds[,1],type="l", lwd =1.5, col="red")
  lines(x, preds[,2],lty="dashed", lwd =1.5, col="blue")
  lines(x, preds[,3],lty="dashed", lwd =1.5, col="blue")
  legend(x = "topright",          # Position
         legend = c("Actual data", "Predictions", "95% Confidence interval"),  # Legend texts
         lty = c(1, 1,1),           # Line types
         col = c("black","red", "blue"),           # Line colors
         lwd = 2)
}





