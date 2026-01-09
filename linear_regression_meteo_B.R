# Author: Alberto Fernández Comesaña

# ---- Data preprocessing ----
source("R/meteo_average.R")

# Prepare metereological data for interfaces:
interface <- 'C'

coords <- read.csv('discharges/coords.csv')
coords <- coords[coords$Case == interface,]

loc_counts <- table(coords$Location)

for(loc in names(loc_counts)) {
  for(coord in 1:loc_counts[[loc]]) {
    xt <- coords[coords$Location == loc,]$xt[coord]
    yt <- coords[coords$Location == loc,]$yt[coord]
    
    print(paste0('Getting meteo data for file: ',interface, loc, '_',coord,'.csv'))
    
    meteo_average('meteo/', xt, yt, 'meteo-csv/', paste0(interface, loc, '_',coord))
  }
}


# Read flows for every interface
B1 <- read.csv('discharges/discharge_B1.csv')$flow
B2 <- read.csv('discharges/discharge_B2.csv')$flow
B3 <- read.csv('discharges/discharge_B3.csv')$flow
B4 <- read.csv('discharges/discharge_B4.csv')$flow

Y <- data.frame(B1, B2, B3, B4)

X <- read.csv('meteo-csv/B1_1.csv')
X <- cbind(X, read.csv('meteo-csv/B1_2.csv'))
X <- cbind(X, read.csv('meteo-csv/B2_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_2.csv'))
X <- cbind(X, read.csv('meteo-csv/B4_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_2.csv'))

X <- X[,!grepl("^date", names(X))]

X_train <- X[1:1100,]
X_test <- X[1101:nrow(X),]

Y_train <- Y[1:1100,]
Y_test <- Y[1101:nrow(Y),]

# ---- Linear regression ----
lr <- lm(cbind(B1, B2, B3, B4) ~ ., data = cbind(X_train, Y_train))

predictions <- predict(lr, newdata = X_test)

start <- 1
end <- 48

# B1
plot(1:48, Y_test$B1[start:end], type="l")
lines(1:48, predictions[start:end,1], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B2
plot(1:48, Y_test$B2[start:end], type="l")
lines(1:48, predictions[start:end,2], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B3
plot(1:48, Y_test$B3[start:end], type="l")
lines(1:48, predictions[start:end,3], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B4
plot(1:48, Y_test$B4[start:end], type="l")
lines(1:48, predictions[start:end,4], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# ---- Linear regression adding up rain from past 2 weeks

# Sum rain lagged k=1
X_train_agg <- X_train
X_test_agg <- X_test
for(colname in colnames(X_train_agg)) {
  if(!startsWith(colname, "precip")) next()
  X_train_agg[,colname] <- X_train_agg[,colname] + c(0, head(X_train_agg[,colname], -1))
  X_test_agg[,colname] <- X_test_agg[,colname] + c(0, head(X_test_agg[,colname], -1))
}

X_train_agg <- X_train_agg[2:nrow(X_train_agg),]
Y_train_agg <- Y_train[2:nrow(Y_train),]
Y_test_agg <- Y_test[2:nrow(Y_test),]
# Train again linear regression
lr_agg <- lm(cbind(B1, B2, B3, B4) ~ ., data = cbind(X_train_agg, Y_train_agg))


predictions_agg <- predict(lr_agg, newdata = X_test_agg)

start <- 1
end <- 48

# B1
plot(1:48, Y_test_agg$B1[start:end], type="l")
lines(1:48, predictions_agg[start:end,1], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B2
plot(1:48, Y_test_agg$B2[start:end], type="l")
lines(1:48, predictions_agg[start:end,2], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B3
plot(1:48, Y_test_agg$B3[start:end], type="l")
lines(1:48, predictions_agg[start:end,3], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B4
plot(1:48, Y_test_agg$B4[start:end], type="l")
lines(1:48, predictions_agg[start:end,4], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))


# ---- Linear regression using one feature for rain from past 2 weeks

# Add column with rain lagged k=1
X_train_lag <- X_train
X_test_lag <- X_test
for(colname in colnames(X_train_lag)) {
  if(!startsWith(colname, "precip")) next()
  X_train_lag[,paste0(colname,'_lag')] <- c(0, head(X_train_lag[,colname], -1))
  X_test_lag[,paste0(colname,'_lag')] <- c(0, head(X_test_lag[,colname], -1))
}

X_train_lag <- X_train_lag[2:nrow(X_train_lag),]
Y_train_lag <- Y_train[2:nrow(Y_train),]
Y_test_lag <- Y_test[2:nrow(Y_test),]
# Train again linear regression
lr_lag <- lm(cbind(B1, B2, B3, B4) ~ ., data = cbind(X_train_lag, Y_train_lag))
predictions_lag <- predict(lr, newdata = X_test_lag)

start <- 1
end <- 48

# B1
plot(1:48, Y_test_lag$B1[start:end], type="l")
lines(1:48, predictions_lag[start:end,1], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B2
plot(1:48, Y_test$B2[start:end], type="l")
lines(1:48, predictions[start:end,2], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B3
plot(1:48, Y_test$B3[start:end], type="l")
lines(1:48, predictions[start:end,3], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))

# B4
plot(1:48, Y_test$B4[start:end], type="l")
lines(1:48, predictions[start:end,4], col="red", type="l")
legend("topleft", legend=c("Real", "Predicted"), col = c("black", "red"), lty=c(1,1))


#### Comparison
summary(lr)
summary(lr_agg)
summary(lr_lag)