# Author: Alberto Fernández Comesaña

# ---- Libraries and sources ----
library(keras3)
source("R/meteo_average.R")

# ---- Data preprocessing ----
# Prepare metereological data for interfaces:
coords <- read.csv('discharges/coords.csv')
coords <- coords[coords$Case == 'B',]

loc_counts <- table(coords$Location)

for(loc in names(loc_counts)) {
  for(coord in 1:loc_counts[[loc]]) {
    xt <- coords[coords$Location == loc,]$xt[coord]
    yt <- coords[coords$Location == loc,]$yt[coord]
    
    print(paste0('Getting meteo data for file: B', loc, '_',coord,'.csv'))
    
    meteo_average('meteo/', xt, yt, 'meteo-csv/', paste0('B', loc, '_',coord))
  }
}


# Read flows for every interface and convert them to "percentages"
B1 <- read.csv('discharges/discharge_B1.csv')$flow
B2 <- read.csv('discharges/discharge_B2.csv')$flow
B3 <- read.csv('discharges/discharge_B3.csv')$flow
B4 <- read.csv('discharges/discharge_B4.csv')$flow

Y <- data.frame(B1, B2, B3, B4)
Y[Y < 0] = 0
Y <- Y/rowSums(Y)


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

train <- cbind(X_train, Y_train)

# ---- 1 layer perceptron with softmax activation function ----
model <- keras_model_sequential() %>%
  layer_dense(
    units = 4,
    activation = "softmax",
    input_shape = ncol(X_train)
  )

model %>% compile(
  optimizer='adam',
  loss='kullback_leibler_divergence',
  metrics=c('accuracy')
)

history <- model %>% fit(
  X_train,
  Y_train,
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2
)

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
