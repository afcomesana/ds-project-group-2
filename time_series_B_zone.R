# Author: Alberto Fernández Comesaña
library(stats)
library(forecast)

# None of the interfaces is on the east-west direction so there aren't
# any flows which sign needs to be inverted
B1 <- read.csv("discharges/discharge_B1.csv")$flow
B2 <- read.csv("discharges/discharge_B2.csv")$flow
B3 <- read.csv("discharges/discharge_B3.csv")$flow
B4 <- read.csv("discharges/discharge_B4.csv")$flow

plot(seq(2016,2025-1/48,(1/48)), B1, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from B1 (2016-2024)" )
plot(seq(2016,2025-1/48,(1/48)), B2, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from B2 (2016-2024)" )
plot(seq(2016,2025-1/48,(1/48)), B3, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from B3 (2016-2024)" )
plot(seq(2016,2025-1/48,(1/48)), B4, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Weekly average of inflow from B4 (2016-2024)" )


# Reserve last year for test data
B1_train <- B1[1:384]
B1_test <- B1[385:432]

B2_train <- B2[1:384]
B2_test <- B2[385:432]

B3_train <- B3[1:384]
B3_test <- B3[385:432]

B4_train <- B4[1:384]
B4_test <- B4[385:432]

# River files:
river_files <- file.path("Rivers_csv", list.files(path = "Rivers_csv/"))

# Sum together rivers coming from the north and from the west

# Function for summing up inflows from the rivers whose names are provided
sum_rivers_inflows <- function(files, river_names, output_length) {
  inflows <- numeric(output_length)
  for(filepath in files) {
    if (any(grepl(paste(river_names, collapse = '|'), filepath))) {
      inflows <- inflows + read.csv(filepath)$flow
    }
  }
  
  return(inflows)
}

# West rivers
west_river_names <- c("kopingsan", "hedstrommen", "arbogaan", "kolbacksan", "svartan", "eskilstunaan", "sagan", "enkopingsan", "rackstaan", "brobacken")
west_inflows = sum_rivers_inflows(river_files, west_river_names, length(B1))
west_train = west_inflows[1:384]
west_test = west_inflows[385:432]

# North rivers
north_river_names <- c("fyrisan", "orsundaan", "savaan", "lovstaan", "marstaan", "oxundaan")
north_inflows = sum_rivers_inflows(river_files, north_river_names, length(B1))
north_train = north_inflows[1:384]
north_test = north_inflows[385:432]

all_river_names <- c(west_river_names, north_river_names)
### ballstaan was left out for being south-east to the basins ###


plot(seq (2016,2025-1/48,(1/48)), west_inflows, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Summed weekly average of inflow from west rivers (2016-2024)")
plot(seq (2016,2025-1/48,(1/48)), north_inflows, type="l" , lwd =1.5, xlab = "Time" , ylab = "Flow m3/s",  main = "Summed weekly average of inflow from north rivers (2016-2024)")

# De-season river inflows and basins flows
de_west_rivers <- stl(ts(west_train, frequency=48), s.window="periodic")$time.series[,3]
de_north_rivers <- stl(ts(north_train, frequency=48), s.window="periodic")$time.series[,3]
de_B1_train <- stl(ts(B1_train, frequency=48), s.window="periodic")$time.series[,3]
de_B2_train <- stl(ts(B2_train, frequency=48), s.window="periodic")$time.series[,3]
de_B3_train <- stl(ts(B3_train, frequency=48), s.window="periodic")$time.series[,3]
de_B4_train <- stl(ts(B4_train, frequency=48), s.window="periodic")$time.series[,3]

# Pre-whiten with north and west rivers to see CCFs for each of them
west_model <- auto.arima(de_west_rivers)
north_model <- auto.arima(de_north_rivers)

west_filt <- residuals(west_model)
north_filt <- residuals(north_model)

B1_west_filt <- residuals(Arima(de_B1_train, model=west_model))
B1_north_filt <- residuals(Arima(de_B1_train, model=north_model))

B2_west_filt <- residuals(Arima(de_B2_train, model=west_model))
B2_north_filt <- residuals(Arima(de_B2_train, model=north_model))

ccf(west_filt, B2_west_filt, main="Prewhitened CCF: West inflow vs B1 flow")
ccf(north_filt, B2_north_filt, main="Prewhitened CCF: North inflow vs B1 flow")

### Get significant correlation lags for every river:
river_lags <- list()
for(r in all_river_names) {
  # Read river flow from CSV
  print(paste0(r,":"))
  filepath <- river_files[grep(paste0("_",r,".csv"), river_files)]
  flow <- read.csv(filepath)$flow[1:384]
  
  # De-trend the flow and basin inflow
  de_flow <- stl(ts(flow, frequency=48), s.window="periodic")$time.series[,3]
  
  # Pre-whiten
  de_model <- auto.arima(de_flow)
  de_filtered <- residuals(de_model)
  
  B2_filtered <- residuals(Arima(de_B2_train, model=de_model))
  corr <- ccf(de_filtered, B2_filtered, plot=FALSE)
  
  river_lags[[r]] <- -as.vector(corr$lag[corr$lag <= 0][(abs(as.vector(corr$acf))[corr$lag <= 0] >= 2/sqrt(384))])*frequency(de_flow)
}

# Filter rivers with not a single lag with significant correlation
river_lags <- river_lags[lengths(river_lags) > 0]

# (should we also remove those with lags lower than for example -8, like 2 months ago??)
#for(r in names(river_lags)) {
#  river_lags[[r]] <- river_lags[[r]][river_lags[[r]] <= 8]
#}

make_lags <- function(x, lags) {
  lagmat <- sapply(lags, function(k) dplyr::lag(x, k))
  colnames(lagmat) <- paste0("lag", lags)
  return(lagmat)
}

# Create dataset with all the river lags
X <- list()
for(r in seq_along(river_lags)) {
  river_name <- names(river_lags)[r]
  lags <- river_lags[[river_name]]
  filepath <- river_files[grep(paste0("_",river_name,".csv"), river_files)]
  flow <- read.csv(filepath)$flow[1:384]
  flow <- as.data.frame(sapply(lags, function(k) dplyr::lag(flow,as.integer(k))))
  colnames(flow) <- paste0(river_name,"_lag_",lags)
  X[[r]] <- flow
}

# Combine all the dataframes and remove nans:
X <- do.call(cbind, X)
X <- na.omit(X)

lag_offset <- length(B1_train) - nrow(X)

y <- B1_train[(lag_offset+1):length(B1_train)]

fit_sarimax <- auto.arima(y, xreg=as.matrix(X), seasonal=TRUE)
summary(fit_sarimax)

y_test <- B1[(385-lag_offset):length(B1)]
X_test <- list()
for(r in seq_along(river_lags)) {
  river_name <- names(river_lags)[r]
  lags <- river_lags[[river_name]]
  filepath <- river_files[grep(paste0("_",river_name,".csv"), river_files)]
  flow <- read.csv(filepath)$flow[(385-lag_offset):432]
  flow <- as.data.frame(sapply(lags, function(k) dplyr::lag(flow,as.integer(k))))
  colnames(flow) <- paste0(river_name,"_lag_",lags)
  X_test[[r]] <- flow
}

X_test <- do.call(cbind, X_test)
X_test <- na.omit(X_test)

fc_2024 <- forecast(fit_sarimax, xreg = as.matrix(X_test), h = 48)
plot(seq (2024,2025-1/48,(1/48)), B1_test, type="l", lwd =1.5, xlim=c(2024,2025), xlab = "Time" , ylab = "Flow m3/s",  main = "ARIMA Regression model: Actual vs. predicted flow of B1")
lines(seq (2024,2025-1/48,(1/48)), fc_2024$mean,type="l", lwd =1.5, col="red")
lines(seq (2024,2025-1/48,(1/48)), fc_2024$upper[,2],lty="dashed", lwd =1.5, col="blue")
lines(seq (2024,2025-1/48,(1/48)), fc_2024$lower[,2],lty="dashed", lwd =1.5, col="blue")
legend(x = "topright",          # Position
       legend = c("Actual data", "Predictions", "95% Confidence interval"),  # Legend texts
       lty = c(1, 1,1),           # Line types
       col = c("black","red", "blue"),           # Line colors
       lwd = 2)

