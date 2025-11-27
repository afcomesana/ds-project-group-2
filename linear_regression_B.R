source("R/meteo_average.R")

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

X <- read.csv('meteo-csv/B1_1.csv')
X <- cbind(X, read.csv('meteo-csv/B1_2.csv'))
X <- cbind(X, read.csv('meteo-csv/B2_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_2.csv'))
X <- cbind(X, read.csv('meteo-csv/B4_1.csv'))
X <- cbind(X, read.csv('meteo-csv/B3_2.csv'))

X <- X[,!grepl("^date", names(X))]

# Get rivers data:

# River files:
river_files <- file.path("rivers-csv", list.files(path = "rivers-csv"))
west_river_names <- c("kopingsan", "hedstrommen", "arbogaan", "kolbacksan", "svartan", "eskilstunaan", "sagan", "enkopingsan", "rackstaan", "brobacken")
north_river_names <- c("fyrisan", "orsundaan", "savaan", "lovstaan", "marstaan", "oxundaan")
river_names <- c(west_river_names, north_river_names)

for (river_name in river_names) {
  filename <- river_files[grepl(paste0('_',river_name,'.csv'), river_files)]
  X[[river_name]] <- read.csv(filename)$flow
}

X_train <- X[1:1000,]
X_test <- X[1001:nrow(X),]

Y_train <- Y[1:1000,]
Y_test <- Y[1001:nrow(Y),]

train <- cbind(X_train, Y_train)
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


# North rivers
north_inflows = sum_rivers_inflows(river_files, north_river_names, length(B1))
