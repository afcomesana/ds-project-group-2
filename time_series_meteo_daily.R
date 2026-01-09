# Author: Alberto Fernández Comesaña
library(forecast)

# ---- Target data (discharges) ----
# None of the interfaces is on the east-west direction so there aren't
# any flows which sign needs to be inverted
B1 <- read.csv("discharges/discharge_B1.csv")$flow
B2 <- read.csv("discharges/discharge_B2.csv")$flow
B3 <- read.csv("discharges/discharge_B3.csv")$flow
B4 <- read.csv("discharges/discharge_B4.csv")$flow

# -- Train-Test split --
B1_train <- B1[1:1100]
B2_train <- B2[1:1100]
B3_train <- B3[1:1100]
B4_train <- B4[1:1100]

B1_test <- B1[1101:1296]
B2_test <- B2[1101:1296]
B3_test <- B3[1101:1296]
B4_test <- B4[1101:1296]

# ---- Input data (meteo) ----
if (!file.exists('meteo-csv/B1_1_daily.csv')) {
  print("Computing daily average data for B basin")
  
  # Prepare metereological data for interfaces:
  coords <- read.csv('discharges/coords.csv')
  coords <- coords[coords$Case == 'B',]
  
  loc_counts <- table(coords$Location)
  
  for(loc in names(loc_counts)) {
    for(coord in 1:loc_counts[[loc]]) {
      xt <- coords[coords$Location == loc,]$xt[coord]
      yt <- coords[coords$Location == loc,]$yt[coord]
      
      print(paste0('Getting meteo data for file: B', loc, '_',coord,'.csv'))
      
      meteo_average('meteo/', xt, yt, 'meteo-csv/', paste0('B', loc, '_',coord,'_daily'), daily = TRUE)
    }
  }
}


meteo_b11 <- read.csv('meteo-csv/B1_1_daily.csv')
meteo_b12 <- read.csv('meteo-csv/B1_2_daily.csv')
meteo_b21 <- read.csv('meteo-csv/B2_1_daily.csv')
meteo_b31 <- read.csv('meteo-csv/B3_1_daily.csv')
meteo_b32 <- read.csv('meteo-csv/B3_2_daily.csv')
meteo_b41 <- read.csv('meteo-csv/B4_1_daily.csv')
meteo_b42 <- read.csv('meteo-csv/B4_2_daily.csv')

# ---- Expand weekly flow time series to daily ----
weekly_dates <- read.csv("discharges/discharge_B1.csv")$date
daily_dates <- meteo_b11$date
test_idx <- max(which(daily_dates %in% weekly_dates[1:1100]))

meteo_b11_train <- meteo_b11[1:test_idx,]
meteo_b12_train <- meteo_b12[1:test_idx,]
meteo_b21_train <- meteo_b21[1:test_idx,]
meteo_b31_train <- meteo_b31[1:test_idx,]
meteo_b32_train <- meteo_b32[1:test_idx,]
meteo_b41_train <- meteo_b41[1:test_idx,]
meteo_b42_train <- meteo_b42[1:test_idx,]

b1_flows <- as.numeric(meteo_b11_train$date %in% weekly_dates)
b2_flows <- b1_flows
b3_flows <- b1_flows
b4_flows <- b1_flows

flow_idx <- 1
cur_flow_b1 <- B1_train[flow_idx]
cur_flow_b2 <- B2_train[flow_idx]
cur_flow_b3 <- B3_train[flow_idx]
cur_flow_b4 <- B4_train[flow_idx]
for(idx in 1:length(b1_flows)) {
  if( b1_flows[idx] == 1) {
    b1_flows[idx] <- cur_flow_b1
    b2_flows[idx] <- cur_flow_b2
    b3_flows[idx] <- cur_flow_b3
    b4_flows[idx] <- cur_flow_b4

    flow_idx <- flow_idx + 1
    
    cur_flow_b1 <- B1_train[flow_idx] 
    cur_flow_b2 <- B2_train[flow_idx] 
    cur_flow_b3 <- B3_train[flow_idx] 
    cur_flow_b4 <- B4_train[flow_idx] 
    
  } else {
    
    b1_flows[idx] <- cur_flow_b1
    b2_flows[idx] <- cur_flow_b2
    b3_flows[idx] <- cur_flow_b3
    b4_flows[idx] <- cur_flow_b4
    
  }
}

# De-season and pre-whiten:
de_b1 <- stl(ts(b1_flows, frequency=365), s.window="periodic")$time.series[,3]
de_b2 <- stl(ts(b2_flows, frequency=365), s.window="periodic")$time.series[,3]
de_b3 <- stl(ts(b3_flows, frequency=365), s.window="periodic")$time.series[,3]
de_b4 <- stl(ts(b4_flows, frequency=365), s.window="periodic")$time.series[,3]

de_meteo_b11 <- meteo_b11_train
de_meteo_b12 <- meteo_b12_train
de_meteo_b21 <- meteo_b21_train
de_meteo_b31 <- meteo_b31_train
de_meteo_b32 <- meteo_b32_train
de_meteo_b41 <- meteo_b41_train
de_meteo_b42 <- meteo_b42_train

dfs <- list(
  'b1' = list(
    'flow' = de_b1,
    'interfaces' = list(de_meteo_b11,de_meteo_b12)
    ),
  'b2' = list(
    'flow' = de_b2,
    'interfaces' = list(de_meteo_b21)
    ),
  'b3' = list(
    'flow' = de_b3,
    'interfaces' = list(de_meteo_b31,de_meteo_b32)
    ),
  'b4' = list(
    'flow' = de_b4,
    'interfaces' = list(de_meteo_b41,de_meteo_b42)
    )
)

dfs[['b1']]
# Iterate over b1, b2...
for(basin in names(dfs)) {
  
  flow <- dfs[[basin]][['flow']]
  print(paste("Basin", basin))
  # For every interface corresponding to the basin:
  for(df in dfs[[basin]][['interfaces']]) {
    
    # For every meteorological column in the interface dataframe:
    for(colname in colnames(df)) {
      
      if(colname == 'date') next()
      
      # Detrend column:
      de_col <- stl(ts(df[,colname], frequency=365), s.window="periodic")$time.series[,3]
      
      # Model column and get residuals:
      de_model <- auto.arima(de_col)
      df[,colname] <- residuals(de_model)
      
      # Apply model to corresponding basin
      df[paste0(basin,'_',colname)] <- residuals(Arima(flow, model=de_model))
    }
  }
}

for(df in dfs) {
  for(colname in colnames(df)) {
    
    if(colname == 'date') next()
    
    # Detrend column:
    de_col <- <- stl(ts(de_meteo_b11[,colname], frequency=365), s.window="periodic")$time.series[,3]
    
    # Model column and get residuals:
    de_model <- auto.arima(de_col)
    de_meteo_b11[,colname] <- residuals(de_model)
    
    # Apply model to corresponding basin
    de_meteo_b11[paste0('b1_', colname)] <- residuals(Arima(de_b1, model=model_b11))
    
  }
}
for(colname in colnames(de_meteo_b11)) {
  if (colname == 'date') next()
  print("meteo b11")
  de_meteo_b11[,colname] <- stl(ts(de_meteo_b11[,colname], frequency=365), s.window="periodic")$time.series[,3]
  model_b11 <- auto.arima(de_meteo_b11[,colname])
  de_meteo_b11[,colname] <- residuals(model_b11)
  de_meteo_b11[paste0('b1_', colname)] <- residuals(Arima(de_b1, model=model_b11))
  print("meteo other meteos")
  de_meteo_b12[,colname] <- stl(ts(de_meteo_b12[,colname], frequency=365), s.window="periodic")$time.series[,3]
  de_meteo_b21[,colname] <- stl(ts(de_meteo_b21[,colname], frequency=365), s.window="periodic")$time.series[,3]
  de_meteo_b31[,colname] <- stl(ts(de_meteo_b31[,colname], frequency=365), s.window="periodic")$time.series[,3]
  de_meteo_b32[,colname] <- stl(ts(de_meteo_b32[,colname], frequency=365), s.window="periodic")$time.series[,3]
  de_meteo_b41[,colname] <- stl(ts(de_meteo_b41[,colname], frequency=365), s.window="periodic")$time.series[,3]
  de_meteo_b42[,colname] <- stl(ts(de_meteo_b42[,colname], frequency=365), s.window="periodic")$time.series[,3]
}

# --- B1

# Zone 1
ccf(de_meteo_b11$wind_speed, de_b1)
ccf(de_meteo_b11$wind_dir, de_b1)
ccf(de_meteo_b11$precip, de_b1)
ccf(de_meteo_b11$temp, de_b1)
ccf(de_meteo_b11$dew, de_b1)

# Zone 2
ccf(de_meteo_b12$wind_speed, de_b1)
ccf(de_meteo_b12$wind_dir, de_b1)
ccf(de_meteo_b12$precip, de_b1)
ccf(de_meteo_b12$temp, de_b1)
ccf(de_meteo_b12$dew, de_b1)

# --- B2
ccf(de_meteo_b21$wind_speed, de_b2)
ccf(de_meteo_b21$wind_dir, de_b2)
ccf(de_meteo_b21$precip, de_b2)
ccf(de_meteo_b21$temp, de_b2)
ccf(de_meteo_b21$dew, de_b2)


# --- B3

# Zone 1
ccf(de_meteo_b31$wind_speed, de_b3)
ccf(de_meteo_b31$wind_dir, de_b3)
ccf(de_meteo_b31$precip, de_b3)
ccf(de_meteo_b31$temp, de_b3)
ccf(de_meteo_b31$dew, de_b3)

# Zone 2
ccf(de_meteo_b32$wind_speed, de_b3)
ccf(de_meteo_b32$wind_dir, de_b3)
ccf(de_meteo_b32$precip, de_b3)
ccf(de_meteo_b32$temp, de_b3)
ccf(de_meteo_b32$dew, de_b3)

# --- B4

# Zone 1
ccf(de_meteo_b41$wind_speed, de_b4)
ccf(de_meteo_b41$wind_dir, de_b4)
ccf(de_meteo_b41$precip, de_b4)
ccf(de_meteo_b41$temp, de_b4)
ccf(de_meteo_b41$dew, de_b4)

# Zone 2
ccf(de_meteo_b42$wind_speed, de_b4)
ccf(de_meteo_b42$wind_dir, de_b4)
ccf(de_meteo_b42$precip, de_b4)
ccf(de_meteo_b42$temp, de_b4)
ccf(de_meteo_b42$dew, de_b4)
