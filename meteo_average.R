# Import dependencies
library("ncdf4")
library("pygetmtools")
library(sf)

#' Preprocess metereological data from PyGETM output file (3D) into weekly averaged time series.
#'
#' @details
#'   Read a PyGETM 3D output file for meterological data into R, calculate weekly averages, and store as csv time series.
#'   Options: For now the timespan is hard coded to fit our discharge file, could be changed.
#'
#' @param dir_path  character; folder which contains all meterological nc files
#' @param latitude numeric; (approx) coordinate of interface
#' @param longitude numeric; (approx) coordinate of interface
#' @param output_path character; folder 
#' @param output_name character; file name
#' @examples
#'  \dontrun{
#'  meteo_average( dir_path = "./Meteo/",  
#'  latitude = 59.47209, 
#'  longitude = 17.79508, 
#'  output_path = "./Meteo_csv/", 
#'  output_name = "A1_meteo")
#'  }
#' @import ncdf4
#' @export

meteo_average = function(dir_path, x_coord, y_coord, output_path, output_name, daily = FALSE){
  
  # Input check
  if((!is.character(dir_path))
     |!is.numeric(x_coord)
     |!is.numeric(y_coord)){
    stop("Some argument types are wrong!")
  }
  
  # Get file names
  file_names <- list.files(path = dir_path)
  if(length(file_names) == 0){
    stop("Empty or non-existing file_path")
  }
  
  # Convert to latitude and longitude:
  conv <- sweref_to_latlon(x_coord,y_coord)
  latitude <- conv[1]
  longitude <- conv[2]
  
  wind_speeds = c()
  wind_dirs = c()
  precips = c()
  temps = c()
  dates = c()
  
  df_wind_temp <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_wind_temp) <- c('date', 'wind_speed', 'wind_dir', 'temp', 'dew')
  
  df_prec <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df_prec) <- c('date', 'precip')
  
  
  # Loop through all files in folder and merge into dataframes for wind and precip
  for(file_name in file_names){
    
    # Open nc (and always close on exit)
    nc = nc_open(paste(dir_path, file_name, sep = ""))
    on.exit({
      nc_close(nc)
    })
    
    # Get latitude and longitude indexes
    lat_index = which.min(abs(nc$dim$latitude$vals - latitude))
    lon_index = which.min(abs(nc$dim$longitude$vals - longitude))
    
    time_vals = nc$dim$time$vals
    time_att = nc$dim$time
    
    time_dates <- convert_nc_time(time_vals, time_att)
    
    if( 'u10' %in% names(nc$var) ) {
      
      # Wind variables
      u10 = ncvar_get(nc, varid='u10')[lon_index, lat_index,]
      v10 = ncvar_get(nc, varid='v10')[lon_index, lat_index,]
      
      wind_speed = vect_to_speed(u10, v10)
      wind_dir = vect_to_dir(u10, v10)
      
      # Temperature variables
      t2m = ncvar_get(nc, varid='t2m')[lon_index, lat_index,]
      d2m = ncvar_get(nc, varid='d2m')[lon_index, lat_index,]
      
      # Append data
      df_tmp <- data.frame(time_dates, wind_speed, wind_dir, t2m, d2m)
      colnames(df_tmp) <- c('date', 'wind_speed', 'wind_dir', 'temp', 'dew')
      df_wind_temp <- rbind(df_wind_temp, df_tmp)
      
    } else if ( 'tp' %in% names(nc$var) ) {
      
      df_tmp <- data.frame(time_dates, ncvar_get(nc, varid='tp')[lon_index, lat_index,])
      colnames(df_tmp) <- c('date', 'precip')
      df_prec <- rbind(df_prec, df_tmp)
    } else {
      stop("File does not contain any variable of interest.")
    }
    
  }
  
  # Merge into one dataframe
  df <- cbind(df_wind_temp, df_prec[2])
  
  # Extract day/month/year for easier loop
  df$Day <- format(as.Date(df$date, format="%Y-m%-d%"),"%d")
  df$Month <- format(as.Date(df$date, format="%Y-m%-d%"),"%m")
  df$Year <- format(as.Date(df$date, format="%Y-m%-d%"),"%Y")
  
  # ---------------------
  # --- Daily average ---
  # ---------------------
  daily_average <- data.frame(
    Year = character(),
    Month = character(),
    Day = character(),
    wind_speed = numeric(),
    wind_dir = numeric(),
    temp = numeric(),
    dew = numeric(),
    precip = numeric()
  )
  
  sum_wind_speed <- 0
  sum_wind_dir <- 0
  sum_temp <- 0
  sum_dew <- 0
  sum_precip <- 0
  
  i <- 1
  count <-0
  
  day = as.numeric(df$Day[i])
  
  while(i < dim(df)[1]) {
    sum_wind_speed <- sum_wind_speed + df$wind_speed[i]
    sum_wind_dir <- sum_wind_dir + df$wind_dir[i]
    sum_temp <- sum_temp + df$temp[i]
    sum_dew <- sum_dew + df$dew[i]
    sum_precip <-  sum_precip + df$precip[i]
    
    i <- i + 1
    count <- count + 1
    
    current_day = as.numeric(df$Day[i])
    
    if (current_day != day) {
      daily_average[nrow(daily_average) + 1,] <- list(
                                                    df$Year[i-1],
                                                    df$Month[i-1],
                                                    df$Day[i-1],
                                                    sum_wind_speed/count,
                                                    sum_wind_dir/count,
                                                    sum_temp/count,
                                                    sum_dew/count,
                                                    sum_precip/count
                                                  )
      
      sum_wind_speed <- 0
      sum_wind_dir <- 0
      sum_temp <- 0
      sum_dew <- 0
      sum_precip <- 0
      
      count <- 0
      day = current_day
    }
  }
  
  if (daily) {
    daily_average['date'] <- apply(daily_average[, c("Year", "Month", "Day")], 1, function(item) paste0(item[1],"-", item[2], '-', item[3]))
    daily_average <- daily_average[, !names(daily_average) %in% c("Year", "Month", "Day")]
    daily_average <- daily_average[, c('date', names(daily_average)[names(daily_average) != 'date'])]
    
    write.csv(daily_average, file = paste(output_path, output_name, ".csv", sep = ""), row.names = FALSE)
    return()
  }
  
  # ----------------------
  # --- Weekly average ---
  # ----------------------
  
  
  weekly_average <- data.frame(
    date = as.POSIXct(character()),
    wind_speed = numeric(),
    wind_dir = numeric(),
    temp = numeric(),
    dew = numeric(),
    precip = numeric()
  )
  
  
  sum_wind_speed <- 0
  sum_wind_dir <- 0
  sum_temp <- 0
  sum_dew <- 0
  sum_precip <- 0
  
  # Start
  i <- 1
  
  # End at 2024-10-31 to match discharge
  end <- dim(daily_average)[1]
  end_row <- which(daily_average$Year == "2024" & daily_average$Month == "10" & daily_average$Day == "31" & seq_len(nrow(daily_average)) > i)
  # Check that end date exists
  if (length(end_row) > 0) {
    end <- end_row[1]
  }
  else{
    print(paste("File ends before 2024-10-31: ", file_name))
  }
  
  
  # Day within week counter (First entry slightly different, only 5 days for discharge before 2000-01-06)
  j <- 3
  
  while(i < end){
    sum_wind_speed <- sum_wind_speed + daily_average$wind_speed[i]
    sum_wind_dir <- sum_wind_dir + daily_average$wind_dir[i]
    sum_temp <- sum_temp + daily_average$temp[i]
    sum_dew <- sum_dew + daily_average$dew[i]
    sum_precip <- sum_precip + daily_average$precip[i]
    
    #If we're at end of a week, append average to weekly average
    if ( j == 7){
      date <- paste(daily_average$Year[i+1], daily_average$Month[i+1], daily_average$Day[i+1], sep = "-")
      weekly_average[nrow(weekly_average) + 1,] <- c(date, sum_wind_speed/7, sum_wind_dir/7, sum_temp/7, sum_dew/7, sum_precip/7)
      # Reset sum and week counter
      sum_wind_speed <- 0
      sum_wind_dir <- 0
      sum_temp <- 0
      sum_dew <- 0
      sum_precip <- 0
      j <- 1
    }
    # Else, continue summing next loop
    else{
      j <- j + 1
    }
    i <- i+1
  }
  
  # Convert wind speed and direction to numeric and write to output
  weekly_average$wind_speed <- as.numeric(weekly_average$wind_speed)
  weekly_average$wind_dir <- as.numeric(weekly_average$wind_dir)
  weekly_average$temp <- as.numeric(weekly_average$temp)
  weekly_average$dew <- as.numeric(weekly_average$dew)
  weekly_average$precip <- as.numeric(weekly_average$precip)
  
  write.csv(weekly_average, file = paste(output_path, output_name, ".csv", sep = ""), row.names = FALSE)
}

# Positive u_wind is west-to-east
# Positive v_wind is south-to-north
# 0 deg = northern wind, 90 deg = eastern wind, 180 deg = southern wind, 270 deg = western wind
vect_to_speed = function(u_wind, v_wind){
  sqrt(u_wind^2 + v_wind^2)
}

vect_to_dir = function(u_wind, v_wind){
  windspeed = sqrt(u_wind^2 + v_wind^2)
  wind_dir_rad = atan2(v_wind / windspeed, u_wind / windspeed) 
  wind_dir_degrees = wind_dir_rad * 180 / pi
  return(270-wind_dir_degrees)
}

wind_to_vectors = function(speed, direction){
  # Wind speed in m/s, direction in degrees
  direction = 270 - direction # Converting the wind direction to the "math" direction
  rads = direction / 180 * pi
  xcomp = speed * cos(rads)
  ycomp = speed * sin(rads)
  wsvectors = list(u_wind = xcomp, v_wind = ycomp)
  return(wsvectors)
}

# Get lat/long
sweref_to_latlon = function(x, y){
  loc_sweref = st_sfc(st_point(x = c(x, y)), crs = st_crs(3006))
  loc = st_transform(loc_sweref, crs = st_crs(4326))
  
  c(loc[[1]][2], loc[[1]][1])
}



