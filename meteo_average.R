# Import dependencies
library("ncdf4")
library("pygetmtools")

#' Preprocess metereological data from PyGETM output file (3D) into weekly averaged time series.
#'
#' @details
#'   Read a PyGETM 3D output file for rivers into R, calculate weekly averages, and store as csv time series.
#'   Options: For now the timespan is hard coded to fit our discharge file, could be changed.
#'
#' @param dir_path  character; folder which contains all river nc files
#' @param output_path character; folder 
#' @examples
#'  \dontrun{
#'  meteo_average(dir_path = "./Rivers/",
#'                59.25,17.75)
#'  }
#' @import ncdf4
#' @export

meteo_average = function(dir_path, latitude, longitude){
  
  # Input check
  if((!is.character(dir_path))
     |!is.numeric(latitude)
     |!is.numeric(longitude)){
    stop("Some argument types are wrong!")
  }
  
  # Get file names
  file_names <- list.files(path = dir_path)
  if(length(file_names) == 0){
    stop("Empty or non-existing file_path")
  }
  
  wind_speeds = c()
  wind_dirs = c()
  precips = c()
  dates = c()
  
  # Loop through all files in folder
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
      u10 = ncvar_get(nc, varid='u10')[lon_index, lat_index,]
      v10 = ncvar_get(nc, varid='v10')[lon_index, lat_index,]
      
      wind_speed = vect_to_speed(u10, v10)
      wind_dir = vect_to_dir(u10, v10)
      
      df <- data.frame(time_dates, wind_speed, wind_dir)
      colnames(df) <- c('date', 'wind_speed', 'wind_dir')
    } else if ( 'tp' %in% names(nc$var) ) {
      
      df <- data.frame(time_dates, ncvar_get(nc, varid='tp')[lon_index, lat_index,])
      colnames(df) <- c('date', 'precip')
    } else {
      stop("File does not contain any variable of interest.")
    }
    
    # Extract day/month/year for easier loop
    df$Day <- format(as.Date(df$date, format="%Y-m%-d%"),"%d")
    df$Month <- format(as.Date(df$date, format="%Y-m%-d%"),"%m")
    df$Year <- format(as.Date(df$date, format="%Y-m%-d%"),"%Y")
    
    if('u10' %in% names(nc$var)) {
      # ---------------------
      # --- Daily average ---
      # ---------------------
      daily_average <- data.frame(
        Year = character(),
        Month = character(),
        Day = character(),
        wind_speed = numeric(),
        wind_dir = numeric()
      )
      
      sum_wind_speed <- 0
      sum_wind_dir <- 0
      
      i <- 1
      count <-0
      
      day = as.numeric(df$Day[i])
      
      while(i < dim(df)[1]) {
        sum_wind_speed <- sum_wind_speed + df$wind_speed[i]
        sum_wind_dir <- sum_wind_dir + df$wind_dir[i]
        
        i <- i + 1
        count <- count + 1
        
        current_day = as.numeric(df$Day[i])
        
        if (current_day != day) {
          daily_average[nrow(daily_average) + 1,] <- list(df$Year[i-1], df$Month[i-1], df$Day[i-1], sum_wind_speed/count, sum_wind_dir/count)
          sum_wind_speed <- 0
          sum_wind_dir <- 0
          
          count <- 0
          day = current_day
        }
      }
      
      # ----------------------
      # --- Weekly average ---
      # ----------------------
      
      
      weekly_average <- data.frame(
        date = as.POSIXct(character()),
        wind_speed = numeric(),
        wind_dir = numeric()
      )
      
      
      sum_wind_speed <- 0
      sum_wind_dir <- 0
      
      i <- 1
      
      while(i < dim(daily_average)[1]){
        sum_wind_speed <- sum_wind_speed + daily_average$wind_speed[i]
        sum_wind_dir <- sum_wind_dir + daily_average$wind_dir[i]
        
        #If we're at end of a "week", append average to weekly average
        if ( (as.numeric(daily_average$Day[i]) %% 7) == 0){
          date <- paste(daily_average$Year[i+1], daily_average$Month[i+1], daily_average$Day[i+1], sep = "-")
          weekly_average[nrow(weekly_average) + 1,] <- c(date, sum_wind_speed/7, sum_wind_dir/7)
          sum_wind_speed <- 0
          sum_wind_dir <- 0
        }
        
        # If we're at day 28, jump to next month
        if(daily_average$Day[i] == "28"){
          next_rows <- which(daily_average$Day == "01" & seq_len(nrow(daily_average)) > i)
          # Check that we are not at end of file
          if (length(next_rows) > 0) {
            i = next_rows[1]
          } else{
            break
          }
        } else{
          i <- i+1
        }
      }
      
      # Convert wind speed and direction to numeric and write to output
      weekly_average$wind_speed <- as.numeric(weekly_average$wind_speed)
      weekly_average$wind_dir <- as.numeric(weekly_average$wind_dir)
      
      wind_speeds = c(wind_speeds, weekly_average$wind_speed)
      wind_dirs = c(wind_dirs, weekly_average$wind_dir)
      dates = union(dates, weekly_average$date)
      
      
    } else if('tp' %in% names(nc$var)) {
      # ---------------------
      # --- Daily average ---
      # ---------------------
      daily_average <- data.frame(
        Year = character(),
        Month = character(),
        Day = character(),
        precip = numeric()
      )
      
      sum_precip <- 0
      
      i <- 1
      count <-0
      
      day = as.numeric(df$Day[i])
      
      while(i < dim(df)[1]) {
        sum_precip <- sum_precip + df$precip[i]
        
        i <- i + 1
        count <- count + 1
        
        current_day = as.numeric(df$Day[i])
        
        if (current_day != day) {
          daily_average[nrow(daily_average) + 1,] <- list(df$Year[i-1], df$Month[i-1], df$Day[i-1], sum_precip/count)
          sum_precip <- 0
          
          count <- 0
          day = current_day
        }
      }
      
      # ----------------------
      # --- Weekly average ---
      # ----------------------
      
      
      weekly_average <- data.frame(
        date = as.POSIXct(character()),
        precip = numeric()
      )
      
      
      sum_precip <- 0
      
      i <- 1
      
      while(i < dim(daily_average)[1]){
        sum_precip <- sum_precip + daily_average$precip[i]
        
        #If we're at end of a "week", append average to weekly average
        if ( (as.numeric(daily_average$Day[i]) %% 7) == 0){
          date <- paste(daily_average$Year[i+1], daily_average$Month[i+1], daily_average$Day[i+1], sep = "-")
          weekly_average[nrow(weekly_average) + 1,] <- c(date, sum_precip/7)
          sum_precip <- 0
        }
        
        # If we're at day 28, jump to next month
        if(daily_average$Day[i] == "28"){
          next_rows <- which(daily_average$Day == "01" & seq_len(nrow(daily_average)) > i)
          # Check that we are not at end of file
          if (length(next_rows) > 0) {
            i = next_rows[1]
          } else{
            break
          }
        } else{
          i <- i+1
        }
      }
      
      # Convert wind speed and direction to numeric and write to output
      weekly_average$precip <- as.numeric(weekly_average$precip)
      
      precips = c(precips, weekly_average$precip)
      dates = union(dates, weekly_average$date)
      
    }
  }
  
  output_df <- data.frame(as.POSIXct(dates), wind_speeds, wind_dirs, precips)
  colnames(output_df) <- c('dates', 'wind_speeds', 'wind_dirs', 'precips')
  
  return(output_df)
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
