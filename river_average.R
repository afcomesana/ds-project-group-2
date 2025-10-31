# Import dependencies
library("ncdf4")
library("pygetmtools")

#' Preprocess river flow from PyGETM output file (3D) into weekly averaged time series.
#'
#' @details
#'   Read a PyGETM 3D output file for rivers into R, calculate weekly averages, and store as csv time series.
#'   Options: For now the timespan is hard coded to fit our discharge file, could be changed.
#'
#' @param file_path  character; folder which contains all river nc files
#' @param output_path character; folder 
#' @examples
#'  \dontrun{
#'  river_average(file_path = "./Rivers/",
#'                output_path = "./Rivers_csv/")
#'  }
#' @import ncdf4
#' @export

river_average = function(file_path, output_path){
  
  # Input check
  if((!is.character(file_path)) |
     (!is.character(output_path))){
    stop("Some argument types are wrong!")
  }
  # Get file names
  file_names <- list.files(path = file_path)
  if(length(file_names) == 0){
    stop("Empty or non-existing file_path")
  }
  
  # Loop through all files in folder
  for(file_name in file_names){
    
    # Open nc (and always close on exit)
    nc = nc_open(paste(file_path, file_name, sep = ""))
    on.exit({
      nc_close(nc)
    })
    
    # Extract flow and date from nc file
    q_values = ncvar_get(nc, "q")
    
    time_vals = nc$dim$time$vals
    time_att = nc$dim$time
    
    time_dates <- convert_nc_time(time_vals, time_att)
    
    # Store in dataframe
    df <- data.frame(time_dates, q_values)
    colnames(df) <- c("date", "flow")
    # Extract day/month/year for easier loop
    df$Day <- format(as.Date(df$date, format="%Y-m%-d%"),"%d")
    df$Month <- format(as.Date(df$date, format="%Y-m%-d%"),"%m")
    df$Year <- format(as.Date(df$date, format="%Y-m%-d%"),"%Y")
    
    # Initialize output frame
    weekly_average <- data.frame(
      date = as.POSIXct(character()),  
      flow = numeric()  
    )
    sum <- 0
    i <- 1
    
    # Start at 2000 to match discharge files
    next_rows <- which(df$Year == "2000" & seq_len(nrow(df)) > i)
    # Check that we are not at end of file
    if (length(next_rows) > 0) {
      i = next_rows[1]
    } else{
      i = dim(df)[1]
      print(paste("Empty file at", file_name))
    }
    
    # End at 2024-10-31 to match discharge
    end <- dim(df)[1]
    end_row <- which(df$date == as.Date("2024-10-31") & seq_len(nrow(df)) > i)
    # Check that end date exists
    if (length(end_row) > 0) {
      end <- end_row[1]
    }
    else{
      print(paste("File ends before 2024-10-31: ", file_name))
    }
    
    # First entry slightly different, discharges start at 2000-01-06
    i <- i-2
    # Day within week counter
    j <- 1
    
    # Average over 7-days interval
    while(i <= end){
      sum <- sum + df$flow[i]
      
      #If we're at end of a week, append average to weekly average
      if ( j == 7){
        date <- paste(df$Year[i+1], df$Month[i+1], df$Day[i+1], sep = "-")
        weekly_average[nrow(weekly_average) + 1,] <- c(date, sum/7)
        # Reset sum and week counter
        sum <- 0
        j <- 1
      }
      # Else, continue summing next loop
      else{
        j <- j + 1
      }
      i <- i+1
    }
    # Convert flow to numeric and write to output
    weekly_average$flow <- as.numeric(weekly_average$flow)
    write.csv(weekly_average, file = paste(output_path, sub("\\.nc$", ".csv", file_name), sep = ""), row.names = FALSE)
    
    
  }
  
}

# Test run
river_average("./Rivers/", "./Rivers_csv/")


