# Import dependencies
library("ncdf4")
library("data.table")
source("flow_at_coordinate.R")

#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, and calculate total flow (summed over all depth layers) at an interface containing several cells at each timestep.
#'   Option to use several successive PyGETM files and concatenate flow into a single time series
#'   
#'
#' @param ncdf  vector; vector of  character name(s) of the output nc file(s)
#' @param interface list; list containing coordinates on the form list(direction, x, y), where direction is character, and x,y numeric
#' @examples
#'  \dontrun{
#'  flow_at_coordinate(ncdf = c("malaren_3d_20190801_multi_t.nc"),
#'                     interface = list(c1 = list(direction = "x", x = 652650, y = 6592050), c2 = list(direction = "x", x = 652650, y = 6591750)) )
#'  }
#' @import ncdf4
#' @export
#' 
flow_at_interface = function(ncdf, interface){
  # Time series for all successive files
  lst_flow_files = list()
  for (file_name in ncdf){
    # Summed flow series for all cells in interface
    lst_tmp = list()
    current_coord <- 1
    for( coordinate in interface){
      
      # Check that all coordinates are formatted correctly
      if((is.null(coordinate$x) | !is.numeric(coordinate$x)) |
         (is.null(coordinate$y)| !is.numeric(coordinate$y)) |
         (is.null(coordinate$direction) | !is.character(coordinate$direction))){
        stop(paste("The coordinate c", current_coord, "has wrong or missing input. Should be on the form list(direction = character, x = numeric, y = numeric)"))
      }
      
      # Input is further checked within point flow function
      # Get flow at current cell, and sum into interface total
      df_at_coord = flow_at_coordinate(file_name, coordinate$x, coordinate$y, coordinate$direction)
      lst_tmp[[length(lst_tmp) + 1]] = df_at_coord
      current_coord <- current_coord + 1
    }
    df_file_flow = rbindlist(lst_tmp)
    df_file_flow = df_file_flow[, .(flow = sum(flow)), by = date]
    lst_flow_files[[length(lst_flow_files) + 1]] = df_file_flow
  }
  
  return(rbindlist(lst_flow_files))
}

# # Check, one file and interface of several cells
#  total_flow <- flow_at_interface(ncdf = "malaren_3d_20190801_multi_t.nc", interface = list(c1 = list(direction = "x" , x = 652650, y = 6592050), c2 = list(direction = "x", x = 652650, y = 6591750)) )
# 
# # Check, two successive files with the same interface, extracted into one time series
#  total_flow_2 <- flow_at_interface(ncdf = c("malaren_3d_20020601.nc", "malaren_3d_20020701.nc"), interface = list(c1 = list(direction = "x" , x = 652650, y = 6592050), c2 = list(direction = "x", x = 652650, y = 6591750)) )
# 

