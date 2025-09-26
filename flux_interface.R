library(pygetmtools)
library(ncdf4)

#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, and calculate total flow
#'   (summed over all depth layers) at a certain coordinate at each timestep.
#'
#' @param filepath  character; path to the nc file
#' @param target named list(x =c(...), y =c(...)) coordinates. Selected cell will be that one
#' whose center coordinates are closest to the ones provided in this parameter.
#' 
#' TODO: INCLUDE PROFILE PARAMETER
#' 
#' @examples
#'  \dontrun{
#'  flux_interface(ncdf = "malaren_3d_20190801_multi_t.nc", list(x = 658350, y =  6596400))
#'  }
#' @import ncdf4
#' @export
#' 


# TODO: avoid opening the nc file in every iteration to speed up computation
flux_interface <- function(filepath, target) {
  
  if (!is.character(filepath)) {
    stop("Filepath argument must be of type character.")
  }
  
  if (!setequal(names(target), list('x', 'y'))
      | !all(sapply(target, is.numeric))) {
    stop("Target argument must be a named list with names 'x' and 'y', and its values numeric vectors or numbers.")
  }
  
  if (length(target$x) != length(target$y)) {
    stop("x and y coordinates must have the same length.")
  }
  
  nc_data = nc_open(filepath)
  
  # Ensure file gets closed when function ends
  on.exit({ nc_close(nc_data)})
  
  
  # Check dimensions are present:
  if(!all(c('xt', 'yt') %in% names(nc_data$dim))){
    stop("Provided file does not include the required variables 'uk' and 'vk'")
  }
  
  # Check if all flow variables are present:
  if(!all(c('vk', 'uk') %in% names(nc_data$var))){
    stop("Provided file does not include the required variables 'uk' and 'vk'")
  }
  
  # Get width and sign for x
  x_coord = ncvar_get(nc_data, varid='xt')[,1]
  x_width = abs(x_coord[1] - x_coord[2])
  x_sign  = ifelse(x_coord[2] > x_coord[1], 1, -1)
  
  # Get width and sign for y
  y_coord = ncvar_get(nc_data, varid='yt')[1,]
  y_width = abs(y_coord[1] - y_coord[2])
  y_sign  = ifelse(y_coord[2] > y_coord[1], 1, -1)
  
  # Get indexes
  x_index = sapply(target$x, function(x) which.min(abs(x_coord - x)))
  y_index = sapply(target$y, function(y) which.min(abs(y_coord - y)))
  
  # Get heights for cells
  heights = ncvar_get(nc_data, varid='hnt')
  
  # Get flow speed in x and y directions
  uk = ncvar_get(nc_data, varid='uk') # x direction
  vk = ncvar_get(nc_data, varid='vk') # y direction
  
  # Adjust dimensionality:
  if (length(dim(heights)) == 3) {
    dim(heights) = c(dim(heights), 1)
    dim(uk) = c(dim(uk), 1)
    dim(vk) = c(dim(vk), 1)
  }
  
  time_dim = dim(heights)[4]
  
  discharge_x = rep(0, time_dim)
  discharge_y = rep(0, time_dim)
  
  for(coord in 1:length(x_index)) {
    coord_heights = heights[x_index[coord], y_index[coord], ,]
    coord_uk = uk[x_index[coord], y_index[coord], ,]
    coord_vk = vk[x_index[coord], y_index[coord], ,]
    
    discharge_x = discharge_x + apply(coord_uk*coord_heights*y_width*x_sign, 2, function(values) ifelse(any(is.na(values)), 0, sum(values)))
    discharge_y = discharge_y + apply(coord_vk*coord_heights*x_width*y_sign, 2, function(values) ifelse(any(is.na(values)), 0, sum(values)))
  }
  
  
  # Return discharges
  return(list(x = discharge_x, y = discharge_y))
}