library(pygetmtools)
library(ncdf4)

#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, and calculate total flow
#'   (summed over all depth layers) at a certain coordinate at each timestep.
#'
#' @param filepath  character; path to the nc file
#' @param target named list(x =, y =) coordinates. Selected cell will be that one
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

flux_interface <- function(filepath, target) {
  
  # If the input is more than one point, call function with single points and sum
  if (length(target$x) > 1) {
    print("More than one point to compute")
    
    return(sapply(
      Map(list, target$x, target$y),
      function (coord) flux_interface(filepath, target=list(x=unlist(coord[1]), y=unlist(coord[2])))
    ))
    
    # for(coord in Map(list, target$x, target$y)) {
    #   
    #   sub_target = list(x = coord[1], y=coord[2])
    #   sub_target
    # }
  }
  
  # Check filepath
  if(!is.character(filepath)
     | !setequal(names(target), list('x', 'y'))
     | !all(sapply(target, is.numeric))) {
    stop("Some argument types are wrong!")
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
  
  
  
  # TODO: Check coordinates are valid, when does this can happen?
  #if(is.na(x_index)){
  # stop("x-coordinate + direction doesn't match any interface in model.")
  #}
  #if(is.na(y_index)){
  #  stop("y-coordinate + direction doesn't match any interface in model.")
  #}
  
  # Get index, width and sign for x
  x_coord = ncvar_get(nc_data, varid='xt')[,1]
  x_width = abs(x_coord[1] - x_coord[2])
  x_sign  = ifelse(x_coord[2] > x_coord[1], 1, -1)
  x_diffs = abs(x_coord - target$x)
  x_index = which(min(x_diffs) == x_diffs)
  
  # Get index, width and sign for y
  y_coord = ncvar_get(nc_data, varid='yt')[1,]
  y_width = abs(y_coord[1] - y_coord[2])
  y_sign  = ifelse(y_coord[2] > y_coord[1], 1, -1)
  y_diffs = abs(y_coord - target$y)
  y_index = which(min(y_diffs) == y_diffs)
  
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
  
  heights = heights[x_index, y_index, ,]
  uk = uk[x_index, y_index, ,]
  vk = vk[x_index, y_index, ,]
  
  if (any(is.na(uk)) | any(is.na(vk))) {
    return(list(x=0, y=0))
  }
  
  # Return discharges
  return(list(x = sum(uk*heights*y_width)*x_sign, y = sum(vk*heights*x_width)*y_sign))
}