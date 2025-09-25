# Import dependencies
library("ncdf4")

#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, and calculate total flow (summed over all depth layers) at a certain coordinate at each timestep.
#'   Options for direction of flow ('direction' = "x" or "y" direction)
#'
#' @param ncdf  character; name of the output nc file
#' @param x,y numeric; xt and yt coordinates for the cell
#' @param direction character; Direction of the flow
#' @param profile logical; if TRUE, return profile of flow through depth segments, if FALSE, sum all flow over cells
#' @examples
#'  \dontrun{
#'  flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc",
#'                     x = 658350, 
#'                     y = 6596550,
#'                     direction = "x",
#'                     profile = FALSE)
#'  }
#' @import ncdf4
#' @export
#' 
flow_at_coordinate = function(ncdf, x, y, direction, profile=FALSE){
  # Input check
  if((!is.character(ncdf)) |
     (!is.numeric(x)) |
     (!is.numeric(y)) |
     (!is.character(direction)) |
     (!is.logical(profile))){
    stop("Some argument types are wrong!")
  }
  # Extra check for viable directions
  viable_directions <- c("x", "y")
  if(!(direction %in% viable_directions)){
    stop("Input 'direction' should be 'x' or 'y'")
  }
  
  # Read file
  nc = nc_open(ncdf)
  # Always close on exit
  on.exit({
    nc_close(nc)
  })
  
  # Fetch model coordinates for all cells
  model_cell_coords_x = ncvar_get(nc, "xt")[,1]
  model_cell_coords_y = ncvar_get(nc, "yt")[1,]
  
  # Get the closest index match for our coordinates
  # (Note that the interfaces will have the same indices as the cell)
  x_index <- which.min(abs(model_cell_coords_x - x))
  y_index <- which.min(abs(model_cell_coords_y - y))
  
  # Choose flow variable according to direction
  flow_var = ifelse(direction == "y", "vk", "uk")
  
  # Fetch flow attribute and check if time dimension is present
  # If not, add an empty time axis
  added_dimension = FALSE
  flow = ncvar_get(nc, flow_var)
  if(length(dim(flow)) == 3L){
    dim(flow) = c(dim(flow), 1)
    added_dimension = TRUE
  }
  
  # Slice at x-, y- indices to get flow at point over depth and time 
  flow_point <- flow[x_index, y_index, ,]
  
  # Check if interfaces exist at given coordinates
  if(any(is.na(flow_point))){
    stop("NA detected in flow. Check if x-, y-coordinates matches cell with existing interface in chosen direction")
  }
    
  
  # Fetch layer heights at same point (indices are the same as for cell)
  hnt = ncvar_get(nc, "hnt")
  # Dimension check
  if(length(dim(hnt)) == 3L){
    dim(hnt) = c(dim(hnt), 1)
    added_dimension = TRUE
  }
  # Slice to get depth at coordinates
  hnt_point <- hnt[x_index, y_index, ,]
  
  # Get width of interface as spacing between cells in the other direction
  # (This assumes that all cells in the model are of equal dimensions)
  width <- 0
  if(direction == "y"){
    width <- model_cell_coords_x[2] - model_cell_coords_x[1]
  }
  else{
    # negative as order of y coordinates are reversed in this model
    width <- -(model_cell_coords_y[2] - model_cell_coords_y[1])
  }
  
  if(!profile){
    # Multiply flow with height element-wise, sum along the layer axis, and multiply with width to get total flow at each time step
    if(!added_dimension){ 
      flow <- apply(flow_point*hnt_point, 2, sum) * width
    }
    # If no time axis present, need to use sum insted of apply
    else{
      flow <- sum(flow_point*hnt_point)* width
    }
  }
  else{
    # Don't sum but keep flow at each segment
    flow = flow_point*hnt_point* width
  }
  
  # Negate if direction is y, as reversed in this model
  sign_y = ifelse(all(diff(model_cell_coords_y) > 0), 1, -1)
  if(direction == "y"){
    flow <- sign_y * flow
  }

  return(flow)
}

# Present flow and time axis check
# flow1 = flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc", x = 658350, y = 6596550, direction = "y")

# Profile option check
# flow2 = flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc", x = 658350, y = 6596550, direction = "y", profile = TRUE)

# No present interface error check
# flow3 = flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc", x = 658350, y = 6596550, direction = "x")

# No time axis in file check
# flow4 = flow_at_coordinate(ncdf = "malaren_3d_20020501.nc", x = 658350, y = 6596550, direction = "y")


