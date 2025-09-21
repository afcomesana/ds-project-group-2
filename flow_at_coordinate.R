# Import dependencies
library("ncdf4")

#' Calculate flow from a PyGETM output file (3D)
#'
#' @details
#'   Read a PyGETM 3D output file into R, and calculate total flow (summed over all depth layers) at a certain coordinate at each timestep.
#'   Options for direction of flow ('direction' = "x" or "y" direction)
#'
#' @param ncdf  character; name of the output nc file
#' @param x,y numeric; x and y coordinates. NOTE: xu/xv yu/yv coordinates for the interface of interest and not xt yt coordinates for the cell
#' @param direction character; Direction of the flow
#' @examples
#'  \dontrun{
#'  flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc",
#'                     x = 658350, 
#'                     y =  6596400,
#'                     direction = "y",)
#'  }
#' @import ncdf4
#' @export
#' 
flow_at_coordinate = function(ncdf, x, y, direction){
  # Input check
  if((!is.character(ncdf)) |
     (!is.numeric(x)) |
     (!is.numeric(y)) |
     (!is.character(direction))){
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
  
  # Fetch model coordinates for all interfaces in the chosen direction
  # (If y-direction then v interfaces, else u interfaces)
  type_x = ifelse(direction == "y", "xv", "xu")
  type_y = ifelse(direction == "y", "yv", "yu")
  model_interface_coords_x = ncvar_get(nc, type_x)[,1]
  model_interface_coords_y = ncvar_get(nc, type_y)[1,]
  
  # Get the corresponding indices for our interface coordinates
  x_index <- match(x, model_interface_coords_x)
  y_index <- match(y, model_interface_coords_y)
  
  # Check if coordinates are valid
  if(is.na(x_index)){
    stop("x-coordinate + direction doesn't match any interface in model.")
  }
  if(is.na(y_index)){
    stop("y-coordinate + direction doesn't match any interface in model.")
  }
  
  # Choose flow variable according to direction
  flow_var = ifelse(direction == "y", "vk", "uk")
  
  # Fetch flow attribute at current interface (will have dimension layers x time)
  flow = ncvar_get(nc, flow_var)
  flow_point <- flow[x_index, y_index, ,]
  
  # Fetch layer heights at same point (indices are the same as for interface v, even if coordinates are not)
  # Also of dimension layers x time
  hnt = ncvar_get(nc, "hnt")
  hnt_point <- hnt[x_index, y_index, ,]
  
  # Fetch interface end points (same as coordinates for interfaces in the other direction)
  # and calculate the width as difference between current and previous end point
  width <- 0
  if(direction == "y"){
    interface_end_points <- ncvar_get(nc, "xu")[,1]
    width <- interface_end_points[x_index] - interface_end_points[x_index-1]
  }
  else{
    interface_end_points <- ncvar_get(nc, "yv")[1,]
    # negative as order of y coordinates are reversed in this model
    width <- -(interface_end_points[y_index] - interface_end_points[y_index-1])
  }
  
  # Multiply flow with height element-wise, sum along the layer axis, and multiply with width to get total flow at each time step
  flow <- apply(flow_point*hnt_point, 2, sum) * width
  
  # Negate if direction is y, as reversed in this model
  sign_y = ifelse(all(diff(model_interface_coords_y) > 0), 1, -1)
  if(direction == "y"){
    flow <- sign_y * flow
  }

  return(flow)
}

flow1 = flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc", x = 658350, y = 6596400, direction = "y")

flow2 = flow_at_coordinate(ncdf = "malaren_3d_20190801_multi_t.nc", x = 640200, y = 6591750, direction = "x")


