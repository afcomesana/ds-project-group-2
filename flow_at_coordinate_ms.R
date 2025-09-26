library(ncdf4)

#' Calculate flow from a PyGETM output file (3D)
#'
#' @param ncdf  character; name of the output nc file
#' @param x,y numeric; xt and yt coordinates for the cell
#' @param direction character; "x" or "y" for flow direction
#' @param profile logical; if TRUE, return depth profile, otherwise total flow
#' @return numeric vector (time series) or matrix (profile × time)
#'
flow_at_coordinate <- function(ncdf, x, y, direction, profile = FALSE) {
  # Input checks
  stopifnot(is.character(ncdf), is.numeric(x), is.numeric(y),
            direction %in% c("x", "y"), is.logical(profile))
  
  # Open file
  nc <- nc_open(ncdf)
  on.exit(nc_close(nc))
  
  # Get cell center coordinates
  xt <- ncvar_get(nc, "xt")[, 1]
  yt <- ncvar_get(nc, "yt")[1, ]
  
  # Closest index
  ix <- which.min(abs(xt - x))
  iy <- which.min(abs(yt - y))
  
  # Velocity variable
  vel_name <- ifelse(direction == "y", "vk", "uk")
  vel <- ncvar_get(nc, vel_name)
  
  # Ensure time axis exists
  if (length(dim(vel)) == 3L) {
    dim(vel) <- c(dim(vel), 1)
  }
  vel_point <- vel[ix, iy, , ]
  
  # Layer thickness
  hnt <- ncvar_get(nc, "hnt")
  if (length(dim(hnt)) == 3L) {
    dim(hnt) <- c(dim(hnt), 1)
  }
  hnt_point <- hnt[ix, iy, , ]
  
  # Grid spacing (interface width)
  if (direction == "y") {
    width <- xt[2] - xt[1]
  } else {
    width <- -(yt[2] - yt[1])  # negative: reversed y-order
  }
  
  # Compute flow
  flow_matrix <- vel_point * hnt_point * width
  
  if (!profile) {
    # Collapse over depth
    flow <- colSums(flow_matrix, na.rm = TRUE)
  } else {
    flow <- flow_matrix
  }
  
  # Fix sign for y
  if (direction == "y") {
    sign_y <- ifelse(all(diff(yt) > 0), 1, -1)
    flow <- sign_y * flow
  }
  
  return(flow)
}

# Example usage
# Regular discharge (time series)
flow1 <- flow_at_coordinate("malaren_3d_20190801_multi_t.nc",x = 658350, y = 6596550,direction = "y")

# Depth profile (layers × time)
flow2 <- flow_at_coordinate("malaren_3d_20190801_multi_t.nc",x = 658350, y = 6596550,direction = "y", profile = TRUE)

