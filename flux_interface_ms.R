library(ncdf4)

#' Calculate discharge through an interface
#'
#' @param filepath character; path to NetCDF file
#' @param target list(x=..., y=...); xt, yt coordinates for the target cell center
#' @return list with discharge in x and y directions
#'
flux_interface <- function(filepath, target) {
  nc <- nc_open(filepath)
  on.exit(nc_close(nc))
  
  # --- Coordinates ---
  xt <- ncvar_get(nc, "xt")[, 1]
  yt <- ncvar_get(nc, "yt")[1, ]
  
  # Find closest indices
  ix <- which.min(abs(xt - target$x))
  iy <- which.min(abs(yt - target$y))
  
  # Grid spacing (assumes uniform)
  dx <- mean(diff(xt))
  dy <- mean(diff(yt))
  
  # --- Layer thickness ---
  hnt <- ncvar_get(nc, "hnt")[ix, iy, ]
  
  # --- Velocity ---
  uk <- ncvar_get(nc, "uk")[ix, iy, ]   # x velocity
  vk <- ncvar_get(nc, "vk")[ix, iy, ]   # y velocity
  
  # --- Discharges ---
  qx <- uk * hnt * dy
  qy <- vk * hnt * dx
  
  return(list(x = qx, y = qy, indices = c(ix, iy)))
}

# Example usage
mal_3d_filename <- file.path("data", "malaren_3d_20020501.nc")
target <- list(x = 586950, y = 6596550)

discharge <- flux_interface(mal_3d_filename, target)

# Total discharge in x and y
sum(discharge$x)
sum(discharge$y)
