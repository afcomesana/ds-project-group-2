library(pygetmtools)
library(ncdf4)

mal_3d_filename <- file.path('data', 'malaren_3d_20020501.nc')
target = list(x = 586950, y = 6596550)

flux_interface <- function(filepath, target, var = "discharge") {

  nc_data = nc_open(filepath)
  
  # Ensure file gets closed when function ends
  on.exit({ nc_close(nc_data)})
  
  #if(!(var %in% names(nc_data$var))){
  #  stop("'var' cannot be found in the 'ncdf' file!")
  #}
  
  # Get index and width for x
  x_diffs = abs(ncvar_get(nc_data, varid='xt')[,1] - target$x)
  x_index = which(min(x_diffs) == x_diffs)
  x_width = abs(x_diffs[1] - x_diffs[2])
  
  # Get index and width for y
  y_diffs = abs(ncvar_get(nc_data, varid='yt')[1,] - target$y)
  y_index = which(min(y_diffs) == y_diffs)
  y_width = abs(y_diffs[1] - y_diffs[2])
  
  print(x_index)
  print(y_index)
  
  # Get heights for cells
  heights = ncvar_get(nc_data, varid='hnt')[x_index, y_index, ]
  
  # Get flow speed in x and y directions
  uk = ncvar_get(nc_data, varid='uk')[x_index, y_index, ] # x direction
  vk = ncvar_get(nc_data, varid='vk')[x_index, y_index, ] # y direction
  
  # Return discharges
  return(list(x = uk*heights*y_width, y = vk*heights*x_width))
}

discharge = flux_interface(mal_3d_filename, target)

