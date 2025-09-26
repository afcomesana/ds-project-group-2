library(ncdf4)

#' Calculate flow from a PyGETM output file (3D) across an interface
#'
#' @details
#'   Reads one or more PyGETM 3D output files into R, and calculates total flow
#'   (summed over all depth layers and interface cells) at each timestep.
#'   Multiple NetCDF files are concatenated into a single time series.
#'
#' @param ncdf character vector; one or more NetCDF output files
#' @param interface list; each element is list(direction=char, x=numeric, y=numeric)
#' @return numeric vector of flow time series
#'
#' @examples
#' flow_at_interface(
#'   ncdf = c("malaren_3d_20020601.nc", "malaren_3d_20020701.nc"),
#'   interface = list(
#'     list(direction="x", x=652650, y=6592050),
#'     list(direction="x", x=652650, y=6591750)
#'   )
#' )
#'
flow_at_interface <- function(ncdf, interface) {
  stopifnot(is.character(ncdf), is.list(interface))
  
  # Helper to check one coordinate
  check_coord <- function(coord) {
    if (!is.list(coord) ||
        is.null(coord$direction) || !is.character(coord$direction) ||
        is.null(coord$x) || !is.numeric(coord$x) ||
        is.null(coord$y) || !is.numeric(coord$y)) {
      stop("Each interface element must be list(direction=char, x=num, y=num)")
    }
  }
  lapply(interface, check_coord)
  
  total_flow <- unlist(lapply(ncdf, function(file_name) {
    # Compute sum across all interface cells
    flows <- lapply(interface, function(coord) {
      flow_at_coordinate(file_name, coord$x, coord$y, coord$direction)
    })
    # sum flows across interface cells
    Reduce("+", flows)  
  }))
  
  return(total_flow)
}

# Examples

# One file, multiple cells in interface
total_flow <- flow_at_interface(
  ncdf = "malaren_3d_20190801_multi_t.nc",
  interface = list(
    list(direction="x", x=652650, y=6592050),
    list(direction="x", x=652650, y=6591750)
  )
)

# Two files concatenated
total_flow_2 <- flow_at_interface(
  ncdf = c("malaren_3d_20020601.nc", "malaren_3d_20020701.nc"),
  interface = list(
    list(direction="x", x=652650, y=6592050),
    list(direction="x", x=652650, y=6591750)
  )
)

