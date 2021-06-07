
proj.get.axes.names <- function(proj4_params){

  grid_mapping <- proj.to.grid.mapping(proj4_params)
  proj4_args <- proj.get.args(proj4_params)
  proj4_unit <- proj4_args$units

  axes <- list(
    x=list(name="lon",long_name="longitude of grid cell center"),
    y=list(name="lat",long_name="latitude of grid cell center"))
  # x_name <- 'lon'
  # x_longname <- 'longitude of grid cell center'
  # y_name <- 'lat'
  # y_longname <- 'latitude of grid cell center'
  if(is.null(proj4_unit) && grid_mapping$grid_mapping_name == 'latitude_longitude'){
    axes$unit <- "degrees"
    #proj4_unit <- "degrees"
    proj_units_x <-  paste(axes$unit,"_east")
    proj_units_y <-  paste(axes$unit,"_north")
  }
  else{
    proj_units_x <- 'meter'
    proj_units_y <- proj_units_x
    axes$x$name <- 'x'
    axes$x$long_name <- 'x coordinate of grid cell center'
    axes$y$name <- 'y'
    axes$y$long_name <- 'y coordinate of grid cell center'
  }

  axes$x$unit <- proj_units_x
  axes$y$unit <- proj_units_y
  return(axes)
}
