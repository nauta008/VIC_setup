
domain.set <- function(raster_domain){

  grid <- VICSetup$grid

  stars_domain <- st_as_stars(raster_domain)
  dim_info <- st_dimensions(stars_domain)

  grid$resolution <- c(dim_info[[VICSetup$config$domain$dim$x]]$delta, dim_info[[VICSetup$config$domain$dim$y]]$delta)
  grid$reverse_y <- FALSE
  if(grid$resolution[2] <0){
    grid$reverse_y <- TRUE
  }
  grid$x_vals <- seq(from=raster_domain@extent@xmin + abs(grid$resolution[1])/2,
                     to=raster_domain@extent@xmax- abs(grid$resolution[1])/2, by=abs(grid$resolution[1]))

  grid$y_vals <- seq(from=raster_domain@extent@ymin + abs(grid$resolution[2])/2,
                     to=raster_domain@extent@ymax- abs(grid$resolution[2])/2, by=abs(grid$resolution[2]))

  grid$raster <- raster_domain
  crs(grid$raster) <- grid$proj4

  VICSetup$grid <- grid
  log_info(sprintf("Domain set. VICsetup will use domain with %s and resolution [%s,%s]", extent(grid$raster), grid$resolution[1],grid$resolution[2]))
}


