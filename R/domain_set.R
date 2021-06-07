
domain.set <- function(raster_domain){

  if(is.null(crs(raster_domain))){
    log_error("Cannot set domain. CRS projection not known.")
    stop()
  }

  # set a local
  grid <- list()

  stars_domain <- st_as_stars(raster_domain)
  dim_info <- st_dimensions(stars_domain)

  grid$resolution <- c(dim_info[[VICSetup$config$domain$dim$x]]$delta, dim_info[[VICSetup$config$domain$dim$y]]$delta)

  # check the orientation of the data
  grid$reverse_y <- FALSE

  if(grid$resolution[2] <0){
    grid$reverse_y <- TRUE
  }

  grid$raster <- raster_domain

  grid$proj4 <- CRSargs(crs(raster_domain))
  grid$grid_mapping <- proj.to.grid.mapping(grid$proj4)

  log_info(sprintf("Domain set. VICsetup will use domain with %s and resolution [%s,%s]", extent(grid$raster), grid$resolution[1],grid$resolution[2]))

  grid$isLonLat <- TRUE
  # if no lonlat grid and no lat and lon attributes. create lat and lon variables
  if(!isLonLat(grid$raster) && !all(c("lat","lon") %in% names(grid$raster))){
    grid$raster <- raster.latlon.transform(grid$raster)
    grid$isLonLat <- FALSE
  }

  VICSetup$grid <- grid
}


