
domain.init <- function() {

  grid <- list()
  if(is.null(VICSetup$config$domain)){
    log_error("Domain not specified in config")
    stop()
  }
  stars_domain <- read_ncdf(VICSetup$config$domain$file,var = VICSetup$config$domain$var)
  nc_domain <- nc_open(VICSetup$config$domain$file)
  grid_mapping <- ncatt_get(nc_domain,VICSetup$config$domain$var, 'grid_mapping')$value
  nc_close(nc_domain)
  proj4 <- NULL
  if(!is.null(VICSetup$config$domain$proj)){
    if(!is.null(VICSetup$config$domain$proj$proj4)){
      proj4 <- VICSetup$config$domain$proj$proj4
    }
    else if(!is.null(VICSetup$config$domain$proj$code)){
      epsg_list <- make_EPSG()
      epsg_idx <- which(epsg_list$code==VICSetup$config$domain$proj$code)
      epsg <- epsg_list[epsg_idx,]
      proj4 <- as.character(epsg$prj4)
    }
  }
  else{
    # try to extract from file
    proj4 <- crs(stars_domain)
  }

  if(is.null(proj4)){
    log_error("Projection not defined. Set either proj4 or EPSG code in config.")
    stop()
  }

  grid$mask <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$var)
  grid$proj4 <- proj4
  grid$type <-  st_raster_type(stars_domain)
  grid$grid_mapping_name <- grid_mapping

  grid_raster <- raster(VICSetup$config$domain$file, varname=VICSetup$config$domain$var)
  crs(grid_raster) <- grid$proj4
  VICSetup$grid <- grid
  domain.set(grid_raster)
}
