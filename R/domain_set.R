
domain.set <- function(){
  grid <- list()
  if(is.null(VICSetup$config$domain)){
    stop("Domain not specified in config")
  }
  stars_domain <- read_ncdf(VICSetup$config$domain$file,var = VICSetup$config$domain$var)
  dim_info <- st_dimensions(stars_domain)
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
    stop("Projection not defined. Set either proj4 or EPSG code in config.")
  }

  grid$proj4 <- proj4
  grid$type <-  st_raster_type(stars_domain)
  grid$grid_mapping_name <- grid_mapping
  grid$resolution <- c(dim_info[[VICSetup$config$domain$dim$x]]$delta, dim_info[[VICSetup$config$domain$dim$y]]$delta)

  grid$x_vals <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$dim$x)
  grid$y_vals <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$dim$y)
  grid$mask <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$var)
  grid$raster <- raster(VICSetup$config$domain$file, varname=VICSetup$config$domain$var)
  crs(grid$raster) <- grid$proj4

  VICSetup$grid <- grid
}


