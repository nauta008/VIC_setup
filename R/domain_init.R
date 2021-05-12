
domain.init <- function() {

  #grid <- list()
  if(is.null(VICSetup$config$domain)){
    log_error("Domain not specified in config")
    stop()
  }
  stars_domain <- read_ncdf(VICSetup$config$domain$file,var = VICSetup$config$domain$var)
  nc_domain <- nc_open(VICSetup$config$domain$file)
  # Try to extract grid mapping from netcdf using CF convention
  grid_mapping <- nc.grid.mapping.get(nc_domain, VICSetup$config$domain$var)
  proj4_params <- NULL

  # Try to proj4 params from config
  if(is.null(grid_mapping) && !is.null(VICSetup$config$domain$proj)){
    if(!is.null(VICSetup$config$domain$proj$proj4)){
      proj4_params <- VICSetup$config$domain$proj$proj4
    }
    else if(!is.null(VICSetup$config$domain$proj$code)){
      epsg_list <- make_EPSG()
      epsg_idx <- which(epsg_list$code==VICSetup$config$domain$proj$code)
      epsg <- epsg_list[epsg_idx,]
      proj4_params <- as.character(epsg$prj4)
    }
  }
  # try to get proj4 from stars package
  else{
    # try to extract from file
    proj4_params <- crs(stars_domain)
    log_warn("Implicitly set the CRS from domain file.")
  }

  if(is.null(grid_mapping) && !is.null(proj4_params)){
    grid_mapping <- proj.to.grid.mapping(proj4_params)
  }
  else if(is.null(proj4_params) && !is.null(grid_mapping)){
    proj4_params <- nc_gm_to_prj(grid_mapping)
  }
  else{
    # no grid mapping found and no proj4_params found
    log_error("Grid mapping could not be set from domain file or domain proj config.")
    nc_close(nc_domain)
    stop()
  }

  # both proj4_params and grid_mapping should be filled here.

  #grid$mask <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$var)
  #grid$proj4 <- proj4
  #grid$type <-  st_raster_type(stars_domain)
  #grid$grid_mapping <- grid_mapping

  grid_raster <- raster(VICSetup$config$domain$file, varname=VICSetup$config$domain$var)
  names(grid_raster) <- "mask"
  crs(grid_raster) <- proj4_params
  stars_domain <- st_set_crs(stars_domain, crs=proj4_params)

  lat <- NULL
  lon <- NULL
  # if not latitude_longitude mapping, search for lat and lon definitions
  if(grid_mapping$grid_mapping_name != "latitude_longitude"){
    # check for existance of lat and lon variables
    var_names <- names(nc_domain$var)
    if(!is.null(VICSetup$config$domain$lat) && !is.null(VICSetup$config$domain$lat,VICSetup$config$domain$lon)){
      log_debug("lat and lon set in domain config")
    }
    else if(all(c("latitude","longitude") %in% var_names)){
      VICSetup$config$domain$lat <- "latitude"
      VICSetup$config$domain$lon <- "longitude"
    }
    else if(all(c("lat","lon") %in% var_names)){
      VICSetup$config$domain$lat <- "lat"
      VICSetup$config$domain$lon <- "lon"
    }

    if(!is.null(VICSetup$config$domain$lat) && !is.null(VICSetup$config$domain$lat,VICSetup$config$domain$lon)){
      lat <- ncvar_get(nc_domain,VICSetup$config$domain$lat)
      lon <- ncvar_get(nc_domain,VICSetup$config$domain$lat)
    }
    else{
      # transform grid
      grid_latlon <- st_transform_proj(stars_domain,crs=4326)
      lon <-st_get_dimension_values(grid_latlon, VICSetup$config$domain$x)
      lat <-st_get_dimension_values(grid_latlon, VICSetup$config$domain$y)
    }
    r_lon <- raster(t(lon), template=grid_raster)
    names(r_lon) <- "lon"
    r_lat <- raster(t(lat), template=grid_raster)
    names(r_lat) <- "lat"
    # stack raster with lat and lon
    grid_raster <- stack(grid_raster,r_lat,r_lon)
  }

  domain.set(grid_raster)
}
