
raster.latlon.transform <- function(raster_obj, x_dim="x", y_dim="y"){

  if(is.null(crs(raster_obj))){
    log_error("Cannot transform raster. CRS origin not known.")
    stop()
  }

  stars_obj <- st_as_stars(raster_obj)

  grid_latlon <- st_transform_proj(stars_obj,crs=4326)
  lon <-st_get_dimension_values(grid_latlon, x_dim)
  lat <-st_get_dimension_values(grid_latlon, y_dim)

  # transpose array, because raster uses lat in first dimension
  r_lon <- raster(t(lon), template=raster_obj)
  names(r_lon) <- "lon"
  r_lon@data@unit <- "degrees_east"
  r_lon@title <- "longitude"
  r_lat <- raster(t(lat), template=raster_obj)
  names(r_lat) <- "lat"
  r_lat@data@unit <- "degrees_north"
  r_lat@title <- "latitude"
  r_latlon <- stack(raster_obj,r_lat,r_lon)
  return(r_latlon)
}
