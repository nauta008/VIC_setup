
nc.grid.mapping.get <- function(nc, var){

  grid_mapping_name <- ncatt_get(nc,var, 'grid_mapping')$value
  grid_mapping_alias <- names(CONSTANTS$grid_mapping$lookup)[as.numeric(which(CONSTANTS$grid_mapping$lookup==grid_mapping_name))]
  grid_mapping <- NULL
  if(grid_mapping_name %in% names(nc$var)){
    grid_mapping <- ncatt_get(nc,grid_mapping_name)
  }
  else if(grid_mapping_alias %in% names(nc$var)){
    grid_mapping <- ncatt_get(nc,grid_mapping_alias)
  }
  # fallback for lat lon
  else if(all(!c("lon","lat") %in% names(nc$dim)) || all(!c("longitude","latitude") %in% names(nc$dim))){
    grid_mapping <- CONSTANTS$grid_mapping$default
    log_warn("Implicitly found latitude_longitude grid mapping.")
  }
  else{
    log_warn(sprintf("Could determine grid mapping from %s",nc$filename))
  }

  return(grid_mapping)

}
