
nc.grid.mapping.get <- function(nc, var=NULL){

  if(is.null(var)){
    var = 0
  }
  # gm <- nc_grid_mapping_atts(nc$filename)
  # if(length(gm$value)==0){
  #   gm <- NULL
  # }
  grid_mapping_name <- ncatt_get(nc,var, 'grid_mapping')$value
  grid_mapping_alias <- names(CONSTANTS$grid_mapping$lookup)[as.numeric(which(CONSTANTS$grid_mapping$lookup==grid_mapping_name))]
  grid_mapping <- NULL
  if(grid_mapping_name %in% names(nc$var)){
    grid_mapping <- ncatt_get(nc,grid_mapping_name)
  }
  else if(length(grid_mapping_alias)>0 && grid_mapping_alias %in% names(nc$var)){
    grid_mapping <- ncatt_get(nc,grid_mapping_alias)
  }
  else{
    log_warn(sprintf("Could not determine grid mapping from %s",nc$filename))
  }

  return(grid_mapping)

}
