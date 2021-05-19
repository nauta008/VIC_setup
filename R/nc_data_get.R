
nc.data.get <- function(file,var){
  nc <- nc_open(file)
  data <- ncvar_get(nc,var)
  nc_close(nc)
  return(data)
}


nc.data.get.stars <- function(file,var, crs=NULL){
  # NOTE: can produce errors like: Error in UseMethod("GPFN") : no applicable method for 'GPFN' applied to an object of class "list". In this case it cannot map the grid mapping,
  # but we solve this in nc.gid.mapping.get.
  stars_data <- read_ncdf(file,var = var, make_units = FALSE)
  stars_data_crs <- st_crs(stars_data)
  if(is.na(stars_data_crs) || is.null(stars_data_crs)){
    nc <- nc_open(file)
    grid_mapping <- nc.grid.mapping.get(nc, var)
    nc_close(nc)
    if(is.null(grid_mapping$proj4_params)){
      grid_mapping$proj4_params <- nc_gm_to_prj(grid_mapping)
    }
    else if(!is.null(crs)){
      grid_mapping$proj4_params <- crs
    }
    stars_data <- st_set_crs(stars_data, grid_mapping$proj4_params)
  }

  return(stars_data)
}
