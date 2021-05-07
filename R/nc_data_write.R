
nc.data.write <- function(file_name, raster_obj){

  proj_units <- 'degrees'
  x_name <- 'lon'
  x_longname <- 'longitude of grid cell center'
  y_name <- 'lat'
  y_longname <- 'latitude of grid cell center'
  if(length(grep("units=m", VICSetup$grid$proj4 , fixed = TRUE))>0){
    proj_units <- 'meter'
    x_name <- 'x'
    x_longname <- 'x coordinate of grid cell center'
    y_name <- 'y'
    y_longname <- 'y coordinate of grid cell center'
  }


  dim_x <- ncdim_def(name = x_name,units = proj_units, vals = VICSetup$grid$x_vals, longname = x_longname)
  dim_y <- ncdim_def(name = y_name,units = proj_units, vals = VICSetup$grid$y_vals, longname = y_longname)

  nc_var_list <- list()

  for(n in 1:nlayers(raster_obj)){
    r <- raster_obj[[n]]
    nc_var <- ncvar_def(name = names(r)[1],dim = list(dim_x,dim_y), units = r@data@unit, missval = NaN, prec ='integer',compression = 1)
    nc_var_list[[as.character(n)]] <- nc_var
  }

  nc <- nc_create(file_name, nc_var_list)
  #nc_close(nc)

  #nc <- nc_open(filename, write=T)
  for(n in 1:nlayers(raster_obj)){
    r <- raster_obj[[n]]
    if(VICSetup$grid$reverse_y){
      r <- flip(r,direction = 'y' )
    }
    ncvar_put(nc, nc_var_list[[n]], vals=as.vector(r))
    ncatt_put(nc,nc_var_list[[n]],attname = 'proj4',attval = VICSetup$grid$proj4)
    ncatt_put(nc,nc_var_list[[n]], attname = 'grid_mapping','crs')
  }

  crs_var <- ncvar_def('crs',units=proj_units,dim=list(),prec = 'char')

  ncvar_add(nc,crs_var)
  nc_close(nc)

  nc <- nc_open(file_name,write=T)


  ncatt_put(nc,crs_var, attname = 'grid_mapping_name', attval = VICSetup$grid$grid_mapping_name, prec='char')
  ncatt_put(nc,crs_var, attname = 'proj4_params', attval = VICSetup$grid$proj4, prec='char')

  ncatt_put(nc,varid = 0, attname = 'date_created', attval = date(), prec='char')
  ncatt_put(nc, varid = 0, attname = 'user', attval = Sys.getenv("USERNAME"), prec = 'char')
  ncatt_put(nc, varid= 0, attname='R_version',attval = as.character(getRversion()), prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'R_package_name', attval = 'VICsetup', prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'R_package_source', attval = 'https://github.com/nauta008/VIC_setup', prec = 'char')

  nc_close(nc)
  log_info(sprintf("Output %s created.", file_name))
}
