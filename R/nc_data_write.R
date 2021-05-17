
nc.data.write <- function(raster_obj, file_name){

  # get crs
  proj4_params <- CRSargs(crs(raster_obj))
  grid_mapping <- proj.to.grid.mapping(proj4_params)
  # get args
  proj4_args <- proj.get.args(proj4_params)
  proj4_unit <- proj4_args$units

  x_name <- 'lon'
  x_longname <- 'longitude of grid cell center'
  y_name <- 'lat'
  y_longname <- 'latitude of grid cell center'
  if(is.null(proj4_unit) && grid_mapping$grid_mapping_name == 'latitude_longitude'){
    proj4_unit <- "degrees"
    proj_units_x <- paste(proj4_unit,"_east")
    proj_units_y <- paste(proj4_unit,"_north")
  }
  else{
    proj_units_x <- 'meter'
    proj_units_y <- proj_units_x
    x_name <- 'x'
    x_longname <- 'x coordinate of grid cell center'
    y_name <- 'y'
    y_longname <- 'y coordinate of grid cell center'
  }

  x_vals <- seq(from=raster_obj@extent@xmin + abs(VICSetup$grid$resolution[1])/2,
                      to=raster_obj@extent@xmax- abs( VICSetup$grid$resolution[1])/2, by=abs(VICSetup$grid$resolution[1]))

  y_vals <- seq(from=raster_obj@extent@ymin + abs( VICSetup$grid$resolution[2])/2,
                      to=raster_obj@extent@ymax- abs(VICSetup$grid$resolution[2])/2, by=abs(VICSetup$grid$resolution[2]))

  dim_x <- ncdim_def(name = x_name,units = proj_units_x, vals = x_vals, longname = x_longname)
  dim_y <- ncdim_def(name = y_name,units = proj_units_y, vals = y_vals, longname = y_longname)

  nc_var_list <- list()

  for(n in 1:nlayers(raster_obj)){
    r <- raster_obj[[n]]
    data_type <- dataType(r)
    data_prec <- "integer"
    if(any(data_type == c("FLT4S","FLT8S"))){
      data_prec <- "float"
    }
    nc_var <- ncvar_def(name = names(r)[1],dim = list(dim_x,dim_y), units = r@data@unit, missval = 1e20, prec =data_prec,compression = 1)
    nc_var_list[[as.character(n)]] <- nc_var
  }

  if(file.exists(file_name)){
    file.remove(file_name)
  }
  nc <- nc_create(file_name, nc_var_list)

  for(n in 1:nlayers(raster_obj)){
    r <- raster_obj[[n]]
    if(VICSetup$grid$reverse_y){
      r <- flip(r,direction = 'y' )
    }
    ncvar_put(nc, nc_var_list[[n]], vals=as.vector(r))
    #ncatt_put(nc,nc_var_list[[n]],attname = 'proj4',attval = VICSetup$grid$proj4)
    ncatt_put(nc,nc_var_list[[n]], attname = 'grid_mapping',grid_mapping$grid_mapping_name)
  }

  # add grid_mapping variable
  nc_grid_mapping_var <- ncvar_def(grid_mapping$grid_mapping_name,units="",dim=list(),prec = 'char')
  nc <- ncvar_add(nc,nc_grid_mapping_var)
  nc_close(nc)
  nc <- nc_open(file_name, write=TRUE)
  for(grid_mapping_attr in names(grid_mapping)){
    ncatt_put(nc, nc_grid_mapping_var,attname = grid_mapping_attr, attval = grid_mapping[[grid_mapping_attr]])
  }
  ncatt_put(nc, grid_mapping$grid_mapping_name, attname = "proj4_params", attval = proj4_params)
  nc_close(nc)

  nc <- nc_open(file_name,write=T)

  # Add global attributes
  ncatt_put(nc,varid = 0, attname = 'date_created', attval = date(), prec='char')
  ncatt_put(nc, varid = 0, attname = 'user', attval = Sys.getenv("USERNAME"), prec = 'char')
  ncatt_put(nc, varid= 0, attname='R_version',attval = as.character(getRversion()), prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'R_package_name', attval = 'VICsetup', prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'R_package_source', attval = 'https://github.com/nauta008/VIC_setup', prec = 'char')

  nc_close(nc)
  log_info(sprintf("Output %s created.", file_name))
}


# nc.data.write.raster <- function(filename, raster_obj){
#
# }
