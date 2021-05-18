
nc.data.write <- function(raster_obj, file_name){

  if(file.exists(file_name)){
    file.remove(file_name)
  }

  if(typeof(raster_obj)=='list'){
    count <- 1
    for(r in raster_obj){
      append_file = FALSE
      if(count>1){
        append_file = TRUE
      }
      var_name <- names(raster_obj)[count]
      nc.data.write.raster(r, file_name, do_append = append_file, var_name=var_name)
      count = count + 1
    }
  }
  else{
    nc.data.write.raster(raster_obj, file_name)
  }
}


nc.data.write.raster <- function(raster_obj, file_name, do_append=FALSE, var_name="dummy"){

  dim_list <- list()
  dim_names <- c()
  if(do_append){
    nc <- nc_open(file_name, write= T)
    dim_list <- nc$dim
    dim_names <- names(dim_list)
  }


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



  if(!x_name %in% dim_names){
    x_vals <- seq(from=raster_obj@extent@xmin + abs(VICSetup$grid$resolution[1])/2,
                  to=raster_obj@extent@xmax- abs( VICSetup$grid$resolution[1])/2, by=abs(VICSetup$grid$resolution[1]))
    dim_x <- ncdim_def(name = x_name,units = proj_units_x, vals = x_vals, longname = x_longname)
  }
  else{
    dim_x <- nc$dim[[x_name]]
  }
  if(!y_name %in% dim_names){
    y_vals <- seq(from=raster_obj@extent@ymin + abs( VICSetup$grid$resolution[2])/2,
                  to=raster_obj@extent@ymax- abs(VICSetup$grid$resolution[2])/2, by=abs(VICSetup$grid$resolution[2]))
    dim_y <- ncdim_def(name = y_name,units = proj_units_y, vals = y_vals, longname = y_longname)
  }
  else{
    dim_y <- nc$dim[[y_name]]
  }

  nc_var_list <- list()


  time_vals <- getZ(raster_obj)
  dim_time <- NULL
  # FOR 3D data
  if(!is.null(time_vals)){
    dim_time <- ncdim_def(name= "time", units="seconds", vals=time_vals,longname= "time in seconds")
    dim_list <- list(dim_x,dim_y, dim_time)
    data_type <- dataType(raster_obj[[1]])
    data_prec <- nc.data.get.precision(data_type)
    unit <- raster_obj[[1]]@data@unit
    if(is.null(unit) || unit=="" ){
      unit <- "-"
    }
    long_name <- raster_obj[[1]]@title
    if(length(long_name)==0){
      long_name <- var_name
    }
    nc_var <- ncvar_def(name = var_name,dim = dim_list, units = unit,longname = long_name,compression = 1, prec = data_prec ,missval = CONSTANTS$missing_vals[[data_prec]])
    nc_var_list[[1]] <- nc_var
  }
  else{
    # FOR 2D data
    for(n in 1:nlayers(raster_obj)){
      r <- raster_obj[[n]]
      data_type <- dataType(r)
      data_prec <- nc.data.get.precision(data_type)
      dim_list <- list(dim_x,dim_y)
      unit <- r@data@unit
      if(is.null(unit) || unit=="" ){
        unit <- "-"
      }
      long_name <- r@title
      if(length(long_name)==0){
        long_name <- names(r)[1]
      }
      nc_var <- ncvar_def(name = names(r)[1],dim = dim_list, units = unit,prec =data_prec,longname = long_name,compression = 1, missval = CONSTANTS$missing_vals[[data_prec]])
      nc_var_list[[as.character(n)]] <- nc_var
    }
  }

  if(do_append){
    # file is already open
    for(i_var in nc_var_list){
      ncvar_add(nc,i_var)
    }
    nc_close(nc)
    nc <- nc_open(file_name, write=TRUE)
  }
  else{
    nc <- nc_create(file_name, nc_var_list)
  }

  if(VICSetup$grid$reverse_y){
    raster_obj <- flip(raster_obj,direction = 'y' )
  }

  if(is.null(dim_time)){
    for(n in 1:nlayers(raster_obj)){
      r <- raster_obj[[n]]
      ncvar_put(nc, nc_var_list[[n]], vals=as.vector(r))
      ncatt_put(nc,nc_var_list[[n]], attname = 'grid_mapping',grid_mapping$grid_mapping_name)
      ncatt_put(nc,nc_var_list[[n]], attname = 'missing_value', attval = CONSTANTS$missing_vals[[nc_var_list[[n]]$prec]], prec = nc_var_list[[n]]$prec)
      #ncatt_put(nc,nc_var_list[[n]], attname = '_FillValue', attval = 1e20, prec = 'double')
    }
  }
  else{
    ncvar_put(nc, nc_var_list[[1]], vals=as.vector(raster_obj))
    ncatt_put(nc,nc_var_list[[1]], attname = 'grid_mapping',grid_mapping$grid_mapping_name)
    ncatt_put(nc,nc_var_list[[1]], attname = 'missing_value', attval = 1e20, prec = 'double')
    #ncatt_put(nc,nc_var_list[[1]], attname = '_FillValue', attval = 1e20, prec = 'double')
  }


  # add grid_mapping variable
  if(!do_append){
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
    ncatt_put(nc,varid=0, attname = 'institution', attval = 'Wageningen University and Research', prec='char')
    ncatt_put(nc,varid = 0, attname = 'date_created', attval = date(), prec='char')
    ncatt_put(nc, varid = 0, attname = 'user', attval = Sys.getenv("USERNAME"), prec = 'char')
    ncatt_put(nc, varid= 0, attname='R_version',attval = as.character(getRversion()), prec = 'char')
    ncatt_put(nc, varid = 0, attname = 'R_package_name', attval = 'VICsetup', prec = 'char')
    ncatt_put(nc, varid = 0, attname = 'references', attval = 'https://github.com/nauta008/VIC_setup', prec = 'char')
    log_info(sprintf("Output %s created.", file_name))
  }
  else{
    log_info(sprintf("Output %s appended", file_name))
  }

  nc_close(nc)

}

nc.data.get.precision <- function(raster_data_type){
  data_prec <- "integer"
  if(any(raster_data_type== c("INT1S","INT1U"))){
    data_prec = 'byte'
  }
  else if(any(raster_data_type== c("INT2S","IN21U"))){
    data_prec <- 'short'
  }
  else if(raster_data_type == "FLT4S"){
    data_prec <- "float"
  }
  else if(raster_data_type == 'FLT8S'){
    data_prec <- 'double'
  }
  return(data_prec)
}
