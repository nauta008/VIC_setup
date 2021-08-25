
nc.data.write <- function(data_obj, file_name, var_attr=NULL){

  if(file.exists(file_name)){
    file.remove(file_name)
  }

  item <- data_obj
  data_type <- typeof(data_obj)
  data_class <- NULL
  if(data_type=="list"){
    item <- data_obj[[1]]
  }
  if(class(item)=="stars"){
    crs_item <- crs(st_crs(item)$input)
    data_class <- "stars"
  }
  else{
    crs_item <- crs(item)
    data_class <- "raster"
  }

  proj4_params <- CRSargs(crs_item)
  grid_mapping <- proj.to.grid.mapping(proj4_params)
  nc.data.create.file(file_name, grid_mapping)

  if(data_type=='list'){
    count <- 1
    for(data_item in data_obj){
      var_name <- names(data_obj)[count]
      if(data_class=="stars"){
        nc.data.write.stars(data_item,file_name, grid_mapping)
      }
      # assume raster
      else{
        nc.data.write.raster(data_item, file_name, grid_mapping ,var_name=var_name)
      }

      count = count + 1
    }
  }
  else{
    if(class(data_obj)=="stars"){
      nc.data.write.stars(data_obj,file_name, grid_mapping)
    }
    else{
      nc.data.write.raster(data_obj, file_name, grid_mapping)
    }

  }

  nc.data.write.lonlat(file_name, proj4_params, grid_mapping)
  log_info(sprintf("Output %s created.", file_name))

}


nc.data.write.raster <- function(raster_obj, file_name, grid_mapping ,var_name="dummy"){

  # get crs
  proj4_params <- CRSargs(crs(raster_obj))
  #grid_mapping <- proj.to.grid.mapping(proj4_params)

  bbox <- st_bbox(c(xmin=raster_obj@extent@xmin, xmax=raster_obj@extent@xmax, ymin=raster_obj@extent@ymin, ymax=raster_obj@extent@ymax), st_crs(proj4_params))
  reso <- res(raster_obj)
  grid_dim <- nc.data.get.grid.dims(file_name, proj4_params, bbox,  reso)

  dim_x <- grid_dim$x
  dim_y <- grid_dim$y

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

  for(i_var in nc_var_list){
    nc <- nc_open(file_name, write = TRUE)
    ncvar_add(nc,i_var)
    nc_close(nc)
  }

  nc <- nc_open(file_name, write=TRUE)

  if(VICSetup$grid$reverse_y){
    raster_obj <- flip(raster_obj,direction = 'y' )
  }

  if(is.null(dim_time)){
    for(n in 1:nlayers(raster_obj)){
      r <- raster_obj[[n]]
      ncvar_put(nc, nc_var_list[[n]], vals=as.vector(r))
      ncatt_put(nc,nc_var_list[[n]], attname = 'grid_mapping',grid_mapping$grid_mapping_name)
      ncatt_put(nc,nc_var_list[[n]], attname = 'missing_value', attval = CONSTANTS$missing_vals[[nc_var_list[[n]]$prec]], prec = nc_var_list[[n]]$prec)
    }
  }
  else{
    ncvar_put(nc, nc_var_list[[1]], vals=as.vector(raster_obj))
    ncatt_put(nc,nc_var_list[[1]], attname = 'grid_mapping',grid_mapping$grid_mapping_name)
    ncatt_put(nc,nc_var_list[[1]], attname = 'missing_value', attval = CONSTANTS$missing_vals[[nc_var_list[[1]]$prec]], prec = nc_var_list[[1]]$prec)
  }

  nc_close(nc)

}

nc.data.write.stars <- function(stars_data,file_name, grid_mapping, var_attr=NULL){

  var_names <- names(stars_data)
  data_dims <- st_dimensions(stars_data)
  data_dim_names <- names(data_dims)

  nc <- nc_open(file_name, write=T)
  file_dim_names <- names(nc$dim)

  nc_dim_list <- list()
  for(dim_name in data_dim_names){
    dim_attr <- data_dims[[dim_name]]
    if(dim_name %in% file_dim_names){
      nc_dim <- nc$dim[[dim_name]]
    }
    else{
      nc_dim <- ncdim_def(dim_name, units = "-",vals = st_get_dimension_values(stars_data,which = dim_name))
    }
    nc_dim_list[[dim_name]] <- nc_dim
  }

  nc_close(nc)

  data_prec <- "float"
  unit_name <- ""
  for(var_name in var_names){
    nc <- nc_open(file_name, write = T)
    # get variable attributes
    long_name <- var_name
    if(!is.null(var_attr) && any(tolower(var_attr$name) == tolower(var_name))){
      tmp_attr <- var_attr
      tmp_attr$name <- tolower(tmp_attr$name)

      tmp_attr %>% filter(name==tolower(var_name)) -> i_var_attr
      long_name <- i_var_attr$long_name
      unit_name <- i_var_attr$unit
      data_prec <- i_var_attr$data_type
    }
    nc_var <- ncvar_def(var_name,units = unit_name,dim = nc_dim_list ,longname = long_name,prec = data_prec ,missval =  CONSTANTS$missing_vals[[data_prec]], compression = 1)
    ncvar_add(nc,nc_var)
    nc_close(nc)
  }

  nc <- nc_open(file_name, write=T)
  for(var_name in var_names){
    var_data <- stars_data[[var_name]]
    # Try solution for 4D data
    if(length(dim(var_data))==4){
      for(i_dim in 1:dim(var_data)[4]){
        ncvar_put(nc,var_name, as.vector(var_data[,,,i_dim]), start = c(1,1,1,i_dim), count = c(-1,-1,-1,1))
      }
    }
    else{
      ncvar_put(nc,var_name, as.vector(var_data))
    }

    # TODO: add description from the var_attr
    ncatt_put(nc,var_name, attname = 'grid_mapping',grid_mapping$grid_mapping_name)
    ncatt_put(nc,var_name, attname = 'missing_value', attval = CONSTANTS$missing_vals[[data_prec]], prec = data_prec)
  }
  nc_close(nc)
  log_info(sprintf("Variables %s written to %s", var_names, file_name))
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

nc.data.write.lonlat <- function(file_name, proj4_params, grid_mapping){
  #TODO: determine of latlon grid. If not check for lat lon variables in netcdf. If not present transform x,y to lon and lat and write variables.
  nc <- nc_open(file_name, write=T)
  r_latlon <- NULL
  if(!isLonLat(crs(nc_gm_to_prj(grid_mapping))) && !all(c("lat","lon") %in% names(nc$var))){
    axes <- proj.get.axes.names(proj4_params)
    x_vals <- nc$dim[[axes$x$name]]$vals
    y_vals <- nc$dim[[axes$y$name]]$vals
    pts <- SpatialPoints(expand.grid(x=x_vals,y=y_vals), crs(proj4_params))
    r <- rasterFromCells(SpatialPixels(pts), cells=seq(1,length(pts)))
    r_latlon <- raster.latlon.transform(r,axes$x$name, axes$y$name)
    if(VICSetup$grid$reverse_y){
      r_latlon <- flip(r_latlon,direction = "y")
    }
  }

  nc_close(nc)
  if(!is.null(r_latlon)){
    nc.data.write.raster(raster::subset(r_latlon,c("lat","lon")), file_name,grid_mapping)
  }
}

