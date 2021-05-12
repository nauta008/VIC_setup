

proj.to.grid.mapping <- function(proj4_params){

  if(typeof(proj4_params)!='character'){
    log_error("proj4_params must be a of type character")
    stop()
  }

  proj4_args <- proj.get.args(proj4_params)
  if(!"proj" %in% names(proj4_args)){
    log_error("not a valid CRS")
    stop()
  }
  grid_mapping_name <- proj.grid.mapping.get.name(proj4_args$proj)
  grid_mapping <- NULL
  if(grid_mapping_name=='crs'){
    grid_mapping <- proj.grid.mapping.set.crs(proj4_params)
  }
  else{
    grid_mapping <- proj.grid.mapping.set(grid_mapping_name, proj4_args)
  }
  return(grid_mapping)

}


proj.get.args <- function(proj4_params){
  if(typeof(proj4_params)!='character'){
    log_error("proj4_params must be a of type character")
    stop()
  }

  args <- unique(unlist(strsplit(proj4_params, " ")))
  argList <- list()

  for(arg in args) {
    a <- unlist(strsplit(sub("\\+", "", arg), "="))
    argList[a[1]] <- a[2]
  }
  return(argList)

}

proj.grid.mapping.set <- function(name, proj4_args){
  grid_mapping <- NULL
  if(name=='latitude_longitude'){
    #proj.grid.mapping.set.longlat(grid_mapping,proj4_args)
    grid_mapping <- ncmeta:::GGFP.latitude_longitude(proj4_args)

  }
  else if(name=='lambert_azimuthal_equal_area'){
    #proj.grid.mapping.set.laea(grid_mapping, proj4_args)
    grid_mapping <- ncmeta:::GGFP.lambert_azimuthal_equal_area(proj4_args)
  }
  return(grid_mapping)
}


proj.grid.mapping.set.crs <- function(proj4_params){
  log_error("Method not yet supported.")
  stop()
}


proj.grid.mapping.get.name <- function(proj_value){

  grid_mapping_name <- CONSTANTS$grid_mapping$lookup[[proj_value]]
  if(is.null(grid_mapping_name)){
    log_warn(sprintf("%s not supported. Set grid_mapping to crs"),proj_value)
    grid_mapping_name <- "crs"
  }
  return(grid_mapping_name)
}
