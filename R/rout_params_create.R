
rout.params.create <- function(out_uh=TRUE){
  routing <- list()
  if(is.null(VICSetup$grid)){
    domain.init()
  }
  domain <- st_as_stars(VICSetup$grid$raster)
  # for grid mapping other than lat lon
  if(nlayers(VICSetup$grid$raster)>1){
    domain <- split(domain,"band")
  }
  dim_mask <- st_dimensions(domain)

  flow_dir <- nc.data.get.stars(VICSetup$config$routing$direction$file, VICSetup$config$routing$direction$var)
  distance <- nc.data.get.stars(VICSetup$config$routing$distance$file, VICSetup$config$routing$distance$var)
  slope <- nc.data.get.stars(VICSetup$config$routing$slope$file, VICSetup$config$routing$slope$var)

  dim_flow_dir <- st_dimensions(flow_dir)
  dim_distance <- st_dimensions(distance)
  dim_slope <- st_dimensions(slope)

  if(!isTRUE(all.equal(dim_mask[[1]]$refsys, dim_flow_dir[[1]]$refsys)) ||
     !isTRUE(all.equal(dim_mask[[1]]$refsys, dim_distance[[1]]$refsys)) ||
     !isTRUE(all.equal(dim_mask[[1]]$refsys, dim_slope[[1]]$refsys))){
    log_error("Reference system of input routing files does not match.")
    stop()
  }
  else if(!all(dim(domain)==dim(flow_dir)) || !all(dim(domain)==dim(distance)) || !all(dim(domain)==dim(slope)) ){
    d <- st_as_sf(domain)
    flow_dir <- st_crop(flow_dir,d)
    distance <- st_crop(distance, d)
    slope <- st_crop(slope, d)
    log_info("Routing input files cropped to new domain.")
    rm(d)
  }

#
#   if(!isTRUE(all.equal(dim_mask,st_dimensions(flow_dir),check.names=FALSE)) ||
#       !isTRUE(all.equal(dim_mask, st_dimensions(distance), check.names=FALSE)) ||
#      !isTRUE(all.equal(dim_mask,st_dimensions(slope), check.names=FALSE))){
#     log_error("Dimensions of input routing files does not match.")
#     stop("Dimensions of input routing files does not match.")
#   }

  mask <- domain$mask
  flow_dir <- flow_dir[[VICSetup$config$routing$direction$var]]
  slope <-  slope[[VICSetup$config$routing$slope$var]]
  distance <- distance[[VICSetup$config$routing$distance$var]]

  routing$downstream <- rout.downstream.create(flow_dir, mask, VICSetup$grid$reverse_y)
  #routing$basins <- rout.basin.create(routing$downstream)

  if(out_uh){
    routing$uh_grid <- rout.uh.grid.create(slope, distance)
  }


  VICSetup$routing <- routing

}

