
rout.params.create <- function(){
  routing <- list()
  if(is.null(VICSetup$grid)){
    domain.init()
  }
  domain <- st_as_stars(VICSetup$domain$raster)
  # for grid mapping other than lat lon
  if(nlayers(VICSetup$domain$raster)>1){
    domain <- split(domain,"band")
  }
  dim_mask <- st_dimensions(mask)

  flow_dir <- nc.data.get.stars(VICSetup$config$routing$direction$file, VICSetup$config$routing$direction$var)
  distance <- nc.data.get.stars(VICSetup$config$routing$distance$file, VICSetup$config$routing$distance$var)
  slope <- nc.data.get.stars(VICSetup$config$routing$slope$file, VICSetup$config$routing$slope$var)

  if(!identical(dim_mask,st_dimensions(flow_dir)) || !identical(dim_mask, st_dimensions(distance)) || !identical(dim_mask,st_dimensions(slope))){
    stop("Dimensions of input routing files does not match.")
  }

  mask <- domain$mask
  flow_dir <- flow_dir[[VICSetup$config$routing$direction$var]]
  slope <-  slope[[VICSetup$config$routing$slope$var]]
  distance <- distance[[VICSetup$config$routing$distance$var]]

  routing$downstream <- rout.downstream.create(flow_dir, mask, VICSetup$grid$reverse_y)
  #routing$basins <- rout.basin.create(routing$downstream)

  uh_grid_map <- rout.uh.grid.create(slope, distance)
  VICSetup$routing <- routing
}

