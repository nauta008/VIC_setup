
rout.params.create <- function(){
  routing <- list()
  if(is.null(VICSetup$grid)){
    domain.set()
  }
  mask <- nc.data.get.stars(VICSetup$config$domain$file, VICSetup$config$domain$var)
  dim_mask <- st_dimensions(mask)
  flow_dir <- nc.data.get.stars(VICSetup$config$routing$direction$file, VICSetup$config$routing$direction$var)
  distance <- nc.data.get.stars(VICSetup$config$routing$distance$file, VICSetup$config$routing$distance$var)
  slope <- nc.data.get.stars(VICSetup$config$routing$slope$file, VICSetup$config$routing$slope$var)

  if(!identical(dim_mask,st_dimensions(flow_dir)) || !identical(dim_mask, st_dimensions(distance)) || !identical(dim_mask,st_dimensions(slope))){
    stop("Dimensions of input routing files does not match.")
  }

  mask <- nc.data.get(VICSetup$config$domain$file, VICSetup$config$domain$var)
  flow_dir <- nc.data.get(VICSetup$config$routing$direction$file, VICSetup$config$routing$direction$var)
  image(flow_dir)
  routing$downstream <- rout.downstream.create(flow_dir, mask, VICSetup$grid$reverse_y)
  routing$basins <- rout.basin.create(routing$downstream)

  VICSetup[['routing']] <- routing
}

