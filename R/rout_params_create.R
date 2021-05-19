
rout.params.create <- function(out_uh=TRUE,out_basins=TRUE,write_file=TRUE){
  routing <- list()
  routing_rasters <- list()
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

  mask <- domain$mask
  # mask the routing variables
  flow_dir <- flow_dir[[VICSetup$config$routing$direction$var]]
  slope <-  slope[[VICSetup$config$routing$slope$var]]
  distance <- distance[[VICSetup$config$routing$distance$var]]

  # always create downstream
  raster_downstream <- stack()
  routing$downstream <- rout.downstream.create(flow_dir, mask, VICSetup$grid$reverse_y)

  downstream_id <- rout.downstream.id.create(routing$downstream)
  r_downstream_id <- raster.create(downstream_id,"downstream_id","ID used to identify downstream cell","-" ,"INT4S")

  downstream_2d <- rout.downstream.2d.create(routing$downstream, downstream_id)
  r_downstream <- raster.create(downstream_2d, "downstream","ID of the downstream cell","-" ,"INT4S")

  r_slope <- raster.create(slope,"gradient","channel gradient","-","FLT4S")

  raster_downstream <- addLayer(raster_downstream,r_downstream,r_downstream_id, r_slope)



  # add the lat and lon for x,y grids
  if(!VICSetup$grid$isLonLat){
    raster_downstream <- addLayer(raster_downstream, VICSetup$grid$raster$lon)
    raster_downstream <- addLayer(raster_downstream, VICSetup$grid$raster$lat)
  }

  # optional output
  if(out_basins){
    routing$basins <- rout.basin.create(routing$downstream)
    r_basins <- raster.create(routing$basins,'basin_id',"ID of the basin","-","INT4S")
    raster_downstream <- addLayer(raster_downstream,r_basins)
  }

  # optional output
  if(out_uh){
    routing$uh_grid <- rout.uh.grid.create(slope, distance) # uh_runoff
    routing$uh_river <- rout.uh.river.create(distance) # uh_inflow
    time <- (1:dim(routing$uh_grid)[3] -1) * 3600
    r_uh_grid <- stack()
    r_uh_river <- stack()
    for(i in 1:length(time)){
      r <- raster.create(routing$uh_grid[,,i], "uh_runoff","Unit hydrograph of the grid (cell)","-","FLT4S")
      r_uh_grid <- addLayer(r_uh_grid,r)

      r <- raster.create(routing$uh_river[,,i],"uh_inflow_%s","Unit hydrograph of the river (river)","-","FLT4S" )
      r_uh_river <- addLayer(r_uh_river, r)
    }

    r_uh_grid <- setZ(r_uh_grid,time,name = "time")
    r_uh_river <- setZ(r_uh_river,time,name = "time")
    routing_rasters$uh_runoff <- r_uh_grid
    routing_rasters$uh_inflow <- r_uh_river
  }

  VICSetup$routing <- routing
  routing_rasters$downstream <- raster_downstream
  # create raster stack
  if(write_file){
    out_file <- file.path(VICSetup$config$output$path,VICSetup$config$output$routing$file)
    nc.data.write(routing_rasters, out_file)
  }

}

