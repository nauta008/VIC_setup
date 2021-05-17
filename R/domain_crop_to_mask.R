
domain.crop.to.mask <- function(mask){
  if(!class(mask)=='RasterLayer'){
    log_error('argument mask should be of type RastterLayer')
    stop()
  }
  active_cells <- Which(mask==1, cells=TRUE)
  to_extent <- extentFromCells(mask, active_cells)
  new_domain <- extend(crop(mask, to_extent), c(1,1))
  domain.set(new_domain)
}


domain.crop.by.basin <- function(settings){
  if(is.null(settings$crop$outlet) || is.null(settings$crop$outlet$coords) || length(settings$crop$outlet$coords)!=2){
    log_error("Missing outlet or outlet coordinates in crop config.")
    stop()
  }

  if(is.null(VICSetup$routing$downstream)){
    rout.params.create(out_uh=FALSE)
  }
  basins <- rout.basin.create(VICSetup$routing$downstream)
  #r_basins <- raster(t(basins),template=VICSetup$grid$raster)
  r_upstream_cells <- rout.upstream.select(settings$crop$outlet$coords,VICSetup$routing$downstream, basins)
  domain.crop.to.mask(r_upstream_cells)
}
