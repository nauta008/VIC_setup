
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
