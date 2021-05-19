
raster.create <- function(arr,name, longname=NULL,unit="", data_type=NULL, mask_area=TRUE){
  # transpose array, because raster package uses latitude (2nd dim = y) as rows (1st dim)
  r <- raster(t(arr), template=VICSetup$grid$raster)
  if(mask_area){
    r <- r*VICSetup$grid$raster$mask
  }
  names(r) <- name
  r@data@unit <- unit
  r@title <- longname
  if(!is.null(data_type)){
    dataType(r) <- data_type
  }
  return(r)
}
