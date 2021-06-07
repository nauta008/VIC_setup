
domain.create <- function(){
  settings <- VICSetup$config$output$domain
  out_file <- file.path(VICSetup$config$output$path, VICSetup$config$output$domain$file)
  if(!is.null(settings$crop)){
    if(settings$crop$by=='basin'){
      domain.crop.by.basin(settings)
    }
  }
  # add area
  if(VICSetup$grid$isLonLat){
    area <- raster::as.array(area(VICSetup$grid$raster))
    area <- area[,,1]
  }
  else{
    area <- array(abs(prod(VICSetup$grid$resolution)), dim = c(ncol(VICSetup$grid$raster),nrow(VICSetup$grid$raster)))
  }
  r_area <- raster.create(area,name = "area",longname = "area of grid cell",unit = "m2",data_type = "FLT4S")
  frac <- array(1, dim = c(ncol(VICSetup$grid$raster),nrow(VICSetup$grid$raster)))
  r_frac <- raster.create(frac,"frac","fraction of grid cell that is active","-", data_type = "FLT4S")
  VICSetup$grid$raster <- addLayer(VICSetup$grid$raster,r_area,r_frac)

  nc.data.write(VICSetup$grid$raster, out_file)
}
