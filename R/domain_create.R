
domain.create <- function(){
  settings <- VICSetup$config$output$domain
  out_file <- file.path(VICSetup$config$output$path, VICSetup$config$output$domain$file)
  if(!is.null(settings$crop)){
    if(settings$crop$by=='basin'){
      domain.crop.by.basin(settings)
    }
  }

  nc.data.write(VICSetup$grid$raster, out_file)
}
