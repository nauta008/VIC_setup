
nc.data.get.grid.dims <- function(file_name, proj4_params,bbox,reso){

  dim_list <- list()
  dim_names <- c()

  if(file.exists(file_name)){
    nc <- nc_open(file_name)
    dim_list <- nc$dim
    dim_names <- names(dim_list)
    nc_close(nc)
  }
  else if(!file.exists(file_name)){
    log_error(sprintf("File %s not found",file_name))
    stop()
  }

  axes <- proj.get.axes.names(proj4_params)

  if(!axes$x$name %in% dim_names){
    x_vals <- seq(from=bbox$xmin + abs(reso[1])/2,
                  to=bbox$xmax- abs(reso[1])/2, by=abs(reso[1]))
    dim_x <- ncdim_def(name = axes$x$name,units = axes$x$unit, vals = x_vals, longname = axes$x$long_name)
  }
  else{
    dim_x <- nc$dim[[axes$x$name]]
  }
  if(!axes$y$name %in% dim_names){
    y_vals <- seq(from=bbox$ymin + abs(reso[2])/2,
                  to=bbox$ymax- abs(reso[2])/2, by=abs(reso[2]))
    dim_y <- ncdim_def(name = axes$y$name,units = axes$y$unit, vals = y_vals, longname = axes$y$long_name)
  }
  else{
    dim_y <- nc$dim[[axes$y$name]]
  }

  return(list(x=dim_x, y=dim_y))

}
