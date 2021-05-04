
nc.data.get <- function(file,var){
  nc <- nc_open(file)
  data <- ncvar_get(nc,var)
  nc_close(nc)
  return(data)
}


nc.data.get.stars <- function(file,var){
  stars_data <- read_ncdf(file,var = var)
  return(stars_data)
}
