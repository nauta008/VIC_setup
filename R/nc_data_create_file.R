

nc.data.create.file <- function(file_name, grid_mapping){

  nc_grid_mapping_var <- ncvar_def(grid_mapping$grid_mapping_name,units="",dim=list(),prec = 'char')

  nc <- nc_create(file_name,nc_grid_mapping_var, force_v4 = TRUE)

  for(grid_mapping_attr in names(grid_mapping)){
    ncatt_put(nc, nc_grid_mapping_var,attname = grid_mapping_attr, attval = grid_mapping[[grid_mapping_attr]])
  }
  #ncatt_put(nc, grid_mapping$grid_mapping_name, attname = "proj4_params", attval = proj4_params)

  # Add global attributes
  ncatt_put(nc,varid=0, attname = 'institution', attval = 'Wageningen University and Research', prec='char')
  ncatt_put(nc,varid = 0, attname = 'date_created', attval = date(), prec='char')
  ncatt_put(nc, varid = 0, attname = 'user', attval = Sys.getenv("USERNAME"), prec = 'char')
  ncatt_put(nc, varid= 0, attname='R_version',attval = as.character(getRversion()), prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'R_package_name', attval = 'VICsetup', prec = 'char')
  ncatt_put(nc, varid = 0, attname = 'VICsetup_version', attval = as.character(packageVersion('VICsetup')), prec='char')
  ncatt_put(nc, varid = 0, attname = 'references', attval = 'https://github.com/nauta008/VIC_setup', prec = 'char')

  nc_close(nc)

}
