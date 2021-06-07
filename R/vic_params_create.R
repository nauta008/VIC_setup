
vic.params.create <- function(){
  # run cells must match mask from domain
  run_cells <- nc.data.get.stars(VICSetup$config$params$file,var= "run_cell")
  vic_params_dims <- st_dimensions(run_cells)
  raster_type <- st_raster_type(run_cells)
  vic_params_crs <- st_crs(run_cells)
  vic_params_proj4 <- vic_params_crs$proj4string
  st_domain <- st_as_stars(VICSetup$grid$raster)
  if(nlayers(VICSetup$grid$raster)>1){
    st_domain <- split(st_domain,"band")
    st_domain %>% select(mask) -> mask
  }

  nc_params <- nc_open(VICSetup$config$params$file)

  out_file_name <- file.path(VICSetup$config$output$path,VICSetup$config$output$params$file)
  if(file.exists(out_file_name)){
    file.remove(out_file_name)
  }
  nc.data.create.file(out_file_name, VICSetup$grid$grid_mapping)

  for(vic_param in nc_params$var){
    var_name <- vic_param$name
    if(tolower(var_name) %in% tolower(vic_model_params$name)){
      ndims <- vic_param$ndims
      # TODO: can be optimized by subsetting for potentially cropped domain
      param <- nc.data.get.stars(VICSetup$config$params$file, var= var_name)

      # we need to warp/remap
      if(vic_params_proj4!=VICSetup$grid$proj4){
        log_warn(sprintf("Projection of %s does not match projection of model domain. VIC params are remapped using nearest-neighbor interpolation.", var_name))
        if(var_name == "LAI"){
          param <- st_warp(param, mask)
        }
        else{
          param <- st_warp(param, mask)
        }

      }
      # for now assume equal dimensions. TODO: check if the grid (reso, x and y) match domain grid
      else{
        log_warn(sprintf("Assume grid VIC param %s equals the final domain grid. Please check output.", var_name))
      }
      # now check for missing values
      missing <- (is.na(param) != is.na(mask)) * mask
      missing_cells_idx <- which(missing[[var_name]]==1, arr.ind = T)

      if(length(missing_cells_idx>0) ){
        log_warn(sprintf("Found %s missing run cells in param file compared to model domain.", dim(missing_cells_idx)[1]))
        log_info(sprintf("Use nearest-neighbor interpolation to fill gaps in %s",var_name))

        if(length(ndims)>2 && length(ndims)<5){
          if(ndims==3){
            n_int <-vic_param$dim[[3]]$len
            for(j in 1:vic_param$dim[[3]]$len){
              sub_param <- param %>% slice(vic_param$dim[[3]]$name,j)
              param[[var_name]][,,j] <- vic.params.fill(sub_param, var_name)
            }
          }
          else if(ndims==4){
            n_int <-vic_param$dim[[3]]$len * vic_param$dim[[4]]$len
            count <- 0
            for(j in 1:vic_param$dim[[3]]$len){
              for(k in 1:vic_param$dim[[4]]$len){
                count <- count + 1
                log_debug(sprintf("Start interpolation %s/%s for %s",count,n_int,))
                sub_param <- param %>% slice(vic_param$dim[[3]]$name,j) %>% slice(vic_param$dim[[4]]$name,k)
                param[[var_name]][,,j,k] <- vic.params.fill(sub_param, var_name)
              }
            }
          }
        }
        else if(ndims==2){
          param[[var_name]] <- vic.params.fill(param, var_name)

        }
        else{
          log_warn(sprintf("Skipping spatial interpolation of %s. Code does not support spatial interpolation for %s dimensions",var_name, ndims))
        }
      }

      # mask to domain mask
      param <- param * mask
      nc.data.write.stars(param,out_file_name,VICSetup$grid$grid_mapping,vic_model_params)

    }

  }

  nc.data.write.lonlat(out_file_name, VICSetup$grid$proj4, VICSetup$grid$grid_mapping)
}


vic.params.fill <- function(param_2d, var_name){
  param_pts <- st_centroid(st_as_sf(param_2d))
  gs <- gstat(formula=as.formula(paste(var_name, "~ 1")), locations=param_pts) # nearest-neighbor when idw power not given?
  # get interpolated raster
  r_param <- interpolate(VICSetup$grid$raster, gs)
  # convert to stars object
  st_param <- st_as_stars(r_param)
  return(st_param)
}
