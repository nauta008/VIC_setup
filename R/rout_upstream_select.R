
rout.upstream.select <- function(xy, downstream, basins, raster=VICSetup$grid$raster){

  # only one item
  if( is.null(dim(xy)) && length(xy)==2){
    xy <- array(xy, dim = c(1,2))
  }

  upstreams <- stack()

  for(xy_idx in 1:dim(xy)[1]){
    x <- xy[xy_idx,1]
    y <- xy[xy_idx,2]
    downstream_c <- downstream
    cell <- cellFromXY(raster, xy[xy_idx,])
    rowcol <- rowColFromCell(raster, cell)
    i <- rowcol[2]
    j <- rowcol[1]
    basin_mask <- basins==basins[cell]
    # set cell to outlet. NOTE: for raster package the y coordinates are rows. So raster col is first dimension and row is second dimension.
    downstream_c[i,j,] <- c(i,j)
    downstream_c[basin_mask==FALSE] <- NA

    sub_basins <-  rout.basin.create(downstream_c)
    upstream_sub_basin <- sub_basins[cell]
    sub_basins[sub_basins!=upstream_sub_basin] <- NA

    sub_basins <- as.integer(sub_basins)

    r_sub_mask <- raster(t(sub_basins), template=raster)
    names(r_sub_mask) <- "mask"
    upstreams <- addLayer(upstreams, r_sub_mask)
  }

  if(nlayers(upstreams)==1){
    upstreams <- upstreams[[1]]
  }

  return(upstreams)

}


