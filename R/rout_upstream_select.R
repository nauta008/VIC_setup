
rout.upstream.select <- function(xy, downstream, basins){

  upstreams <- list()

  for(xy_idx in 1:dim(xy)[1]){
    x <- xy[xy_idx,1]
    y <- xy[xy_idx,2]
    downstream_c <- downstream
    cell <- cellFromXY(VICSetup$grid$raster, xy[xy_idx,])
    rowcol <- rowColFromCell(VICSetup$grid$raster, cell)
    i <- rowcol[2]
    j <- rowcol[1]
    basin_mask <- basins==basins[cell]
    # set cell to outlet. NOTE: for raster package the y coordinates are rows. So raster col is first dimension and row is second dimension.
    downstream_c[i,j,] <- c(i,j)
    downstream_c[basin_mask==FALSE] <- NA

    sub_basins <-  rout.basin.create(downstream_c)
    upstream_sub_basin <- sub_basins[cell]
    sub_basins[sub_basins!=upstream_sub_basin] <- NA
    upstreams[[xy_idx]] <- sub_basins
  }

  return(upstreams)

}


