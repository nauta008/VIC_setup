
rout.upstream.area.create <- function(downstream){

  # read or calc area
  if(VICSetup$grid$isLonLat){
    area <- raster::subset(VICSetup$grid$raster, "area")
  }
  else{
    area <- abs(prod(VICSetup$grid$resolution))
  }

  upstream_area <- array(0, dim = dim(downstream)[1:2])
  cell_count <- array(0, dim = dim(downstream)[1:2])
  for (x in 1:dim(downstream)[1]) {
    for (y in 1:dim(downstream)[2]) {
      if (is.na(downstream[x, y, 1])) {
        next
      }

      cur <- c(x, y)
      nex <- downstream[x, y, ]
      # for a regular x,y grid
      area_xy <- area
      # for a 2D area map
      if(VICSetup$grid$isLonLat){
        area_xy <- area[x,y]
      }
      count <- 0
      while (TRUE) {
        upstream_area[cur[1], cur[2]] <- upstream_area[cur[1], cur[2]] + area_xy
        cell_count[cur[1], cur[2]] <- cell_count[cur[1], cur[2]] + 1

        if (cur[1] == nex[1] && cur[2] == nex[2]) {
          break
        }

        cur <- nex
        nex <- downstream[cur[1], cur[2], ]
      }
    }
  }
  return(list(upstream_area = upstream_area, cell_count=cell_count))
}
