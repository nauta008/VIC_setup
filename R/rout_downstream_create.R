
rout.downstream.create <- function(flow_dir,mask,rev_y=FALSE){
  flow_dir_origin <- toupper(VICSetup$config$routing$direction$origin)
  log_info(sprintf('Using %s flow directions',flow_dir_origin))
  if(rev_y){
    log_info("Correct for upside down latitude orientation in downstream routine.")
  }
  downstream = array(NA, dim = c(dim(flow_dir), 2))

  for(x in 1:dim(mask)[1]){
    for(y in 1:dim(mask)[2]){
      # set flow_dir cell to NA if outside mask
      if(is.na(mask[x,y])){
        flow_dir[x,y] = NA
      }
      # set flow dir cell to outlet if flow_dir inside mask but has NA value
      else if (is.na(flow_dir[x,y])){
        flow_dir[x,y] = CONSTANTS$routing$origin[[flow_dir]]$outlet_val
      }
    }
  }

  for(x in 1:dim(flow_dir)[1]){
    log_debug(sprintf("Calc downstream for [%s,].",x))
    for(y in 1:dim(flow_dir)[2]){
      if(is.na(flow_dir[x,y])){
        next
      }

      dx_dy <- direction.to.index(flow_dir[x,y],flow_dir_origin,rev_y)
      next.cell = c(x,y) + dx_dy
      if(is.na(mask[next.cell[1], next.cell[2]])){
        next.cell = c(x,y)
      }

      downstream[x,y,] = next.cell
    }
  }
  return(downstream)
}


rout.downstream.2d.create <- function(downstream, downstream_id){
  downstream_2d <- array(NA, dim = dim(downstream_id))
  for (x in 1:dim(downstream)[1]) {
    for (y in 1:dim(downstream)[2]) {
      if (is.na(downstream[x, y, 1])) {
        next
      }
      downstream_cell <- downstream[x, y, ]
      downstream_2d[x, y] <- downstream_id[downstream_cell[1], downstream_cell[2]]
    }
  }
  return(downstream_2d)
}

direction.to.index <- function(flow_dir, flow_dir_origin ,rev_y=FALSE){

  index <- c(0,0)

  if(flow_dir_origin == 'RVIC'){
    index <- get.index.rvic(flow_dir)
  }
  else if(flow_dir_origin == 'ARCMAP'){
    index <- get.index.arcmap(flow_dir)
  }
  else if(flow_dir_origin == 'LISFLOOD'){
    index <- get.index.lisflood(flow_dir)
  }
  else{
    log.wan(sprintf('Flow direction %s not implemented. Set routing origin in config to RVIC, LISFLOOD or ARCMAP',flow_dir_origin))
  }

  if(rev_y){
    index[2] <- index[2]*-1 # -1 is north and 1
  }
  return(index)
}


get.index.rvic <- function(flow_dir){
  index <- switch(flow_dir,
                  c(0, 1), # north
                  c(1, 1), # north east
                  c(1, 0), # east
                  c(1, -1), # south east
                  c(0, -1), # south
                  c(-1, -1), # south west
                  c(-1, 0), # west
                  c(-1, 1), # north west
                  c(0, 0)
  ) # outlet
  return(index)
}

get.index.arcmap <- function(flow_dir){
  index <- c(0,0)
  if(flow_dir==1){
    index <- c(1,0) # east
  }
  else if(flow_dir==2){
    index <- c(1,-1) # south east
  }
  else if(flow_dir==4){
    index <- c(0,-1) # south
  }
  else if(flow_dir==8){
    index <- c(-1,-1)  # south west
  }
  else if(flow_dir==16){
    index <- c(0,-1) # west
  }
  else if(flow_dir==32){
    index <- c(1,-1) # north west
  }
  else if(flow_dir==64){
    index <- c(0,-1) # north
  }
  else if(flow_dir==128){
    index <- c(1,1) # north east
  }
  else{
    index <- c(0,0)
  }
  return(index)
}

get.index.lisflood <- function(flow_dir){
  if(flow_dir < 1 || flow_dir > 9){
    log_warn(sprintf("Direction %s is outside range [1-9], defaulting to 5 [outlet]",flow_dir))
    flow_dir = CONSTANTS$routing$origin$LISFLOOD$outlet_val
  }
  index = switch(flow_dir,
                 c(-1,-1), # south west
                 c(0,-1),  # south
                 c(1,-1),  # south east
                 c(-1,0),  # west
                 c(0,0),   # outlet
                 c(1,0),   # east
                 c(-1,1),  # north west
                 c(0,1),   # north
                 c(1,1))   # north east
  return(index)
}
