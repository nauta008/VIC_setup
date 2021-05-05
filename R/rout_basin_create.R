
rout.basin.create <- function(downstream){

  # Calculate
  basin <- array(NA,dim = dim(downstream)[1:2])
  cell_warnings <- list()
  id.counter <- 0
  for (i in 1:dim(downstream)[1]) {
    log_debug(sprintf("Running basin routine for cell [%s,]", i))
    for (j in 1:dim(downstream)[2]) {
      if (is.na(downstream[i, j, 1])) {
        next
      }

      # current cell
      cur <- c(i, j)
      # next cell in stream direction
      nex <- downstream[i, j, ]
      river <- list()

      # follow stream
      while (TRUE) {
        # store current river cell
        river[[length(river) + 1]] <- cur
        # if cell has already basin id, store id and stop loop.
        # TODO: if statement outside while routine?
        if (!is.na(basin[cur[1], cur[2]])) {
          id <- basin[cur[1], cur[2]]
          break
        }
        # check if next cell is current cell. i.e. is outlet. Set id, raise counter and stop follow river loop.
        if (cur[1] == nex[1] && cur[2] == nex[2]) {
          id <- id.counter
          id.counter <- id.counter + 1
          # stop follow river loop
          break
        }

        arr_river <- t(array(unlist(river), dim = c(2,length(river))))
        match1 <- which(arr_river[,1]==nex[1])
        match2 <- which(arr_river[,2]==nex[2])
        if(any(match1 %in% match2)){
          log_warn(sprintf("found circular flow at [%s,%s]",nex[1],nex[2]))
          break
        }
        # current cell is next downstream cell
        cur <- nex
        nex <- downstream[cur[1], cur[2], ]
      }

      for (k in 1:length(river)) {
        r.x <- river[[k]][1]
        r.y <- river[[k]][2]
        # set all cells of river to same basin id.
        basin[r.x, r.y] <- id
      }
    }
  }
  return(basin)
}
