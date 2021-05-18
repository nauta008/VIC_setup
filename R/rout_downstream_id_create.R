
rout.downstream.id.create <- function(downstream){
  downstream_id <- array(NA, dim = c(dim(downstream)[1], dim(downstream)[2]))
  id_counter <- 1
  for (x in 1:dim(downstream)[1]) {
    for (y in 1:dim(downstream)[2]) {
      if (is.na(downstream[x, y, 1])) {
        next
      }
      downstream_id[x, y] <- id_counter
      id_counter <- id_counter + 1
    }
  }
  return(downstream_id)
}
