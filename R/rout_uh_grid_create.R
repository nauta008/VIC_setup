
rout.uh.grid.create <- function(slope,distance){

  uh <- dimensionless_uh
  # Setup WATERSIS
  times <- cumsum(rep(3600, 24 * 2))
  times <- c(0,times)
  # Setup BRAM
  # times <- cumsum(rep(3600, 24 * 7))
  # times <- c(0, times)

  interp <- function(x, x1, x2, y1, y2) {
    if (x < x1[1]) {
      return(y1[1])
    }
    if (x > x2[length(x2)]) {
      return(y2[length(y2)])
    }

    idx <- max(which(x1 - x <= 0))
    xdist <- x2[idx] - x1[idx]
    xfrac <- (x - x1[idx]) / xdist
    ydist <- y2[idx] - y1[idx]
    return(y1[idx] + ydist * xfrac)
  }

  get.grid.tp <- function(local_distance, local_slope) {
    if (local_slope == 0) {
      local_slope <- 1e-10
    }

    tc <- ((0.01947 * local_distance)^0.77) / (local_slope^0.385)
    tp <- 0.6 * tc
    tp <- tp * 60
    return(tp)
  }

  get.grid.uh <- function(tp) {
    uh_grid_temp <- uh
    uh_grid_temp[, 1] <- uh_grid_temp[, 1] * tp

    uh_grid <- data.frame(Time = times, Fraction = rep(NA, length(times)))
    uh_grid$Fraction <- apply(
      X = uh_grid[, 1, drop = F], MARGIN = 1, FUN = interp,
      x1 = uh_grid_temp[1:(nrow(uh_grid_temp) - 1), 1],
      x2 = uh_grid_temp[2:nrow(uh_grid_temp), 1],
      y1 = uh_grid_temp[1:(nrow(uh_grid_temp) - 1), 2],
      y2 = uh_grid_temp[2:nrow(uh_grid_temp), 2]
    )

    uh_grid$Fraction <- uh_grid$Fraction / sum(uh_grid$Fraction)
    return(uh_grid)
  }

  uh_grid_map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(times)))

  for (x in 1:dim(uh_grid_map)[1]) {
    log_debug(sprintf("Calc uh_grid for [%s,]",x))
    for (y in 1:dim(uh_grid_map)[2]) {
      if (is.na(slope[x, y]) || is.na(distance[x,y])) {
        next
      }

      tp_grid <- get.grid.tp(distance[x, y], slope[x, y])
      uh_grid <- get.grid.uh(tp_grid)

      uh_grid_map[x, y, ] <- uh_grid$Fraction / sum(uh_grid$Fraction)
    }
  }

  return(uh_grid_map)

}
