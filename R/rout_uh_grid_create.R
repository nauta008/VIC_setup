
rout.uh.grid.create <- function(slope,distance){
  uh <- dimensionless_uh

  int_sec <- 3600*24 / as.integer(VICSetup$config$output$routing$steps_per_day)
  times <- cumsum(rep(int_sec, VICSetup$config$output$routing$steps_per_day *VICSetup$config$output$routing$ndays))
  times <- c(0, times)

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
    # Kirpitch equation (1940)
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
  n_cell_warnings <- 0

  for (x in 1:dim(uh_grid_map)[1]) {
    log_debug(sprintf("Calc uh_grid for [%s,]",x))
    for (y in 1:dim(uh_grid_map)[2]) {
      if (is.na(slope[x, y]) || is.na(distance[x,y])) {
        #log_warn(sprintf("no slope and distance at [%s,%s]", x,y))
        next
      }
      tp_grid <- get.grid.tp(distance[x, y], slope[x, y])
      uh_grid <- get.grid.uh(tp_grid)
      # all water flows outside the cell within 1 hour
      if(all(is.na(uh_grid$Fraction))){
        n_cell_warnings <- n_cell_warnings + 1
        uh_grid$Fraction <- rep(0, length(uh_grid$Fraction))
        uh_grid$Fraction[1] <- 1
      }
      uh_grid_map[x, y, ] <- uh_grid$Fraction / sum(uh_grid$Fraction)
    }
  }

  if(n_cell_warnings > 0){
    log_warn(sprintf("Time to peak is smaller than the routing interval of %s [sec] at %s cells. All water flows outside the grid cell within first routing step. Consider raising the routing steps per day.",int_sec,n_cell_warnings))
  }

  return(uh_grid_map)

}
