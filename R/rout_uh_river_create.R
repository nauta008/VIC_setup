
rout.uh.river.create <- function(distance, velocity=1,diffusion =2000){

  int_sec <- 3600*24 / as.integer(VICSetup$config$output$routing$steps_per_day)
  times <- cumsum(rep(int_sec, VICSetup$config$output$routing$steps_per_day *VICSetup$config$output$routing$ndays))
  times <- c(0, times)

  # Calculate
  uh_river_map <- array(NA, dim = c(dim(distance)[1], dim(distance)[2], length(times)))
  for (x in 1:dim(uh_river_map)[1]) {
    log_debug(sprintf("Calc uh river for [%s,]",x))
    for (y in 1:dim(uh_river_map)[2]) {
      if (is.na(distance[x, y])) {
        next
      }

      uh_river <- rout.uh.river.get(velocity, diffusion, distance[x, y], times)
      uh_river_map[x, y, ] <- uh_river$Fraction / sum(uh_river$Fraction)
    }
  }
  return(uh_river_map)
}

rout.uh.river.get <- function(velocity, diffusion, distance, time){

  uh <- rep(0, length(time))

  h <- apply(array(time, dim = c(length(time))), MARGIN = 1, FUN = rout.calc.h, velocity = velocity, diffusion = diffusion, distance = distance)

  uh <- h / sum(h)

  return(data.frame(Time = time - time[1], Fraction = uh))
}

rout.calc.h <- function(time, velocity, diffusion, distance){
  pot <- ((velocity * time - distance)^2) / (4 * diffusion * time)

  if (pot <= 69) {
    h <- 1 / (2 * sqrt(pi * diffusion)) * distance / (time^1.5) * exp(-pot)
  } else {
    h <- 0
  }
  return(h)

}




