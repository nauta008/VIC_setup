
rout.uh.river.get <- function(velocity, diffusion, distance, time){
  uh <- rep(0, length(time))

  h <- apply(array(time, dim = c(length(time))), MARGIN = 1, FUN = rout.calc.h, velocity = velocity, diffusion = diffusion, distance = distance)

  uh <- h / sum(h)

  return(data.frame(Time = time - time[1], Fraction = uh))
}
