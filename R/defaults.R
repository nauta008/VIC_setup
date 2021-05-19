

defaults.set <- function(){

  if(!is.null(VICSetup$config$output$routing)){
    defaults.routing.set()
  }
}



defaults.routing.set <- function(){
  if(is.null(VICSetup$config$output$routing$steps_per_day)){
    VICSetup$config$output$routing$steps_per_day <- 24

  }
  if(is.null(VICSetup$config$output$routing$ndays)){
    VICSetup$config$output$routing$ndays <- 7
  }
}
