
VICSetup <- new.env()
constants <- list(routing=list())
routing.origin=list()
routing.origin[['LISFLOOD']] <- list(outlet_val=5)
routing.origin[['RVIC']] <- list(outlet_val=9)
routing.origin[['ARCMAP']] <- list(outlet_val=9)

constants$routing[['origin']] <- routing.origin
VICSetup[['constants']] <- constants

config.read <- function(file){
  VICSetup$config <- read_yaml(file)
}
