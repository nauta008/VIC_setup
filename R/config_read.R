

CONSTANTS <- list(routing=list())
routing.origin=list()
routing.origin[['LISFLOOD']] <- list(outlet_val=5)
routing.origin[['RVIC']] <- list(outlet_val=9)
routing.origin[['ARCMAP']] <- list(outlet_val=9)

CONSTANTS$routing[['origin']] <- routing.origin
rm(routing.origin)

VICSetup <- new.env()

config.read <- function(file){
  config.restore()
  VICSetup$config <- read_yaml(file)
  domain.init()
}

config.restore <- function(){
  rm(list = ls(envir=VICSetup), envir = VICSetup)
}
