
VICSetup <- new.env()


config.read <- function(file){
  VICSetup$config <- read_yaml(file)
}
