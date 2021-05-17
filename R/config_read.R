

CONSTANTS <- list(routing=list())
#CONSTANTS$grid_mapping <- list()
#CONSTANTS$grid_mapping$lambert_azimuthal_equal_area <- list(name="lambert_azimuthal_equal_area",alias="laea")
#CONSTANTS$grid_mapping$latitude_longitude <- list(name="latitude_longitude",proj4_params="+proj=longlat +datum=WGS84 +no_defs", EPSG_code="EPSG:4326")
CONSTANTS$grid_mapping$lookup <- list(
  #aea = "albers_conical_equal_area",
  #aeqd = "azimuthal_equidistant",
  laea = "lambert_azimuthal_equal_area",
  #lcc = "lambert_conformal_conic",
  #cea = "lambert_cylindrical_equal_area",
  longlat = "latitude_longitude"
  #merc = "mercator",
  #omerc = "oblique_mercator",
  #ortho = "orthographic",
  #stere = "stereographic",
  #tmerc = "transverse_mercator"
)

CONSTANTS$grid_mapping$default <- list(
  grid_mapping_name = "latitude_longitude",
  semi_major_axis = 6378137,
  inverse_flattening = 298.257223563,
  longitude_of_prime_meridian = 0
)

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
  # create setup output
  if(!is.null(VICSetup$config$output)){
    if(!is.null(VICSetup$config$output$domain)){
      # domain_out_config <- VICSetup$config$output$domain
      # domain_file_out <- file.path(VICSetup$config$output$path, domain_out_config$file)
      domain.create()
    }
  }
  rout.params.create()
}

config.restore <- function(){
  rm(list = ls(envir=VICSetup), envir = VICSetup)
}
