## code to prepare `DATASET` dataset goes here

vic_model_params <- read.table("./inst/extdata/vic_model_params.csv", sep=";", header=TRUE, stringsAsFactors = FALSE)

usethis::use_data(vic_model_params, overwrite = TRUE, internal=TRUE)
