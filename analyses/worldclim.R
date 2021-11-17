#' Prepare climate rasters


## Import bioclimatic variables ----

fls <- list.files(here::here("data", "worldclim", "wc2.1_5m_bio"), 
                  pattern = "_1\\.tif$|_12\\.tif$", full.names = TRUE)

ras <- terra::rast(fls)


## Import study area ----

europe <- sf::st_read(here::here("outputs", "europe_boundaries.gpkg"))


## Crop rasters ----

ras <- terra::crop(ras, europe)
names(ras) <- c("annual_mean_temp", "annual_tot_prec")

terra::writeRaster(terra::subset(ras, 1), here::here("outputs", "annual_mean_temp.tif"))
terra::writeRaster(terra::subset(ras, 2), here::here("outputs", "annual_tot_prec.tif"))
