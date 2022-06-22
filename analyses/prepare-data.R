#' 
#' Create `funbiogeo` datasets
#' 


## Setup project ----

renv::activate()
renv::restore()

devtools::load_all()
sf::sf_use_s2(FALSE)


## CRS ----

albers <- paste0("+proj=aea +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 ", 
                 "+x_0=0 +y_0=0 +ellps=intl +units=m +no_defs")

lonlat <- 4326


## Import world mammals IUCN ranges ----

mammals <- sf::st_read(here::here("data", "IUCN", "MAMMALS_TERRESTRIAL_ONLY",
                                  "MAMMALS_TERRESTRIAL_ONLY.shp"))

## Remove uncertain presences ----

mammals <- mammals[-grep("Presence Uncertain", mammals$"legend"), ]


## Define projection ----

sf::st_crs(mammals) <- lonlat


## List European countries ----

europe <- rnaturalearth::ne_countries(continent = "Europe")
europe <- sort(unique(europe@data$"sovereignt"))
europe <- europe[europe != "Russia"]

europe <- c(europe, "Monaco", "San Marino", "Andorra", "Czechia", 
            "Liechtenstein")


## Get Europe boundaries (Natural Earth) ----

europe <- rnaturalearth::ne_countries(country = europe, scale = 'large', 
                                      returnclass = "sf")

europe <- sf::st_transform(europe, albers)


## Crop shapefile ----

clippers <- readRDS(here::here("data", "clipper_europe.rds"))
clippers <- sf::st_set_crs(clippers, sf::st_crs(europe))
                           
europe <- sf::st_intersection(europe, clippers)
europe <- sf::st_crop(europe, xmin = -1700795.5, ymin = 734328.2, 
                      xmax = 750000, ymax = 3326027)

europe <- sf::st_transform(europe, lonlat)
europe <- sf::st_union(europe)


## Select and crop European mammals ranges ----

mammals <- sf::st_intersection(mammals, europe)


## Combine polygons by species ----

species <- sort(unique(mammals$"binomial"))
species <- species[-which(species %in% c("Canis lupaster", "Myotis escalerai",
                                         "Eptesicus serotinus"))]

mammals <- do.call(rbind, parallel::mclapply(1:length(species), function(i) {

  sp  <- species[i]
  dat <- data.frame("species" = sp)
  
  if (nrow(mammals[mammals$"binomial" == sp, ]) > 1) {
    
    geom <- sf::st_geometry(sf::st_union(mammals[mammals$"binomial" == sp, ]))
    
  } else {
    
    geom <- sf::st_geometry(mammals[mammals$"binomial" == sp, ])
  }
  
  sf::st_geometry(dat) <- geom
  dat
  
}, mc.cores = 6))


## Create grid ----

ras <- terra::rast(terra::vect(europe), res = 0.5)
ras <- terra::rasterize(terra::vect(europe), ras, field = 1)
terra::crs(ras) <- "epsg:4326"
names(ras)      <- "europe_grid"


## Rasterize species ranges ----

mammals <- lapply(1:nrow(mammals), function(i) { 
  terra::rasterize(terra::vect(mammals[i, ]), ras, field = 1)
})

mammals <- terra::rast(mammals)
names(mammals) <- tolower(gsub("\\s", "_", species))


## Convert to sf ----

mammals <- stars::st_as_stars(mammals)
mammals <- sf::st_as_sf(mammals)
mammals$"cell" <- 1:nrow(mammals)



# 
# mammals_df  <- data.frame("cell" = paste0("fb_", cells), 
#                           terra::xyFromCell(mammals, cells),
#                           terra::values(mammals))
# 
# empty_cells <- which(is.na(sum(mammals, na.rm = TRUE)[]))
# mammals_df  <- mammals_df[-empty_cells, ]
# 
# for (i in 4:ncol(mammals_df)) 
#   mammals_df[ , i] <- ifelse(is.na(mammals_df[ , i]), 0, mammals_df[ , i])
# 
# mammals_sf <- sf::st_as_sf(mammals_df, coords = 2:3)
# rownames(mammals_sf) <- NULL
# sf::st_crs(mammals_sf) <- lonlat


## Import traits (Pantheria database) ----

pantheria <- readr::read_delim(here::here("data", 
                                          "PanTHERIA_1-0_WR05_Aug2008.txt"), 
                               delim = "\t")
pantheria <- as.data.frame(pantheria)


## Rename species ----

species_list <- list()
species_list[[1]] <- c("Crocidura pachyura", "Crocidura ichnusae")
species_list[[2]] <- c("Eptesicus isabellinus", "Eptesicus serotinus")
species_list[[3]] <- c("Eutamias sibiricus", "Tamias sibiricus")
species_list[[4]] <- c("Myotis aurascens", "Myotis davidii")
species_list[[5]] <- c("Nannospalax leucodon", "Spalax leucodon")
species_list[[6]] <- c("Plecotus macrobullaris", "Plecotus alpinus")
species_list[[7]] <- c("Rhinopoma cystops", "Rhinopoma hardwickii")

for (i in 1:length(species_list)) {
  
  num_row <- which(pantheria$"MSW05_Binomial" == species_list[[i]][2])
  pantheria[num_row, "MSW05_Binomial"] <- species_list[[i]][1]
  pantheria[num_row, "MSW05_Genus"]    <- strsplit(species_list[[i]][1], " ")[[1]][1]
  pantheria[num_row, "MSW05_Species"]  <- strsplit(species_list[[i]][1], " ")[[1]][2]
}

species[which(!(species %in% pantheria$"MSW05_Binomial"))]


## Select species ----

pantheria <- pantheria[pantheria$"MSW05_Binomial" %in% species, ]


## Select traits ----

traits <- c("5-1_AdultBodyMass_g", "9-1_GestationLen_d", "15-1_LitterSize", 
            "17-1_MaxLongevity_m", "23-1_SexualMaturityAge_d", "6-1_DietBreadth")

pantheria <- pantheria[ , c("MSW05_Order", "MSW05_Family", "MSW05_Binomial", 
                            traits)]

colnames(pantheria) <- c("order", "family", "species", "adult_body_mass", 
                         "gestation_length", "litter_size", "max_longevity",
                         "sexual_maturity_age", "diet_breadth")


## Replace NA ----

for (i in 4:ncol(pantheria)) 
  pantheria[ , i] <- ifelse(pantheria[ , i] == -999, NA, pantheria[ , i])

pantheria <- pantheria[order(pantheria$"species"), ]
rownames(pantheria) <- NULL


## Anonymized species ----


splabels <- paste0("00", 1:length(species))
splabels <- substr(splabels, nchar(splabels) - 2, nchar(splabels))
splabels <- paste0("sp_", splabels)

for (i in 1:length(species)) {
  
  spname <- tolower(gsub("\\s", "_", species[i]))
  
  pos <- which(colnames(mammals) == spname)
  colnames(mammals)[pos] <- splabels[i]
  
  pos <- which(pantheria$"species" == species[i])
  pantheria$"species"[pos] <- splabels[i]
}

pantheria <- pantheria[ , -c(1:2)]


## Replace NA in mammals ----

mammals_df <- sf::st_drop_geometry(mammals)

col_s <- grep("sp_", colnames(mammals_df))

for (i in col_s) {
  mammals_df[ , i] <- ifelse(is.na(mammals_df[ , i]), 0, mammals_df[ , i])
}

## Richness map ----

# plot(sf::st_geometry(europe))
# terra::plot(sum(mammals, na.rm = TRUE), add = TRUE, 
#             col = heat.colors(255, rev = TRUE))
# plot(sf::st_geometry(europe), add = TRUE)


## Export layers ----

sites_locs     <- mammals[ , -col_s]
site_species   <- mammals_df[ , c(ncol(mammals_df), grep("^sp_", colnames(mammals_df)))]
species_traits <- pantheria

# terra::writeRaster(ras,  here::here("outputs", "europe_grid.tif"), overwrite = TRUE)
# sf::st_write(europe,     here::here("outputs", "europe_boundaries.gpkg"), delete_dsn = TRUE)
sf::st_write(sites_locs, here::here("outputs", "sites_locations.gpkg"), delete_dsn = TRUE)

save(species_traits, file = here::here("outputs", "species_traits.rda"))
save(site_species,   file = here::here("outputs", "site_species.rda"))
