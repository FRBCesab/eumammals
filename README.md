# eumammals

<!-- badges: start -->
<!-- badges: end -->



Research compendium to create reproducible examples for the R package 
[`funbiogeo`](https://github.com/frbcesab/funbiogeo). These datasets are derived
from IUCN mammals range maps (raw data available [here](https://www.iucnredlist.org/resources/spatial-data-download))
and the traits database PanTHERIA (database available [here](https://figshare.com/collections/PanTHERIA_a_species-level_database_of_life_history_ecology_and_geography_of_extant_and_recently_extinct_mammals/3301274) and
metadata available [here](https://esapubs.org/archive/ecol/E090/184/metadata.htm)).



### Content

This repository is structured as follow:

- :file_folder: [data/](https://github.com/frbcesab/eumammals/tree/main/data): contains raw data to create datasets (except for IUCN range maps).

- :file_folder: [analyses/](https://github.com/frbcesab/eumammals/tree/main/analyses): contains the R script `prepare-data.R` to create datasets

- :file_folder: [outputs/](https://github.com/frbcesab/eumammals/tree/main/outputs): contains all the datasets that we will be used as examples in the project [`funbiogeo`](https://github.com/frbcesab/funbiogeo)

- :page_facing_up: [DESCRIPTION](https://github.com/frbcesab/eumammals/tree/main/DESCRIPTION): contains project metadata (author, date, dependencies, etc.)

- :page_facing_up: [renv.lock](https://github.com/frbcesab/eumammals/tree/main/renv.lock): contains recipe to locally install required packages (and their versions)



### Usage

Clone the repository and run this command in R/RStudio:

``` r
source(here::here("analyses/prepare-data.R"))
```

Note that all required packages, listed in the `DESCRIPTION` and `renv.lock` files, will be installed.

