library(mapmaker)
library(tidyverse)
library(sf)

# Load shapefile data and mangrove data
shape_city <- read_sf(dsn = "shape_city/", layer = "PH_Adm3_MuniCities.shp")
shape_province <- read_sf(dsn = "shape_province/",layer = "PH_Adm2_ProvDists.shp")
shape_region <- read_sf(dsn = "shape_region/",layer = "PH_Adm1_Regions.shp")
mangrove_data <- read_csv(file = "mangrove_data.csv")

# Merge map
merged_map <- shape_province %>%
  merge_map(., shape_region %>% select(adm1_psgc, adm1_en), by = "adm1_psgc") %>%
  merge_map(., mangrove_data %>% select(PROVINCE, COVER_2010, COVER_2019), by.x = "adm2_en", by.y = "PROVINCE")

# Complete calculations
merged_map$CHANGE <- 100 * merged_map$COVER_2019 / merged_map$COVER_2010 - 100
merged_map$CHANGE[is.na(merged_map$CHANGE)] <- 0

# Create hover text
merged_map$HOVER <- paste0(merged_map$adm2_en, 
                           "\n2010: ", merged_map$COVER_2010,
                           "\n2019: ", merged_map$COVER_2019,
                           "\n% change:", merged_map$CHANGE)

# Prep map
breaks = c(-Inf, 0, 1000, 5000, 10000, 25000, 50000)
labels = c("0", "1-1000", "1001-5000", "5001-10000", "10001-25000", "25001-50000")
prepped_map <- prep_map(merged_map,
                        fill = merged_map$COVER_2019,
                        tooltip = merged_map$HOVER,
                        data_id = merged_map$adm2_en,
                        breaks = breaks,
                        labels = labels
)

made_map <- make_map(prepped_map,
                     title = "Philippine Mangrove Coverage",
                     legend.title = "Mangrove coverage",
                     palette = "Greens",
                     theme = theme_classic()
)

print(made_map)
