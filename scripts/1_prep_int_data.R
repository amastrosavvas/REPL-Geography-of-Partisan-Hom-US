# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "data.table",
    "sf",
    "stringr",
    "tigris"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop(
    "The following required packages are not installed:\n\n  ",
    paste(setdiff(packages, installed.packages()), collapse= "\n  "),
    "\n\nPlease install these and rerun the script."
  )
}

library(tidyverse)

# GET INTERACTION DATA  --------------------------------------------------------

# Read ZCTA lat-lon and convert to point geometries
zcta_sf <- 
  tigris::zctas(year = 2020) %>%
  dplyr::select(ZCTA5CE20, INTPTLAT20, INTPTLON20) %>%
  sf::st_drop_geometry() %>%
  sf::st_as_sf(coords = c("INTPTLON20", "INTPTLAT20")) %>%
  sf::st_set_crs(4269) %>%
  sf::st_transform(4326)

# Calculate geographic distances between ZCTAs, with a dataset for every ZCTA 
for (zip in unique(zcta_sf$ZCTA5CE20)) { 
  
  combinations <- data.frame(
    user_loc = zip,
    fr_loc = filter(zcta_sf, ZCTA5CE20 != zip)$ZCTA5CE20,
    geodist = sf::st_distance(filter(zcta_sf, ZCTA5CE20 == zip), filter(zcta_sf, ZCTA5CE20 != zip))[1,]
  )
  
  saveRDS(combinations, paste0("./output/datasets/main/interaction/int", zip, ".RDS"))
}


rm(list = ls())
gc() 

# Get and partition SCI data for each ZCTA into files and identify ZCTAs without sci data
intfiles <- paste0("./output/datasets/main/interaction/", list.files(path = "./output/datasets/main/interaction/", pattern = ".RDS"))
scifiles <- paste0("./rawdata/sci/", list.files(path = "./rawdata/sci/", pattern = "tsv"))

zctas_with_sci <- c()
own_sci_df <- data.frame()

for (i in scifiles){
  shard <-
    data.table::fread( 
      file=i,
      sep="\t",
      header=TRUE,
      colClasses = c("user_loc" = "character", "fr_loc" = "character")
    ) %>% 
    mutate(
      scaled_sci = as.numeric(scaled_sci)
    ) 
  
  own_sci_df <- rbind(own_sci, select(filter(shard, user_loc == fr_loc), -fr_loc))
    
  shard <-
   shard %>%
     filter( #  Exclude own ties
      user_loc != fr_loc
    ) 
  
  zctas_with_sci <- append(zctas_with_sci, setdiff(shard$user_loc, zctas_with_sci))
  
  for (zip in unique(shard$user_loc)) {
    
    if (!is_empty(intfiles[grepl(zip, intfiles)])){
      
      int_zcta_df <- 
        readRDS(intfiles[grepl(zip, intfiles)]) %>%
        left_join(
          filter(shard, user_loc == zip),
          by = c("user_loc", "fr_loc")
        )
      
      saveRDS(int_zcta_df, paste0("./output/datasets/main/interaction/int", zip, ".RDS"))
    }
  }
} 

# CLEAN UP ENVIRONMENT -------------------------------------------
saveRDS(zctas_with_sci, "./output/datasets/zctas_with_sci.RDS")
saveRDS(own_sci_df, "./output/datasets/own_sci_df.RDS")

rm(list = ls())
gc()
