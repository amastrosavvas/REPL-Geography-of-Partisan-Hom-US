# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "sf",
    "raster",
    "tigris",
    "foreach",
    "doParallel",
    "parallel"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop(
    "The following required packages are not installed:\n\n  ",
    paste(setdiff(packages, installed.packages()), collapse= "\n  "),
    "\n\nPlease install these and rerun the script."
  )
}

library(tidyverse)

# PREPARE PRECINCT-LEVEL VOTING DATA  ------------------------------------------

# Define state zip files
votfiles <- paste0("./rawdata/voting/", list.files(path = "./rawdata/voting/", pattern = "zip"))

# Read zip files into sf, ignore Z dimensions, set same crs
vot_lst <- 
  lapply(
    votfiles,
    function(x){
      tempf <- tempfile()
      unzip(x, exdir = tempf)
      sf::st_transform(sf::st_zm(sf::st_read(tempf), drop = T, what = "ZM"), 4326)
      
    }
  )

names(vot_lst) <- substr(votfiles, 18, 19)

# Calculate rep/dem/tot votes
vot_lst <- 
  lapply(
    vot_lst, 
    function(x){ # Voting columns to integer
      
    cols <- names(x)[startsWith(names(x), "G20PRE")]
    
    x %>% 
      mutate(
        across(all_of(cols), ~as.integer(.)),
        tot_votes = rowSums(dplyr::select(sf::st_drop_geometry(.), contains("G20PRE"))),
        dem_votes = rowSums(dplyr::select(sf::st_drop_geometry(.), contains("G20PRED"))),
        rep_votes = rowSums(dplyr::select(sf::st_drop_geometry(.), contains("G20PRER")))
      ) %>%
      dplyr::select(tot_votes, dem_votes, rep_votes, geometry)

    }
  ) 

vot_lst <- # Get state column
  lapply(names(vot_lst), function(x){transform(vot_lst[[x]], state = x)})

vot_sf <- # Merge and set IDs
  dplyr::bind_rows(vot_lst) %>%
  mutate(P_ID = as.character(1:n()))

rm(vot_lst)
gc()

# PRECINT TO ZCTA RASTER LOOKUP  ------------------------------------------------
library("sf")
library("raster")

# Read ZCTA polygons and look up states
zcta_sf <- 
  st_transform(tigris::zctas(year = 2020), 4326) %>%
  dplyr::select(ZCTA5CE20, INTPTLAT20, INTPTLON20, geometry) 

states_sf <- 
  st_transform(tigris::states(), 4326) %>%
  dplyr::select(STUSPS, geometry)

zcta_sf <- 
  st_join(zcta_sf, states_sf) %>%
  rename(state = STUSPS) %>%
  mutate(state = tolower(state)) %>%
  filter(!(state %in% c("as", "gu", "pr", "vi", "mp")))

rm(states_sf)
gc()

# Make ZCTA and precinct polygons valid 
vot_sf <- st_make_valid(st_make_valid(vot_sf))
zcta_sf <- st_make_valid(zcta_sf)

# Get state-by-state intersections of ZCTAs and precincts

get_int <- function(st) {
  isct_sf <- sf::st_intersection(zcta_sf[zcta_sf$state == st,], vot_sf[vot_sf$state == st,])
  saveRDS(isct_sf, paste0("./output/geometries/isct_sf_", st, ".RDS"))
}

num_cores <- max(1, parallel::detectCores() - 8)  
cl <- parallel::makeCluster(num_cores)
parallel::clusterExport(cl, list("zcta_sf", "vot_sf", "get_int"))  # Export necessary variables
parallel::clusterEvalQ(cl, library(sf))  # Load necessary library on each worker

states <- unique(zcta_sf$state)

parallel::parLapply(cl, states, get_int)

rm(vot_sf, zcta_sf)
gc()

# Get population raster sum within intersections
library(foreach)
library(doParallel)

num_cores <-  max(1, parallel::detectCores() - 2)  
cl <- makeCluster(num_cores) # Set up parallel backend
registerDoParallel(cl)

rst_files <- paste0("./rawdata/population/", list.files(path = "./rawdata/population/", pattern = "tif"))
isct_files <- paste0("./output/geometries/", list.files(path = "./output/geometries/", pattern = "RDS"))

process_isct_file <- function(isct_file) { # Create a function to process each isct file
  
  isct_st <- substr(isct_file, nchar(isct_file) - 5, nchar(isct_file) - 4)
  
  isct <- readRDS(isct_file)
  
  # Isolate and unroll geometrycollections
  tb <- tibble::tibble() 
  isct_gcoll <- isct[grepl("GEOMETRYCOLLECTION", sf::st_geometry_type(isct)),]
  for (i in 1:nrow(isct_gcoll)){tb <- rbind(tb, sf::st_cast(isct_gcoll[i, ]))}
  
  # Isolate non-geometry collections
  isct_ngcoll <- isct[!grepl("GEOMETRYCOLLECTION", sf::st_geometry_type(isct)),] 
  
  # Merge all geometry types and retain only polygons and points
  isct_poly <- rbind(
    isct_ngcoll[!grepl("MULTILINESTRING|LINESTRING|MULTIPOINT|POINT", sf::st_geometry_type(isct_ngcoll)),],
    tb[!grepl("MULTILINESTRING|LINESTRING|MULTIPOINT|POINT", sf::st_geometry_type(tb)),]
  )
  
  isct_poly <- dplyr::filter(isct_poly, !sf::st_is_empty(geometry))
  
  if (nrow(isct_poly) != 0) {  # Check the number of rows in isct_poly
    for (j in 1:length(rst_files)) {
      rst <- raster::raster(rst_files[j])
      if (!is.null(raster::intersect(raster::extent(isct_poly), raster::extent(rst)))) {
        isct_poly[[paste0("pop_", j)]] <- raster::extract(rst, isct_poly, fun = sum, na.rm = TRUE)
      }
    }
    saveRDS(sf::st_drop_geometry(isct_poly), paste0("./output/datasets/pop/isct_poly_pop_", isct_st, ".RDS"))
  }
}

foreach (isct_file = isct_files) %dopar% { # Parallelize the loop using foreach
  process_isct_file(isct_file)
}

stopCluster(cl) # Stop the parallel backend

# Apportion precinct votes to ZCTAs
isct_pop_df <- 
  lapply(
    paste0("./output/datasets/pop/", list.files(path = "./output/datasets/pop/", pattern = "RDS")),
    readRDS
  ) 

isct_pop_df <-
  lapply(
    isct_pop_df,
    function(x){ # Get total intersection populations
      
      x %>% 
        dplyr::select(where(~!all(is.null(unlist(.))))) %>%
        mutate(
          tpop = rowSums(.[, grepl("^pop_", colnames(.))], na.rm = T)
        ) %>%
        dplyr::select(-starts_with("pop_"))
        
    }
  ) %>%
  bind_rows()

isct_pop_df <-
  isct_pop_df %>%
  group_by(P_ID) %>%
  mutate( # Get precinct populations, intersection/precinct pop shares, within-precinct max, ind votes
    ptpop = sum(tpop, na.rm = T),
    t_ptpop = ifelse(ptpop == 0, 0, tpop/ptpop),
    max_t_ptpop = max(t_ptpop),
    ind_votes = tot_votes - dem_votes - rep_votes
  ) %>%
  ungroup()

isct_pop_pct_df <- # Get precinct statistics
  isct_pop_df %>%
  group_by(P_ID) %>%
  summarise(
    dem_votes = mean(dem_votes),
    rep_votes = mean(rep_votes),
    ind_votes = mean(ind_votes),
    tot_votes = mean(tot_votes),
    max_t_ptpop = max(t_ptpop),
    ptpop = mean(ptpop)
  ) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(
    max_vote_share = ifelse(tot_votes == 0, 0, max(dem_votes, rep_votes, ind_votes)/tot_votes)
  )
  
isct_pop_df <-
  isct_pop_df %>%
  group_by(ZCTA5CE20) %>%
  summarise(
    INTPTLAT20 = mean(as.numeric(INTPTLAT20)),
    INTPTLON20 =  mean(as.numeric(INTPTLON20)),
    ind_votes = sum(ind_votes*tpop/ptpop, na.rm = T),
    dem_votes = sum(dem_votes*tpop/ptpop, na.rm = T),
    rep_votes = sum(rep_votes*tpop/ptpop, na.rm = T),
    tot_votes = sum(tot_votes*tpop/ptpop, na.rm = T),
    tpop = sum(tpop)
  ) %>%
  ungroup() 

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------

saveRDS(isct_pop_df, "./output/datasets/isct_pop_df.RDS")
saveRDS(isct_pop_df, "./output/datasets/isct_pop_pct_df.RDS")

rm(list = ls())
gc()
