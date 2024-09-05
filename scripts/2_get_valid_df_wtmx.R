# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "rgeoda",
    "spdep",
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

# GET MAIN DF AND WEIGHTS MATRIX  ----------------------------------------------

main_sf <- # Get all ZCTA codes 
  tigris::zctas(year = 2020) %>%
  dplyr::select(ZCTA5CE20, ALAND20) %>% # Remove ZCTAs in non-states
  dplyr::filter(!grepl("^001|^006|^007|^008|^009|^969|^96799", ZCTA5CE20)) 

vot_df <- # Get voting data for each ZCTA and calc dem tp share and ind share
  readRDS("./output/datasets/isct_pop_df.RDS") %>%
  dplyr::select(ZCTA5CE20, dem_votes, rep_votes, ind_votes, tot_votes) %>%
  mutate(
    dem_tp_sh = dem_votes/(dem_votes + rep_votes),
    ind_sh = ind_votes/(tot_votes)
  )

lkup <- # Get census and geoclassification data
  readRDS("./output/datasets/lkup.RDS") 

main_sf <- # Merge ZCTA, ID, population, and voting data  
  main_sf %>%
  left_join(vot_df) %>%
  left_join(lkup) 

zctas_with_sci <- # Get identified zctas with SCI data
  readRDS("./output/datasets/zctas_with_sci.RDS")

# Identify valid ZCTAs (i.e. has population, voting, and sci data)
main_sf <-
  main_sf %>%
  mutate(
    data_flag = case_when(
      !(ZCTA5CE20 %in% zctas_with_sci) ~ "no sci",
      tot_votes == 0 | is.na(tot_votes) ~ "no votes",
      tpop == 0 | is.na(tpop) ~ "no pop",
      TRUE ~ "valid"
    )
  )

saveRDS(main_sf, "./output/datasets/main/main_sf.RDS")

rm(vot_df, lkup, zctas_with_sci)
gc()

valid_df <- # Create valid ZCTA df including voting and population data
  main_sf %>%
  sf::st_drop_geometry() %>%
  dplyr::filter(data_flag == "valid") %>%
  select(ZCTA5CE20, ALAND20, dem_tp_sh, tpop) %>%
  mutate(ID = 1:n()) # set IDS
  
rm(main_sf)
gc()

valid_intfiles <-  # Set interaction data filepaths for valid ZCTAs
  paste0("./output/datasets/main/interaction/", list.files(path = "./output/datasets/main/interaction/", pattern = ".RDS"))

valid_intfiles <-
  valid_intfiles[gsub("\\D", "", valid_intfiles) %in% valid_df$ZCTA5CE20]

# Get list of avg interactions, ID, neighbour set, and weights
library(parallel)

lst <- vector("list", length = nrow(valid_df))

lst <- parallel::mclapply(valid_intfiles, function(file_path){
  
  int_zcta_df <- readRDS(file_path)
  
  int_zcta_df <-
    int_zcta_df %>%
    left_join(
      select(valid_df, ZCTA5CE20, dem_tp_sh, ID), 
      by = c("user_loc" = "ZCTA5CE20")
    ) %>%
    rename(
      "user_dem_tp_sh" = "dem_tp_sh",
      "user_ID" = "ID"
    ) %>%
    left_join(
      select(valid_df, ZCTA5CE20, dem_tp_sh, ID, tpop, ALAND20), 
      by = c("fr_loc" = "ZCTA5CE20")
    ) %>%
    rename(
      "fr_dem_tp_sh" = "dem_tp_sh",
      "fr_ID" = "ID",
      "fr_tpop" = "tpop",
      "fr_land" = "ALAND20"
    ) %>%
    dplyr::filter(fr_loc %in% valid_df$ZCTA5CE20) 
    
  
  n1_pct <- nrow(filter(int_zcta_df, scaled_sci == 1))/nrow(int_zcta_df)
  sci_min_pre1 <- min(filter(int_zcta_df, scaled_sci > 1)$scaled_sci)
  
  int_zcta_df <-
    int_zcta_df %>%
    arrange(geodist) %>%
    mutate(
      gnb = ifelse(row_number() <= 1347, 1, 0)
    ) %>%
    group_by(user_loc) %>%
    mutate(
      mean_gnb_geodist = mean(gnb*geodist)
    ) %>%
    ungroup() %>%
    arrange(-scaled_sci) %>%
    mutate(
      min_nb_wt = ifelse(row_number() <= 1347, 1, 0),
      sci_min_nb_pct = sum(min_nb_wt*scaled_sci*fr_tpop/sum(scaled_sci*fr_tpop))
    )
  
  sci_min_nb_pct <- unique(int_zcta_df$sci_min_nb_pct)
    
  int_zcta_df <-
    int_zcta_df %>%
    filter(row_number() <= 1347) %>%
    mutate(sci_weight = scaled_sci/sum(scaled_sci))
  
  self <- unique(int_zcta_df$user_ID)
  neighb <- int_zcta_df$fr_ID
  wt <- int_zcta_df$sci_weight
  
  int_zcta_df <- 
    int_zcta_df %>% 
    group_by(user_loc) %>%
    summarise(
      mean_gnb_geodist = mean(mean_gnb_geodist, na.rm =T),
      sci_mean = mean(scaled_sci, na.rm = T),
      sci_pop_w_mean = sum(fr_tpop*scaled_sci/sum(fr_tpop, na.rm = T), na.rm = T),
      sci_w_geodistance = sum(sci_weight*geodist, na.rm = T),
      min_geodistance = min(geodist, na.rm = T),
      sci_pop_w_geodistance = sum(sci_weight*geodist*fr_tpop/sum(fr_tpop, na.rm = T), na.rm = T),
      sci_w_land = sum(sci_weight*fr_land, na.rm = T),
      sci_pop_w_land = sum(sci_weight*fr_land*fr_tpop/sum(fr_tpop, na.rm = T), na.rm = T),
    )
  
  result <- list(int_df = int_zcta_df, id = self, nb = neighb, wt = wt, n1_pct = n1_pct, sci_min_pre1 = sci_min_pre1, sci_min_nb_pct = sci_min_nb_pct)
  
  return(result)
}, mc.cores = 7
) # Mean SCI in global min of n of neighbours

# Create spdep weights matrix

wmx <- 
  data.frame(
    style = "raw",
    id = as.numeric(unlist(lapply(lst, function(x) x$id))),
    neighbours =  I(lapply(lst, function(x) x$nb)),
    weights = I(lapply(lst, function(x) x$wt))
  ) %>%
  arrange(id) %>% # Sort to match int_df (sorted pre-localmoran calculation)
  select(-id)

wmx <- as.list(wmx)

class(wmx) <- "listw"
class(wmx[["neighbours"]]) <- "nb"
class(wmx[["weights"]]) <- "list"

# Update valid_df with average interaction data

int_df <- 
  bind_rows(lapply(lst, function(i){mutate(i$int_df, ID = i$id, n1_pct = i$n1_pct, sci_min_pre1 = i$sci_min_pre1, sci_min_nb_pct = i$sci_min_nb_pct)})) %>%
  mutate(ID = as.character(ID))

own_sci_df <- 
  readRDS("./output/datasets/own_sci_df.RDS") %>%
  rename(own_sci = scaled_sci)

int_df <-
  int_df %>%
  left_join(own_sci_df) %>%
  mutate(
    sci_mean_rel_own = sci_mean/own_sci,
    sci_w_geodistance_rel_gnb = sci_w_geodistance / mean_gnb_geodist,
    sci_w_geodistance_rel_min = sci_w_geodistance/min_geodistance
  ) %>%
  left_join(select(valid_df, -ID), by = c("user_loc" = "ZCTA5CE20"))
  
# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------
saveRDS(lst, "./output/datasets/main/lst.RDS")
saveRDS(int_df, "./output/datasets/main/int_df.RDS")
saveRDS(wmx, "./output/datasets/main/wmx.RDS")
rgeoda::save_weights(weights_obj, int_df["user_loc"], "./output/datasets/main/weights_obj.gal")


rm(list = ls())
gc()




