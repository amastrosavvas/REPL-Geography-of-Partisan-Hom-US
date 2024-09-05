# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "rgeoda",
    "spdep",
    "parallel",
    "sf"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop(
    "The following required packages are not installed:\n\n  ",
    paste(setdiff(packages, installed.packages()), collapse= "\n  "),
    "\n\nPlease install these and rerun the script."
  )
}

library(tidyverse)

# GET MAIN SF AND LISA STATS ---------------------------------------------------

# Read main sf with voting, demographic, boundary data
main_sf <- 
  readRDS("./output/datasets/main/main_sf.RDS") 

# Read interaction df with order of ZCTAs matching weights matrix, add count data
int_df <-
  readRDS("./output/datasets/main/int_df.RDS") %>%
  arrange(as.numeric(ID)) %>% # Sort to match weights matrix
  left_join(select(sf::st_drop_geometry(main_sf), ZCTA5CE20, dem_votes, rep_votes), by = c("user_loc" = "ZCTA5CE20")) %>%
  mutate(tp_votes = dem_votes + rep_votes)

# Read weights matrix
wmx <-
  readRDS("./output/datasets/main/wmx.RDS")

# Calculate empirical bayes rates
int_df <-
  int_df %>%
  mutate(dem_tp_sh_eb = rgeoda::eb_rates(int_df[c("dem_votes", "tp_votes")])$EB.Rate)

# Calculate local moran's statistics using eb rates

dem_tp_sh_eb <- int_df$dem_tp_sh_eb
rm(int_df, main_sf)
gc()

# invisible(spdep::set.coresOption(max(parallel::detectCores()-5L, 1L)))
set.seed(123)

lisa <-
  spdep::localmoran_perm(
    x = dem_tp_sh_eb,
    listw = wmx,
    nsim = 499999,
    iseed = 123
  )

saveRDS(lisa, "./output/datasets/main/lisa.RDS")


pva <- function(pv) {
  cbind(
    "none" = pv, 
    "FDR" = p.adjust(pv, "fdr"), 
    "BY" = p.adjust(pv, "BY"),
     "Bonferroni" = p.adjust(pv, "bonferroni")
  )
}

cluster <-
  lisa %>%
  spdep::hotspot(Prname="Pr(z != E(Ii))", cutoff = 0.05, droplevels=FALSE)


pvsp <- 
  lisa %>% 
  subset(select = "Pr(z != E(Ii))", drop = TRUE) |> 
  pva()

lisa <- 
  as.data.frame(cbind(lisa, pvsp, cluster)) %>%
  mutate(
    cluster = case_when(
      is.na(cluster) | FDR > 0.05 ~ "Not significant",
      cluster == 4 ~ "Low-Low",
      cluster == 3 ~ "High-Low",
      cluster == 2 ~ "Low-High",
      cluster == 1 ~ "High-High"
    )
  )


# Append local moran's statistics to main sf
main_sf <- 
  readRDS("./output/datasets/main/main_sf.RDS") 

int_df <-
  readRDS("./output/datasets/main/int_df.RDS") %>%
  arrange(as.numeric(ID)) %>% # Sort to match weights matrix
  left_join(select(sf::st_drop_geometry(main_sf), ZCTA5CE20, dem_votes, rep_votes), by = c("user_loc" = "ZCTA5CE20")) %>%
  mutate(tp_votes = dem_votes + rep_votes)

int_df$lisa_clusters <- lisa$cluster
int_df$lisa_zvalues <- lisa$Z.Ii
int_df$lag_dem_tp_sh <- spdep::lag.listw(wmx, int_df$dem_tp_sh)

main_sf <-
  main_sf %>%
  left_join(select(int_df, -ALAND20, -dem_tp_sh, -dem_votes, -rep_votes, -tp_votes, -tpop), by = c("ZCTA5CE20"="user_loc")) 

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------
saveRDS(main_sf, "./output/datasets/main/main_sf.RDS")

rm(list = ls())
gc()

