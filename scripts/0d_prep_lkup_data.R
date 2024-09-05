# LOAD PACKAGES  ---------------------------------------------------------------

packages <-
  c(
    "tidyverse",
    "tidycensus",
    "tigris",
    "readxl",
    "sf",
    "foreign"
  )

if(length(setdiff(packages, installed.packages())) > 0){
  stop(
    "The following required packages are not installed:\n\n  ",
    paste(setdiff(packages, installed.packages()), collapse= "\n  "),
    "\n\nPlease install these and rerun the script."
  )
}

library(tidyverse)

# PREPARE ZCTA lookups  --------------------------------------------------------

# ZCTA to County and State lookups from Missouri Census Centre
geo_lkup <-
  read.csv("./rawdata/lookups/geocorr2022_2329704436.csv", header = TRUE)[-1, ] %>%
  filter(zcta != " ") %>%
  select(zcta, county, stab, afact) %>%
  group_by(zcta) %>%
  filter(afact == max(afact)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-afact) %>%
  rename(ZCTA5CE20 = zcta)


# Read NCES ZCTA-level classification
nces <- 
  foreign::read.dbf("./rawdata/lookups/EDGE_ZCTALOCALE_2021_LOCALE.dbf") %>%
  mutate(
  ) %>%
  mutate(
    LOCALE = as.integer(as.character(LOCALE)),
    LOCALE = case_when(
      LOCALE %in% c(11, 12, 13) ~ "City",
      LOCALE %in% c(21, 22, 23) ~ "Suburban",
      LOCALE %in% c(31, 32, 33) ~ "Rural - Fringe",
      LOCALE == 41 ~ "Rural - Fringe",
      LOCALE == 42 ~ "Rural - Distant",
      LOCALE == 43 ~ "Rural - Remote",
      TRUE ~ NA
    )
  ) %>%
  dplyr::select(ZCTA5CE20, LOCALE) %>%
  rename(nces = LOCALE)


# GET ZCTA-level demographic data
library(tidycensus)
#census_api_key("", install = TRUE)

variables <- load_variables(dataset = "acs5", year = "2020", cache = TRUE)

demog_df <- 
  get_acs(
    geography = "zcta", 
    variables = c("B02001_001", "B02001_002", "B25003_001", "B25003_002",  "B28001_001",  "B28001_005", "B12001_001", "B12001_004", "B01002_001", "B19013_001"),
    year = 2020
  ) %>%
  mutate(
    variable = case_when(
      variable == "B02001_001" ~ "tpop", 
      variable == "B02001_002" ~ "white", 
      variable == "B25003_001" ~ "toccup",
      variable == "B25003_002" ~ "ownoccup",
      variable == "B28001_001" ~ "thh",
      variable == "B28001_005" ~ "hhsmartphone",
      variable == "B12001_001" ~ "tpop25ov",
      variable == "B12001_004" ~ "undergradplus25ov",
      variable == "B01002_001" ~ "medianage", 
      variable == "B19013_001" ~ "medianhhinc"
    )
  ) %>%
  dplyr::select(-moe, -NAME) %>%
  rename(ZCTA5CE20 = GEOID) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(
    white_pct = ifelse(tpop == 0, 0, white/tpop),
    ownoccup_pct = ifelse(toccup == 0, 0, ownoccup/toccup),
    undergrad_pct25ov = ifelse(tpop25ov == 0, 0, undergradplus25ov/tpop25ov),
    hhsmartphone_pc = ifelse(thh == 0, 0, hhsmartphone/thh)
  )

# Get county-level domestic migration rate data 2019-2015

net_dommig_lst <- list()

net_dommig_lst[[1]] <- get_estimates(geography = "county", variables = c("DOMESTICMIG", "RDOMESTICMIG"), year = 2015)
net_dommig_lst[[1]] <- net_dommig_lst[[1]] %>% mutate(year = 2015)
net_dommig_lst[[2]] <- get_estimates(geography = "county", variables = c("DOMESTICMIG", "RDOMESTICMIG"), year = 2016)
net_dommig_lst[[2]] <- net_dommig_lst[[2]] %>% mutate(year = 2016)
net_dommig_lst[[3]] <- get_estimates(geography = "county", variables = c("DOMESTICMIG", "RDOMESTICMIG"), year = 2017)
net_dommig_lst[[3]] <- net_dommig_lst[[3]] %>% mutate(year = 2017)
net_dommig_lst[[4]] <- get_estimates(geography = "county", variables = c("DOMESTICMIG", "RDOMESTICMIG"), year = 2018)
net_dommig_lst[[4]] <- net_dommig_lst[[4]] %>% mutate(year = 2018)
net_dommig_lst[[5]] <- get_estimates(geography = "county", variables = c("DOMESTICMIG", "RDOMESTICMIG"), year = 2019)
net_dommig_lst[[5]] <- net_dommig_lst[[5]] %>% mutate(year = 2019)

net_dommig_lst <- 
  bind_rows(net_dommig_lst) %>%
  select(-NAME) %>%
  tidyr::pivot_wider(names_from = variable, values_from = value) %>%
  mutate(denom = ifelse(RDOMESTICMIG != 0, DOMESTICMIG/RDOMESTICMIG, 0)) %>%
  group_by(GEOID) %>%
  summarise(
    RDOMESTICMIG = ifelse(mean(denom) != 0, sum(DOMESTICMIG)/mean(denom), 0)
  ) %>%
  ungroup() %>%
  rename(county = GEOID)

# Get broadband speed data 2020

load("./rawdata/broadband/38567-0002-Data.rda")

da38567.0002 <-
  da38567.0002 %>%
  filter(YEAR == 2020) %>%
  select(ZCTA19, AVG_DOWNLOAD_SPEED)


# Merge

lkup <- 
  left_join(nces, demog_df, by = "ZCTA5CE20")

lkup <- 
  left_join(lkup, geo_lkup, by = "ZCTA5CE20")

lkup <- 
  left_join(lkup, net_dommig_lst, by = "county")

lkup <- 
  left_join(lkup, da38567.0002, by = c("ZCTA5CE20" = "ZCTA19"))

# Remove data for non-states
lkup <-
  lkup %>%
  filter(!grepl("^PR|^MP|^GU|^VI|^AS", stab)) %>% 
  filter(!is.na(stab))

# SAVE OUTPUT & CLEAN UP ENVIRONMENT -------------------------------------------
saveRDS(lkup, "./output/datasets/lkup.RDS")

rm(list = ls())
gc()
