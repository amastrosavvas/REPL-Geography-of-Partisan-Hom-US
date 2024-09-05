# LOAD PACKAGES ----------------------------------------------------------------

packages <-
  c(
    "dplyr",
    "rvest",
    "httr",
    "stringr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)

# DEFINE FUNCTIONS  ------------------------------------------------------------

# Function to get data files that can be directly downloaded from the web
get.download <- 
  function(url, fpath, overwrite){
    tryCatch(
      {
        if(overwrite == T | (!file.exists(fpath) & overwrite == F)){
          download.file(url, destfile= fpath, mode = "wb")
          cat("File", paste0("'", fpath, "'"), "downloaded from: \n", url,"\n")
          
        } else if (file.exists(fpath) & overwrite == F) {
          cat("Did not overwrite existing file", paste0("'", fpath, "'"), "\n")
        }
      }, 
      error=function(cond) {message(cond, "\n")},
      warning=function(cond) {message(cond, "\n")}
    )
  }

# DOWNLOAD FILES ---------------------------------------------------------------

# Increase timeout limit
options(timeout = max(10000, getOption("timeout")))

# NCES CLASSIFICATION

url <- # Define the NCES file URL and download
  "https://nces.ed.gov/programs/edge/data/2021_ZCTA.zip"

temp_zip <- tempfile(fileext = ".zip")

get.download(url, temp_zip, overwrite = TRUE)

target_file <-  # Define the target file within the ZIP
  "EDGE_ZCTALOCALE_2021_LOCALE.dbf"

output_path <- "rawdata/lookups/EDGE_ZCTALOCALE_2021_LOCALE.dbf"  

zip_contents <-  # List files in the ZIP
  unzip(temp_zip, list = TRUE)

if (target_file %in% zip_contents$Name) { # Check if the target file is in the ZIP and extract it
  unzip(temp_zip, files = target_file, exdir = dirname(output_path))
  file.rename(file.path(dirname(output_path), target_file), output_path)
  cat("Extracted", target_file, "to", output_path, "\n")
} else {
  cat("Target file", target_file, "not found in the ZIP archive.\n")
}

unlink(temp_zip)

# FACEBOOK POPULATION LAYER

url <- # Define the HDX webpage URL
  "https://data.humdata.org/dataset/united-states-high-resolution-population-density-maps-demographic-estimates"

webpage <-  # Read the webpage content
  rvest::read_html(url)

links <- # Extract all the links on the webpage
  webpage %>%
  rvest::html_nodes("a") %>%
  rvest::html_attr("href")

tif_zip_files <- # Filter the links that end with .tif.zip and construct full URLs
  paste0("https://data.humdata.org", links[grepl("2019-07-01\\.tif\\.zip$", links)])

for (url in tif_zip_files) {
  
  # Define the paths for the download and extraction
  zip_path <- paste0("./rawdata/population/", basename(url))
  extract_dir <- "./rawdata/population"
  
  # Download the .zip file
  get.download(url, zip_path, overwrite = TRUE)
  
  # Extract the .tif files from the .zip
  unzip(zip_path, exdir = extract_dir)
  
  # Delete the downloaded .zip file after extraction
  unlink(zip_path)
  
  cat("Extracted and removed:", zip_path, "\n")
}

# FACEBOOK SCI

url <- # Define the HDX file URL and download
  "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/3fff416a-824a-43a3-a6cd-aab2880ce684/download/us-zip-code-us-zip-code-fb-social-connectedness-index-october-2021.zip"

temp_zip <- tempfile(fileext = ".zip")

output_dir <- "./rawdata/sci"

get.download(url, temp_zip, overwrite = TRUE)   

unzip(temp_zip, exdir = output_dir) # Extract the files from the .zip

unlink(temp_zip) # Delete the downloaded .zip file after extraction

# VOTING DATA



# CLEAN UP ENVIRONMENT ---------------------------------------------------------

rm(list = ls())
gc()






