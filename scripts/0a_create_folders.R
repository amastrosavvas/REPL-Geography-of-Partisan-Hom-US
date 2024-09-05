# LOAD PACKAGES ----------------------------------------------------------------

packages <-
  c(
    "dplyr"
  )

for (p in packages){ 
  if (! (p %in% installed.packages())){
    install.packages(p)
  }
}

library(dplyr)

# DEFINE FUNCTIONS  ------------------------------------------------------------

# Check if folder in directory, and create if not
get.folder <- 
  function(fpath){
    tryCatch(
      {
        if(!dir.exists(file.path(fpath))){
          dir.create(file.path(fpath))
          cat("Folder", paste0("'", fpath, "'"), "created.\n")
          
        } else {
          cat("Folder", paste0("'", fpath, "'"), "already exists in the directory.\n")
        }
      }, 
      error=function(cond) {
        message(cond, "\n")
      }, 
      warning=function(cond) {
        message(cond, "\n")
      } 
    )
  }

# SET UP DIRECTORY STRUCTURE  --------------------------------------------------

folders <-
  c(
    "rawdata",
    "rawdata/broadband", 
    "rawdata/lookups", 
    "rawdata/population",
    "rawdata/sci",
    "rawdata/voting",
    "output", 
    "output/datasets", 
    "output/datasets/main",
    "output/datasets/pop",
    "output/geometries",
    "output/plots",
    "output/tables"
  )

for (folder in folders){get.folder(folder)}









