
# lib/helpers.R
# Utility functions to load the networks and convert them for visNetwork

suppressPackageStartupMessages({
  library(dplyr)
  library(igraph)
  library(data.table)
})

# ---- Load networks and build an index ---------------------------------------
# Expects data/SEnetworks_Nov2024.Rdata to save either:
#  (A) mpa_index (data.frame: mpa_id, country, mpa_name) and networks_list (list of igraphs)
#  (B) only networks_list (named list); we derive mpa_index from names

load_network_data <- function(pathnet = "data/SEnetworks_Nov2024.Rdata",pathmeta="data/mpa_metadata.csv") {
  
  env <- new.env(parent = emptyenv())
  obj_names <- load(pathnet, envir = env)
  SEnetworks <- env$SEnetworks
  
  idx_name<-names(SEnetworks)
  
  metadata <<- fread(pathmeta)
    
  # Build or validate the index
  mpa_index <- tibble(metadata)
  
  
  # Return a list containing index + list
  list(meta = mpa_index, #mpa, region_name, NAME variables of interest
       networks = SEnetworks,
       index=idx_name)
}


