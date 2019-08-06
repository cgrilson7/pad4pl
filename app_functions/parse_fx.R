# Helper functions for parsing raw filenames
library(stringr)
library(dplyr)

ex_fnames <- c("/users/Colin/GRNZYME_Plate1.csv","/users/Colin/IL2_Plate2.csv","/users/Colin/IFN_31.csv")


parse_fname <- function(fname){
  cytokines = stringr::str_extract(fname, "(?!.*\\/).+(?=_.+?)")
  pnums = stringr::str_extract(fname, "(?!.*\\/)(?!.*_)\\d+(?=\\.csv?)")
  df_out = as_tibble(list(fname = fname, cytokine = cytokines, p384 = pnums))
  return(df_out)
}


# Deprecated --------------------------------------------------------------



# Need to extract three fields:
# - Which Cytokine (IFNg / IL2 / GZB)?
# - Which 96-well plates were stamped (1234 / 5678 / 9101112 / 13141516)?

# parse_fname <- function(fname){
#   
#   fsplit <- fname %>%
#     stringr::str_extract(".+?(?=(.csv|csv.csv))") %>%
#     stringr::str_extract("^?(?=TSU).+") %>%
#     stringr::str_split("_")
#   
#   fsplit %>%
#     unlist() %>%
#     `[`(c(4, 6)) %>%
#     setNames(nm=c("cytokine","plates")) %>%
#     as.list() %>% as_tibble()
#   
# }

