# Read in all the forecast files in data-processed/

library("dplyr")
library("tidyr")
#library("readr")
library("data.table")
library("drake")

read_my_csv = function(f, into) {
  filename = f
  f = paste(c("../../data-processed/",f), collapse="")
  tryCatch(
    data.table::fread(f,
                    colClasses =c(
                      "forecast_date"   = "Date",
                      "target"          = "character",
                      "target_end_date" = "Date",
                      "location"        = "character",
                      "type"            = "character",
                      "quantile"        = "double",
                      "value"           = "double"
                    )),
    warning = function(w) {
      w$message <- paste0(f,": ", gsub("simpleWarning: ","",w))
      warning(w)
      suppressWarnings(
        data.table::fread(f,
                          colClasses =c(
                            "forecast_date"   = "Date",
                            "target"          = "character",
                            "target_end_date" = "Date",
                            "location"        = "character",
                            "type"            = "character",
                            "quantile"        = "double",
                            "value"           = "double"
                        ))
      )
    }
  ) %>%
    dplyr::mutate(file = filename)%>%
    tidyr::separate(file, into, sep="-|/") 

}

read_my_dir = function(path, pattern, into, exclude = NULL) {
  files = list.files(path       = path,
                     pattern    = pattern,
                     recursive  = TRUE,
                     full.names = FALSE) %>%
    setdiff(exclude)
  plyr::ldply(files, read_my_csv, into = into)
}

# above from https://gist.github.com/jarad/8f3b79b33489828ab8244e82a4a0c5b3
#############################################################################

locations <-  data.table::fread("../../data-locations/locations.csv",
                             colClasses=c(
                               "abbreviation"  = "character",
                               "location"      = "character",
                               "location_name" = "character"
                             )) 


#all_data = read_my_dir("../../data-processed/", "*.csv",into = c("team","model","year","month","day","team2","model_etc")) %>%
  
#  dplyr::select(team, model, forecast_date, type, location, target, quantile, 
#                value, target_end_date) %>%
    
#  dplyr::left_join(locations, by=c("location")) 
