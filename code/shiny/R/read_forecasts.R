read_forecasts <-function(f,locations,team){
  # locations <- readr::read_csv("data-locations/locations.csv",
  #                              col_types = readr::cols(
  #                                abbreviation  = readr::col_character(),
  #                                location      = readr::col_character(),
  #                                location_name = readr::col_character()
  #                              )) 
  tryCatch(
    readr::read_csv(f,
                    col_types = readr::cols_only(
                      forecast_date   = readr::col_date(format = ""),
                      target          = readr::col_character(),
                      target_end_date = readr::col_date(format = ""),
                      location        = readr::col_character(),
                      type            = readr::col_character(),
                      quantile        = readr::col_double(),
                      value           = readr::col_double()
                    )),
    warning = function(w) {
      w$message <- paste0(f,": ", gsub("simpleWarning: ","",w))
      warning(w)
      suppressWarnings(
        readr::read_csv(f,
                        col_types = readr::cols_only(
                          forecast_date   = readr::col_date(format = ""),
                          target          = readr::col_character(),
                          target_end_date = readr::col_date(format = ""),
                          location        = readr::col_character(),
                          type            = readr::col_character(),
                          quantile        = readr::col_double(),
                          value           = readr::col_double()
                        ))
      )
    }
  ) %>%
    dplyr::mutate(file = f) %>%
    tidyr::separate(file, into = c("period","processed","team","model",
                                       "year","month","day","team2","model_etc"), sep="-|/") %>%
    
    dplyr::select(team, model, forecast_date, type, location, target, quantile, 
                  value, target_end_date) %>%
    
    dplyr::left_join(locations, by=c("location"))  
}