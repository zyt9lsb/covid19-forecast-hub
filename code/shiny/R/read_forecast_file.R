read_forecast_file <- function(f) {
  readr::read_csv(f,
                  col_types = readr::cols_only(
                    forecast_date   = readr::col_date(format = ""),
                    target          = readr::col_character(),
                    target_end_date = readr::col_date(format = ""),
                    location        = readr::col_character(),
                    type            = readr::col_character(),
                    quantile        = readr::col_double(),
                    value           = readr::col_double()
                  )
  ) %>%
    dplyr::mutate(file = f) %>%
    tidyr::separate(file, into = c("period","processed","team","model",
                                   "year","month","day","team2","model_etc"), 
                    sep="-|/") %>%
    
    dplyr::select(team, model, forecast_date, type, location, target, quantile, 
                  value, target_end_date)   
}
