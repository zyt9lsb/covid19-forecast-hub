#' Gets all data from raw data
#' 
#' @param d a data.frame of all raw data
#' @return a data.frame with all selected columns from raw data
#' 
get_all_data <- function(d,locations) {
  d %>%dplyr::select(team, model, forecast_date, type, location, target, quantile, 
                           value, target_end_date) %>%
    dplyr::left_join(locations, by=c("location"))
}
  