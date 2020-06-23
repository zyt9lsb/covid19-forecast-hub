#' Gets latest model forecast
#' 
#' @param d a data.frame of all forecasts for a model
#' @return a data.frame with the latest forecasts for a model
#' 
get_latest <- function(d) {
  d %>% 
  filter(!is.na(forecast_date)) %>% # why are there NAs?
  group_by(team, model) %>%
  dplyr::filter(forecast_date == max(forecast_date)) %>%
  ungroup() %>%
  tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_cases"),
                  remove = FALSE)
}
