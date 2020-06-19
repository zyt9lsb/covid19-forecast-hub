#' Gets latest model forecast
#' 
#' @param d a forecast data.frame
#' @return a data.frame with a count for the number of forecasts for each target
#' 
get_latest_targets <- function(d) {
  d %>%
    dplyr::group_by(team, model, forecast_date, type, unit, ahead, inc_cum, death_cases) %>%
    dplyr::summarize(max_n = max(as.numeric(n_unit))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = paste(unit, ahead, inc_cum, death_cases)) %>%
    dplyr::select(team, model, forecast_date, type, max_n, target) %>%
    dplyr::arrange(team, model, forecast_date, type, target)
}
