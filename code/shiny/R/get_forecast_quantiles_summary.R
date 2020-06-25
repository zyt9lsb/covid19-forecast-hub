get_forecast_quantiles_summary <- function(d){
  d %>%
    dplyr::summarize(
      all_full = ifelse(all(full), "Yes", "-"),
      any_full = ifelse(any(full), "Yes", "-"),
      all_min  = ifelse(all(min),  "Yes", "-"),
      any_min  = ifelse(any(min),  "Yes", "-"))
}