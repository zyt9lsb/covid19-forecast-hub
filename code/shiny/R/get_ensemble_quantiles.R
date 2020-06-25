get_ensemble_quantiles <- function(d){
  d %>%
    filter(model != "ensemble", !is.na(quantile)) %>%
    select(team, model, forecast_date, quantile) %>%
    unique() %>%
    mutate(yes = "Yes",
           quantile = as.character(quantile))
}