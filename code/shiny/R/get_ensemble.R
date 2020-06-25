get_ensemble<- function(d){
  d %>%
    group_by(team, model, forecast_date) %>%
    filter(model != "ensemble",
           unit == "wk",
           type == "quantile",
           death_cases == "death") %>%
    dplyr::summarize(
      median    = ifelse(any(quantile == 0.5, na.rm = TRUE), "Yes", "-"),
      cum_death = ifelse(all(paste(1:4, "wk ahead cum death") %in% target), "Yes", "-"),
      inc_death = ifelse(all(paste(1:4, "wk ahead inc death") %in% target), "Yes", "-"),
      all_weeks = ifelse(all(1:4 %in% n_unit), "Yes", "-"),
      has_US    = ifelse("US" %in% abbreviation, "Yes", "-"),
      has_states = ifelse(all(state.abb %in% abbreviation), "Yes", "-"))
}