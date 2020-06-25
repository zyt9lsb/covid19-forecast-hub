get_ensemble_data <- function(d, offset){
  d %>%
    filter(forecast_date > get_next_saturday(Sys.Date())-offset-9) 
  # filter( (target_end_date == get_next_saturday(Sys.Date()-offset) & grepl("1 wk", target)) |
  #           (target_end_date == get_next_saturday(Sys.Date()+ 7-offset) & grepl("2 wk", target))|
  #           (target_end_date == get_next_saturday(Sys.Date()+14-offset) & grepl("3 wk", target))|
  #           (target_end_date == get_next_saturday(Sys.Date()+21-offset) & grepl("4 wk", target)))
  
  
}