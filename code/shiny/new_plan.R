R.utils::sourceDirectory("code/shiny/R",modifiedOnly=FALSE)

forecast_files        = get_forecast_files()
latest_forecast_files = get_latest_forecast_files(forecast_files)

# only keep team_model? 
ids = rlang::syms(lapply(latest_forecast_files, function(f) unlist(strsplit(f, "/"))[2]))

plan = drake::drake_plan(
  locations = get_locations(file_in("data-locations/locations.csv")),
  
  ##############
  # Latest Forecasts
  
  latest_forecasts = target(
    read_forecast_file(file_in(file)) %>%
      dplyr::left_join(locations, by = c("location")) %>%
      tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_cases"),
                      remove = FALSE),
    transform = map(file = !!latest_forecast_files,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest = target(
    dplyr::bind_rows(latest_forecasts),
    transform = combine(latest_forecasts)
  ),
  
  # Locations
  latest_locations_by_model = target(
    get_forecast_locations(latest_forecasts),
    transform = map(latest_forecasts,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest_locations = target(
    dplyr::bind_rows(latest_locations_by_model),
    transform = combine(latest_locations_by_model)
  ),
  
  # Quantiles
  latest_quantiles_by_model = target(
    get_forecast_quantiles(latest_forecasts),
    transform = map(latest_forecasts,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest_quantiles_summary_by_model = target(
    get_forecast_quantiles_summary(latest_quantiles_by_model),
    transform = map(latest_quantiles_by_model,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest_quantiles = target(
    dplyr::bind_rows(latest_quantiles_by_model),
    transform = combine(latest_quantiles_by_model)
  ),
  
  latest_quantiles_summary = target(
    dplyr::bind_rows(latest_quantiles_summary_by_model),
    transform = combine(latest_quantiles_summary_by_model)
  ),
  
  # Targets
  latest_targets_by_model = target(
    get_forecast_targets(latest_forecasts),
    transform = map(latest_forecasts,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest_targets = target(
    dplyr::bind_rows(latest_targets_by_model) %>%
      dplyr::select(team, model, forecast_date, type, max_n, target) %>%
      dplyr::arrange(team, model, forecast_date, type, target),
    transform = combine(latest_targets_by_model)
  ),
  
  # Plot data
  latest_plot_data_by_model = target(
    get_latest_plot_data(latest_forecasts),
    transform = map(latest_forecasts,id_var = !!paste0(ids,"_"), .id=id_var)
  ),
  
  latest_plot_data = target(
    dplyr::bind_rows(latest_plot_data_by_model),
    transform = combine(latest_plot_data_by_model)
  ),
  
  ##############
  # Truth
  inc_jhu = get_truth(file_in("data-truth/truth-Incident Deaths.csv"),                     "inc", "JHU-CSSE"),
  cum_jhu = get_truth(file_in("data-truth/truth-Cumulative Deaths.csv"),                   "cum", "JHU-CSSE"),
  inc_usa = get_truth(file_in("data-truth/usafacts/truth_usafacts-Incident Deaths.csv"),   "inc", "USAFacts"),
  cum_usa = get_truth(file_in("data-truth/usafacts/truth_usafacts-Cumulative Deaths.csv"), "cum", "USAFacts"),
  inc_nyt = get_truth(file_in("data-truth/nytimes/truth_nytimes-Incident Deaths.csv"),     "inc", "NYTimes"),
  cum_nyt = get_truth(file_in("data-truth/nytimes/truth_nytimes-Cumulative Deaths.csv"),   "cum", "NYTimes"),
  
  truth = combine_truth(inc_jhu, inc_usa, inc_nyt,
                        cum_jhu, cum_usa, cum_nyt) %>%
    dplyr::left_join(locations, by = c("location"))
  ##############
)
