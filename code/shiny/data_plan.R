library("drake")
library("dplyr")
library("purrr")
library("tidyverse")
source("code/shiny/read_processed_data.R")
source("code/processing-fxns/get_next_saturday.R")

cache <- storr::storr_environment()

data_plan <- drake::drake_plan(
  raw_data = read_my_dir("data-processed/", "*.csv",into = c("team","model","year","month","day","team2","model_etc")),
  all_data = raw_data %>%dplyr::select(team, model, forecast_date, type, location, target, quantile, 
                                   value, target_end_date) %>%
    dplyr::left_join(locations, by=c("location")), 
  fourweek_date = get_next_saturday(Sys.Date() + 3*7),
  truth_cols = c(
    "date"          = "Date",
    "location"      = "character",
    # location_name = readr::col_character(),
    "value"         = "double"
  ),
  
  # JHU
  inc_jhu = data.table::fread("data-truth/truth-Incident Deaths.csv",   
                              colClasses  = truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "JHU-CSSE") %>%
    na.omit(),
  
  cum_jhu = data.table::fread("data-truth/truth-Cumulative Deaths.csv", 
                              colClasses = truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "JHU-CSSE"),
  
  
  # USAFacts
  inc_usa = data.table::fread("data-truth/usafacts/truth_usafacts-Incident Deaths.csv",
                              colClasses=truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "USAFacts") %>%
    na.omit(),
  
  cum_usa = data.table::fread("data-truth/usafacts/truth_usafacts-Cumulative Deaths.csv", 
                              colClasses=truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "USAFacts"),
  
  
  # NYTimes 
  inc_nyt = data.table::fread("data-truth/nytimes/truth_nytimes-Incident Deaths.csv",
                              colClasses =truth_cols) %>%
    dplyr::mutate(inc_cum = "inc", source = "NYTimes") %>%
    na.omit(),
  
  cum_nyt = data.table::fread("data-truth/nytimes/truth_nytimes-Cumulative Deaths.csv", 
                              colClasses = truth_cols) %>%
    dplyr::mutate(inc_cum = "cum", source = "NYTimes"),
  
  
  
  truth = bind_rows(
    bind_rows(inc_jhu, inc_usa, inc_nyt) %>% dplyr::mutate(unit = "day"),
    
    bind_rows(inc_jhu, inc_usa, inc_nyt) %>% 
      dplyr::mutate(week = MMWRweek::MMWRweek(date)$MMWRweek) %>%
      dplyr::group_by(location, week, inc_cum, source) %>%
      dplyr::summarize(date = max(date),
                       value = sum(value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(weekdays(date) == "Saturday") %>%
      dplyr::mutate(unit = "wk") %>%
      dplyr::select(-week),
    
    bind_rows(cum_jhu, cum_usa, cum_nyt) %>% 
      dplyr::mutate(unit = "wk") %>%
      dplyr::filter(weekdays(date) == "Saturday"),
    
    bind_rows(cum_jhu, cum_usa, cum_nyt) %>% dplyr::mutate(unit = "day")
  ) %>%
    dplyr::left_join(locations, by = c("location")) %>%
    dplyr::mutate(death_cases = "death",
                  simple_target = paste(unit, "ahead", inc_cum, death_cases)),
  
  
  truth_sources = unique(truth$source),

  
  # Further process the processed data for ease of exploration
  latest = all_data %>% 
    filter(!is.na(forecast_date)) %>%
    group_by(team, model) %>%
    dplyr::filter(forecast_date == max(forecast_date)) %>%
    ungroup() %>%
    tidyr::separate(target, into=c("n_unit","unit","ahead","inc_cum","death_cases"),
                    remove = FALSE),
  
  latest_locations = latest %>%
    dplyr::group_by(team, model, forecast_date) %>%
    dplyr::summarize(US = ifelse(any(abbreviation == "US"), "Yes", "-"),
                     n_states = sum(state.abb %in% abbreviation),
                     other = paste(unique(setdiff(abbreviation, c(state.abb,"US"))),
                                   collapse = " "),
                     missing_states = paste(unique(setdiff(state.abb, abbreviation)),
                                            collapse = " "),
                     missing_states = ifelse(missing_states == paste(state.abb, collapse = " "), 
                                             "all", missing_states),
                     missing_states = ifelse(nchar(missing_states) > 7, 
                                             "...lots...", missing_states)
    ),
  
  
  latest_targets =latest %>%
    dplyr::group_by(team, model, forecast_date, type, unit, ahead, inc_cum, death_cases) %>%
    dplyr::summarize(max_n = max(as.numeric(n_unit))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(target = paste(unit, ahead, inc_cum, death_cases)) %>%
    dplyr::select(team, model, forecast_date, type, max_n, target) %>%
    dplyr::arrange(team, model, forecast_date, type, target),
  
  # Quantiles
  quantiles = list(
    full = sprintf("%.3f", c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)),
    min  = sprintf("%.3f", c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99))
  ),
  
  latest_quantiles = latest %>%
    dplyr::filter(type == "quantile") %>%
    dplyr::mutate(quantile = sprintf("%.3f", as.double(quantile))) %>%
    dplyr::group_by(team, model, forecast_date, target) %>%
    dplyr::summarize(
      full = all(quantiles$full %in% quantile),
      min  = all(quantiles$min  %in% quantile),
      quantiles = paste(unique(quantile), collapse = " ")),
  
  latest_quantiles_summary = latest_quantiles %>%
    dplyr::group_by(team, model, forecast_date) %>%
    dplyr::summarize(
      all_full = ifelse(all(full), "Yes", "-"),
      any_full = ifelse(any(full), "Yes", "-"),
      all_min  = ifelse(all(min),  "Yes", "-"),
      any_min  = ifelse(any(min),  "Yes", "-")
    ),
  
  offset = 0, # currently 7 for testing, but should be 0
  ensemble_data = latest %>%
    filter(forecast_date > get_next_saturday(Sys.Date())-offset-9),
  # filter( (target_end_date == get_next_saturday(Sys.Date()-offset) & grepl("1 wk", target)) |
  #           (target_end_date == get_next_saturday(Sys.Date()+ 7-offset) & grepl("2 wk", target))|
  #           (target_end_date == get_next_saturday(Sys.Date()+14-offset) & grepl("3 wk", target))|
  #           (target_end_date == get_next_saturday(Sys.Date()+21-offset) & grepl("4 wk", target)))
  
  
  ensemble = ensemble_data %>%
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
      has_states = ifelse(all(state.abb %in% abbreviation), "Yes", "-")
    ),
  
  ensemble_quantiles = ensemble_data %>%
    filter(model != "ensemble", !is.na(quantile)) %>%
    select(team, model, forecast_date, quantile) %>%
    unique() %>%
    mutate(yes = "Yes",
           quantile = as.character(quantile)),
  
  g_ensemble_quantiles = ggplot(ensemble_quantiles %>%
                                   mutate(team_model = paste(team,model,sep="-")), 
                                 aes(x = quantile, y = team_model, fill = yes)) +
    geom_tile() +
    theme_bw() + 
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1)),
  

  latest_plot_data = latest %>%
    filter(quantile %in% c(.025,.25,.5,.75,.975) | type == "point") %>%
    
    mutate(quantile = ifelse(type == "point", "point", quantile),
           simple_target = paste(unit,ahead,inc_cum, death_cases)) %>%
    select(-type) %>%
    tidyr::pivot_wider(
      names_from = quantile,
      values_from = value
    ),
  #outputs = c(all_data,fourweek_date,truth,truth_sources,latest,latest_locations,latest_targets,quantiles,latest_quantiles,latest_quantiles_summary,ensemble,g_ensemble_quantiles,latest_plot_data),
  all_data_out = saveRDS(all_data, file = file_out("code/shiny/drake_files/all_data.RDS")),
  fourweek_date_out = saveRDS(fourweek_date, file = file_out("code/shiny/drake_files/fourweek_date.RDS")),
  truth_out = saveRDS(truth, file = file_out("code/shiny/drake_files/truth.RDS")),
  truth_sources_out = saveRDS(truth_sources, file = file_out("code/shiny/drake_files/truth_sources.RDS")),
  latest_out =saveRDS(latest, file = file_out("code/shiny/drake_files/latest.RDS")),
  latest_locations_out =saveRDS(latest_locations, file = file_out("code/shiny/drake_files/latest_locations.RDS")),
  latest_targets_out =saveRDS(latest_targets, file = file_out("code/shiny/drake_files/latest_targets.RDS")),
  quantiles_out =saveRDS(quantiles, file = file_out("code/shiny/drake_files/quantiles.RDS")),
  latest_quantiles_out =saveRDS(latest_quantiles, file = file_out("code/shiny/drake_files/latest_quantiles.RDS")),
  latest_quantiles_summary_out =saveRDS(latest_quantiles_summary, file = file_out("code/shiny/drake_files/latest_quantiles_summary.RDS")),
  ensemble_out =saveRDS(ensemble, file = file_out("code/shiny/drake_files/ensemble.RDS")),
  g_ensemble_quantiles_out=saveRDS(g_ensemble_quantiles, file = file_out("code/shiny/drake_files/g_ensemble_quantiles.RDS")),
  latest_plot_data_out =saveRDS(latest_plot_data, file = file_out("code/shiny/drake_files/latest_plot_data.RDS"))
)


drake::make(data_plan,cache = cache)
#drake::vis_drake_graph(data_plan)


