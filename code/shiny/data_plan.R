library("drake")
library("dplyr")
library("readr")
library("purrr")
library("tidyverse")
library("miceadds")
#source("code/shiny/read_processed_data.R")
source("code/processing-fxns/get_next_saturday.R")

#load functions 
miceadds::source.all( "code/shiny/R", grepstring="\\.R",  print.source=TRUE)

cache <- storr::storr_environment()

forecast_files <- list.files(path = "data-processed",
                             pattern = "*.csv",
                             full.names = TRUE,
                             recursive = TRUE)
#teams  = rlang::syms(lapply(forecast_files, function(f) unlist(strsplit(f, "/"))[2]))
data_plan <- drake::drake_plan(
  
  locations = target(
    readr::read_csv("data-locations/locations.csv",
                                 col_types = readr::cols(
                                   abbreviation  = readr::col_character(),
                                   location      = readr::col_character(),
                                   location_name = readr::col_character()))
    ),
  
  raw_data = target(read_forecasts(file,locations,team),transform  = map(file = !!forecast_files,team = !!lapply(forecast_files, function(f) unlist(strsplit(f, "/"))[2]))),
  
  #Each team is a group target with all forecasts
  teams= target(dplyr::bind_rows(raw_data),transform = combine(raw_data,.by=team)), 
  
  all_data = target(dplyr::bind_rows(teams),transform = combine(teams)),


  fourweek_date = get_next_saturday(Sys.Date() + 3*7),
  
  truth = target(
    get_truth(locations)
  ),
  truth_sources = unique(truth$source),
   
   
  #Further process the processed data for ease of exploration
   
  latest = target(
    get_latest(all_data)
    ),
   
  latest_locations = target(
    get_latest_locations(latest)
    ),

  latest_targets = target(
    get_latest_targets(latest)
    ),
 

  latest_quantiles = target(
    get_latest_quantiles(latest)
    ),

  latest_quantiles_summary = target(
    get_latest_quantiles_summary(latest_quantiles)
    ), 
  
  ensemble_data = target(
    get_ensemble_data(latest,offset=0)
  ),
  
  ensemble = target(
    get_ensemble(ensemble_data)
    ),

  ensemble_quantiles = target(
    get_ensemble_quantiles(ensemble_data)
    ),
    
  g_ensemble_quantiles = target(
    get_g_ensemble_quantiles(ensemble_quantiles)
    ),

  latest_plot_data = target(
    get_latest_plot_data(latest)
    ),
    
  #outputs = c(all_data,fourweek_date,truth,truth_sources,latest,latest_locations,latest_targets,quantiles,latest_quantiles,latest_quantiles_summary,ensemble,g_ensemble_quantiles,latest_plot_data),
  #raw_data_out = saveRDS(raw_data, file = file_out("code/shiny/drake_files/raw_data.RDS"))
  all_data_out = saveRDS(all_data, file = file_out("code/shiny/drake_files/all_data.RDS")),
  fourweek_date_out = saveRDS(fourweek_date, file = file_out("code/shiny/drake_files/fourweek_date.RDS")),
  truth_out = saveRDS(truth, file = file_out("code/shiny/drake_files/truth.RDS")),
  truth_sources_out = saveRDS(truth_sources, file = file_out("code/shiny/drake_files/truth_sources.RDS")),
  latest_out =saveRDS(latest, file = file_out("code/shiny/drake_files/latest.RDS")),
  latest_locations_out =saveRDS(latest_locations, file = file_out("code/shiny/drake_files/latest_locations.RDS")),
  latest_targets_out =saveRDS(latest_targets, file = file_out("code/shiny/drake_files/latest_targets.RDS")),
  latest_quantiles_out =saveRDS(latest_quantiles, file = file_out("code/shiny/drake_files/latest_quantiles.RDS")),
  latest_quantiles_summary_out =saveRDS(latest_quantiles_summary, file = file_out("code/shiny/drake_files/latest_quantiles_summary.RDS")),
  ensemble_out =saveRDS(ensemble, file = file_out("code/shiny/drake_files/ensemble.RDS")),
  g_ensemble_quantiles_out=saveRDS(g_ensemble_quantiles, file = file_out("code/shiny/drake_files/g_ensemble_quantiles.RDS")),
  latest_plot_data_out =saveRDS(latest_plot_data, file = file_out("code/shiny/drake_files/latest_plot_data.RDS"))
)

r = data_plan$target
drake::make(data_plan)
drake::vis_drake_graph(data_plan)


#config <- drake_config(data_plan)
#sub <- drake:::subtarget_names("raw_data", config)
#fld <- drake_failed()
