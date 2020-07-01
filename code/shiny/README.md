# COVID-19 Forecast Hub Shiny App

The shiny app is complementary to the COVID-19 Forecast Hub 
[Dashboard](https://reichlab.io/covid19-forecast-hub/).
These files are a work-in-progress version of the files in [data-processed/](../../data-processed).
For now, the data-processed/ files should be used.

If you want to try out this new version of the shiny app you can use 

    source("code/shiny/new_plan.R")
    drake::make(plan, target = shiny)
    source("code/shiny/new_app.R")
    shinyApp(ui = ui, server = server) # if it doesn't automatically run
    
from the base folder of the repository.

## Background

Originally the app was designed to be an internal tool for the COVID-19 Forecast
Hub team to know what forecast existed within the repository,
aid in the building of an ensemble forecast, 
and (quickly) visualize forecasts (before updating of the Dashboard). 

On May 15, the app was introduced to teams submitting forecasts and therefore
the user base is a lot wider than original intended. 

## Uses

The primary use of the app is for the COVID-19 Forecast Hub team to know what
forecasts are in the repository. Most of the shiny app tabs are aimed at this 
goal including the tabs All, Latest, Latest locations, Latest targets, and
Latest quantiles. These tabs all provide interactive access to data sets that 
provide details about the latest forecast for each model with the exception
of the "All" tab which has all of the forecasts for each model 
(not just the latest forecasts). 

The tab that is likely more helpful to teams is the "Latest viz" tab. 
This tab provides a visualization of all the teams forecasts similar to the
Dashboard, but with some slightly different features. 
This tab can be used to visualize forecasts before submitting those forecasts
to the Hub. 

## ToDo

A number of upgrades to the shiny app are on the to do list:

- Improve speed of reading the data in
  - Use [fread](https://www.rdocumentation.org/packages/data.table/versions/1.12.8/topics/fread) rather than [read_csv](https://readr.tidyverse.org/reference/read_delim.html)
  - Use [drake](https://github.com/ropensci/drake) to provide GNU make functionality
- Allow users to specify default team and model through [options](https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html)
- Maintain target and location while changing team and model
- Display figures rather than tables
- Remove "Latest" tab and associated `loadd(latest)` from the app as this is the slowest step
