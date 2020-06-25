library("drake")
library("tidyverse")
library("shiny")
library("DT")

options(DT.options = list(pageLength = 50))

source("code/processing-fxns/get_next_saturday.R")

# all_data = readRDS("code/shiny/drake_files/all_data.RDS")
fourweek_date = get_next_saturday(Sys.Date() + 3*7)
loadd(truth)
truth_sources = unique(truth$source)
loadd(latest)
loadd(latest_locations)
loadd(latest_targets)
loadd(latest_quantiles)
loadd(latest_quantiles_summary)
# ensemble=readRDS("code/shiny/drake_files/ensemble.RDS")
# g_ensemble_quantiles=readRDS("code/shiny/drake_files/g_ensemble_quantiles.RDS")
loadd(latest_plot_data)
# latest_plot_data=readRDS("code/shiny/drake_files/latest_plot_data.RDS")

ui <- navbarPage(
  "Explore:",
  
  tabPanel("Latest locations", 
           DT::DTOutput("latest_locations")),
  
  tabPanel("Latest targets",  
           h5("max_n: the farthest ahead forecast for this target (does not guarantee that all earlier targets exist)"),
           DT::DTOutput("latest_targets")),
  
  tabPanel("Latest quantiles", 
           h3("Quantiles collapsed over targets"),
           h5("all_full: the full set of 23 quantiles exists in all targets"),
           h5("any_full: the full set of 23 quantiles exists in at least one target"),
           h5("all_min: the minimum set of 9 quantiles exists in all targets"),
           h5("any_min: the minimum set of 9 quantiles exists in at least one target"),
           DT::DTOutput("latest_quantiles_summary"), 
           h3("Quantiles by target"),
           DT::DTOutput("latest_quantiles")),
  
  # tabPanel("Ensemble",           
  #          DT::DTOutput("ensemble"),
  #          # DT::DTOutput("ensemble_quantiles"),
  #          plotOutput("ensemble_quantile_plot")),
  
  tabPanel("Latest",           
           DT::DTOutput("latest")),
  
  tabPanel("Latest Viz",
           sidebarLayout(
             sidebarPanel(
               selectInput("team",         "Team", sort(unique(latest_plot_data$team         )), "IHME"),
               selectInput("model",       "Model", sort(unique(latest_plot_data$model        ))),
               selectInput("target",     "Target", sort(unique(latest_plot_data$simple_target))),
               selectInput("abbreviation", "Location", sort(unique(latest_plot_data$abbreviation   ))),
               selectInput("sources", "Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
               dateRangeInput("dates", "Date range", start = "2020-03-01", end = fourweek_date)
               ), 
             mainPanel(
               plotOutput("latest_plot")
             )
           )
  ),
  
  # tabPanel("All",              
  #          DT::DTOutput("all_data")),
  
  tabPanel("Help",
           h3("Explore tabs"),
           # h5("All: contains all of the processed data including those with missing required fields"),
           h5("Latest: most recent forecast for each team-model"),
           h5("Latest targets: summarizes `Latest` to see which targets are included"),
           h5("Latest locations: summarizes `Latest` to see which locations are included"),
           h5("Latest quantiles: summarizes `Latest` to see which quantiles are included"),
           h3("Usage"),
           h4("Each table has the capability to be searched and filtered")
  ),
  
  selected = "Latest Viz"
)

filter_names <- c("input$team", "input$model","input$target", "input$abbreviation")
filters <- c("team %in% input$team","model %in% input$model","target %in% input$target", "abbreviation %in% input$abbreviation")
checknull <- NULL

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$latest_targets   <- DT::renderDT(latest_targets,   filter = "top")
  output$latest_locations <- DT::renderDT(latest_locations, filter = "top")
  output$latest_quantiles <- DT::renderDT(latest_quantiles, filter = "top")
  output$latest_quantiles_summary <- DT::renderDT(latest_quantiles_summary, filter = "top")
  
  output$ensemble         <- DT::renderDT(ensemble,         filter = "top")
  # output$ensemble_quantiles        <- DT::renderDT(ensemble_quantiles,         filter = "top")
  output$ensemble_quantile_plot <- shiny::renderPlot(g_ensemble_quantiles)
  
  output$latest           <- DT::renderDT(latest,           filter = "top")
  
  #############################################################################
  # Latest viz: Filter data based on user input
  
  observe({
    models <- sort(unique(latest_t()$model))
    updateSelectInput(session, "model", choices = models, selected = models[1])
  })
  
  observe({
    targets <- sort(unique(latest_tm()$simple_target))
    updateSelectInput(session, "target", choices = targets, 
                      selected = ifelse("wk ahead cum death" %in% targets, 
                                        "wk ahead cum death", 
                                        targets[1]))
  })
  
  observe({
    abbreviations <- sort(unique(latest_tmt()$abbreviation))
    updateSelectInput(session, "abbreviation", choices = abbreviations, 
                      selected = ifelse("US" %in% abbreviations, 
                                        "US", 
                                        abbreviations[1]))
  })
  
  
  latest_t    <- reactive({ latest_plot_data %>% filter(team          == input$team) })
  latest_tm   <- reactive({ latest_t()       %>% filter(model         == input$model) })
  latest_tmt  <- reactive({ latest_tm()      %>% filter(simple_target == input$target) })
  latest_tmtl <- reactive({ latest_tmt()     %>% filter(abbreviation    == input$abbreviation) })

  truth_plot_data <- reactive({ 
    input_simple_target <- unique(paste(
      latest_tmtl()$unit, "ahead", latest_tmtl()$inc_cum, latest_tmtl()$death_cases))
    
    tmp = truth %>% 
      filter(abbreviation == input$abbreviation,
             grepl(input_simple_target, simple_target),
             source %in% input$sources)
  })
  
  
  
  
  output$latest_plot      <- shiny::renderPlot({
    d    <- latest_tmtl()
    team <- unique(d$team)
    model <- unique(d$model)
    forecast_date <- unique(d$forecast_date)
    
    ggplot(d, aes(x = target_end_date)) + 
      geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`, fill = "95%")) +
      geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`, fill = "50%")) +
      scale_fill_manual(name = "", values = c("95%" = "lightgray", "50%" = "gray")) +
      
      geom_point(aes(y=`0.5`, color = "median")) + geom_line( aes(y=`0.5`, color = "median")) + 
      geom_point(aes(y=point, color = "point")) + geom_line( aes(y=point, color = "point")) + 
      
      scale_color_manual(name = "", values = c("median" = "slategray", "point" = "black")) +
      
      ggnewscale::new_scale_color() +
      geom_line(data = truth_plot_data(),
                aes(x = date, y = value, 
                    linetype = source, color = source, group = source)) +
      
      scale_color_manual(values = c("JHU-CSSE" = "green",
                                    "USAFacts" = "seagreen",
                                    "NYTimes"  = "darkgreen")) +
      
      scale_linetype_manual(values = c("JHU-CSSE" = 1,
                                       "USAFacts" = 2,
                                       "NYTimes"  = 3)) +
      
      xlim(input$dates) + 
      
      labs(x = "Date", y="Number", 
           title = paste("Forecast date:", forecast_date)) +
      theme_bw() +
      theme(plot.title = element_text(color = ifelse(Sys.Date() - forecast_date > 6, "red", "black")))
  })
  
  output$all_data         <- DT::renderDT(all_data,         filter = "top")
}

shinyApp(ui = ui, server = server)
