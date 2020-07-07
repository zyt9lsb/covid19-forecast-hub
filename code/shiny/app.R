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
# loadd(latest)
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
            DT::DTOutput("old_latest_targets")),
  tabPanel("New Latest targets",  
           sidebarLayout(
             sidebarPanel(
               selectInput("targets_type",     "Type", sort(unique(latest_targets$type))),
               selectInput("targets_target",     "Target", sort(unique(latest_targets$target)))
             ), 
             mainPanel(
               plotOutput("latest_targets")
             )
           )
  ),
  
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
  
  # tabPanel("Latest",           
  #          DT::DTOutput("latest")),
  
  tabPanel("Latest Viz",
           sidebarLayout(
             sidebarPanel(
               selectInput("team",         "Team", sort(unique(latest_plot_data$team         )), shiny::getShinyOption("default_team",default = "IHME")),
               selectInput("model",       "Model", sort(unique(latest_plot_data$model        )),shiny::getShinyOption("default_model")),
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
  
  tabPanel("Latest Viz by Location",
           sidebarLayout(
             sidebarPanel (
               selectInput("loc_abbreviation", "Location", sort(unique(latest_plot_data$abbreviation   ))),
               selectInput("loc_target",     "Target", sort(unique(latest_plot_data$simple_target))),
               selectInput("loc_sources", "Truth sources", truth_sources, selected = "JHU-CSSE", multiple = TRUE),
               dateRangeInput("loc_dates", "Date range", start = "2020-03-01", end = fourweek_date)
             ),
             mainPanel(
               plotOutput("latest_plot_by_location")
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
  
  output$old_latest_targets   <- DT::renderDT(latest_targets,   filter = "top")
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
  
  
  #############################################################################
  # Latest viz by Location: Filter data based on user input
  latest_loc_l <- reactive({ latest_plot_data    %>% filter(abbreviation    == input$loc_abbreviation) })
  latest_loc_lt  <- reactive({ latest_loc_l()     %>% filter(simple_target == input$loc_target) })
  
  observe({
    targets <- sort(unique(latest_loc_l()$simple_target))
    updateSelectInput(session, "loc_target", choices = targets, 
                      selected = ifelse("wk ahead cum death" %in% targets, 
                                        "wk ahead cum death", 
                                        targets[1]))
  })

  truth_loc_plot_data <- reactive({ 
    input_simple_target <- unique(paste(
      latest_loc_lt()$unit, "ahead", latest_loc_lt()$inc_cum, latest_loc_lt()$death_cases))
    
    tmp = truth %>% 
      filter(abbreviation == input$loc_abbreviation,
             grepl(input_simple_target, simple_target),
             source %in% input$loc_sources)
  })
  
  set_shiny_plot_height <- function(session, output_width_name){
    function() { 
      session$clientData[[output_width_name]] *2
    }
  }
  
  

  
  output$latest_plot_by_location      <- shiny::renderPlot({
    d    <- latest_loc_lt()
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
      geom_line(data = truth_loc_plot_data(),
                aes(x = date, y = value, 
                    linetype = source, color = source, group = source)) +
      xlim(input$loc_dates) + 
      facet_wrap(~team+model,ncol = 3,labeller = label_wrap_gen(multi_line=FALSE))+
      labs(x = "Date", y="Number", title = paste("Forecast date:", forecast_date)) +
      theme_bw() +
      theme(strip.text.x = element_text(size = 8),plot.title = element_text(color = ifelse(Sys.Date() - forecast_date > 6, "red", "black")))
        
    
    
  },height =set_shiny_plot_height(session,"output_latest_plot_by_location_width"))

  #############################################################################
  # New Latest Targets: Filter data based on user input
  
  latest_t_ty    <- reactive({ latest_targets %>% filter( type         == input$targets_type) })
  latest_t_t     <- reactive({ latest_t_ty()  %>% filter(target        == input$targets_target) })
  
  observe({
    targets <- sort(unique(latest_t_ty()$target))
    updateSelectInput(session, "targets_target", choices = targets, 
                      selected = ifelse("wk ahead cum death" %in% targets, 
                                        "wk ahead cum death", 
                                        targets[1]))
  })
  
  
  output$latest_targets <-shiny::renderPlot({
    d    <- latest_t_t()
    d <- d %>% dplyr::mutate(team_model = paste(d$team,d$model,sep="_"))
    dates <- unique(d$forecast_date)

    
    ggplot(d, aes(x=forecast_date,y=reorder(team_model,forecast_date)))+
      geom_segment(aes(x = as.Date(min(dates))-1, y = reorder(team_model,forecast_date), 
                       xend = as.Date(forecast_date,"%Y-%m-%d"), yend = reorder(team_model,forecast_date))) +
      geom_point()+
      geom_vline(xintercept = Sys.Date(), colour="red")+
      scale_x_date(breaks = sort(c(seq(as.Date(min(dates))-1, as.Date(max(dates)),length.out=7), Sys.Date())),limits = c(as.Date(min(dates))-1, Sys.Date()+1),date_labels="%m/%d")+
      labs(x="Latest Forecast Date", y = "Model",title = paste("System date:", Sys.Date()))+
      theme(axis.text.x = element_text(angle = 60),plot.title = element_text(color = "red"))
  },height=800)
  
  
  
  output$all_data         <- DT::renderDT(all_data,         filter = "top")
}

shinyApp(ui = ui, server = server)