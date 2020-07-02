library("drake")
source("code/shiny/drake_plan.R")
drake_config(plan, targets = shiny)
