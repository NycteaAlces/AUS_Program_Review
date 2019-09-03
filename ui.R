#install and load required packages -----------------
#ipak <- function(pkg){
#  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#  if (length(new.pkg))
#    install.packages(new.pkg, dependencies = TRUE)
#  sapply(pkg, require, character.only = TRUE)
#}

#packages <- c("shiny", "dplyr","ggplot2", "tidyr","DT","googlesheets")
#ipak(packages)
 library(shiny)
 library(dplyr)
 library(ggplot2)
library(tidyr)
library(DT)
library(RCurl)
library(Cairo)
library(shinyjs)

#options(shiny.usecairo=T)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Program Assessment Tool"),
  sidebarPanel(
 #   fileInput('MegaDB', 'Step 1. Choose your base AUS priority file.',
 #             accept='.csv'),
    sliderInput("AEPBudget", "Step 1: Choose the RELM funding amount (thousands)", min=0, max=3000, value=684, step = 5),
    sliderInput("EMSDBudget", "Step 2: Choose the EMSD funding amount (thousands)", min=0, max=1250, value=325, step = 5),
    sliderInput("ForecastYr", "Step 3: What year would you like to forecast to?", min=2020, max=2050, value=2040, step=1),
    checkboxInput("TopUp","Check if Ops funds will be used to top-up WMUs designated for EMSD funding", FALSE)

  ),
  mainPanel(plotOutput("PerfPlot") , plotOutput("DefForecast"),plotOutput("Schedule"),plotOutput("SchedulebyReg"), plotOutput("PredOut20"), plotOutput("PredOut"), DT::dataTableOutput("AUS_Sched"))
    ))
