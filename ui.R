#install and load required packages -----------------
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("shiny", "dplyr","ggplot2", "tidyr","DT")
ipak(packages)

shinyUI(pageWithSidebar(
  headerPanel("Alberta Aerial Ungulate Survey Program Assessment Tool"),
  sidebarPanel(
    fileInput('MegaDB', 'Step 1. Choose your base AUS priority file.',
              accept='.csv'),
    sliderInput("AEPBudget", "Step 1: Choose the funding amount (thousands)", min=0, max=3000, value=1200, step = 5),
    sliderInput("ForecastYr", "Step 2: What year would you like to forecast to?", min=2019, max=2050, value=2040, step=1)

  ),
  mainPanel(plotOutput("DefForecast"), plotOutput("PerfPlot") , DT::dataTableOutput("AUS_Sched"))
    ))