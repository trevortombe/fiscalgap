library(tidyverse)
library(scales)
library(ggthemes)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(png)
library(gridExtra)
library(grid)
data<-read.csv("FiscalGapScenarios.csv")

ui<-fluidPage(
  titlePanel('Visualization of Fiscal Gap Analysis'),
  helpText("Based on estimates from Tombe (2020, Canadian Tax Journal)"),
  fluidRow(
    sidebarPanel(
      sliderInput('rate','Provincial Interest Rate (%)*',min=min(data$r)*100,max=max(data$r)*100,value=4,step=0.5),
      sliderInput('health_inflation', 'Health-Specific Inflation (%)',min=min(data$health_inflation)*100,max=max(data$health_inflation)*100,value=1,step=0.25),
      sliderInput('labour_prod', 'Labour Productivity Growth (%)', min=min(data$labour_prod)*100,max=max(data$labour_prod)*100,value=1,step=0.25),
      selectInput('years', 'Time Horizon (Years)', unique(data$years),selected="75"),
      selectInput('pop_scenario', 'Population Scenario', unique(data$pop_scenario)),
      hr(),
      helpText("* Federal borrowing rates are always 1 percentage points lower than provincial.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",plotOutput('plot1',height=425,width=650)),
        tabPanel("Table",tableOutput("table"))
      ),
      hr(),
      helpText("Note here."),
    ),
  )
)
