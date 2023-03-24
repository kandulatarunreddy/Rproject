library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)
library(scales)

# Define UI
ui <- fluidPage(
  titlePanel("HAWK RECESSION INDEX"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV file",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      checkboxInput("header", "Header", TRUE),
      selectInput("xvar", "X-Axis",
                  choices = c("Month")),
      selectInput("yvar", "Y-Axis",
                  choices = c("Recession")),
      
      #OPTION: Date Range
      dateRangeInput("date_range", 
                     "Date range:", 
                     start = "2000-01-01", 
                     end = "2020-04-01") # Add date range slide
    ),
    mainPanel(
      plotOutput("lineplot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Read CSV file
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath, header = input$header)
    df[[input$xvar]] <- ymd(df[[input$xvar]]) # Convert x-axis variable to Date type
    df
  })
  
  # Filter data by date range
  filtered_data <- reactive({
    df <- data()
    df <- df[df[[input$xvar]] >= input$date_range[1] & df[[input$xvar]] <= input$date_range[2],]
  })
  
  # Generate line plot
  output$lineplot <- renderPlot({
    ggplot(filtered_data(),  #if you don't want the date range option, you can replce `filtered_data` by `data`
           aes_string(x = as.name(input$xvar), 
                      y = as.name(input$yvar),
                      group = 1)) +
      geom_line() +
      coord_cartesian(ylim = c(-1, 1)) +
      theme_minimal()+
      xlab('Year')
  },
  height = 400,
  width = 400)
}

# Run the app
shinyApp(ui, server)