library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(lubridate)

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
                  choices = c("Recession"))
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
    read.csv(input$file$datapath, header = input$header)
  })
  
  # Generate line plot
  output$lineplot <- renderPlot({
    ggplot(data(), aes_string(x = ymd(as.name(input$xvar)), y = as.name(input$yvar), group = 1)) +
      geom_line()+
      coord_cartesian(xlim = c(0, 15), ylim = c(-1, 1))
    #geom_text(aes(label = Month), nudge_x = 0.1, check_overlap = TRUE, size = 3) +
      #theme_minimal()
  }, height = 400, width = 400)
  
  
}

# Run the app
shinyApp(ui, server)