library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(fredr)
library(caret)
library(tidyr)

fredr_set_key('f31374c525421a4b2158ade45fea6a85')

data <- fredr(series_id = "T10YFF", observation_start = as.Date("1962-01-02"), observation_end = as.Date("2023-03-31"), frequency = "m")

# Get the NBER recession indicator data
nber_data <- fredr(series_id = "USREC", observation_start = as.Date("1962-01-02"), observation_end = as.Date("2023-03-31"))

# Merge the yield curve and NBER recession indicator data
merged_data <- merge(data, nber_data, by = "date")
names(merged_data)[3] <- "yield_curve"
names(merged_data)[7] <- "nber_rec"

ui <- fluidPage(
  titlePanel("HAWK RECESSION INDEX"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", label = "Date range", start = min(merged_data$date), end = max(merged_data$date), min = min(merged_data$date), max = max(merged_data$date))
    ),
    mainPanel(
      plotlyOutput("line_plot"),
      verbatimTextOutput("accuracy")
    )
  )
)

server <- function(input, output) {
  
  # Create a reactive expression for the date range and the probit values
  filtered_data <- reactive({
    data <- merged_data %>%
      mutate(probit_values = fitted(glm(nber_rec ~ yield_curve, data = ., family = binomial(link = "probit")), type = "response")) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2]) %>%
      mutate(probit_values = ifelse(is.na(lag(probit_values)), 0, lag(probit_values)))
    
    # Create a group column based on changes in the nber_rec column
    data$group <- cumsum(data$nber_rec != lag(data$nber_rec, default = first(data$nber_rec)))
    
    data
  })
  
  
  
  
  output$line_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(date, probit_values, group = group)) +
      geom_segment(aes(x = date, xend = lead(date), y = probit_values, yend = lead(probit_values), color = factor(nber_rec)), size = 1) +
      scale_color_manual(values = c("black", "blue"), labels = c("Expansion", "Recession")) +
      labs(color = "NBER Recession Indicator", x = "Date", y = "Probit Values")
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  
  
  output$accuracy <- renderPrint({
    # Subset the data based on the selected date range
    subset_data <- filtered_data()
    model <- glm(nber_rec ~ yield_curve, data = subset_data, family = binomial(link = "probit"))
    (Probit.pred <- (fitted(model) > 0.5) %>% as.numeric %>% as.factor) # Prediction
    (actual <- subset_data$nber_rec %>% as.factor) # actual data
    # caret::confusionMatrix(Probit.pred, actual, positive = "1")
    # Predict the recession indicator using the fitted model
    #predicted_y_cap <- ifelse(fitted(model, newdata = subset_data, type = "response") > 0.5, 1, 0)
    # Compute the confusion matrix and accuracy
    confusion <- confusionMatrix(Probit.pred, actual, positive = "1")
    paste0("Accuracy: ", confusion$overall["Accuracy"])
  })
  
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
