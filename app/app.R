pti <- c("shiny", "tidyverse", "ggplot2")
pti <- pti[!(pti %in% installed.packages())]
if (length(pti) > 0) {
  install.packages(pti)
}
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Read data from CSV
raw_data <- read.csv("./athlete_events_2.csv")

# Prepare Data, get sports
olympic_sports <- raw_data %>%
  filter(Year >= 2000) %>%
  select(Sport, Year, Age) 


# Define UI for application that draws a histogram
# Use a fluid Bootstrap layout
ui <- fluidPage(
  
  # Give the page a title
  titlePanel("Olympic Events"),
  
  # Generate a row with a sidebar
  sidebarLayout(
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Sport", "Select Sport:", 
                  choices = unique(olympic_sports$Sport),
                  multiple = TRUE),
      br(),
      sliderInput("year_range", "Select Year Range:",
                  min = min(olympic_sports$Year), max = max(olympic_sports$Year), 
                  value = c(min(olympic_sports$Year), max(olympic_sports$Year))),
      br(),
      sliderInput("age_range", "Select Age Range:",
                  min = 0, max = 60, value = c(0, 60))
    ),
    mainPanel(
      plotOutput("medal_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered_sports <- reactive({
    if (!is.null(input$Sport)) {
      filter(olympic_sports, 
             Sport %in% input$Sport & 
               Age >= input$age_range[1] & 
               Age <= input$age_range[2])
    } else {
      olympic_sports
    }
  })
  
  # ggplot bar plot
  output$medal_plot <- renderPlot({
    ggplot(filtered_sports(), aes(x = Year, fill = Sport)) +
      geom_bar(position = "dodge") +
      labs(title = "Age Distribution by Sports",
           x = "Year",
           y = "Number of Sportsperson",
           fill = "Sport") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
