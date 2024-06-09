library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)



train_raw <- read.csv("data/train.csv")
train_raw <- as_tibble(train_raw)

feature_data <- train_raw %>%
  mutate(priceRange = parse_number(priceRange)) %>%
  select(-uid, -city, -description)

# Sample feature_data
sampled_data <- feature_data %>%
  sample_n(1)

sampled_dataa <- data.frame(
  priceRange = sampled_data$priceRange,
  Bathrooms = sampled_data$numOfBathrooms,
  Bedrooms = sampled_data$numOfBedrooms,
  homeType = sampled_data$homeType,
  latitude = sampled_data$latitude,
  longitude = sampled_data$longitude,
  garageSpaces = sampled_data$garageSpaces,
  Spa = sampled_data$hasSpa,
  yearBuilt = sampled_data$yearBuilt,
  PatioAndPorchFeatures = sampled_data$numOfPatioAndPorchFeatures,
  lotSize = sampled_data$lotSizeSqFt,
  SchoolRating = sampled_data$avgSchoolRating,
  StudentsPerTeacher = sampled_data$MedianStudentsPerTeacher
)

# Transpose the sampled_data data frame
sampled_data <- t(sampled_dataa)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "My Shiny Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Menu Item 1", tabName = "menu1", icon = icon("dashboard")),
      menuItem("Menu Item 2", tabName = "menu2", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "menu1",
              tabsetPanel(
                tabPanel("Tab 1", 
                         fluidRow(
                           box(
                             width = 6,
                             fluidRow(
                               column(width = 6, selectInput("homeType", "Home Type", choices = c("Single Family", "Apartment", "Condo"))),
                               column(width = 6, numericInput("latitude", "Latitude", value = sampled_dataa$latitude))
                             ),
                             fluidRow(
                               column(width = 6, numericInput("longitude", "Longitude", value = sampled_dataa$longitude)),
                               column(width = 6, numericInput("garageSpaces", "Garage Spaces", value = sampled_dataa$garageSpaces))
                             ),
                             fluidRow(
                               column(width = 6, selectInput("hasSpa", "Has Spa", choices = c("True", "False"))),
                               column(width = 6, numericInput("yearBuilt", "Year Built", value = sampled_dataa$yearBuilt))
                             )
                             ,
                             fluidRow(
                               column(width = 6, numericInput("lotSizeSqFt", "Lot Size (sq ft)", value = sampled_dataa$lotSize)),
                               column(width = 6, numericInput("numOfPatioAndPorchFeatures", "Number of Patio and Porch Features", value = sampled_dataa$PatioAndPorchFeatures))
                             ),
                             fluidRow(
                               column(width = 6, numericInput("avgSchoolRating", "Average School Rating", value = sampled_dataa$SchoolRating)),
                               column(width = 6, numericInput("MedianStudentsPerTeacher", "Median Students Per Teacher", value = sampled_dataa$StudentsPerTeacher))
                             ),
                             fluidRow(
                               column(width = 6, numericInput("numOfBathrooms", "Number of Bathrooms", value =  sampled_dataa$Bathrooms)),
                               column(width = 6, numericInput("numOfBedrooms", "Number of Bedrooms", value =  sampled_dataa$Bedrooms)) 
                             ),
                             textOutput("prediction")
                           ),
                           box(title = "Housing feature from Database",
                               DTOutput("random_row_table")
                           )
                         )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  output$random_row_table <- renderDT({
    # Rename the column headers and swap the columns
    sampled_data <- data.frame(values = sampled_data[,1])
    
    datatable(sampled_data, options = list(scrollY = "250px", scrollCollapse = TRUE, paging = FALSE, searching = FALSE, lengthChange = FALSE, info = FALSE))
  })

  # When inputs change, make predictions
  observe({
    new_data <- data.frame(
      homeType = input$homeType,
      latitude = input$latitude,
      longitude = input$longitude,
      garageSpaces = input$garageSpaces,
      hasSpa = input$hasSpa,
      yearBuilt = input$yearBuilt,
      numOfPatioAndPorchFeatures = input$numOfPatioAndPorchFeatures,
      lotSizeSqFt = input$lotSizeSqFt,
      avgSchoolRating = input$avgSchoolRating,
      MedianStudentsPerTeacher = input$MedianStudentsPerTeacher,
      numOfBathrooms = input$numOfBathrooms,
      numOfBedrooms = input$numOfBedrooms,
      priceRange = NA
    )
    
    # Make predictions using the loaded Random Forest model
    predictions <- predict(model, data = new_data)
    
    # Output the prediction
    output$prediction <- renderText({
      paste("Predicted Price: $", round(predictions$predictions, 2))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
