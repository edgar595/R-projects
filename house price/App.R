library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

train_raw <- read.csv("data/train.csv")
train_raw <- as_tibble(train_raw)

model <- readRDS(file = "rf_model.rds")

feature_data <- train_raw %>%
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
                               column(width = 6, numericInput("numOfBathrooms", "Number of Bathrooms", value =  sampled_dataa$Bathrooms)),
                               column(width = 6, numericInput("numOfBedrooms", "Number of Bedrooms", value =  sampled_dataa$Bedrooms)) 
                             ),
                             fluidRow(
                               column(width = 6, selectInput("homeType", "Home Type", choices = c("Single Family", "Apartment", "Condo"))),
                               column(width = 6, numericInput("latitude", "Latitude", value = round(sampled_dataa$latitude, 5)))
                             ),
                             fluidRow(
                               column(width = 6, numericInput("longitude", "Longitude", value = round(sampled_dataa$longitude, 5))),
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
                             textOutput("prediction")
                           ),
                           box(title = "Housing feature from Database",
                               DTOutput("random_row_table")
                           ),
                           box(uiOutput("additional_box"), height = 220)
                         )
                ),
                
                tabPanel("plots",
                         fluidRow(
                           plotOutput("featureplots", height = "580px")
                         )),
                tabPanel("Tab 2", 
                         fluidRow(
                           box(title = "Content for Tab 2", "This is the content for Tab 2"),
                           box(title = "Another Box for Tab 2", "This is another box for Tab 2")
                         )
                )
              )
      ),
      tabItem(tabName = "menu2",
              tabsetPanel(
                tabPanel("Word", 
                         fluidRow(
                           column(width = 12,
                                  box(
                                    width = 12,
                                    title = "proportion of total words used for high price homes",
                                    plotOutput("highwordPlot", height = "200px")
                                  )
                           ),
                           column(width = 12,
                                  box(
                                    width = 12,
                                    title = "proportion of total words used for Low price homes",
                                    plotOutput("lowwordPlot", height = "200px")
                                  )
                           )
                         )
                ),
                tabPanel("Word Model",
                         fluidRow(
                           column(width = 12, box(width = 12, plotOutput("wording_plot"), height = 418))
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
  
  output$additional_box <- renderUI({
    # Extract the number of bedrooms
    bedrooms <- as.numeric(sampled_dataa$Bedrooms)
    
    # Determine the image path based on the number of bedrooms
    if (bedrooms == 1) {
      img_path <- "onebed.png"
    } else if (bedrooms == 2) {
      img_path <- "twobed.png"
    } else if (bedrooms == 3) {
      img_path <- "threebed.png"
    } else if (bedrooms == 4) {
      img_path <- "fourbed.png"
    } else {
      img_path <- "fivebed.jpg"
    }
    
    # Create the image tag
    tags$img(src = img_path, height = 200)
  })
  
  output$wording_plot <- renderPlot({

    word_word_cloud %>%
      mutate(class = factor(class)) %>%
      ggplot(aes(label = word, color = class, size = n)) +
      geom_text_wordcloud_area() +
      facet_wrap(~class) +
      scale_size_area(max_size = 24)
  })
  
  output$highwordPlot <- renderPlot({
    word_freq %>%
      filter(word %in% highest_words) %>%
      ggplot(aes(priceRange, propotion, color = word)) +
      geom_line(size = 2.5, alpha = 0.7, show.legend = FALSE) +
      facet_wrap(vars(word), nrow = 1) +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
      labs(x = NULL, y = "propotion at high price house") +
      theme_light()
  })
  
  output$lowwordPlot <- renderPlot({ 
  word_freq %>%
    filter(word %in% lowest_words) %>%
    ggplot(aes(priceRange, propotion, color = word)) +
    geom_line(size = 2.5, alpha = 0.7, show.legend = FALSE) +
    facet_wrap(vars(word), nrow = 1) +
    scale_x_continuous(labels = scales::dollar) +
    scale_y_continuous(labels = scales::percent, limits = c(0, NA)) +
    labs(x = NULL, y = "proportion of total words used for homes at lower price") 
  })
  
  output$featureplots <- renderPlot({
    price_plot <- train_raw %>%
      mutate(priceRange = parse_number(priceRange)) %>%
      ggplot(aes(longitude, latitude, z = priceRange)) +
      stat_summary_hex(alpha = 0.8, bins = 50) +
      scale_fill_viridis_c() +
      labs( fill = "mean", title = "Price" )
    
    price_plot
    
    plot_austin <- function(var, title)
      train_raw %>%
      ggplot(aes(longitude, latitude, z = {{var}} )) +
      stat_summary_hex(alpha = 0.8, bins = 50) +
      scale_fill_viridis_c() +
      labs( fill = "mean", title = title )
    
    (price_plot + plot_austin(avgSchoolRating, "School Rating")) /
      (plot_austin(yearBuilt, "Year Built") + plot_austin(log(lotSizeSqFt), "Lot Size")) 
    
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
