library(shinydashboard)
library(tidyverse)
library(plotly)
library(sf)

health <- read_csv("data/health.csv")

health_data <- read_csv("data/health_data.csv")

merged_data <- read_csv("data/merged_data.csv")

health_data <- health_data %>%
  mutate(month = month(period, label = TRUE),
         year = lubridate::year(period))

health_longer <- health_data %>%
  arrange(county) %>%
  pivot_longer(total_dewormed:stunted_child, names_to = "Deficiency", values_to = "Cases")

# Read in the Kenyan counties data
counties <- st_read("data/kenyan-counties")

ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Health Analysis",
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                   text = "5 new users today",
                                                   icon("users")
                                                 ),
                                                 notificationItem(
                                                   text = "Clinical Analysis",
                                                   icon("signal"),
                                                   status = "success"
                                                 ),
                                                 notificationItem(
                                                   text = "County summary",
                                                   icon("chart-bar"),
                                                   status = "warning"
                                                 )
                                    )
                    ),
                    dashboardSidebar(
                      sidebar <- dashboardSidebar(
                        sidebarMenu(
                          id = "sidebar",
                          menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                          menuItem("County", icon = icon("th"), tabName = "counties",
                                   badgeLabel = "new", badgeColor = "green")
                        )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "dashboard",
                                tabBox(id = "t1", width = 12,
                                       tabPanel("About", icon = icon("address-card"),
                                                selectInput("county_choice", "Selected County:",
                                                            choices = counties$COUNTY),
                                                fluidRow(
                                                  column(width = 7, plotOutput("county_plot")),
                                                  box(
                                                    title = "Deficiency Cases", width = 5, solidHeader = TRUE, status = "primary",
                                                    column(width = 6, plotOutput("dew_chart", height = 90)),
                                                    column(width = 6, plotOutput("underPlot", height = 90)),
                                                    column(width = 6, plotOutput("acutePlot", height = 90)),
                                                    column(width = 6, plotOutput("dia_chart", height = 90)),
                                                    column(width = 6, plotOutput("stuntedPlot", height = 90))
                                                  )
                                                  )
                                                ),
                                       
                                       
                                       tabPanel(title = "Data", icon = icon("address-card")
                                                ,fluidRow(
                                         column(width = 8, offset = 4,  # Centering the column
                                                box(title = "Select Plot", status = "primary", solidHeader = TRUE,
                                                    selectInput("text_select", label = "Select Text", 
                                                                choices = c("Total Dewormed" = "total_dewormed",
                                                                            "Underweight Children" = "underweight_child",
                                                                            "Acute Malnutrition Cases" = "acute_malnutrition",
                                                                            "Diarrhoea Cases" = "diarrhoea_cases",
                                                                            "Stunted Children" = "stunted_child"),
                                                                selected = "total_dewormed")
                                                )
                                         )
                                       ), fluidRow(
                                         box(plotlyOutput("health_plot"), width = 8),
                                         box(h2("Summary of Deficiencies"), textOutput("summary_text"), width = 4)
                                       )),
                                       tabPanel(title = "structure", icon = icon("address-card"),
                                                fluidRow( 
                                                  column(width = 12,
                                                         tags$br(),
                                                         tags$p("The code generates a line plot to visualize the occurrences of health issues or deficiencies over time for each county. Each line represents a different deficiency type, and the points overlaying the lines mark individual data points. The plot is split into separate panels for each county, facilitating comparison across counties.")),
                                                  column(width = 12,
                                                         fluidRow(
                                                           plotOutput("facet_plot")
                                                         ))
                                                  
                                                )),
                                       tabPanel("summary", icon = icon("address-card"),
                                        verbatimTextOutput("summary"))
                                )
                        ),
                        tabItem(tabName = "counties",
                                tabBox(id = "t2", width = 12,
                                       tabPanel(title = "Counties Summary",
                                                selectInput(
                                                  "town_choice", "Select town:",
                                                  choices = counties$COUNTY
                                                ),
                                                fluidRow(column(width = 9, plotlyOutput("location_plot")),
                                                         
                                                         ),
                                                fluidRow(
                                                  width = 12,
                                                  infoBoxOutput("info_box_1"),
                                                  infoBoxOutput("info_box_2"),
                                                  infoBoxOutput("info_box_3")
                                                )
                                                
                                                )
                                       
                                ))
                      )
                    )
                    
)

server = function(input, output) { 
  # Reactive expression to filter data based on selected input
  selected_data <- reactive({
    case_when(
      input$text_select == "total_dewormed" ~ health_data,
      input$text_select == "underweight_child" ~ health_data,
      input$text_select == "acute_malnutrition" ~ health_data,
      input$text_select == "diarrhoea_cases" ~ health_data,
      input$text_select == "stunted_child" ~ health_data
    )
  })
  
  
  #summary
  output$summary <- renderPrint({
    health_data %>% summary()
  })
  
  # Generate summary sentence
  output$summary_text <- renderText({
    data <- selected_data()
    highest_month <- data %>%
      group_by(month) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(desc(total)) %>%
      slice_head(n = 1)
    
    lowest_month <- data %>%
      group_by(month) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(total) %>%
      slice_head(n = 1)
    
    highest_year <- data %>%
      group_by(year) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(desc(total)) %>%
      slice_head(n = 1)
    
    lowest_year <- data %>%
      group_by(year) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(total) %>%
      slice_head(n = 1)
    
    highest_counties <- data %>%
      group_by(county) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(desc(total)) %>%
      slice_head(n = 3)
    
    lowest_counties <- data %>%
      group_by(county) %>%
      summarize(total = sum(!!sym(input$text_select))) %>%
      arrange(total) %>%
      slice_head(n = 3)
    
    # Extract the county names from lowest_counties
    highest_county_names <- highest_counties$county
    lowest_county_names <- lowest_counties$county
    
    # Concatenate the results properly
    output_text <- paste("In terms of years, the highest number of cases was recorded and observed in", highest_year$year, "with a total of", highest_year$total,
                         "while the lowest number of cases was recorded in", lowest_year$year, "with a total of", lowest_year$total, ".",
                         "Looking at months, the lowest number of cases occurred in", lowest_month$month, "with a total of", lowest_month$total,
                         "and the highest number of cases occurred in", highest_month$month, "with a total of", highest_month$total, ".",
                         "Analyzing by counties, it's notable that the highest number of cases were reported in", highest_county_names[1],",", highest_county_names[2],",", highest_county_names[3],
                         "whereas the lowest number of cases were reported in", lowest_county_names[1],",", lowest_county_names[2],",", lowest_county_names[3], ".")
    
    # Return the concatenated text
    return(output_text)
  })
  
  output$health_plot <- renderPlotly({
    # Function to generate health plots based on selected variable
    health_plots <- function(data, var, title) {
      p <- ggplot(data, aes_string(x = "period", y = var, color = "county")) +
        geom_point(show.legend = FALSE) +
        labs(title = title)
      return(p)
    }
    
    # Get the selected variable from the input
    selected_var <- input$text_select
    
    # Generate plot based on the selected variable
    plot <- health_plots(health_data, selected_var, selected_var)
    
    # Convert ggplot to plotly
    plotly_plot <- ggplotly(plot) %>% layout(showlegend = FALSE, xaxis = list(title = ""))
    
    # Render the plotly plot
    return(plotly_plot)
  })
  
  output$facet_plot <- renderPlot({
    health_longer %>%
      ggplot(aes(period, Cases, color = Deficiency)) +
      geom_line(alpha = 0.7, size = 1.5) +
      geom_point() +
      facet_wrap(~county)+
      labs(x=NULL) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
            strip.text = element_text(size = 10), # Adjust strip text size
            plot.margin = margin(20, 20, 20, 20, "pt")) # Add margins to the plot
    
  }, height = 680)
  
  output$county_plot <- renderPlot({
    # Filter the data based on the selected county
    selected_county <- input$county_choice
    selected_county_data <- counties %>%
      filter(COUNTY == selected_county)
    
    # Plot selected county in green and others in white
    ggplot() +
      geom_sf(data = selected_county_data, fill = "blue") +
      geom_sf(data = counties %>% filter(COUNTY != selected_county), fill = "white") +
      theme_minimal()
  })
  
  output$dia_chart <- renderPlot({
    # Filter the health data based on the selected county
    selected_county <- input$county_choice
    selected_diarrhoea <- health_longer %>%
      filter(county == selected_county, Deficiency == "diarrhoea_cases") %>%
      summarise(county_cases = mean(Cases)) %>%
      pull(county_cases)
    
    # Prepare data for plotting
    data <- data.frame(
      Category = c(selected_county),
      Cases = c(selected_diarrhoea)
    )
    
    data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond 2000", "Below 2000")
    
    # Plot
    ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 10300),  color = "#e1f1fd", size = 18) + 
      labs(title = paste("Diarrhoea ",selected_county),
           y = NULL,
           x = NULL) +
      scale_y_continuous(limits = c(0, 10300)) +  # Setting y-axis limits
      theme(legend.position = "none",
            plot.title = element_text(size = 12)) +
      coord_flip()
  })
  
  output$dew_chart <- renderPlot({
    # Filter the health data based on the selected county
    selected_county <- input$county_choice
    selected_dewormed <- health_longer %>%
      filter(county == selected_county, Deficiency == "total_dewormed") %>%
      summarise(county_cases = mean(Cases)) %>%
      pull(county_cases)
    
    # Prepare data for plotting
    data <- data.frame(
      Category = c(selected_county),
      Cases = c(selected_dewormed)
    )
    
    data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond chart", "Above Chart")
    
    # Plot
    ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 70000), color = "#e1f1fd", size = 18) + 
      labs(title = paste("Dewormed in",selected_county),
           y = NULL,
           x = NULL) +
      scale_y_continuous(limits = c(0, 70000)) +  # Setting y-axis limits
      theme(legend.position = "none",
            plot.title = element_text(size = 12)) +
      coord_flip()
  })
  
  output$underPlot <- renderPlot({
    # Filter the health data based on the selected county
    selected_county <- input$county_choice
    selected_underweight <- health_longer %>%
      filter(county == selected_county, Deficiency == "underweight_child") %>%
      summarise(county_cases = mean(Cases)) %>%
      pull(county_cases)
    
    # Prepare data for plotting
    data <- data.frame(
      Category = c(selected_county),
      Cases = c(selected_underweight)
    )
    
    data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond chart", "Above Chart")
    
    # Plot
    ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 5400),  color = "#e1f1fd", size = 18) + # Adjusting size to make it thicker
      labs(title = paste("Underweight in", selected_county),
           y = NULL,
           x = NULL) +
      scale_y_continuous(limits = c(0, 5400)) +  # Setting y-axis limits
      theme(legend.position = "none",
            plot.title = element_text(size = 12)) +
      coord_flip()
  })
  
  output$acutePlot <- renderPlot({
    # Filter the health data based on the selected county
    selected_county <- input$county_choice
    selected_acute_malnutrition <- health_longer %>%
      filter(county == selected_county, Deficiency == "acute_malnutrition") %>%
      summarise(county_cases = mean(Cases)) %>%
      pull(county_cases)
    
    # Prepare data for plotting
    data <- data.frame(
      Category = c(selected_county),
      Cases = c(selected_acute_malnutrition)
    )
    
    data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond chart", "Above Chart")
    
    # Plot
    ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 1140),  color = "#e1f1fd", size = 18) + # Adjusting size to make it thicker
      labs(title = paste("Underweight in",selected_county),
           y = NULL,
           x = NULL) +
      scale_y_continuous(limits = c(0, 1140)) +  # Setting y-axis limits
      theme(legend.position = "none",
            plot.title = element_text(size = 12)) +
      coord_flip()
  })
  
  output$stuntedPlot <- renderPlot({
    # Filter the health data based on the selected county
    selected_county <- input$county_choice
    selected_stunted_child <- health_longer %>%
      filter(county == selected_county, Deficiency == "stunted_child") %>%
      summarise(county_cases = mean(Cases)) %>%
      pull(county_cases)
    
    # Prepare data for plotting
    data <- data.frame(
      Category = c(selected_county),
      Cases = c(selected_stunted_child)
    )
    
    data$Highlight <- ifelse(data$Cases > (data$Cases + 1), "Beyond chart", "Above Chart")
    
    # Plot
    ggplot(data, aes(x = "", y = Cases, fill = Highlight)) +
      geom_bar(stat = "identity", fill = "blue") +
      geom_segment(aes(x = 1, xend = 1, y = data$Cases + 1, yend = 3570),  color = "#e1f1fd", size = 18) + # Adjusting size to make it thicker
      labs(title = paste("Stunted in",selected_county),
           y = NULL,
           x = NULL) +
      scale_y_continuous(limits = c(0, 3570)) +  # Setting y-axis limits
      theme(legend.position = "none",
            plot.title = element_text(size = 12)) +
      coord_flip()
  })
  
  # Function to determine the color based on the value
  get_color <- function(percent_change) {
    ifelse(percent_change > 0, "blue", "red")
  }
  
  # Render info boxes with dynamically determined colors and text
  output$info_box_1 <- renderInfoBox({
    selected_town <- input$town_choice
    
    previous_dewormed <- merged_data %>%
      filter(county == selected_town, Deficiency == "total_dewormed") %>%
      filter(period == "2023-05-28") %>%
      pull(Cases)
    
    predicted_dewormed <- merged_data %>%
      filter(county == selected_town, Deficiency == "total_dewormed") %>%
      filter(period == "predicted")  %>%
      pull(Cases)
    
    predicted_dewormed <- round(predicted_dewormed, 0)
    
    percent_change <- ((predicted_dewormed - previous_dewormed) / previous_dewormed) * 100
    percent_change <- round(percent_change,0)
    
    value <- abs(percent_change)
    color <- get_color(percent_change)
    infoBox(
      "Deworming cases",
      value = HTML(paste("<div style='font-size: 12px;'>Percentage Change:", percent_change, "%</div><div style='font-size: 12px;'>Previous Dewormed:", previous_dewormed, "</div><div style='font-size: 12px;'>Predicted Deworm:", predicted_dewormed, "</div>")), 
      icon = icon("pills"), 
      color = color
    )
    
  })
  
  # Render info boxes with dynamically determined colors and text
  output$info_box_2 <- renderInfoBox({
    selected_town <- input$town_choice
    
    previous_malnutrition <- merged_data %>%
      filter(county == selected_town, Deficiency == "acute_malnutrition") %>%
      filter(period == "2023-05-28") %>%
      pull(Cases)
    
    predicted_malnutrition <- merged_data %>%
      filter(county == selected_town, Deficiency == "acute_malnutrition") %>%
      filter(period == "predicted")  %>%
      pull(Cases)
    
    predicted_malnutrition <- round(predicted_malnutrition, 0)
    
    percent_change <- ((predicted_malnutrition - previous_malnutrition) / previous_malnutrition) * 100
    percent_change <- round(percent_change,0)
    
    value <- abs(percent_change)
    color <- get_color(percent_change)
    infoBox(
      "Acute Malnutrition",
      value = HTML(paste("<div style='font-size: 12px;'>Percentage Change:", percent_change, "%</div><div style='font-size: 12px;'>Previous Dewormed:", previous_malnutrition, "</div><div style='font-size: 12px;'>Predicted Deworm:", predicted_malnutrition, "</div>")), 
      icon = icon("thermometer"), 
      color = color
    )
  })
  
  # Render info boxes with dynamically determined colors and text
  output$info_box_3 <- renderInfoBox({
    selected_town <- input$town_choice
    
    previous_underweight <- merged_data %>%
      filter(county == selected_town, Deficiency == "underweight_child") %>%
      filter(period == "2023-05-28") %>%
      pull(Cases)
    
    predicted_underweight <- merged_data %>%
      filter(county == selected_town, Deficiency == "underweight_child") %>%
      filter(period == "predicted")  %>%
      pull(Cases)
    
    predicted_underweight <- round(predicted_underweight, 0)
    
    percent_change <- ((predicted_underweight - previous_underweight) / previous_underweight) * 100
    percent_change <- round(percent_change,0)
    
    value <- abs(percent_change)
    color <- get_color(percent_change)
    infoBox(
      "Underweight Child",
      value = HTML(paste("<div style='font-size: 12px;'>Percentage Change:", percent_change, "%</div><div style='font-size: 12px;'>Previous Dewormed:", previous_underweight, "</div><div style='font-size: 12px;'>Predicted Deworm:", predicted_underweight, "</div>")), 
      icon = icon("ambulance"), 
      color = color
    )
  })
  
  output$location_plot <- renderPlotly({
    selected_town <- input$town_choice
    
    mean_cases <- health_longer %>%
      filter(Deficiency == "diarrhoea_cases") %>%
      group_by(period) %>%
      summarize(mean_cases = mean(Cases))
    
    p1 <- health_longer %>%
      ggplot(aes(x = period, y = Cases, color = county)) +
      geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") +
      geom_line(
        data = filter(health_longer, Deficiency == "diarrhoea_cases", county == selected_town),
        color = "red"
      ) +
      labs(x = "Period", y = "Cases", title = "Diarrhoea Cases") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue", "red"))
    
    ##########
    mean_cases_stunted <- health_longer %>%
      filter(Deficiency == "stunted_child") %>%
      group_by(period) %>%
      summarize(mean_cases = mean(Cases))
    
    p2 <- health_longer %>%
      ggplot(aes(x = period, y = Cases, color = county)) +
      geom_line(data = mean_cases_stunted, aes(y = mean_cases), color = "blue") + 
      geom_line(data = filter(health_longer, Deficiency == "stunted_child", county == selected_town), color = "red") + 
      labs(x = "Period", y = "Cases", title = "stunted") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue","red"))
    #############
    mean_cases <- health_longer %>%
      filter(Deficiency == "acute_malnutrition") %>%
      group_by(period) %>%
      summarize(mean_cases = mean(Cases))
    
    p3 <- health_longer %>%
      ggplot(aes(x = period, y = Cases, color = county)) +
      geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
      geom_line(data = filter(health_longer, Deficiency == "acute_malnutrition", county == selected_town), color = "red") + 
      labs(x = "Period", y = "Cases", title = "Malnutrition") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue","red")) 
    ###############dewormed###########
    mean_cases <- health_longer %>%
      filter(Deficiency == "total_dewormed") %>%
      group_by(period) %>%
      summarize(mean_cases = mean(Cases))
    
    # Plotting
    p4 <- health_longer %>%
      ggplot(aes(x = period, y = Cases, color = county)) +
      geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
      geom_line(data = filter(health_longer, Deficiency == "total_dewormed", county == selected_town), color = "red") + 
      labs(x = "Period", y = "Cases", title = "Deworming") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue","red"))
    #######
    # Calculate mean cases for all counties
    mean_cases <- health_longer %>%
      filter(Deficiency == "underweight_child") %>%
      group_by(period) %>%
      summarize(mean_cases = mean(Cases))
    
    # Plotting
    p5 <- health_longer %>%
      ggplot(aes(x = period, y = Cases, color = county)) +
      geom_line(data = mean_cases, aes(y = mean_cases), color = "blue") + 
      geom_line(data = filter(health_longer, Deficiency == "underweight_child", county == selected_town), color = "red") + 
      labs(x = "Period", y = "Cases", title = "Underweight") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_color_manual(values = c("blue","red")) 
    
    subplot(p5, p4, p3, p2, p1, nrows = 2) %>%
      layout(title = "Plots for:Underweight, Deworming, Malnutrition, stunted & Diarhea")
  })
  
}

shinyApp(ui = ui, server = server)