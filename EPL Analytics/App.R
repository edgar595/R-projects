
library(shiny)
library(shinydashboard)
library(elo)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)




create_elo_data <- function(k) {
  elo <- elo.run(winner ~ team + opponent,
                 k = k, data = epl) %>%
    as_tibble() %>%
    cbind(epl %>% select(match_id)) %>%
    select(team.A, team.B, elo.A, elo.B, match_id)
  
  rbind(elo %>% 
          select_at(vars(contains(".A"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".A", "")),
        elo %>% 
          select_at(vars(contains(".B"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".B", ""))) %>%
    left_join(epl %>% 
                select(team, opponent, DateTime, match_id, Season),
              by = c("team", "match_id"))
}



library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "EPL Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Data", tabName = "data", icon =icon("database")),
                menuItem("Visualization", tabName = "viz", icon = icon("dashboard")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'graph'", 
                                 selectInput(inputId = "var1" , label ="Select the Season" , choices = c(c("1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")
))),
conditionalPanel("input.sidebar == 'viz' && input.t2 == 'standings'", 
                 selectInput(inputId = "var2" , label ="Select the Season" , choices = c(c("1993-94", "1994-95", "1995-96", "1996-97", "1997-98", "1998-99", "1999-00", "2000-01", "2001-02", "2002-03", "2003-04", "2004-05", "2005-06", "2006-07", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20", "2020-21", "2021-22")
                 ))),
                menuItem("Head to Head", tabName = "prediction", icon = icon("star"))
                )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "data",
              tabBox( id = "t1", width = 20,
                      tabPanel("About", icon = icon("play"), fluidRow(
                        column(width = 8,  tags$video(src = "PL.mp4", type = "video/mp4", width = "640", height = "360", controls = TRUE),
                               tags$br(),
                               tags$a("Video by PL")),
                        column(width = 4, tags$br(),
                               tags$p("This dataset contains a wealth of information about the English Premier League from the 1993-94 season to the 2021-22 season. By analyzing this data, we can identify the past winners of the league, and even use Elo ratings and head-to-head rankings to predict the outcomes of upcoming matches. Additionally, we can create visualizations that allow us to see trends and patterns over time.

The dataset includes a comprehensive data table that provides information on every game played throughout each season, including the home team, away team, date and time of the match, and the final score. We also have details on the referees officiating the games.

 The dataset's summary provides key information on games played, goals scored, and win-loss-draw distribution. The dataset's highlight is its ability to create graphs showing the league winners by season, providing insights into league progression and trends. "))
                      )
                              
                               
                               
                               
                      )
                      ,
                      tabPanel("Data", icon = icon("key"), dataTableOutput("data_df")),
                      tabPanel("summary",  verbatimTextOutput("summary_df") )
              )),
      tabItem(tabName = "viz",
              tabBox( id = "t2", width = 20,
                      tabPanel("graph", icon = icon("line"), plotlyOutput("topteams")),
                      tabPanel("Distribution", icon = icon("key")),
                      tabPanel("standings", icon = icon("table"), tableOutput("epl_table"))
                      
              )),
      tabItem( tabName = "prediction",
               tabBox(id = "t3", width = 20,
                      tabPanel("Home vs Away", 
                              fluidRow(box(uiOutput("homeTeamSelect")),
                                       box(uiOutput("awayTeamSelect"))),
                              fluidRow(box(valueBoxOutput("homeTeamCard")),
                                       box(valueBoxOutput("awayTeamCard")))),
                      tabPanel("head to head", fluidRow(box(uiOutput("teamSelect")),
                                                                             box(uiOutput("opponentSelect"))) ,
                               verbatimTextOutput("predictt"),tableOutput("fixture")
                      ),
                      box(sliderInput(inputId = "v_k_1",
                                      label = "K for ELO",
                                      min = 1,
                                      max = 100,
                                      value = 20)))
      )
    )
  )
)

server <- function(input, output , session) {
  
  output$data_df <- renderDataTable(
    result <- results %>%
      select(Season, HomeTeam, AwayTeam, DateTime, FTHG, FTAG, FTR, HTHG, HTAG, Referee)
    
  )
    
    output$epl_table <- renderTable({
      
      temp_df %>%
        filter(Season == input$var2) %>%
        group_by(team, match_id) %>%
        slice(1) %>%
        group_by(team) %>%
        summarise(total_points = sum(case_when(outcome == "win" ~ 3, outcome == "Draw" ~ 1, outcome == "loss" ~ 0)),
                  total_wins = sum(outcome == "win"),
                  total_losses = sum(outcome == "loss"),
                  total_draws = sum(outcome == "Draw")) %>%
        arrange(desc(total_points))
    })
    
    output$teamSelect <- renderUI({
      teams <- unique(headtohead$team)
      selectInput("var3", "Select Team", choices = teams)
    })
    
    output$opponentSelect <- renderUI({
      opponents <- unique(headtohead$opponent)
      selectInput("var4",
                  "Select Opponent", 
                  choices = opponents)
    })
    
    
    output$fixture <- renderTable({
      
      headtohead_df <- headtohead %>%
        select(Season, team, opponent, teamgoal, awaygoal) %>%
        filter((team == input$var3 & opponent == input$var4) | (team == input$var4 & opponent == input$var3)) %>%
        mutate(winner = ifelse(teamgoal > awaygoal, input$var3, ifelse(teamgoal < awaygoal, input$var4, "draw")))
    })
    

  output$summary_df <- renderPrint(
    results %>%
      select(Season, HomeTeam, AwayTeam, DateTime, FTHG, FTAG, FTR) %>%
      mutate(HomeGoals = FTHG,
             AwayGoals = FTAG,
             MatchResult = case_when(
               FTR == "H" ~ "H",
               FTR == "A" ~ "A",
               FTR == "D" ~ "D"
             )) %>%
      select(Season, HomeTeam, AwayTeam, DateTime, HomeGoals, AwayGoals, MatchResult) %>%
      mutate(MatchResult = factor(MatchResult, levels = c("H", "A", "D"))) %>%
      mutate(
        HomeTeam = as.factor(HomeTeam),
        AwayTeam = as.factor(AwayTeam)
      ) %>%
      summary()
    
  )
  
  output$topteams <- renderPlotly ({
    # Get top 10 teams for selected season
    top10 <- temp_df %>%
      filter(Season == input$var1) %>%
      group_by(team, match_id) %>%
      slice(1) %>%
      summarise(wins = sum(outcome == "win")) %>%
      group_by(team) %>%
      summarise(total_wins = sum(wins)) %>%
      arrange(desc(total_wins)) %>%
      distinct(team, .keep_all = TRUE) %>%
      top_n(10, total_wins)
    
    # Filter data for top 10 teams and selected season
    df_plot <- temp_df %>%
      filter(Season == input$var1, team %in% top10$team) %>%
      group_by(team, Season) %>%
      summarise(total_wins = sum(outcome == "win")) 
    
    # Plot the graph
    ggplot(df_plot, aes(x = reorder(team, desc(-total_wins)), y = total_wins, fill = team)) +
      geom_col(alpha = 0.8, size = 1.2) +
      scale_fill_discrete(name = "Team") +
      labs(x= "", y = "Total Wins") +
      coord_flip() +
      ggtitle("Total Wins by Team for Season ", input$var1)
    
  })
  
  output$homeTeamSelect<- renderUI({
    
    homeTeam_df <- create_elo_data(input$v_k_1) %>%
      select(team) %>%
      distinct() %>%
      arrange(team)
    
    selectInput(inputId = "homeTeam", 
                label = "Team",
                choices = homeTeam_df$team)
    
  })
  
  output$awayTeamSelect <- renderUI({
    
    awayTeam_df <- create_elo_data(input$v_k_1) %>%
      filter(team != input$homeTeam) %>% 
      select(team) %>%
      distinct() %>%
      arrange(team)
    
    selectInput( inputId = "awayTeam", 
                label = "Team",
                choices = awayTeam_df$team)
    
  })
  
  output$homeTeamCard <- renderValueBox({
    elo <- elo.run(winner ~ team + opponent,
                   k = input$v_k_1,
                   data = epl)
    
    away_prob <- round(100*predict(elo, data.frame(team = input$homeTeam, opponent = input$awayTeam)),0)
    
    valueBox(
      value = paste(away_prob, "%", sep = ""),
      subtitle = paste(input$v_opponent, " Probability", sep = ""),
      color = "red",
      icon = icon("hand-rock")
    )
    
  })
  
  output$awayTeamCard <- renderValueBox({
    elo <- elo.run(winner ~ opponent + team,
                   k = input$v_k_1,
                   data = epl)
    
    away_prob <- round(100*predict(elo, data.frame(team = input$awayTeam, opponent = input$homeTeam)),0)
    
    valueBox(
      value = paste(away_prob, "%", sep = ""),
      subtitle = paste(input$v_opponent, " Probability", sep = ""),
      color = "blue",
      icon = icon("hand-rock")
    )
    
  })
  
  output$predictt <- renderText({
    team1 <- input$var3
    team2 <- input$var4
    team1_wins <- 0
    team2_wins <- 0
    draws <- 0
    
    # Calculate the head-to-head results
    headtohead_df <- headtohead %>%
      filter((team == team1 & opponent == team2) | (team == team2 & opponent == team1)) %>%
      mutate(winner = ifelse(!is.na(teamgoal) & !is.na(awaygoal) & teamgoal > awaygoal, ifelse(team == team1, team1, team2), 
                             ifelse(!is.na(teamgoal) & !is.na(awaygoal) & teamgoal < awaygoal, ifelse(team == team1, team2, team1), "draw")))
    
    
    if (nrow(headtohead_df) == 0) {
      "No head-to-head data found"
    } else {
      winner_counts <- table(headtohead_df$winner)
      team1_wins <- winner_counts[input$var3]
      team2_wins <- winner_counts[input$var4]
      total_matches <- sum(winner_counts)
      
      if (team1_wins > team2_wins) {
        prediction <- paste0(input$var3, " is predicted to win against ", input$var4)
        if (round((team1_wins/total_matches)*100) == 50) {
          prediction <- "The outcome is a draw"
        } else {
          prediction <- paste0(prediction, " by ", round((team1_wins/total_matches)*100), "%")
        }
      } else if (team1_wins < team2_wins) {
        prediction <- paste0(input$var4, " is predicted to win against ", input$var3)
        if (round((team2_wins/total_matches)*100) == 50) {
          prediction <- "The outcome is a draw"
        } else {
          prediction <- paste0(prediction, " by ", round((team2_wins/total_matches)*100), "%")
        }
      } else {
        prediction <- "The prediction is a draw"
      }
      
      prediction
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

