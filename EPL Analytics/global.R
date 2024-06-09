library(tidyverse)

resultss <- read.csv("data/results.csv")
resultss
resultss <- results
  
  
result <- results %>%
    select(Season, HomeTeam, AwayTeam, DateTime, FTHG, FTAG, FTR, HTHG, HTAG, Referee) 
    

results <- results %>% 
  mutate(match_id = row_number())
  
#binding
  
df <- results %>%
    select(Season, HomeTeam, DateTime, FTHG, FTAG, FTR, HTHG, 
           HTAG, Referee, match_id) %>%
    rename(team = HomeTeam) %>%
    mutate(outcome = ifelse(FTHG > FTAG, "win",
                            ifelse(FTHG < FTAG, "loss", "Draw")))
  
rf <- results %>%
    select(Season, AwayTeam, DateTime, FTHG, FTAG, FTR, HTHG, 
           HTAG, Referee, match_id) %>%
    rename(team = AwayTeam) %>%
    mutate(outcome = ifelse(FTAG > FTHG, "win",
                            ifelse(FTAG < FTHG, "loss", "Draw")))
    
  
temp_df <- rbind(df, rf)
  
  #vs team

temp_df <- temp_df %>%
    arrange(match_id, DateTime) %>%
    group_by(match_id) %>%
    mutate(opponent = case_when(
      team == first(team) ~ lead(team),
      TRUE ~ first(team)
    )) 
  
  #home, wins and loss
temp_df <- temp_df %>%
    mutate(winner = case_when(
      outcome == "win" ~ 1,
      outcome == "Draw" ~ 0.5,
      outcome == "loss" ~ 0
    ))
  
  #top 10 and graph
  
top10 <- temp_df %>%
    group_by(team, match_id) %>%
    slice(1) %>%
    summarise(wins = sum(outcome == "win")) %>%
    group_by(team) %>%
    summarise(total_wins = sum(wins)) %>%
    arrange(desc(total_wins)) %>%
    distinct(team, .keep_all = TRUE) %>%
    top_n(10, total_wins)
  
top10 %>%
  mutate(total_wins = as.numeric(total_wins),
           team = fct_reorder(team, total_wins)) %>%
  ggplot(aes(x = total_wins, y = team, fill = team)) +
  geom_col(alpha = 0.8, size = 1.2)
  
     
     #Season wins, draws and losses
   
temp_df %>%
  filter(Season == "2021-22") %>%
  group_by(team, match_id) %>%
  slice(1) %>%
  summarise(wins = sum(outcome == "win"), 
            losses = sum(outcome == "loss"), 
            draws = sum(outcome == "Draw")) %>%
  group_by(team) %>%
  summarise(total_wins = sum(wins), 
            total_losses = sum(losses), 
            total_draws = sum(draws)) %>%
  arrange(desc(total_wins)) %>%
  distinct(team, .keep_all = TRUE) %>%
  top_n(10, total_wins)
   
#points season
temp_df %>%
  filter(Season == "2021-22") %>%
  group_by(team, match_id) %>%
  slice(1) %>%
  group_by(team) %>%
  summarise(total_points = sum(case_when(outcome == "win" ~ 3, outcome == "Draw" ~ 1, outcome == "loss" ~ 0)),
            total_wins = sum(outcome == "win"),
            total_losses = sum(outcome == "loss"),
            total_draws = sum(outcome == "Draw")) %>%
  arrange(desc(total_points))
   
   
   
   
   # Filter data for top 10 teams and selected season
df_plot <- temp_df %>%
  filter(Season == "2021-22", team %in% top10$team) %>%
  group_by(team, Season) %>%
  summarise(total_wins = sum(outcome == "win")) 
   
# Plot the graph
ggplot(df_plot, aes(x = reorder(team, desc(total_wins)), y = total_wins, fill = team)) +
  geom_col(alpha = 0.8, size = 1.2) +
  scale_fill_discrete(name = "Team") +
  labs(x = "Team", y = "Total Wins") +
  ggtitle("Total Wins by Team for Season ", input$var1)
   
   
#creating elo
library(elo)
   
epl <- temp_df %>%
   group_by(match_id) %>%
   slice(1) %>%
   ungroup() %>%
   mutate(DateTime = as.Date(DateTime))
   
elo <- elo.run(winner ~ team + opponent,
                  k = 30, data = epl) %>%
     as_tibble()
   
rank.teams(elo)
   
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

create_elo_data(20) 

   
elo::rank.teams(elo)
   
#############head to head
   
headtohead <- epl %>%
 select(Season, team, opponent, FTHG, FTAG, match_id, outcome) %>%
 rename(teamgoal = FTHG,
        awaygoal= FTAG) %>%
 mutate(winner = ifelse(teamgoal > awaygoal, team, ifelse(teamgoal < awaygoal, opponent, "draw"))) %>%
 mutate(point = ifelse(winner == team, 1, ifelse(winner == "draw", 0.5, 0)))
   
headtohead %>%
 select(Season, team, opponent, teamgoal, awaygoal) %>%
 filter((team == "Arsenal" & opponent == "Coventry") | (team == "Coventry" & opponent == "Arsenal")) %>%
 mutate(winner = ifelse(teamgoal > awaygoal, team, ifelse(teamgoal < awaygoal, opponent, "draw"))) %>%
 mutate(point = ifelse(winner == team, 1, ifelse(winner == "draw", 0.5, 0))) %>%
 select(Season, team, opponent, teamgoal, awaygoal, winner, point)
   
    
arsenalvscoventry <- headtohead %>%
  filter((team == "Arsenal" & opponent == "Coventry") | (team == "Coventry" & opponent == "Arsenal")) %>%
  mutate(point = ifelse(winner == team, 1, ifelse(winner == "draw", 0.5, 0))) %>%
  mutate(winner = ifelse(teamgoal > awaygoal, team, ifelse(teamgoal < awaygoal, opponent, "draw"))) 


ars_elo <- elo.run(point ~ team + opponent, 
       k = 30,
       data = headtohead) 
       rank.teams(ars_elo)
   
   
   
   
   
   
   
   
  
  
   
   
   
     
  
  
    
  
    
  
  
  
  
  
    
  
  
  
  


  
  