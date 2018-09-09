# Initialize --------------------------------------------------------------

### Load Packages ###
library(shiny)
library(shinythemes)
library(tidyverse)
library(fvoa)
library(plotly)

library(rlang)

### Initial Settings ###

reps <-  1e6
ranking_methods <- c("yahoo_rankings", "fvoa_rankings", "strength_schedule", "colley_rankings") %>%
  set_names(c("Yahoo", "FVOA", "Strength of Schedule", "Colley (BCS)"))
sorting <- c("Yahoo Rank", "FVOA Rank", "SoS", "Colley Rank", "Points") %>%
  set_names(ranking_methods, "Points")

# Load Data ---------------------------------------------------------------

load("clt-data.RData")

weeks <- n_distinct(clt_tidy$Week)
teams <- unique(clt_tidy$Team)

# UI ----------------------------------------------------------------------

ui  <- navbarPage(
  
  # Setup -------------------------------------------------------------------
  
  # themeSelector(),
  theme = shinytheme("darkly"),
  
  collapsible = T,
  inverse = F,
  
  # footer = "Frech Takes",
  title = "Charlotte's Finest",
  
  # Frech Takes Tab----------------------------------------------------------
  
  tabPanel("Frech Takes",
           h3("The Frechest of Takes"),
           h5(""),
           hr(),
           p("FVOA went 2-3 against the Yahoo spread last week, 39-30-1 for the season"),
           
           p("Frech Stats:"),
           tags$li("FVOA went 56% against the weekly Yahoo spread and 61% in all 630 possible head-to-head matchups"),
           tags$li("4 of the 5 top teams in FVOA made the playoffs"),
           tags$li("Poor Bobby is stuck in 7th overall despite 4th in FVOA & 3rd most points"),
           tags$li("The last few weeks got interesting as the two teams projected 
                   to make the 3rd and 4th spots fell out of the playoffs entirely"),
           tags$li("And congrats to PFinn for a great run at the end to beat expectations. 
                   You may be #10 in the charts but you're #6 in our hearts"),
           
           
           
           hr(),
           h5("Playoff Projections", align = "center"),
           br(),
           fluidRow(tableOutput("weekly"), align="center"),
           
           # h5(paste("Week", max(weeks) + 1, "Projections"), align = "center"),
           # br(),
           # fluidRow(tableOutput("weekly"), align="center"),
           # br(),
           # h5("Season Projections", align = "center"),
           # br(),
           # fluidRow(tableOutput("simulation"), align = "center"),
           hr(),
           p("FVOA Assumptions:"),
           tags$ol(
             tags$li(HTML("<u><strong>Team scores only</strong></u> - opponent is random and you can't play defense 
                          so win-loss record is irrelevant")),
             tags$li(HTML("<u><strong>Recency bias</strong></u> - recent games are more predictive because of things 
                          like injuries, waiver wire pickups, and trades")),
             tags$li(HTML("<u><strong>Outliers</strong></u> - freak games by your WR3 are not very predictive so very 
                          high/low scores relative to your weekly average will be penalized slightly")),
             tags$li(HTML("Bye weeks cancel out so they're not directly factored in"))
             )
             ),
  
  # Rankings Tab-------------------------------------------------------------
  
  tabPanel("Rankings",
           sidebarLayout(
             sidebarPanel(
               checkboxGroupInput("rankings", "Choose Rankings:", 
                                  # ranking_methods, selected = c("yahoo_rankings", "frech_rankings")),
                                  ranking_methods, selected = c("yahoo_rankings")),
               uiOutput("sorting")
             ), 
             # mainPanel("Rankings")
             mainPanel(tableOutput("rankings"))
           ),
           hr(),
           h5("Ranking Notes:"),
           tags$ol(
             tags$li(HTML("<u><strong>Yahoo</strong></u>: our official ranking based on win percentage and total points")),
             tags$li(HTML("<u><strong>FVOA</strong></u>: Frech-adjusted Value Over Average (similar to DVOA). 
                          This is your percent above/below the average team. 
                          This can also be used to get a rough percent of Team 1 beating Team 2 by
                          calculating (Team 1 FVOA - Team 2 FVOA)/2 + 50")),
             tags$li(HTML("<u><strong>SoS</strong></u>: strength of schedule based on FVOA scores")),
             tags$li(HTML("<u><strong>Colley</strong></u>: one of the models that was used in the BCS computer rankings
                          and is based on win-loss record adjusted for strength of schedule")),
             tags$li("So in other words, Colley ranks on record only, 
                     FVOA and SoS rank on points only,
                     and Yahoo ranks on record and points")
             )
             ),
  
  # League Tab---------------------------------------------------------------
  
  tabPanel("League",
           
           sidebarLayout(
             sidebarPanel(sliderInput("league_week", "Weeks to Include:", 1,
                                      max(weeks), c(1, max), step = 1)),
             mainPanel(
               p("How do all the teams compare to each other?"),
               fluidRow(column(8, plotOutput("heatmap")), align = "center"),
               br(),
               br(),
               p("Just because you aren't matched up doesn't mean you can't still gamble on your scores:"),
               tableOutput("lines")
             )
           )
           
  ),
  
  # Matchups Tab-------------------------------------------------------------
  
  tabPanel("Matchups",
           h3("Individual Matchups"),
           hr(),
           h5("Matchup Breakdown", align = "center"),
           br(),
           fluidRow(tableOutput("matchup_breakdown"), align = "center"),
           hr(),
           fluidRow(column(4, offset = 1,
                           selectInput("team1", "Team 1:", teams, selected = "Scott"), 
                           selectInput("team2", "Team 2:", teams, selected = "Barrett")),
                    column(4, offset = 1,
                           wellPanel(sliderInput("matchup_week", "Weeks to Include:", 1,
                                                 max(weeks), c(1, max), step = 1))))
  ),
  
  # Team Charts Menu---------------------------------------------------------
  
  navbarMenu("Team Charts",
             tabPanel("Weekly Scores",
                      plotlyOutput("weekly_plot"),
                      hr(),
                      # fluidRow(column(4, offset = 4, wellPanel(sliderInput("weekly_week", "Weeks to Include:", 1,
                      #                                                      max(weeks), c(1, max), step = 1)))),
                      fluidRow(checkboxGroupInput("team_week", "Teams to Highlight:", sort(teams), inline=T), align = "center"),
                      fluidRow(actionButton("clear_teams_week", "Clear Teams"), align = "center"),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li("Dotted grey line gives the average score for each week")
                      )
             ),
             
             tabPanel("Weekly FVOA",
                      plotlyOutput("fvoa_plot"),
                      hr(),
                      fluidRow(checkboxGroupInput("team_fvoa", "Teams to Highlight:", sort(teams), inline=T), align = "center"),
                      fluidRow(actionButton("clear_teams_fvoa", "Clear Teams"), align = "center"),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li("Dotted grey line gives the average team")
                      )
             ),
             
             tabPanel("Simulated Wins",
                      plotlyOutput("sim_wins"),
                      hr(),
                      fluidRow(checkboxGroupInput("team_wins", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_wins", "Clear Teams"), align = "center"),
                      br(),
                      p("Calculated by simulating all remaining matchups in the season and 
                        figuring out the total number of wins for each team. I then do this another 999 times and 
                        take the average. This will happen every week to see which teams are getting stronger 
                        against their competition."),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li("Dotted grey line gives the league average (7 wins)")
                      )
                      ),
             
             tabPanel("Simulated Total Points",
                      plotlyOutput("sim_points"),
                      hr(),
                      fluidRow(checkboxGroupInput("team_points", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_points", "Clear Teams"), align = "center"),
                      br(),
                      p("Calculated by simulating all remaining matchups in the season and figuring out the total
                        amount of points for each team. I then do this another 999 times and take the average.
                        This will happen every week to see which teams are getting stronger."),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li("Dotted grey line gives the average total points for each week")
                      )
                      ),
             
             tabPanel("Simulated Playoff Chances",
                      plotlyOutput("sim_playoffs"),
                      hr(),
                      fluidRow(checkboxGroupInput("team_playoffs", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_playoffs", "Clear Teams"), align = "center"),
                      br(),
                      p("Calculated by simulating all remaining matchups in the season and figuring out the best 4 teams.
                        I then do this another 999 times and figure out the percentage of each team making the playoffs.
                        This will happen every week to see which teams have the best chance to make that money."),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li("Dotted grey line gives the baseline (40%)")
                      )
                      ),
             
             # tabPanel("Playoff Leverage",
             #          h5("How much will winning/losing your next game affect your playoff chances?"),
             #          plotOutput("playoff_leverage"),
             #          h5("Chart Notes:"),
             #          tags$ol(
             #            tags$li("Full bar is your chance of making playoffs with win"),
             #            tags$li("Darker portion is your chance of making playoffs with loss"),
             #            tags$li("The difference between these two, the light portion and number listed to the right,
             #                    is the playoff leverage of this game"),
             #            tags$li("Note: these are all treated independently of the other teams winning/losing so it's not exact")
             #            )
             # ),
             
             tabPanel("Projected v Actual Scores",
                      h5("How did your team perform against Yahoo projections?"),
                      plotOutput("projected"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("proj_week", "Weeks to Include:", 1,
                                                                           max(weeks), c(1, max), step = 1))))
             ),
             
             tabPanel("Boxplots",
                      plotOutput("boxplot"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("boxplot_week", "Weeks to Include:", 1,
                                                                           max(weeks), c(1, max), step = 1))))
             ),
             
             tabPanel("Density Plots",
                      plotlyOutput("density"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("density_week", "Weeks to Include:", 1,
                                                                           max(weeks), c(1, max), step = 1)))),
                      fluidRow(checkboxGroupInput("team_density", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_denisty", "Clear Teams"), align = "center"),
                      hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Use slide to choose specific weeks to include"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)")
                      )
             )
             ),
  
  # Model Evaluation Tab-----------------------------------------------------
  
  tabPanel("FVOA Evaluation",
           h4("FVOA Accuracy by Week"),
           hr(),
           fluidRow(textOutput("eval_accuracy"), align = "center"),
           br(),
           fluidRow(column(8, offset = 2, plotOutput("eval_plot")), align = "center"),
           br(),
           p("Which teams screwed my model last week?", align = "center"),
           fluidRow(tableOutput("eval_team"), align = "center")
  )
  
  # End of navbarPage
             )

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  ### Weekly Projections ###

  output$weekly <- renderTable({
    current_matchups
  }, align = 'c', digits = 1)
  
  output$simulation <- renderTable({
    simulated_seasons %>% 
      filter(Week == max(Week)) %>% 
      mutate(Playoffs = paste0(Percent, "%")) %>% 
      select(Team:Wins, Playoffs)
  }, align = 'c', digits = 1)
  
  ### Rankings Tab ###
  
  output$sorting <- renderUI({
    selectInput("sorting", "Sort Rankings By:", 
                c(ranking_methods[ranking_methods %in% input$rankings], "Points"))
  })
  
  output$rankings <- renderTable({
    rankings <- clt_stats
    for(r in ranking_methods) {
      if (r %in% input$rankings) {
        tmp <- eval_tidy(parse_quosure(r))
        rankings <- rankings %>% left_join(tmp, by = "Team")
      } else next 
    }
    sort <- sorting[[input$sorting]]
    rank_sort <- rankings %>%
      arrange(rankings[[sort]]) %>%
      mutate(Percent = format_pct(Percent))
    point_sort <- rankings %>%
      arrange(desc(rankings[[sort]])) %>%
      mutate(Percent = format_pct(Percent))
    if (sort == "Points") {
      point_sort
    } else {
      rank_sort
    }
  }, align = 'c', digits = 3)
  
  ### League Tab ###
  matchups_prob <- reactive({
    set.seed(42)
    teams <- clt_tidy %>% select(Team) %>% distinct()
    reg_games <- 6
    team.names <- teams[[1]]
    df <- as.data.frame(matrix(nrow=nrow(teams), ncol=nrow(teams)))
    rownames(df) <- team.names
    colnames(df) <- team.names
    if (input$league_week[2] - input$league_week[1] < 5) {
      league <- clt_tidy %>%
        filter(Week %in% input$league_week[1]:input$league_week[2]) %>%
        group_by(Team) %>%
        mutate(team_mean = weighted.mean(c(Score, 115), weight_games(Score, reg_games)),
               team_sd = sqrt(wtd.var(c(Score, 115), weight_games(Score, reg_games), method="ML"))) %>%
        select(Team, team_mean, team_sd) %>%
        ungroup() %>%
        distinct()
    } else {
      league <- clt_tidy %>%
        filter(Week %in% input$league_week[1]:input$league_week[2]) %>%
        group_by(Team) %>%
        mutate(team_mean = weighted.mean(Score, weight_games(Score, reg_games)),
               team_sd = sqrt(wtd.var(Score, weight_games(Score, reg_games), method="ML"))) %>%
        select(Team, team_mean, team_sd) %>%
        ungroup() %>%
        distinct()
    }
    sims <- list()
    sim_scores <- for(i in 1:nrow(teams)) {sims[[i]] <- rnorm(reps, league[[i, 2]], league[[i,3]])}
    names(sims) <- teams[[1]]
    matchup_season <- function(i, j) {
      t1 <- rownames(df)[i] # index ID
      t2 <- colnames(df)[j]
      sim <- sims[[t1]] - sims[[t2]]
      t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE)* 100, 2)
    }
    for(i in 1:dim(df)[1]) {
      for(j in 1:dim(df)[2]) {
        if (i == j) {NA} 
        else if (is.na(df[i,j])==F) {next} 
        else {
          df[i,j] = matchup_season(i, j)
          df[j,i] = 100 - df[i,j]
        }
      }
    }
    df
  })
  
  matchups_spread <- reactive({
    set.seed(42)
    teams <- clt_tidy %>% select(Team) %>% distinct()
    reg_games <- 6
    team.names <- teams[[1]]
    df <- as.data.frame(matrix(nrow=nrow(teams), ncol=nrow(teams)))
    rownames(df) <- team.names
    colnames(df) <- team.names
    if (input$league_week[2] - input$league_week[1] < 5) {
      league <- clt_tidy %>% 
        filter(Week %in% input$league_week[1]:input$league_week[2]) %>%
        group_by(Team) %>% 
        mutate(team_mean = weighted.mean(c(Score, 115), weight_games(Score, reg_games)),
               team_sd = sqrt(wtd.var(c(Score, 115), weight_games(Score, reg_games), method="ML"))) %>%
        select(Team, team_mean, team_sd) %>% 
        ungroup() %>% 
        distinct()
    } else {
      league <- clt_tidy %>% 
        filter(Week %in% input$league_week[1]:input$league_week[2]) %>%
        group_by(Team) %>% 
        mutate(team_mean = weighted.mean(Score, weight_games(Score, reg_games)),
               team_sd = sqrt(wtd.var(Score, weight_games(Score, reg_games), method="ML"))) %>%
        select(Team, team_mean, team_sd) %>% 
        ungroup() %>% 
        distinct()
    }
    sims <- list()
    sim_scores <- for(i in 1:nrow(teams)) {sims[[i]] <- rnorm(reps, league[[i, 2]], league[[i,3]])}
    names(sims) <- teams[[1]]
    matchup_season <- function(i, j) {
      t1 <- rownames(df)[i] # index ID
      t2 <- colnames(df)[j]
      sim <- sims[[t1]] - sims[[t2]]
      spread <- -mean(sim)
      spread <- round(spread * 2) / 2
      if_else(spread > 0, paste0("+", spread), 
              if_else(spread < 0, paste(spread), paste(0)))
    }
    for(i in 1:dim(df)[1]) {
      for(j in 1:dim(df)[2]) {
        if (i == j) {df[i, j] <- "0"} 
        else if (is.na(df[i,j])==F) {next}
        else {
          df[i,j] = matchup_season(i, j)
        }
      }
    }
    df
  })
  
  output$heatmap <- renderPlot({
    hm <- matchups_prob() %>%
      mutate(winner = rownames(.)) %>% 
      gather(loser, score, -winner) %>% 
      mutate(loser = factor(loser, levels = sort(unique(loser))))
    hm %>% 
      ggplot(aes(loser, winner)) + 
      geom_tile(aes(fill=score)) +
      scale_fill_distiller(palette = "Spectral", direction=1) +
      theme(panel.background=element_rect(fill="white", colour="white")) +
      ylim(rev(levels(hm$loser))) +
      labs(x = "Team 2", y="Team 1", fill="% Chance", title="Chance Team 1 Beats Team 2")
  })
  
  output$lines <- renderTable({
    matchups_spread()%>% 
      add_column(Team = teams, .before = 'Scott')
  }, align = 'c')
  
  ### Matchups Tab ###
  output$matchup_breakdown <- renderTable({
    reg_games <- 6
    league <- clt_tidy %>% filter(Week %in% input$matchup_week[1]:input$matchup_week[2])
    t1.scores <- league %>% filter(Team == input$team1) %>% pull(Score)
    t2.scores <- league %>% filter(Team == input$team2) %>% pull(Score)
    if (input$matchup_week[2] - input$matchup_week[1] < 5) {
      t1.mean <- weighted.mean(c(t1.scores, 115), weight_games(t1.scores, reg_games))
      t1.sd <- sqrt(wtd.var(c(t1.scores, 115), weight_games(t1.scores, reg_games), method="ML"))
      t2.mean <- weighted.mean(c(t2.scores, 115), weight_games(t2.scores, reg_games))
      t2.sd <- sqrt(wtd.var(c(t2.scores, 115), weight_games(t2.scores, reg_games), method="ML"))
      
    } else {
      t1.mean <- weighted.mean(t1.scores, weight_games(t1.scores, reg_games))
      t1.sd <- sqrt(wtd.var(t1.scores, weight_games(t1.scores, reg_games), method="ML"))
      t2.mean <- weighted.mean(t2.scores, weight_games(t2.scores, reg_games))
      t2.sd <- sqrt(wtd.var(t2.scores, weight_games(t2.scores, reg_games), method="ML"))
    }
    
    t1.sim <- rnorm(reps, t1.mean, t1.sd)
    t2.sim <- rnorm(reps, t2.mean, t2.sd)
    sim <- t1.sim-t2.sim
    tie <- round(dnorm(0, mean(sim), sd(sim)), 4)
    t1wins <- round(pnorm(0, mean(sim), sd(sim), lower.tail=FALSE) - tie/2, 4)
    t1squeak <- t1wins - round(pnorm(5, mean(sim), sd(sim), lower.tail=FALSE), 4)
    t1blowout <- round(pnorm(20, mean(sim), sd(sim), lower.tail=FALSE), 4)
    t1normal <- t1wins - t1squeak - t1blowout
    t2wins <- round(pnorm(0, mean(sim), sd(sim)) - tie/2, 2)
    t2squeak <- t2wins - round(pnorm(-5, mean(sim), sd(sim)), 4)
    t2blowout <- round(pnorm(-20, mean(sim), sd(sim)), 4)
    t2normal <- t2wins - t2squeak - t2blowout
    data.frame(Winner = c(input$team1, input$team1, input$team1, "Tie", input$team2, input$team2, input$team2),
               Type = c("Blowout", "Normal", "Squeaker", "Tie", "Squeaker", "Normal", "Blowout"),
               Margin = c("20+ points", "5-20", "<5 points", "-", "<5 points", "5-20", "20+ points"),
               Probability = format_pct(c(t1blowout, t1normal, t1squeak, tie, t2squeak, t2normal, t2blowout)))
  }, align = 'c')
  
  ### Team Charts ###
  
  observeEvent(input$clear_teams_week, {
    updateCheckboxGroupInput(session, "team_week", selected = character(0))
  })
  
  output$weekly_plot <- renderPlotly({
    
    if (is.null(input$team_week)) {
      x <- ggplot(clt_tidy, aes(Week, Score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
        geom_point(aes(group=Team, color=Team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        # scale_x_continuous(breaks = pretty_breaks(n = 7)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- clt_tidy %>% filter(Team %in% input$team_week)
      
      x <- ggplot(clt_tidy, aes(Week, Score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=Team, color=Team), size = 1.5) +
        geom_line(data = tm, aes(group=Team, color=Team), size = 2) + 
        geom_point(aes(group=Team, color=Team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    }
  })
  
  observeEvent(input$clear_teams_fvoa, {
    updateCheckboxGroupInput(session, "team_fvoa", selected = character(0))
  })
  
  output$fvoa_plot <- renderPlotly({
    
    if (is.null(input$team_fvoa)) {
      x <- ggplot(fvoa_season, aes(Week, FVOA)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- fvoa_season %>% filter(Team %in% input$team_fvoa)
      
      x <- ggplot(fvoa_season, aes(Week, FVOA)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=Team, color=Team), size = 1.5) +
        geom_line(data = tm, aes(group=Team, color=Team), size = 2) + 
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    }
  })
  
  observeEvent(input$clear_teams_wins, {
    updateCheckboxGroupInput(session, "team_wins", selected = character(0))
  })
  
  output$sim_wins <- renderPlotly({
    
    if (is.null(input$team_wins)) {
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Wins)) +
        geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- simulated_seasons %>% filter(Team %in% input$team_wins)
      
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Wins)) +
        geom_line(alpha = 0.2, aes(group=Team, color=Team), size = 1.5) +
        geom_line(data = tm, aes(group=Team, color=Team), size = 2) + 
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
    }
  })
  
  observeEvent(input$clear_teams_points, {
    updateCheckboxGroupInput(session, "team_points", selected = character(0))
  })
  
  output$sim_points <- renderPlotly({
    
    if (is.null(input$team_points)) {
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Points)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype = 2) +
        geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
        geom_point(aes(group=Team, color=Team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- simulated_seasons %>% filter(Team %in% input$team_points)
      
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Points)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_tidy$Week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=Team, color=Team), size = 1.5) +
        geom_line(data = tm, aes(group=Team, color=Team), size = 2) + 
        geom_point(aes(group=Team, color=Team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
    }
  })
  
  observeEvent(input$clear_teams_playoffs, {
    updateCheckboxGroupInput(session, "team_playoffs", selected = character(0))
  })
  
  output$sim_playoffs <- renderPlotly({
    
    if (is.null(input$team_playoffs)) {
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Percent)) +
        geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- simulated_seasons %>% filter(Team %in% input$team_playoffs)
      
      x <- simulated_seasons %>% 
        ggplot(aes(Week, Percent)) +
        geom_line(alpha = 0.2, aes(group=Team, color=Team), size = 1.5) +
        geom_line(data = tm, aes(group=Team, color=Team), size = 2) + 
        geom_point(aes(group=Team, color=Team)) +
        geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
        guides(color=FALSE) +
        theme_fvoa()
      
      ggplotly(x, tooltip = c("group", "x", "y"))
    }
  })
  
  # output$playoff_leverage <- renderPlot({
  #   playoff_leverage_chart
  # })
  
  output$projected <- renderPlot({
    clt_proj %>%
      filter(Week %in% input$proj_week[1]:input$proj_week[2]) %>%
      spread(Type, Score) %>% 
      group_by(Team) %>% 
      mutate(margin = act-proj,
             sign = if_else(margin >=0, "positive", "negative"),
             avg = mean(margin),
             pos_count = sum(if_else(sign == "positive", 1, 0))) %>%
      ggplot(aes(x= Week, y = margin, fill=sign)) +
      geom_bar(stat="identity") + 
      scale_x_continuous(breaks = pretty_breaks(n = 7)) +
      facet_wrap(~reorder(Team, - pos_count), ncol=n_distinct(clt_proj$Team)/2) +
      guides(fill=FALSE) +
      labs(y = "Margin") +
      scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
      theme_fvoa() + 
      theme(panel.grid.major.y = element_blank())
  })
  
  output$boxplot <- renderPlot({
    clt_tidy %>% 
      filter(Week %in% input$boxplot_week[1]:input$boxplot_week[2]) %>%
      ggplot(aes(x=reorder(Team, -Score, fun=mean), y=Score, fill=Team)) + 
      geom_boxplot(coef = 1.25, outlier.alpha = 0.6) + 
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend=FALSE) + 
      guides(fill=F) +
      labs(y = "Score", x = "", title = "Team Boxplots") +
      theme_fvoa() + 
      theme(panel.border = element_blank())
  })
  
  observeEvent(input$clear_teams_density, {
    updateCheckboxGroupInput(session, "team_density", selected = character(0))
  })
  
  output$density <- renderPlotly({
    
    if (is.null(input$team_density)) {
      x <- clt_tidy %>% 
        filter(Week %in% input$density_week[1]:input$density_week[2]) %>%
        ggplot(aes(Score)) +
        geom_density(aes(fill = Team, color = Team), alpha = 0.2) + 
        labs(x = "Weekly Scores", y = "Density", title = "Density Plots") +
        guides(fill=FALSE) + 
        theme_fvoa()
      
      ggplotly(x, tooltip = c("fill", "x"))
      
    } else {
      tm <- clt_tidy %>% filter(Team %in% input$team_density)
      
      x <- clt_tidy %>% 
        filter(Week %in% input$density_week[1]:input$density_week[2]) %>% 
        ggplot(aes(Score)) +
        geom_density(aes(fill = Team, color = Team), alpha = 0.1) + 
        geom_density(data = tm, aes(fill=Team, color = Team), alpha = 0.8) + 
        labs(x = "Weekly Scores", y = "Density", title = "Density Plots") +
        guides(fill=FALSE, color = FALSE) + 
        theme_fvoa()
      
      ggplotly(x, tooltip = c("fill", "x"))
    }
  })
  
  ### Model Evaluation ###
  
  output$eval_accuracy <- renderText({
    correct <- round(sum(map_dbl(model_eval %>% select(-week), sum, na.rm=T))/
                       (((ncol(model_eval)-1)^2 - (ncol(model_eval)-1)) * (max(model_eval$week)-1)), 4) * 100
    paste0("The model has correctly predicted ", correct, "% of games this season")
  })
  
  output$eval_plot <- renderPlot({
    benchmark <- length(unique(teams))^2 - length(teams)
    nested_model_eval <- model_eval %>% group_by(week) %>% nest()
    data_frame(weekly = map_dbl(nested_model_eval$data, sum, na.rm=T)) %>% 
      mutate(week = 2:(length(weekly)+1),
             delta = weekly - benchmark/2,
             percent = round(weekly/benchmark * 100, 1),
             sign = ifelse(delta > 0, "positive", ifelse(delta < 0, "negative", "equal"))) %>% 
      ggplot(aes(week, delta, fill=sign, label=percent)) + 
      geom_bar(stat = 'identity') + 
      geom_text(size = 3, alpha = 0.7) +
      scale_x_continuous(name = "Week", breaks = 2:weeks) +
      scale_y_continuous(limits = c(0-benchmark/2, benchmark/2), 
                         breaks = c(0-benchmark/2, ((0-benchmark/2)/2), 0, benchmark/4, benchmark/2),
                         labels = c(0, 25, 50, 75, 100)) +
      scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
      labs(title = "Weekly Evaluation of Model", x = "Week (starting with Week 2)", y = "Percent Correct") +
      theme(panel.background= element_blank(), panel.border = element_blank()) +
      guides(fill=F)
  })
  
  output$eval_team = renderTable({
    team_accuracy <- sapply(paste("Week", 1:weeks), function(x) NULL)
    for (i in 2:15) {team_accuracy[[i]] <- map_dbl(model_eval %>% filter(week == i) %>% select(-week), sum, na.rm=T) %>% sort()}
    current_team_accuracy <- team_accuracy[[weeks]]
    data_frame(Team = names(current_team_accuracy),
               "Games Wrong" = length(current_team_accuracy) - current_team_accuracy) %>%
      arrange(-`Games Wrong`, Team)
  }, digits = 0, align = 'c')
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)