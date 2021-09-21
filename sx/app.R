####### SX App #######


# Initialize --------------------------------------------------------------

### Load Packages ###
library(shiny)
library(shinythemes)
library(tidyverse)
library(fvoa)
library(plotly)
library(scales)
library(lubridate)
library(rlang)

### Initial Settings ###

ranking_methods <- c("espn_rank", "fvoa_rank", "sos_rank", "colley_rank") %>%
  set_names(c("ESPN", "FVOA", "Strength of Schedule", "Colley (BCS)"))
sorting <- c("ESPN Rank", "FVOA Rank", "SoS Rank", "Colley Rank", "PF", "PA") %>%
  set_names(ranking_methods, "PF", "PA")

today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1
frech_stats <- 1

fvoa_colors <- c("#0055AA", "#C40003", "#00C19B", "#EAC862", "#894FC6",
                 "#7FD2FF", "#b2df8a", "#FF9D1E", "#C3EF00", "#cab2d6")

# Load Data ---------------------------------------------------------------

load(here::here("sx", "sx-data.RData"))

weeks <- n_distinct(sx_scores$week)
teams <- unique(sx_scores$team)

# UI ----------------------------------------------------------------------

ui  <- navbarPage(
  
  # Setup -------------------------------------------------------------------
  
  # themeSelector(),
  theme = shinytheme("darkly"),
  
  collapsible = T,
  inverse = F,
  
  # footer = "Frech Takes",
  title = "Seacows Football",
  
  # Frech Takes Tab----------------------------------------------------------
  
  tabPanel("Frech Takes",
           h3("The Frechest of Takes"),
           h5(""),
           hr(),
           
           p(str_glue("Week {weeks_played}:")),
           tags$li(
             if(weeks_played == frech_stats) {
               "Well looks like the strongest 4 teams according to FVOA are in the playoffs"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "Hoop looks to be the Chiefs of our league not only as strong favorite to win it all but because he scored 240 points more than second-highest scoring team (and had 3rd most points scored against him)"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "That said if I was looking to place a wager I'd be looking at that +725 for Herndon pretty seriously"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "Also happy to point out FVOA went almost 60% on the season, including a very strong 7-week run until the last week"
             } else {
               "TBD"
             }
           ),
           
           hr(),
           # h5("Playoff Projections", align = "center"),
           # fluidRow(tableOutput("playoffs"), align = "center"),
           h5(paste("Week", max(weeks) + 1, "Projections"), align = "center"),
           br(),
           fluidRow(tableOutput("weekly"), align="center"),
           br(),
           h5("Season Projections", align = "center"),
           br(),
           fluidRow(tableOutput("simulation"), align = "center"),
           hr(),
           p("FVOA Assumptions:"),
           tags$ol(
             tags$li(HTML("<u><strong>Team scores only</strong></u> - opponent is random and you can't play defense 
                          so win-loss record is irrelevant")),
             tags$li(HTML("<u><strong>Regression to mean</strong></u> - scores are slightly regressed to 
                          our league's historical average for first six weeks on a 
                          sliding scale (50% for week 1, 33% for week 2, etc.)")),
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
               # checkboxGroupInput("rankings", "Choose Rankings:", 
               #                    ranking_methods, selected = c("espn_rankings")),
               uiOutput("sorting")
             ), 
             # mainPanel("Rankings")
             mainPanel(tableOutput("rankings"))
           ),
           hr(),
           h5("Ranking Notes:"),
           tags$ol(
             tags$li(HTML("<u><strong>ESPN</strong></u>: our official ranking based on win percentage and total points")),
             tags$li(HTML("<u><strong>FVOA</strong></u>: Frech-adjusted Value Over Average (similar to DVOA) - 
                          equivalent to spread against an average team")),
             tags$li(HTML("<u><strong>SoS</strong></u>: strength of schedule based on FVOA scores (higher means harder schedule)")),
             tags$li(HTML("<u><strong>Colley</strong></u>: one of the models that was used in the BCS computer rankings
                          and is based on win-loss record adjusted for strength of schedule")),
             tags$li("So in other words, Colley ranks on record only, 
                     FVOA and SoS rank on points only,
                     and espn ranks on record and points")
           )
  ),
  
  # Compare -----------------------------------------------------------------
  
  navbarMenu("Compare",
             tabPanel("Head-to-Head",
                      h3("Individual Matchups"),
                      hr(),
                      fluidRow(column(2, offset = 4,
                                      selectInput("team1", "Team 1:", teams, selected = teams[[1]])),
                               column(2, selectInput("team2", "Team 2:", teams, selected = teams[[2]]))),
                      hr(),
                      fluidRow(plotOutput("matchup_plot", width = "600px", height = "600px"), align = 'center'),
                      hr(),
                      h5("Matchup Breakdown", align = "center"),
                      fluidRow(tableOutput("matchup_breakdown"), align = "center")
             ),
             
             tabPanel("Playoff Leverage",
                      h5("How much will winning/losing your next game affect your playoff chances?"),
                      fluidRow(plotOutput("playoff_leverage", width = "80%"), align = "center")
             ),
             
             tabPanel("Skill v Luck",
                      h5("How good or lucky is your team?"),
                      fluidRow(plotOutput("quadrant", width = "80%"), align = "center"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("quad_week", "Weeks to Include:",
                                                                           1, weeks_played,
                                                                           c(1, weeks_played), step = 1))))
             ),
             
             tabPanel("League Gambling",
                      p("How do all the teams compare to each other?"),
                      fluidRow(column(8, plotOutput("heatmap")), align = "center"),
                      br(),
                      br(),
                      p("Just because you aren't matched up doesn't mean you can't still gamble on your scores:"),
                      tableOutput("lines")
             )
  ),
  
  
  # Visuals -----------------------------------------------------------------
  
  navbarMenu("Visuals",
             tabPanel("Team Scores",
                      plotlyOutput("weekly_plot"),
                      hr(),
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

             tabPanel("Team FVOA",
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

             tabPanel("Simulation Charts",
                      plotlyOutput("sim_chart"),
                      hr(),
                      fluidRow(selectizeInput("sim_chart_selection", "Show Simulations For:", selected = "Playoff Chances",
                                              c("Playoff Chances", "Wins", "Points")),
                               align = "center"),
                      fluidRow(checkboxGroupInput("team_sims", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_sims", "Clear Teams"), align = "center"),
                      br(),
                      # p("Calculated by simulating all remaining matchups in the season and figuring out the best 4 teams.
                      #   I then do this another 999 times and figure out the percentage of each team making the playoffs.
                      #   This will happen every week to see which teams have the best chance to make that money."),
                      # hr(),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Click Team names on right to add/remove"),
                        tags$li("Use checkboxes below to highlight"),
                        tags$li("Hover over any point to get detailed info"),
                        tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
                        tags$li(textOutput("sim_text"))
                      )
             ),

             tabPanel("Manager Evaluation",
                      h5("How well did you manage your team?"),
                      fluidRow(plotOutput("manager", width = "80%"), align = "center"),
                      hr()
             ),

             tabPanel("Projected v Actual Scores",
                      h5("How did your team perform against ESPN projections?"),
                      fluidRow(plotOutput("projected", width = "80%"), align = "center")
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
  
  # output$playoffs <- renderTable({
  #   tibble(Winner = c("Hoop", "Wikle", "Herndon", "Ford"),
  #          Percent = c("58%", "16%", "12%", "12%"),
  #          Odds = c("3:2", "13:2", "8:1", "17:2"),
  #          BettingLine = c("-125", "+525", "+725", "+775"))
  # }, align = "c")
  
  output$weekly <- renderTable({
    sx_current_matchups #%>% select(-espn)
  }, align = 'c', digits = 0)
  
  output$simulation <- renderTable({
    sx_simulated_records %>% 
      filter(week == max(week)) %>% 
      mutate(PF = as.integer(pf),
             PA = as.integer(pa),
             Playoffs = case_when(
               week < 8 & playoffs < 0.01 ~ "<1%",
               playoffs > 0 & playoffs <= 0.01 ~ "<1%",
               TRUE ~ percent(playoffs, accuracy = 1)
             )) %>% 
      select(Team = team, PF,
             Wins = wins, Playoffs)
  }, align = 'c', digits = 1)
  
  ### Rankings Tab ###
  
  output$sorting <- renderUI({
    selectInput("sorting", "Sort Rankings By:", 
                c(ranking_methods, "PF", "PA"))
  })
  
  ranking_selections <- function(rankings, ...) {
    
    dots <- quos(...)
    
    rankings %>% 
      rename_all(snakecase::to_sentence_case) %>% 
      select(Team:Percent, contains(paste(!!! dots, collapse = "|")))
  }
  
  output$rankings <- renderTable({
    
    rankings <- sx_rankings 
    sort <- sorting[[input$sorting]]
    rank_sort <- rankings %>%
      arrange(rankings[[sort]])
    point_sort <- rankings %>%
      arrange(desc(rankings[[sort]]))
    if (sort %in% c("PF", "PA")) {
      point_sort
    } else {
      rank_sort
    }
  }, align = 'c', digits = 2)
  
  ### League Tab ###

  output$heatmap <- renderPlot({
    sx_matchups_prob %>%
      rename(winner = team) %>%
      gather(loser, score, -winner) %>%
      mutate(loser = factor(loser, levels = sort(unique(loser)))) %>% 
      rename(team1 = 1, team2 = 2, wp = 3) %>% 
      plot_matchups_hm()
  }, res = 96)
  
  output$lines <- renderTable({
    sx_matchups_spread %>% 
      rename(Team = team)
  }, align = 'c')
  
  ### Matchups Tab ###
  output$matchup_breakdown <- renderTable(
    
    compare_teams(sx_fit, input$team1, input$team2, .verbose = T)  %>% 
      mutate(PctChance = paste0(round(PctChance, 0), "%")) %>% 
      rename(Margin = MarginVictory,
             Probability = PctChance), 
    align = 'c')
  
  output$matchup_plot <- renderPlot(plot_h2h_matchup(sx_fit, input$team1, input$team2), res = 96)
  
  ### Team Charts ###
  
  observeEvent(input$clear_teams_week, {
    updateCheckboxGroupInput(session, "team_week", selected = character(0))
  })
  
  output$weekly_plot <- renderPlotly({
    
    if (is.null(input$team_week)) {
      x <- ggplot(sx_scores, aes(week, score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
        geom_point(aes(group=team, color=team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        # scale_x_continuous(breaks = pretty_breaks(n = 7)) +
        scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- sx_scores %>% filter(team %in% input$team_week)
      
      x <- ggplot(sx_scores, aes(week, score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
        geom_line(data = tm, aes(group=team, color=team), size = 2) + 
        geom_point(aes(group=team, color=team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    }
  })
  
  observeEvent(input$clear_teams_fvoa, {
    updateCheckboxGroupInput(session, "team_fvoa", selected = character(0))
  })
  
  output$fvoa_plot <- renderPlotly({
    
    if (is.null(input$team_fvoa)) {
      x <- ggplot(sx_fvoa_season, aes(week, fvoa)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$Week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
        geom_point(aes(group=team, color=team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
        labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- sx_fvoa_season %>% filter(team %in% input$team_fvoa)
      
      x <- ggplot(sx_fvoa_season, aes(week, fvoa)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$Week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
        geom_line(data = tm, aes(group=team, color=team), size = 2) + 
        geom_point(aes(group=team, color=team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    }
  })
  
  observeEvent(input$clear_teams_sims, {
    updateCheckboxGroupInput(session, "team_sims", selected = character(0))
  })
  
  output$sim_text <- renderText({
    
    if(input$sim_chart_selection == "Wins") {
      "Dotted grey line gives the league average (7 wins)"
    } else if(input$sim_chart_selection == "Points") {
      "Dotted grey line gives the average total points for each week"
    } else if(input$sim_chart_selection == "Playoff Chances") {
      "Dotted grey line gives the baseline (40%)"
    } else {
      "ERROR"
    }
    
  })
  
  output$sim_chart <- renderPlotly({
    
    if (input$sim_chart_selection == "Wins") {
      
      if (is.null(input$team_sims)) {
        x <- sx_simulated_records %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.5, aes(color=team), size = 1.5) +
          geom_point(aes(color=team)) +
          # geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
          # geom_point(aes(group=Team, color=Team)) +
          geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- sx_simulated_records %>% filter(team %in% input$team_sims)
        
        x <- sx_simulated_records %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
      }
      
    } else if (input$sim_chart_selection == "Points") {
      
      if (is.null(input$team_sims)) {
        x <- sx_simulated_records %>% 
          rename(points = pf) %>% 
          ggplot(aes(week, points)) +
          geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$week), linetype = 2) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- sx_simulated_records %>% filter(team %in% input$team_sims)
        
        x <- sx_simulated_records %>% 
          rename(points = pf) %>% 
          ggplot(aes(Week, points)) +
          geom_smooth(se=F, color = "darkgrey", n = n_distinct(sx_scores$week), linetype=2) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
      }
      
    } else if (input$sim_chart_selection == "Playoff Chances") {
      
      if (is.null(input$team_sims)) {
        x <- sx_simulated_records %>% 
          ggplot(aes(week, playoffs)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1),
                             labels = percent) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- sx_simulated_records %>% filter(team %in% input$team_sims)
        
        x <- sx_simulated_records %>% 
          ggplot(aes(week, playoffs)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1),
                             labels = percent) +
          scale_x_continuous(breaks = c(1:14), limits = c(1, 14)) +
          labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
      }
      
      
    } else {
      "ERROR"
    }
    
  })
  
  
  output$playoff_leverage <- renderPlot({
    sx_playoff_leverage_chart #+ scale_fill_manual(values = fvoa_colors)
  }, res = 96)
  
  output$manager <- renderPlot({
    sx_lineup_eval
  }, res = 96)
  
  output$quadrant <- renderPlot({
    
    sx_scores %>% 
      filter(week %in% input$quad_week[1]:input$quad_week[2]) %>%
      calculate_quadrants(sx_schedule, .) %>% 
      plot_quadrant()
  }, res = 96)
  
  output$projected <- renderPlot({
    sx_proj %>%
      group_by(team) %>% 
      mutate(margin = actual - projected,
             sign = if_else(margin >=0, "positive", "negative"),
             avg = mean(margin),
             pos_count = sum(if_else(sign == "positive", 1, 0))) %>%
      ggplot(aes(x= week, y = margin, fill=sign)) +
      geom_bar(stat="identity") + 
      scale_x_continuous(breaks = 1:max(sx_proj$week),
                         labels = 1:max(sx_proj$week)) +
      facet_wrap(~reorder(team, - pos_count), ncol=n_distinct(sx_proj$team)/2) +
      guides(fill=FALSE) +
      labs(x = "Week", y = "Margin") +
      scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
      theme_fvoa() + 
      theme(panel.grid.major.y = element_blank())
  }, res = 96)
  
  ### Model Evaluation ###
  
  output$eval_accuracy <- renderText({

    accuracy <- sx_model_eval %>%
      dplyr::summarize(x = sum(correct) / n()) %>%
      pull() %>%
      scales::percent()

    str_glue("FVOA has correctly predicted {accuracy} of games this season")
  })

  output$eval_plot <- renderPlot({
    plot_model_eval_weekly(sx_model_eval)
  }, res= 96)

  output$eval_team = renderTable(
    evaluate_team_accuracy(sx_model_eval, .latest = TRUE),
    digits = 0, align = 'c')
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)