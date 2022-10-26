####### CLT App #######

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

ranking_methods <- c("yahoo_rank", "fvoa_rank", "sos_rank", "sor_rank", "colley_rank") %>%
  set_names(c("Yahoo", "FVOA", "Strength of Schedule", "Strength of Record", "Colley (BCS)"))
sorting <- c("Yahoo Rank", "FVOA Rank", "SOS Rank", "SOR Rank", "Colley Rank", "PF", "PA") %>%
  set_names(ranking_methods, "PF", "PA")

today_week <- today() %>%
  floor_date(unit = "week", week_start = 2) %>%
  week()
start_week <- 35
current_week <- today_week - start_week
weeks_played <- current_week - 1
frech_stats <- 6

fvoa_colors <- c("#0055AA", "#C40003", "#00C19B", "#EAC862", "#894FC6",
                 "#7FD2FF", "#b2df8a", "#FF9D1E", "#C3EF00", "#cab2d6")

# Load Data ---------------------------------------------------------------

load(here::here("clt", "clt-data.RData"))

clt_scores <- extract_scores(clt_team)
clt_proj <- extract_projections(clt_team)

weeks <- n_distinct(clt_scores$week)
max_weeks <- max(clt_schedule$week)
teams <- unique(clt_scores$team)

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
           p(str_glue("Week {weeks_played}:")),
           tags$li(
             if(weeks_played == frech_stats) {
               ""
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               ""
             } else {
               "TBD"
             }
           ),           t
           tags$li(
             if(weeks_played == frech_stats) {
               ""
             } else {
               "TBD"
             }
           ),
           hr(),
           # h5("Playoff Projections", align = "center"),
           # br(),
           # fluidRow(tableOutput("playoffs"), align = "center"),
           h5(paste("Week", max(weeks) + 1, "Projections"), align = "center"),
           br(),
           fluidRow(tableOutput("weekly"), align="center"),
           h5("Season Projections", align = "center"),
           br(),
           fluidRow(tableOutput("simulation"), align = "center"),
           hr(),
           p("FVOA Assumptions:"),
           tags$ol(
             tags$li(HTML("<u><strong>Team scores only</strong></u> - opponent is random and you can't play defense 
                          so win-loss record is irrelevant")),
             tags$li(HTML("<u><strong>Regression to mean</strong></u> - scores are regressed to 
                          our league's historical average earlier in the season until enough evidence of true strength")),
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
               uiOutput("sorting")
             ), 
             mainPanel(tableOutput("rankings"))
           ),
           hr(),
           h5("Ranking Notes:"),
           tags$ol(
             tags$li(HTML("<u><strong>Yahoo</strong></u>: our official ranking based on win percentage and total points")),
             tags$li(HTML("<u><strong>FVOA</strong></u>: Frech-adjusted Value Over Average measures team's true strength
                          on net points scale, expected points margin vs average opponent")),
             tags$li(HTML("<u><strong>SOS</strong></u>: strength of schedule played, from perspective of an average team")),
             tags$li(HTML("<u><strong>SOR</strong></u>: strength of record reflects chance an average team would 
                          have team's record or better, given the schedule")),
             tags$li(HTML("<u><strong>Colley</strong></u>: one of the models that was used in the BCS computer rankings
                          and is based on win-loss record adjusted for strength of schedule")),
             tags$li("So in other words, FVOA and SOS rank on points,
                     SOR and Colley rank on record, 
                     and Yahoo ranks on both")
           )
  ),
  
  
  # Simulate ----------------------------------------------------------------
  
  navbarMenu("Simulate",
             tabPanel("Matchups",
                      h3("Head-to-Head Matchups"),
                      p("Simulate any potential matchup:"),
                      fluidRow(column(2, offset = 4,
                                      selectInput("team1", "Team 1:", teams, selected = teams[[1]])),
                               column(2, selectInput("team2", "Team 2:", teams, selected = teams[[2]]))),
                      fluidRow(plotOutput("matchup_plot", width = "600px", height = "600px"), align = 'center'),
                      hr(),
                      h3("League Gambling"),
                      p("Just because you aren't matched up doesn't mean you can't still gamble on any spread:"),
                      fluidRow(tableOutput("lines"), align = 'center')
                      
             ),
             tabPanel("Season",
                      h3("Playoff Leverage"),
                      h5("How much will winning/losing your next game affect your playoff chances?"),
                      fluidRow(plotOutput("playoff_leverage", width = "700px", height = "600px"), align = 'center'),
                      fluidRow(plotOutput("playoff_leverage_legend", width = "700px", height = "100px"), align = 'center')#,
                      # fluidRow(plotOutput("playoff_leverage", width = "80%"), align = "center"),
                      # fluidRow(plotOutput("playoff_leverage_legend", width = "80%", height = "100px"), align = "center")

             )
  ),
  
  # Skill v Luck ------------------------------------------------------------
  
  tabPanel("Skill v Luck",
           h3("Are you skilled or lucky?"),
           hr(),
           h4("Roster Evaluation"),
           p("How well have you managed your roster each week?"),
           # fluidRow(plotOutput("manager", width = "80%"), align = "center"),
           fluidRow(plotOutput("manager", width = "600px", height = "600px"), align = 'center'),
           hr(),
           h4("Schedule Luck"),
           p("What would your rank be with other simulated schedules?"),
           # fluidRow(plotOutput("schedule_luck", width = "80%"), align = "center"),
           fluidRow(plotOutput("schedule_luck", width = "600px", height = "600px"), align = 'center'),
           hr(),
           h4("Win Percentage for All Games"),
           p("How well would your team do if you were measured against every team each week?"),
           # fluidRow(plotOutput("wpag", width = "80%"), align = "center"),
           fluidRow(plotOutput("wpag", width = "600px", height = "600px"), align = 'center'),
           hr(),
           h4("Win Percentage vs Points"),
           p("How is your team doing relative to total points scored?"),
           # fluidRow(plotOutput("points_luck", width = "80%"), align = "center")
           fluidRow(plotOutput("points_luck", width = "600px", height = "600px"), align = 'center')
  ),
  
  # Weekly Charts -----------------------------------------------------------
  
  tabPanel("Weekly Charts",
           div(plotlyOutput("weekly_chart", width = "600px", height = "600px"), align = "center"),
           hr(),
           fluidRow(selectizeInput("weekly_chart_selection", 
                                   "Show Chart For:", 
                                   selected = "Scores",
                                   c("Scores", "FVOA", 
                                     "Simulated Playoff Chances", "Simulated Wins", "Simulated Points")),
                    align = "center"),
           fluidRow(checkboxGroupInput("team_weekly", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
           fluidRow(actionButton("clear_teams_weekly", "Clear Teams"), align = "center"),
           hr(),
           h5("Chart Notes:"),
           tags$ol(
             tags$li("Click Team names on right to add/remove"),
             tags$li("Use checkboxes below to highlight"),
             tags$li("Hover over any point to get detailed info"),
             tags$li("Zoom in on any part of the chart by dragging box over that area (double-click to return)"),
             tags$li(textOutput("weekly_text"))
           )
  ),
  
  # Model Evaluation Tab ----------------------------------------------------
  
  navbarMenu("Evaluate",
             tabPanel("FVOA Evaluation",
                      h3("How well is the FVOA model performing?"),
                      hr(),
                      fluidRow(plotOutput("eval_fvoa_plot", width = "600px", height = "300px"), align = "center"),
                      br(),
                      # p("Which teams screwed my model last week?"),
                      fluidRow(plotOutput("eval_fvoa_team", width = "600px", height = "400px"), align = "center")
             ),
             tabPanel("Yahoo Evaluation",
                      h3("How well are Yahoo projections performing?"),
                      hr(),
                      fluidRow(plotOutput("eval_proj_plot", width = "600px", height = "300px"), align = "center"),
                      br(),
                      # h5("How did your team perform against Yahoo projections?"),
                      fluidRow(plotOutput("projected", width = "600px", height = "400px"), align = "center")
             )
  )
  
  
  # End of navbarPage
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Weekly Projections ------------------------------------------------------
  
  output$weekly <- renderTable({
    select(clt_current_matchups, -Overall)
  }, align = 'c', digits = 0)
  
  output$simulation <- renderTable({
    clt_simulated_records %>% 
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
  
  output$playoffs <- renderTable({
    tibble(Winner = c("Scott", "David", "Eric", "German"),
           Percent = c("29%", "27%", "23%", "16%"),
           Odds = c("7:2", "7:2", "9:2", "13:2"),
           BettingLine = c("+250", "+275", "+350", "+525"))
  }, align = "c")
  
  # Rankings ----------------------------------------------------------------
  
  output$sorting <- renderUI({
    selectInput("sorting", "Sort Rankings By:", 
                c(ranking_methods, "PF", "PA"))
  })
  
  output$rankings <- renderTable({
    
    rankings <- clt_rankings %>% 
      select(1:8, SOS = 14, `SOS Rank` = 16, 20:23) %>% 
      mutate(SOR = percent(SOR, accuracy = 1))
    
    sort <- sorting[[input$sorting]]
    
    rank_sort <- arrange(rankings, rankings[[sort]])
    
    point_sort <- arrange(rankings, desc(rankings[[sort]]))
    
    if (sort %in% c("PF", "PA")) {
      point_sort
    } else {
      rank_sort
    }
  }, align = 'c', digits = 2)
  
  # H2H Matchups ------------------------------------------------------------
  
  output$matchup_plot <- renderPlot(plot_h2h_matchup(input$team1, input$team2, draws = clt_draws), 
                                    res = 96)
  
  # League Comparison -------------------------------------------------------
  
  output$lines <- renderTable(clt_lines, align = 'c')
  
  # Weekly Charts -----------------------------------------------------------
  
  observeEvent(input$clear_teams_weekly, {
    updateCheckboxGroupInput(session, "team_weekly", selected = character(0))
  })
  
  output$weekly_text <- renderText({
    
    if (input$weekly_chart_selection == "Scores") {
      "Dotted grey line gives the average score for each week"
    } else if (input$weekly_chart_selection == "FVOA") {
      "Dotted grey line gives the average team"
    } else if (input$weekly_chart_selection == "Simulated Wins") {
      "Dotted grey line gives the league average (7 wins)"
    } else if (input$weekly_chart_selection == "Simulated Points") {
      "Dotted grey line gives the average total points for each week"
    } else if (input$weekly_chart_selection == "Simulated Playoff Chances") {
      "Dotted grey line gives the baseline (40%)"
    } else {
      "ERROR"
    }
    
  })
  
  output$weekly_chart <- renderPlotly({
    
    if (input$weekly_chart_selection == "Scores") {
      
      if (is.null(input$team_weekly)) {
        
        x <- ggplot(clt_scores, aes(week, score)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 115, xend = max_weeks, yend = 115, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Score", x = "Week", title = "Weekly Scores") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      } else {
        
        tm <- filter(clt_scores, team %in% input$team_weekly)
        
        x <- ggplot(clt_scores, aes(week, score)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 115, xend = max_weeks, yend = 115, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Score", x = "Week", title = "Weekly Scores") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      }
      
    } else if (input$weekly_chart_selection == "FVOA") {
      
      if (is.null(input$team_weekly)) {
        
        x <- ggplot(clt_fvoa_season, aes(week, fvoa)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 0, xend = max_weeks, yend = 0, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      } else {
        
        tm <- filter(clt_fvoa_season, team %in% input$team_weekly)
        
        x <- ggplot(clt_fvoa_season, aes(week, fvoa)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 0, xend = max_weeks, yend = 0, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      }
      
      
    } else if (input$weekly_chart_selection == "Simulated Wins") {
      
      if (is.null(input$team_weekly)) {
        
        x <- clt_simulated_records %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.5, aes(color=team), size = 1.5) +
          geom_point(aes(color=team)) +
          geom_segment(x = 1, y = 7.5, xend = max_weeks, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      } else {
        
        tm <- filter(clt_simulated_records, team %in% input$team_weekly)
        
        x <- clt_simulated_records %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 7.5, xend = max_weeks, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
      }
      
    } else if (input$weekly_chart_selection == "Simulated Points") {
      
      if (is.null(input$team_weekly)) {
        
        x <- clt_simulated_records %>% 
          rename(points = pf) %>% 
          ggplot(aes(week, points)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 115 * max_weeks, xend = max_weeks, yend = 115 * max_weeks, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      } else {
        
        tm <- filter(clt_simulated_records, team %in% input$team_weekly)
        
        x <- clt_simulated_records %>% 
          rename(points = pf) %>% 
          ggplot(aes(week, points)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(y = pf, group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 115 * max_weeks, xend = max_weeks, yend = 115 * max_weeks, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
      }
      
    } else if (input$weekly_chart_selection == "Simulated Playoff Chances") {
      
      if (is.null(input$team_weekly)) {
        
        x <- clt_simulated_records %>% 
          ggplot(aes(week, playoffs)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 0.4, xend = max_weeks, yend = 0.4, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1),
                             labels = percent) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
        
      } else {
        
        tm <- filter(clt_simulated_records, team %in% input$team_weekly)
        
        x <- clt_simulated_records %>% 
          ggplot(aes(week, playoffs)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 0.4, xend = max_weeks, yend = 0.4, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), limits = c(0, 1),
                             labels = percent) +
          scale_x_continuous(breaks = c(1:max_weeks), limits = c(1, max_weeks)) +
          labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
          guides(color = "none") +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y")) %>%
          layout(legend = list(orientation = "h", x = 0.2, y = -0.2))
      }
      
      
    } else {
      "ERROR"
    }
    
  })
  
  # Playoff Leverage --------------------------------------------------------
  
  output$playoff_leverage <- renderPlot({
    clt_playoff_leverage + 
      scale_fill_manual(values = fvoa_colors)
  }, res = 96)
  
  output$playoff_leverage_legend <- renderPlot({
    
    # Core dataset with the basic labels
    label_df <- tibble(
      x = c(15, 15, 77, 101),
      y = c(1.6, 0.35, 0.35, 1),
      label = c("Chance to make playoffs with win ", "Chance to make playoffs with loss ", "Leverage", "X%")
    )
    
    
    # the horizontal lines
    seg_df <- tibble(
      x1 = c(0.2, 90, 0.2, 74.8, 75.3, 90, 103),
      x2 = c(0.2, 90, 0.2, 74.8, 75.3, 90, 103),
      y1 = c(1.3, 1.3, rep(.7, 5)),
      y2 = c(1.61, 1.61, rep(.343, 5))
      
    )
    
    # vertical lines
    seg2_df <- tibble(
      x1 = c(0.2, 0.2, 75.3),
      x2 = c(90, 74.8, 103),
      y1 = c(1.6, .35, .35),
      y2 = c(1.6, .35, .35)
    )
    
    tibble(x = 75,
           y = factor("Y"),
           x2 = 90) %>%
      ggplot(aes(x = x, y = y)) +
      geom_col(aes(x = 100), fill = "white", color = "grey", width = 0.4) +
      geom_col(aes(x = x2), width = 0.4, color = "#DC143C", fill = "grey") +
      geom_col(width = 0.4, color = "black", fill = "black") +
      geom_segment(data = seg_df,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = c(rep("black", 4), rep("#DC143C", 3)),
                   size = 1) +
      geom_segment(data = seg2_df,
                   aes(x = x1, y = y1, xend = x2, yend = y2),
                   color = c("black", "black", "#DC143C"),
                   size = 1) +
      geom_label(data = label_df,
                 aes(x = x, y = y, label = label),
                 hjust = 0, size = 6, fontface = "bold", fill = "white",
                 color = c("black", "black", "#DC143C", "#DC143C"),
                 label.size = NA,
                 # family = "Oswald",
                 label.padding = unit(0.05, "lines")) +
      coord_cartesian(ylim = c(0.7, 1.2), xlim = c(0, 108)) +
      theme_void() +
      theme(plot.margin = unit(c(0.5, 1.5, 0.5, 1.5), "cm"))
  })
  
  # Skill v Luck ------------------------------------------------------------
  
  output$points_luck <- renderPlot(plot_points_luck(clt_schedule, clt_scores, x = "pf"),
                                   res = 96)
  
  output$schedule_luck <- renderPlot(clt_schedule_luck, res = 96)
  
  output$wpag <- renderPlot(plot_wp_allplay(clt_schedule, clt_scores), res = 96)
  
  # Roster Evaluation -------------------------------------------------------
  
  output$manager <- renderPlot(clt_lineup_eval, res = 96)
  
  # FVOA Evaluation ---------------------------------------------------------
  
  output$eval_fvoa_plot <- renderPlot({
    plot_model_eval_weekly(clt_model_eval)
  }, res= 96)
  
  output$eval_fvoa_team = renderPlot({
    plot_model_eval_team(clt_model_eval) + 
      scale_fill_manual(values = fvoa_colors)
  }, res = 96)
  
  # Yahoo Evaluation ---------------------------------------------------------
  
  output$eval_proj_plot <- renderPlot({
    evaluate_projections(clt_proj) %>% 
      filter(week > 1) %>% 
      plot_projection_eval()
  }, res = 96)
  
  output$projected <- renderPlot({
    clt_proj %>%
      group_by(team) %>% 
      mutate(margin = actual - projected,
             sign = if_else(margin >=0, "positive", "negative"),
             avg = mean(margin),
             pos_count = sum(if_else(sign == "positive", 1, 0))) %>%
      ggplot(aes(x= week, y = margin, fill=sign)) +
      geom_bar(stat="identity") + 
      scale_x_continuous(breaks = 1:max(clt_proj$week),
                         labels = 1:max(clt_proj$week)) +
      facet_wrap(~reorder(team, - pos_count), ncol=n_distinct(clt_proj$team)/2) +
      guides(fill = "none") +
      labs(x = "Week", 
           y = "Margin",
           title = "League Projections by Team") +
      scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
      theme_fvoa() + 
      theme(panel.grid.major.y = element_blank())
  }, res = 96)
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)