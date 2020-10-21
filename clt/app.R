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

reps <-  1e6
ranking_methods <- c("yahoo_rank", "fvoa_rank", "sos_rank", "colley_rank") %>%
  set_names(c("Yahoo", "FVOA", "Strength of Schedule", "Colley (BCS)"))
sorting <- c("Yahoo Rank", "FVOA Rank", "SoS Rank", "Colley Rank", "Points") %>%
  set_names(ranking_methods, "Points")

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

weeks <- n_distinct(clt_scores$week)
teams <- pull(distinct(select(clt_scores, starts_with("team"))))

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
               "Looks like that 4th spot is up for grabs, and maybe even Barrett's 3rd spot"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "I would say I can't figure out why the top scorer through 6 weeks is in 8th until I look at weekly points and see Josh plummeting last 3 weeks"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "ERam and I both got a sizable playoff boost with strong wins last week, and Diaz really locked up the 2nd spot for now"
             } else {
               "TBD"
             }
           ),
           tags$li(
             if(weeks_played == frech_stats) {
               "Not to be outdone German laid a real stinker with lowest score of the season and just barely edged out PFinn for least likely to make playoffs (though both still around 12%)"
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
               #                    ranking_methods, selected = c("yahoo_rankings")),
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
             tags$li(HTML("<u><strong>SoS</strong></u>: strength of schedule based on FVOA scores (higher means harder schedule)")),
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
           
           # p("How do all the teams compare to each other?"),
           # fluidRow(column(8, plotOutput("heatmap")), align = "center"),
           # br(),
           # br(),
           # p("Just because you aren't matched up doesn't mean you can't still gamble on your scores:"),
           # tableOutput("lines")
           
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
                           selectInput("team1", "Team 1:", teams, selected = teams[[1]]), 
                           selectInput("team2", "Team 2:", teams, selected = teams[[2]])),
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
             
             tabPanel("Simulation Charts",
                      plotlyOutput("sim_chart"),
                      hr(),
                      fluidRow(selectizeInput("sim_chart_selection", "Show Simulations For:", selected = "Playoff Chances",
                                              c("Playoff Chances", "Wins", "Points")),
                               align = "center"),
                      fluidRow(checkboxGroupInput("team_sims", "Teams to Highlight:", sort(teams), inline = T), align = "center"),
                      fluidRow(actionButton("clear_teams_sims", "Clear Teams"), align = "center"),
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
                        tags$li(textOutput("sim_text"))
                      )
                      ),
             
             tabPanel("Playoff Leverage",
                      h5("How much will winning/losing your next game affect your playoff chances?"),
                      plotOutput("playoff_leverage"),
                      h5("Chart Notes:"),
                      tags$ol(
                        tags$li("Full bar is your chance of making playoffs with win"),
                        tags$li("Darker portion is your chance of making playoffs with loss"),
                        tags$li("The difference between these two, the light portion and number listed to the right,
                                is the playoff leverage of this game"),
                        tags$li("Note: these are all treated independently of the other teams winning/losing so it's not exact")
                        )
             ),
             
             tabPanel("Manager Evaluation",
                      h5("How well did you manage your team?"),
                      plotOutput("manager"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("proj_week", "Weeks to Include:", 1,
                                                                           max(weeks), c(1, max), step = 1))))
             ),
             
             tabPanel("Skill v Luck",
                      h5("How good or lucky is your team?"),
                      plotOutput("quadrant"),
                      hr(),
                      fluidRow(column(4, offset = 4, wellPanel(sliderInput("quad_week", "Weeks to Include:", 1,
                                                                           max(weeks), c(1, max), step = 1))))
             ),
             
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
  
  output$playoffs <- renderTable({
    tibble(Winner = c("Bobby", "PFinn", "Diaz", "David"),
           Percent = c("37%", "21%", "20%", "18%"),
           Odds = c("5:2", "9:2", "5:1", "11:2"),
           BettingLine = c("+175", "+375", "+400", "+450"))
  }, align = "c")

  output$weekly <- renderTable({
    clt_current_matchups #%>% select(-Yahoo)
  }, align = 'c', digits = 0)
  
  output$simulation <- renderTable({
    clt_simulated_season %>% 
      filter(week == max(week)) %>% 
      mutate(points = as.integer(points),
             Playoffs = paste0(round(percent, 0), "%")) %>% 
      select(Team = team, Points = points,
             Wins = wins, Playoffs)
  }, align = 'c', digits = 1)
  
  ### Rankings Tab ###
  
  output$sorting <- renderUI({
    selectInput("sorting", "Sort Rankings By:", 
                c(ranking_methods, "Points"))
  })
  
  ranking_selections <- function(rankings, ...) {
    
    dots <- quos(...)
    
    rankings %>% 
      rename_all(snakecase::to_sentence_case) %>% 
      select(Team:Percent, contains(paste(!!! dots, collapse = "|")))
  }
  
  output$rankings <- renderTable({

    rankings <- clt_rankings
    sort <- sorting[[input$sorting]]
    rank_sort <- rankings %>%
      arrange(rankings[[sort]])
    point_sort <- rankings %>%
      arrange(desc(rankings[[sort]]))
    if (sort == "Points") {
      point_sort
    } else {
      rank_sort
    }
  }, align = 'c', digits = 2)
  
  ### League Tab ###
  
  matchups_prob <- reactive(
    
    clt_scores %>% 
      filter(week %in% input$league_week[1]:input$league_week[2]) %>% 
      compare_league(.reps = 1000) %>% 
      fvoa:::spread_league(.output = "wp")
    
  )
  
  matchups_spread <- reactive(
    
    clt_scores %>% 
      filter(week %in% input$league_week[1]:input$league_week[2]) %>% 
      compare_league(.reps = 1000) %>% 
      fvoa:::spread_league(.output = "spread")
    
  )

  output$heatmap <- renderPlot({
    hm <- matchups_prob() %>%
      rename(winner = team) %>% 
      # mutate(winner = rownames(.)) %>% 
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
    matchups_spread() %>% 
      rename(Team = team)
      # add_column(Team = teams, .before = 1)
  }, align = 'c')
  
  ### Matchups Tab ###
  output$matchup_breakdown <- renderTable(
    
    clt_scores %>% 
      filter(week %in% input$matchup_week[1]:input$matchup_week[2]) %>% 
      compare_teams(input$team1, input$team2, .reps = 1e6, .verbose = TRUE) %>% 
      rename(Margin = MarginVictory,
             Probability = PctChance), 
    align = 'c')
  
  ### Team Charts ###
  
  observeEvent(input$clear_teams_week, {
    updateCheckboxGroupInput(session, "team_week", selected = character(0))
  })
  
  output$weekly_plot <- renderPlotly({
    
    if (is.null(input$team_week)) {
      x <- ggplot(clt_scores, aes(week, score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
        geom_point(aes(group=team, color=team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        # scale_x_continuous(breaks = pretty_breaks(n = 7)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "Score", x = "Week", title = "Weekly Scores") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- clt_scores %>% filter(team %in% input$team_week)
      
      x <- ggplot(clt_scores, aes(week, score)) +
        geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
        geom_line(data = tm, aes(group=team, color=team), size = 2) + 
        geom_point(aes(group=team, color=team)) +
        scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
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
      x <- ggplot(clt_fvoa_season, aes(week, fvoa)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$Week), linetype=2) +
        geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
        geom_point(aes(group=team, color=team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
        labs(y = "FVOA", x = "Week", title = "Weekly FVOA") +
        guides(color=FALSE) +
        theme_fvoa() + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("group", "x", "y"))
      
    } else {
      tm <- clt_fvoa_season %>% filter(team %in% input$team_fvoa)
      
      x <- ggplot(clt_fvoa_season, aes(week, fvoa)) +
        # geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$Week), linetype=2) +
        geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
        geom_line(data = tm, aes(group=team, color=team), size = 2) + 
        geom_point(aes(group=team, color=team)) +
        geom_segment(x = 1, y = 0, xend = 15, yend = 0, color = "darkgrey", linetype = 2) +
        scale_y_continuous(breaks = c(-100, -75, -50, -25, 0, 25, 50, 75, 100), limits = c(-100, 100)) +
        # scale_y_continuous(breaks = pretty_breaks(n = 5)) +
        scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
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
        x <- clt_simulated_season %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.5, aes(color=team), size = 1.5) +
          geom_point(aes(color=team)) +
          # geom_line(alpha = 0.5, aes(group=Team, color=Team), size = 1.5) +
          # geom_point(aes(group=Team, color=Team)) +
          geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- clt_simulated_season %>% filter(team %in% input$team_sims)
        
        x <- clt_simulated_season %>% 
          ggplot(aes(week, wins)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 7.5, xend = 15, yend = 7.5, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 14), limits = c(0, 15)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
          labs(y = "Wins", x = "Week", title = "Projected Wins by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
      }
      
    } else if (input$sim_chart_selection == "Points") {
      
      if (is.null(input$team_sims)) {
        x <- clt_simulated_season %>% 
          ggplot(aes(week, points)) +
          geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$week), linetype = 2) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- clt_simulated_season %>% filter(team %in% input$team_sims)
        
        x <- clt_simulated_season %>% 
          ggplot(aes(Week, Points)) +
          geom_smooth(se=F, color = "darkgrey", n = n_distinct(clt_scores$week), linetype=2) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          scale_y_continuous(breaks = pretty_breaks(n = 5)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
          labs(y = "Points", x = "Week", title = "Projected Total Points by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
      }
      
    } else if (input$sim_chart_selection == "Playoff Chances") {
      
      if (is.null(input$team_sims)) {
        x <- clt_simulated_season %>% 
          ggplot(aes(week, percent)) +
          geom_line(alpha = 0.5, aes(group=team, color=team), size = 1.5) +
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
          labs(y = "Chance", x = "Week", title = "Projected Chance of Making Playoffs by Week") +
          guides(color=FALSE) +
          theme_fvoa() + 
          scale_color_manual(values = fvoa_colors)
        
        ggplotly(x, tooltip = c("group", "x", "y"))
        
      } else {
        tm <- clt_simulated_season %>% filter(team %in% input$team_sims)
        
        x <- clt_simulated_season %>% 
          ggplot(aes(week, percent)) +
          geom_line(alpha = 0.2, aes(group=team, color=team), size = 1.5) +
          geom_line(data = tm, aes(group=team, color=team), size = 2) + 
          geom_point(aes(group=team, color=team)) +
          geom_segment(x = 1, y = 40, xend = 15, yend = 40, color = "darkgrey", linetype = 2) +
          scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
          scale_x_continuous(breaks = c(1:15), limits = c(1, 15)) +
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
    clt_playoff_leverage_chart + 
      scale_fill_manual(values = fvoa_colors)
  })
  
  output$manager <- renderPlot({
    clt_lineup_eval
  })
  
  output$quadrant <- renderPlot({
    
    clt_scores %>% 
      filter(week %in% input$quad_week[1]:input$quad_week[2]) %>% 
      calculate_quadrants(clt_schedule, .) %>% 
      plot_quadrant()
  })
  
  output$projected <- renderPlot({
    clt_proj %>%
      filter(week %in% input$proj_week[1]:input$proj_week[2]) %>%
      # spread(Type, Score) %>% 
      group_by(team) %>% 
      mutate(margin = act - proj,
             sign = if_else(margin >=0, "positive", "negative"),
             avg = mean(margin),
             pos_count = sum(if_else(sign == "positive", 1, 0))) %>%
      ggplot(aes(x= week, y = margin, fill=sign)) +
      geom_bar(stat="identity") + 
      scale_x_continuous(breaks = pretty_breaks(n = 7)) +
      facet_wrap(~reorder(team, - pos_count), ncol=n_distinct(clt_proj$team)/2) +
      guides(fill=FALSE) +
      labs(x = "Week", y = "Margin") +
      scale_fill_manual(values = c(equal = "#619CFF", negative = "#F8766D", positive = "#00BA38")) +
      theme_fvoa() + 
      theme(panel.grid.major.y = element_blank())
  })
  
  output$boxplot <- renderPlot({
    clt_scores %>% 
      filter(week %in% input$boxplot_week[1]:input$boxplot_week[2]) %>%
      ggplot(aes(x=reorder(team, -score, fun=mean), y=score, fill=team)) + 
      geom_boxplot(coef = 1.25, outlier.alpha = 0.6) + 
      stat_summary(fun.y=mean, geom="point", shape=18, size=3, show.legend=FALSE) + 
      guides(fill=F) +
      labs(y = "Score", x = "", title = "Team Boxplots") +
      theme_fvoa() + 
      scale_fill_manual(values = fvoa_colors) + 
      theme(panel.border = element_blank())
  })
  
  observeEvent(input$clear_teams_density, {
    updateCheckboxGroupInput(session, "team_density", selected = character(0))
  })
  
  output$density <- renderPlotly({
    
    if (is.null(input$team_density)) {
      x <- clt_scores %>% 
        filter(week %in% input$density_week[1]:input$density_week[2]) %>%
        ggplot(aes(score)) +
        geom_density(aes(fill = team, color = team), alpha = 0.2) + 
        labs(x = "Weekly Scores", y = "Density", title = "Density Plots") +
        guides(fill=FALSE) + 
        theme_fvoa() + 
        scale_fill_manual(values = fvoa_colors) + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("fill", "x"))
      
    } else {
      tm <- clt_scores %>% filter(team %in% input$team_density)
      
      x <- clt_scores %>% 
        filter(week %in% input$density_week[1]:input$density_week[2]) %>% 
        ggplot(aes(Score)) +
        geom_density(aes(fill = team, color = team), alpha = 0.1) + 
        geom_density(data = tm, aes(fill = team, color = team), alpha = 0.8) + 
        labs(x = "Weekly Scores", y = "Density", title = "Density Plots") +
        guides(fill=FALSE, color = FALSE) + 
        theme_fvoa() + 
        scale_fill_manual(values = fvoa_colors) + 
        scale_color_manual(values = fvoa_colors)
      
      ggplotly(x, tooltip = c("fill", "x"))
    }
  })
  
  ### Model Evaluation ###
  
  output$eval_accuracy <- renderText({
    
    accuracy <- clt_model_eval %>% 
      dplyr::summarize(x = sum(correct) / n()) %>% 
      pull() %>% 
      scales::percent()
    
    str_glue("FVOA has correctly predicted {accuracy} of games this season")
  })
  
  output$eval_plot <- renderPlot({
    plot_model_eval_weekly(clt_model_eval)
  })
  
  output$eval_team = renderTable(
    evaluate_team_accuracy(clt_model_eval, .latest = TRUE), 
    digits = 0, align = 'c')
  
}

# Run App -----------------------------------------------------------------
shinyApp(ui = ui, server = server)