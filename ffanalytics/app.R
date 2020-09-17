####### FF Analytics App #######

library(shiny)
library(tidyverse)

load(here::here("ffanalytics", "projection-data.RData"))

ffanalytics_plot <- function(projections, pstn, n_players, plot_order) {
    
    if (pstn == "FLEX") {
        
        p <- projections %>%
            filter(pos %in% c("RB", "WR", "TE"),
                   !str_detect(team, "FA"),
                   avg_type == "average")
        
    } else if (pstn == "RB/WR") {
        
        p <- projections %>%
            filter(pos %in% c("RB", "WR"),
                   !str_detect(team, "FA"),
                   avg_type == "average")
        
    } else {
        
        
        p <-  projections %>%
            filter(pos == {{pstn}},
                   !str_detect(team, "FA"),
                   avg_type == "average")
    }
    
    if (plot_order == "ECR") {
        
        p <- p %>% 
            top_n(n_players, -pos_ecr) %>%
            mutate(player = str_glue("{first_name} {last_name} ({team})") %>% 
                       fct_reorder(-pos_ecr))
        
    } else {
        
        p <- p %>% 
            top_n(n_players, points) %>%
            mutate(player = str_glue("{first_name} {last_name} ({team})") %>% 
                       fct_reorder(points))
        
    } 
    
    p %>% 
        ggplot(aes(points, player, color = factor(tier))) +
        geom_text(aes(label = round(points)), size = 4) +
        geom_segment(aes(yend = player, x = floor, xend = ceiling)) +
        theme_light() +
        labs(x = "Points", y = "Player",
             caption = str_glue("Last Updated: {lubridate::with_tz(last_updated, tzone = 'America/Chicago')}")) +
        guides(color = FALSE)
    
}

ui <- fluidPage(
    
    titlePanel("FF Analytics Rankings"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("league",
                        "Select League:",
                        choices = c("CLT", "SX"),
                        selected = "CLT"),
            selectInput("position", 
                        "Select Position:",
                        choices = c("QB", "RB", "WR", "TE", 
                                    "FLEX", "RB/WR",
                                    "DST", "K", "DL", "DB"),
                        selected = "QB"
            ),
            numericInput("n_players",
                         "Number of Players:",
                         min = 1,
                         max = 70,
                         value = 30),
            selectInput("order", 
                        "Order Players By:",
                        choices = c("Points", "ECR"),
                        selected = "Points"
            ),
            checkboxGroupInput("groups",
                               "Availability",
                               choices = c("Roster", "Available", "Taken"),
                               selected = c("Roster", "Available")),
            p("Roster players indicated with asterisks")
        ),
        
        mainPanel(
            plotOutput("ranking_plot", height = "600px")
        )
    )
)

server <- function(input, output) {
    
    projections <- reactive({
        
        if(input$league == "CLT") {
            
            # roster <- clt_projections_df
            
            if("Roster" %in% c(input$groups)) {
                roster <- clt_projections_df %>%
                    filter(teamID == "Frech Prince Helaire") %>%
                    mutate(first_name = str_c("*", first_name),
                           last_name = str_c(last_name, "*"))
            } else {
                roster <- NULL
            }
            
            if("Available" %in% input$groups) {
                available <- clt_projections_df %>% filter(teamID == "FA"|is.na(teamID))
            } else {
                available <- NULL
            }
            
            if("Taken" %in% input$groups) {
                taken <- clt_projections_df %>% filter(!teamID %in% c("FA", "Frech Prince Helaire"),
                                                    !is.na(teamID))
            } else {
                taken <- NULL
            }
            
            if(is.null(input$groups)) {
                roster <- clt_projections_df
            }
            
        } else {
            
            if("Roster" %in% c(input$groups)) {
                roster <- sx_projections_df %>% 
                    filter(teamID == 2) %>% 
                    mutate(first_name = str_c("*", first_name),
                           last_name = str_c(last_name, "*"))
            } else {
                roster <- NULL
            }
            
            if("Available" %in% input$groups) {
                available <- sx_projections_df %>% filter(teamID == 0|is.na(teamID))
            } else {
                available <- NULL
            }
            
            if("Taken" %in% input$groups) {
                taken <- sx_projections_df %>% filter(!teamID %in% c(0, 2),
                                                   !is.na(teamID))
            } else {
                taken <- NULL
            }
            
            if(is.null(input$groups)) {
                roster <- sx_projections_df
            }
            
        }
        
        bind_rows(roster, available, taken,
                  .id = "availability") %>% 
            filter(!is.na(pos_ecr)|position == "DST")
        
    })
    
    output$ranking_plot <- renderPlot(
        ffanalytics_plot(projections(), 
                         pstn = input$position, 
                         n_players = input$n_players,
                         plot_order = input$order),
        res = 96
    )
}

shinyApp(ui = ui, server = server)
