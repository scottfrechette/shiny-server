####### FF Analytics App #######

library(shiny)
library(tidyverse)

load(here::here("ffanalytics", "projection-data.RData"))

ffanalytics_plot <- function(projections, pstn, n_players) {
    
    if(pstn == "FLEX") {
        
        projections %>%
            filter(pos %in% c("RB", "WR", "TE"),
                   !str_detect(team, "FA"),
                   avg_type == "average") %>%
            top_n(n_players, points) %>%
            arrange(-pos_rank) %>%
            mutate(player = str_glue("{first_name} {last_name} ({team})")) %>%
            ggplot(aes(points, reorder(player, points), color = factor(tier))) +
            geom_text(aes(label = round(points)), size = 4) +
            geom_segment(aes(yend = player, x = floor, xend = ceiling)) +
            theme_light() +
            labs(x = "Points", y = "Player",
                 caption = str_glue("Last Updated: {last_updated}")) +
            guides(color = FALSE)
        
    } else if (pstn == "RB/WR") {
        
        projections %>%
            filter(pos %in% c("RB", "WR"),
                   !str_detect(team, "FA"),
                   avg_type == "average") %>%
            top_n(n_players, points) %>%
            arrange(-pos_rank) %>%
            mutate(player = str_glue("{first_name} {last_name} ({team})")) %>%
            ggplot(aes(points, reorder(player, points), color = factor(tier))) +
            geom_text(aes(label = round(points)), size = 4) +
            geom_segment(aes(yend = player, x = floor, xend = ceiling)) +
            theme_light() +
            labs(x = "Points", y = "Player",
                 caption = str_glue("Last Updated: {last_updated}")) +
            guides(color = FALSE)
        
    } else {
        
        projections %>%
            filter(pos == {{pstn}},
                   !str_detect(team, "FA"),
                   avg_type == "average") %>%
            top_n(n_players, points) %>%
            arrange(-pos_rank) %>%
            mutate(player = str_glue("{first_name} {last_name} ({team})")) %>%
            ggplot(aes(points, reorder(player, points), color = factor(tier))) +
            geom_text(aes(label = round(points)), size = 4) +
            geom_segment(aes(yend = player, x = floor, xend = ceiling)) +
            theme_light() +
            labs(x = "Points", y = "Player",
                 caption = str_glue("Last Updated: {last_updated}")) +
            guides(color = FALSE) 
    }
    
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
                                    "DST", "K", "DE", "CB"),
                        selected = "QB"
            ),
            numericInput("n_players",
                         "Number of Players:",
                         min = 1,
                         max = 70,
                         value = 30),
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
            
            # roster <- clt_projections
            
            if("Roster" %in% c(input$groups)) {
                roster <- clt_projections %>%
                    filter(teamID == "Big Ass TDs") %>%
                    mutate(first_name = str_c("*", first_name),
                           last_name = str_c(last_name, "*"))
            } else {
                roster <- NULL
            }
            
            if("Available" %in% input$groups) {
                available <- clt_projections %>% filter(teamID == "FA")
            } else {
                available <- NULL
            }
            
            if("Taken" %in% input$groups) {
                taken <- clt_projections %>% filter(!teamID %in% c("FA", "Big Ass TDs"))
            } else {
                taken <- NULL
            }
            
            if(is.null(input$groups)) {
                roster <- clt_projections
            }
            
            bind_rows(roster, available, taken)
            
        } else {
            
            if("Roster" %in% c(input$groups)) {
                roster <- sx_projections %>% 
                    filter(teamID == 2) %>% 
                    mutate(first_name = str_c("*", first_name),
                           last_name = str_c(last_name, "*"))
            } else {
                roster <- NULL
            }
            
            if("Available" %in% input$groups) {
                available <- sx_projections %>% filter(teamID == 0)
            } else {
                available <- NULL
            }
            
            if("Taken" %in% input$groups) {
                taken <- sx_projections %>% filter(!teamID %in% c(0, 2))
            } else {
                taken <- NULL
            }
            
            if(is.null(input$groups)) {
                roster <- sx_projections
            }
            
            bind_rows(roster, available, taken)
        }
        
    })
    
    output$ranking_plot <- renderPlot({
        ffanalytics_plot(projections(), 
                         pstn = input$position, 
                         n_players = input$n_players)
    })
}

shinyApp(ui = ui, server = server)
