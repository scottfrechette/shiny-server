####### FF Analytics App #######

library(shiny)
library(tidyverse)

load(here::here("ffanalytics", "projection-data.RData"))

ffanalytics_plot <- function(projections, pos, n_players) {
    
    projections %>%
        filter(pos == {{pos}},
               !str_detect(team, "FA"),
               avg_type == "average") %>%
        top_n(n_players, points) %>%
        arrange(-pos_rank) %>%
        mutate(player = str_glue("{first_name} {last_name} ({team})")) %>%
        ggplot(aes(points, reorder(player, points), color = factor(tier))) +
        geom_text(aes(label = round(points)), size = 4) +
        geom_segment(aes(yend = player, x = floor, xend = ceiling)) +
        theme_light() +
        labs(x = "Points", y = "Player") +
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
                        choices = c("QB", "RB", "WR", "TE", "DST", "K", "DE", "CB"),
                        selected = "QB"
                        ),
            sliderInput("n_players",
                        "Number of Players:",
                        min = 1,
                        max = 75,
                        value = 30)
        ),

        mainPanel(
           plotOutput("ranking_plot", height = "600px")
        )
    )
)

server <- function(input, output) {

    projections <- reactive({
        
        if(input$league == "CLT") {
            clt_projections
        } else {
            sx_projections
        }
        
    })
    
    output$ranking_plot <- renderPlot({
        ffanalytics_plot(projections(), pos = input$position, n_players = input$n_players)
    })
}

shinyApp(ui = ui, server = server)
