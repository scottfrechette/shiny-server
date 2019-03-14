

library(shiny)
library(googlesheets)
library(tidyverse)

answers <- gs_title("GOT") %>% 
    gs_read(ws = 1)

points <- gs_title("GOT") %>% 
    gs_read(ws = 2)

got <- answers %>% 
    gather(question, answer,
           -Person, -Team) %>% 
    separate_rows(answer, sep = ",") %>% 
    left_join(points, by = c("question", "answer"))

points_earned <- got %>% 
    filter(!is.na(correct)) %>% 
    count(Person, Team, 
          wt = points, name = "Points")

points_remaining <- got %>% 
    filter(is.na(correct)) %>% 
    count(Person, Team, 
          wt = points, name = "PPR")

got_points <- full_join(points_earned, 
                        points_remaining,
                        by = c("Person", "Team")) %>% 
    replace_na(list(Points = 0,
                    PPR = 0)) %>% 
    arrange(-Points)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Game of Thrones Predictions"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("teams", "Choose Teams:", 
                        got_points$Team)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tableOutput("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$table <- renderTable({
        if(input$teams != "All") {
            got_points %>% 
                filter(Team %in% c(input$teams, "All"))
        } else {
            got_points
        }
        
    }, align = "c", digits = 0)
}

# Run the application 
shinyApp(ui = ui, server = server)
