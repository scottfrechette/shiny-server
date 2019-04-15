

library(shiny)
library(googlesheets)
library(tidyverse)

answers <- gs_title("GOT") %>% 
    gs_read(ws = 1) %>% 
    rename(Team = `How do you know me?`) %>% 
    select(-Timestamp, -`Email Address`)

points <- gs_title("GOT") %>% 
    gs_read(ws = 2) %>% 
    filter(correct == "yes")

got <- answers %>% 
    gather(question, answer,
           -Name, -Team) %>% 
    separate_rows(answer, sep = ",") %>% 
    left_join(points, by = c("question", "answer")) %>%
    mutate(points = case_when(
        answer %in% c("Die", "Live",
                      "Reanimated/Turned", 
                      "Not Seen/Unknown") ~ 10,
        !str_detect(answer, "\\d") ~ 50,
        TRUE ~ str_extract(answer, "(?<=\\().*(?=\\))") %>% # fix this 
               str_extract("\\d*") %>% 
               as.numeric))

points_earned <- got %>% 
    filter(correct == "yes") %>% 
    count(Name, Team, 
          wt = points, name = "Points")

points_remaining <- got %>% 
    filter(is.na(correct)) %>% 
    count(Name, Team, 
          wt = points, name = "PPR")

got_points <- full_join(points_earned, 
                        points_remaining,
                        by = c("Name", "Team")) %>% 
    replace_na(list(Points = 0,
                    PPR = 0)) %>% 
    arrange(-Points)



ui <- fluidPage(
    
    titlePanel("Game of Thrones Predictions"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("teams", "Choose Teams:", 
                        got_points$Team,
                        selected = "All")
        ),
        
        mainPanel(
            tableOutput("table")
        )
    )
)

server <- function(input, output) {
    
    output$table <- renderTable({
        if(input$teams != "All") {
            got_points %>% 
                filter(Team %in% c(input$teams, "All")) %>% 
                select(-Team)
        } else {
            got_points
        }
        
    }, align = "c", digits = 0)
}

shinyApp(ui = ui, server = server)
