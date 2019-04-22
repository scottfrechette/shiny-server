

library(shiny)
library(tidyverse)
library(plotly)

load(here::here("got", "got-data.RData"))

last_updated <- file.mtime(here::here("got", "got-data.RData")) %>% 
    format("%m/%d/%y %I:%M %p")

reorder_within <- function(x, by, within, fun = mean, 
                           sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
}
scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

ui <- navbarPage(
    
    title = "Game of Thrones Predictions",
    
    tabPanel("Leaderboard",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("teams", "Choose Teams:",
                                 got_points$Team,
                                 selected = "All")
                 ),
                 
                 mainPanel(
                     dataTableOutput("leaderboard"),
                     hr(),
                     em(paste("Last Updated:", last_updated), align = "right")
                 )
             )
    ),
    
    
    tabPanel("Answers",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("week", "Choose Weeks:",
                                 c("All", got %>% 
                                       filter(!is.na(week)) %>% 
                                       distinct(week) %>% 
                                       pull()),
                                 selected = "All")
                 ),
                 mainPanel(
                     dataTableOutput(("answers")),
                     hr(),
                     tags$em(paste("Last Updated:", last_updated))
                 )
             )
    ),
    
    tabPanel("Submissions",
             sidebarLayout(
                 sidebarPanel(
                     selectInput("individual", "Choose Individual:",
                                 got_points$Name,
                                 selected = "Scott")
                 ),
                 mainPanel(
                     dataTableOutput ("submissions") 
                 )
             )
    ),

    tabPanel("Responses",
             # sidebarLayout(
             #     sidebarPanel(
             # selectInput("category", "Choose Category:",
             #             unique(got$category),
             #             selected = "character_outcome")
             #     ),
             #     mainPanel(
             #         plotlyOutput(("responses"))
             #     )
             # )
             # plotlyOutput(("responses")),
             fluidRow(selectInput("category", "Choose Category:",
                                  unique(got$category),
                                  selected = "character_outcome"), align = "left"),
             plotlyOutput(("responses"))
             
    )
)

server <- function(input, output) {
    
    output$leaderboard <- renderDataTable(
        if(input$teams != "All") {
            got_points %>% 
                filter(Team %in% c(input$teams, "All")) %>% 
                select(-Team)
        } else {
            got_points
        },
        options = list(pageLength = 20)
        
    )
    
    output$submissions <- renderDataTable(
        got %>% 
            filter(Name == input$individual) %>% 
            group_by(question) %>% 
            summarize(answer = paste(answer, collapse = "; ")) %>% 
            rename(Question = question, Answer = answer)
        , options = list(pageLength = 100))
    
    output$responses <- renderPlotly({
        
        if (input$category == "Character Outcome") {
            
            p <- got %>% 
                filter(category == "Character Outcome") %>% 
                select(Name, question, answer) %>% 
                replace_na(list(answer = "No Submission")) %>% 
                mutate(Character = str_extract(question, "\\[.*\\]") %>% 
                           str_remove_all("\\[|\\]") %>% 
                           fct_inorder(), 
                       answer = factor(answer, levels = c("Live", "Die", "Reanimated/Turned", 
                                                          "Not Seen/Unknown", "No Submission")) %>% 
                           fct_rev()) %>% 
                add_count(Character, answer, name = "Guesses") %>% 
                group_by(Character, answer, Guesses) %>% 
                summarize(Guessers = paste(Name, collapse = "\n")) %>%
                ungroup() %>% 
                ggplot(aes(answer, Guesses, fill = answer, label = Guessers)) + 
                geom_col() + 
                scale_fill_manual(values = c("grey35", "#d1c0d8", 
                                             "#4c94db", "#ea0000", "#2a623d")) + 
                coord_flip() + 
                facet_wrap(~ Character) + 
                theme_minimal() + 
                labs(x = NULL,
                     y = "Guesses") + 
                theme(legend.position='none')
            
        } else {
            
            p <- got %>% 
                filter(category == input$category) %>% 
                select(Name, question, answer) %>% 
                replace_na(list(answer = "No Submission")) %>% 
                add_count(answer, name = "Guesses") %>% 
                group_by(question, answer, Guesses) %>% 
                summarize(Guessers = paste(Name, collapse = "\n")) %>%
                ungroup() %>% 
                mutate(answer = reorder_within(answer, Guesses, question)) %>% 
                ggplot(aes(answer, Guesses, label = Guessers)) + 
                geom_col() + 
                coord_flip() + 
                scale_x_reordered() + 
                facet_wrap(~ question, scales = "free", ncol = 2) + 
                theme_minimal() + 
                labs(x = NULL,
                     y = "Guesses")
            
        }
        
        plotly::ggplotly(p, tooltip = c("y", "label"),
                         height = 800, width = 1200) 
        
    })
    
    output$answers <- renderDataTable(
        
        if(input$week == "All") {
            
            got %>% 
                filter(!is.na(week)) %>% 
                distinct(Week = week,
                         Question = question,
                         Answer = answer_clean,
                         Points = points) %>% 
                arrange(Week, -Points)
            
        } else {
            
            got %>% 
                filter(week %in% input$week) %>% 
                distinct(Week = week,
                         Question = question,
                         Answer = answer_clean,
                         Points = points) %>% 
                arrange(-Points)
            
        }
        
        , options = list(pageLength = 100))
}

shinyApp(ui = ui, server = server)
