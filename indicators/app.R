# Economic Indicators

library(shiny)
library(tidyverse)
# library(crosstalk)
library(plotly)

gas_raw <- read_csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T09.04", 
                    show_col_types = F,
                    progress = F)

gas <- bind_rows(gas_raw %>%
                   filter(MSN == "RLUCUUS",
                          !Value %in% c("Not Applicable", "Not Available"),
                          !str_detect(YYYYMM, "13$"),
                          YYYYMM < 197401) %>%
                   transmute(date = lubridate::ym(YYYYMM),
                             gas = as.numeric(Value)),
                 gas_raw %>%
                   filter(MSN == "RLUCUUS",
                          !Value %in% c("Not Applicable", "Not Available"),
                          !str_detect(YYYYMM, "13$"),
                          YYYYMM >= 197401,
                          YYYYMM < 197601) %>%
                   transmute(date = lubridate::ym(YYYYMM),
                             gas = as.numeric(Value)),
                 gas_raw %>% 
                   filter(MSN == 'RUUCUUS',
                          !str_detect(YYYYMM, "13$"),
                          !Value %in% c("Not Applicable", "Not Available"),
                          YYYYMM <= 199012) %>% 
                   transmute(gas = as.numeric(Value),
                             date = lubridate::ym(YYYYMM)),
                 tidyquant::tq_get("GASREGW",
                                   get = "economic.data",
                                   from = "1991-01-21") %>%
                   select(date, gas = price))

wage_raw <- read_html('https://www.dol.gov/agencies/whd/minimum-wage/history/chart') %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(date = 1, wage = 2) %>%
  filter(wage != "") %>%
  mutate(date = lubridate::mdy(str_sub(date, end = 12)), 
         wage = as.numeric(str_extract(wage, "\\d.\\d\\d")))

wage <- tibble(date = seq.Date(from = min(historical_wage$date), 
                               to = Sys.Date(), 
                               by = 'day')) %>% 
  left_join(wage_raw, by = "date") %>% 
  fill(wage)

gas_wage <- left_join(gas, wage, by = 'date') %>%
  mutate(gallons = round(wage / gas, 2)) %>% 
  rename(Date = 1, 
         `Avg Gas Price` = 2,
         `Federal Min Wage` = 3,
         Gallons = 4)

# sd_gas_wage_all <- SharedData$new(gas_wage)
# sd_gas_wage_inc_wage <- SharedData$new(filter(shared_df$data(), `Federal Min Wage` - lag(`Federal Min Wage`) != 0))
# sd_gas_wage_1st_inc_wage <- SharedData$new(filter(shared_df$data(), `Federal Min Wage` - lag(`Federal Min Wage`) != 0)[1,])

ui <- fluidPage(
  
  titlePanel("Frech Indicators"),
  
  navlistPanel(
    tabPanel("Wage-to-Gas Ratio",
             plotlyOutput("gas_wage_plot"),
             DTOutput("gas_wage_table")),
    widths = c(3,9), well = F
  )
  
)

server <- function(input, output) {
  
  output$gas_wage_plot <- renderPlotly({
    p <- gas_wage %>% 
      ggplot(aes(Date, Gallons)) + 
      geom_line(aes(group = 1), alpha = 0.4, size = 0.4) + 
      geom_point(data = filter(gas_wage, `Federal Min Wage` - lag(`Federal Min Wage`) != 0),
                 color = '#004F71',
                 size = 2) +
      geom_text(data = filter(gas_wage, `Federal Min Wage` - lag(`Federal Min Wage`) != 0)[1,],
                aes(x = Date, y = Gallons + 0.7),
                label = "Wage\nincreases",
                color = "#004F71") +
      scale_x_date(date_breaks = "5 years",
                   date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 6),
                         expand = c(0, NA)) +
      labs(x = NULL,
           y = "Gallons Earned per Hour Worked",
           title = "How many gallons of gas can an hour of minimum wage work buy?",
           # title = "Wage-to-Gas Ratio",
           # subtitle = "How many gallons of gas can an hour of minimum wage work buy?",
           caption = "Source: FRED and EIA") +
      ggthemes::theme_clean() + 
      theme(panel.grid.major.y = element_line(linetype = 2, color = 'grey90'))
    
    ggplotly(p)
  })
  
  output$gas_wage_table <- renderDT({
    datatable(gas_wage,
              options = list(dom = 'tlp'))}, 
    server = F)
  
}

shinyApp(ui = ui, server = server)