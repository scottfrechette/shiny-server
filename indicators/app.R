# Economic Indicators

library(shiny)
library(tidyverse)
# library(crosstalk)
library(plotly)
library(DT)

gas_eia <- read_csv("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T09.04", 
                    show_col_types = F,
                    progress = F)

gas_fred <- read_csv('https://fred.stlouisfed.org/series/GASREGW/downloaddata/GASREGW.csv') %>% 
  transmute(date = DATE,
            gas = as.numeric(VALUE)) %>% 
  filter(date >= '1991-01-21')

gas <- bind_rows(gas_eia %>%
                   filter(MSN == "RLUCUUS",
                          !Value %in% c("Not Applicable", "Not Available"),
                          YYYYMM < 197314) %>%
                   transmute(date = if_else(str_detect(YYYYMM, '13'), 
                                            as.Date(paste0(str_sub(YYYYMM, end = 4), '-01-01')),
                                            lubridate::ym(YYYYMM)),
                             gas = as.numeric(Value)),
                 gas_eia %>%
                   filter(MSN == "RLUCUUS",
                          !Value %in% c("Not Applicable", "Not Available"),
                          !str_detect(YYYYMM, "13$"),
                          YYYYMM < 197401) %>%
                   transmute(date = lubridate::ym(YYYYMM),
                             gas = as.numeric(Value)),
                 gas_eia %>%
                   filter(MSN == "RLUCUUS",
                          !Value %in% c("Not Applicable", "Not Available"),
                          !str_detect(YYYYMM, "13$"),
                          YYYYMM >= 197401,
                          YYYYMM < 197601) %>%
                   transmute(date = lubridate::ym(YYYYMM),
                             gas = as.numeric(Value)),
                 gas_eia %>% 
                   filter(MSN == 'RUUCUUS',
                          !str_detect(YYYYMM, "13$"),
                          !Value %in% c("Not Applicable", "Not Available"),
                          YYYYMM <= 199101) %>% 
                   transmute(date = lubridate::ym(YYYYMM),
                             gas = as.numeric(Value)),
                 gas_fred)

min_wage_raw <- rvest::read_html('https://www.dol.gov/agencies/whd/minimum-wage/history/chart') %>% 
  rvest::html_table() %>% 
  .[[1]] %>% 
  select(date = 1, wage = 2) %>%
  filter(wage != "") %>%
  mutate(date = lubridate::mdy(str_sub(date, end = 12)), 
         wage = as.numeric(str_extract(wage, "\\d.\\d\\d")))

min_wage <- tibble(date = seq.Date(from = min(min_wage_raw$date), 
                                   to = Sys.Date(), 
                                   by = 'day')) %>% 
  left_join(min_wage_raw, by = "date") %>% 
  fill(wage)

gas_min_wage <- left_join(gas, min_wage, by = 'date') %>%
  mutate(gallons = round(wage / gas, 2)) %>% 
  rename(Date = 1, 
         `Avg Gas Price` = 2,
         `Federal Min Wage` = 3,
         Gallons = 4)

median_wage_fred <- read_csv('https://fred.stlouisfed.org/series/LEU0252881500Q/downloaddata/LEU0252881500Q.csv') %>% 
  transmute(date = DATE,
            wage = VALUE)

gas_median_wage <- bind_rows(tibble(date = seq.Date(as.Date('1968-01-01'), as.Date('1978-01-01'), 'year'),
                                    wage = round(c(7005.0, 7683, 8330, 8700, 9000, 9648,
                                             10378, 11000, 11700, 12604, 13500) / 52, 2)),
                             median_wage_fred) %>% 
  left_join(gas %>% 
              mutate(date = lubridate::floor_date(date, 'quarter')) %>% 
              distinct(date, .keep_all = T), 
            by = 'date') %>%
  mutate(gallons = round(wage / 40 / gas, 2)) %>% 
  select(Date = 1, 
         `Avg Gas Price` = 3,
         `Median Salary` = 2,
         Gallons = 4)

# sd_gas_wage_all <- SharedData$new(gas_wage)
# sd_gas_wage_inc_wage <- SharedData$new(filter(shared_df$data(), `Federal Min Wage` - lag(`Federal Min Wage`) != 0))
# sd_gas_wage_1st_inc_wage <- SharedData$new(filter(shared_df$data(), `Federal Min Wage` - lag(`Federal Min Wage`) != 0)[1,])

ui <- fluidPage(
  
  titlePanel("Frech Indicators"),
  
  navlistPanel(
    "Wage-to-Gas Ratios",
    tabPanel("Minimum Wage",
             h4('How many gallons of gas can an hour of minimum wage work buy?'),
             plotOutput("gas_min_wage_plot"),
             DTOutput("gas_min_wage_table")),
    tabPanel("Median Salary",
             h4("How many gallons of gas can an hour of median salary work buy?"),
             p("Assuming a typical worker earning median salary works 40 hours/week"),
             plotOutput("gas_median_wage_plot"),
             DTOutput("gas_median_wage_table")),
    widths = c(3,9), well = F
  )
  
)

server <- function(input, output) {

  output$gas_min_wage_plot <- renderPlot({
    gas_min_wage %>% 
      ggplot(aes(Date, Gallons)) + 
      geom_line(aes(group = 1), alpha = 0.4, size = 0.4) + 
      geom_point(data = filter(gas_min_wage, `Federal Min Wage` - lag(`Federal Min Wage`) != 0),
                 color = '#004F71',
                 size = 2) +
      geom_text(data = filter(gas_min_wage, `Federal Min Wage` - lag(`Federal Min Wage`) != 0)[6,],
                aes(x = Date, y = Gallons + 0.7),
                label = "Wage\nincreases",
                color = "#004F71") +
      geom_text(data = bind_rows(slice_max(gas_min_wage, order_by = Gallons, n = 1),
                                 slice_min(gas_min_wage, order_by = Gallons, n = 1),
                                 slice_tail(gas_min_wage, n = 1)),
                aes(x = Date, y = Gallons, label = Gallons),
                color = "grey50", vjust = 'outward') +
      scale_x_date(date_breaks = "5 years",
                   date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 6),
                         expand = c(0, NA)) +
      labs(x = NULL,
           y = "Gallons Earned per Hour Worked",
           caption = "Source: FRED and EIA") +
      ggthemes::theme_clean()
    
  })
  
  output$gas_min_wage_table <- renderDT({
    datatable(gas_min_wage,
              options = list(dom = 'tlp'))}, 
    server = F)
  
  output$gas_median_wage_plot <- renderPlot({
    gas_median_wage %>% 
      ggplot(aes(Date, Gallons)) + 
      geom_line(alpha = 0.4, size = 0.4) + 
      geom_text(data = bind_rows(slice_max(gas_median_wage, order_by = Gallons, n = 1),
                                 slice_min(gas_median_wage, order_by = Gallons, n = 1),
                                 slice_tail(gas_median_wage, n = 1)),
                aes(x = Date, y = Gallons, label = Gallons),
                color = "grey50", vjust = 'outward') +
      scale_x_date(date_breaks = "5 years",
                   date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 15),
                         expand = c(0, NA)) +
      labs(x = NULL,
           y = "Gallons Earned per Hour Worked",
           caption = "Source: FRED and EIA") +
      ggthemes::theme_clean()
  })
  
  output$gas_median_wage_table <- renderDT({
    datatable(gas_median_wage,
              options = list(dom = 'tlp'))}, 
    server = F)

  
}

shinyApp(ui = ui, server = server)