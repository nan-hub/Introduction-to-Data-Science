# Load packages ----
library(shiny)
library(wesanderson)

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Progression of the 2019-2020 Global Coronavirus Outbreak"),
  sidebarLayout(position = "left",
  sidebarPanel(
      helpText("Create demographic plots with 
               information about spread of coronavirus and stock market trend
               since that day"),
      
      selectInput("case", 
                  label = "Choose a kind of case to display",
                  choices = c("death", "confirmed", "recovered"),
                  selected = "confirmed"),
      
      dateRangeInput("dates", 
                     "Date range",
                     start = "2020-01-22", 
                     end = as.character(Sys.Date())),
      
      helpText("Select a stock to examine. 
               Information will be collected from Yahoo finance."),
      textInput("symb", "Symbol", "GOOG"),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale", 
                    value = FALSE),
      
      checkboxInput("adjust", 
                    "Adjust prices for inflation", value = FALSE),
 
),
     mainPanel("Spread of coronavirus on specific date and the stock 
               information since that date",
           fluidRow(
             splitLayout(cellWidths = c("45%", "55%"), plotOutput("map"),
                         plotOutput("stock"))
           ))
 )
)
# Server logic ----
# server function is run once each time a user visits the app
server <- function (input, output) {
output$map <- renderPlot ( {
  plotdate <- input$dates[1]
  case <- input$case
  ncov_tbl %>%
    filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                   "Hong Kong", "Taiwan")) %>%
    filter(Date == plotdate, Case == case) %>%
    group_by(`Province/State`) %>%  
    top_n(1, Date) %>% # take the latest count on that date
    right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
    scale_fill_gradientn(colors = wes_palette("Zissou1", 100, 
                                              type = "continuous"),
                         trans = "log10") + 
    theme_bw() +
    labs(title = str_c(case, " cases"), subtitle = plotdate)
} )



dataInput <- reactive ( {
  getSymbols(input$symb, src = "yahoo", 
             from = input$dates[1],
             to = input$dates[2],
             auto.assign = FALSE)
})

output$stock <- renderPlot({
  chartSeries(dataInput(), theme = chartTheme("white"), 
              type = "line", log.scale = input$log, TA = NULL)
})
}

# Run app ----
shinyApp(ui, server)