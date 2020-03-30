#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
source("./functions.R")

headers <- c(
  "All work and no play make Joe go crazy.",
  "We need more TESTING!",
  "Social isolation more like my everyday life, mirite?",
  "Not the bees!",
  "I want to take his face...off.",
  "The hustle never ends.",
  "Whatttaya hear? whattaya say?",
  "Gabagool? Ova here!",
  "Stay 6 feet away from people. I carry a ruler just in case.",
  "I rate these plots a 10 out of 10",
  "These plots are tremendous.",
  "These plots have the best numbers. They are tremendous.",
  "My current body temperature is 98.6. I have the best temperature",
  "I take no responsibility for these plots.",
  "It's the smell.",
  "I have reduced my breathing to once a minute to prevent infection.",
  "The first rule about covid is you do not talk about covid.",
  "There are 18 different headers. First person to email me them all gets a roll of toliet paper. The soft stuff."
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2 {
        color: #FFFFFF
      }

      body {
        background-color: #252a32;
      }
    "))
  ),
  
  titlePanel(textOutput("header"), windowTitle = "covid"),
  
  # Application title
  plotOutput("p_00_state", width = "70%", height = 800),
  plotOutput("p_01_state", width = "70%", height = 800),
  plotOutput("p_02_state", width = "70%", height = 800),
  plotOutput("p_03_state", width = "70%", height = 800),
  plotlyOutput("p_04_state", width = "70%", height = 800),
  plotOutput("p_05_state", width = "70%", height = 800),
  plotOutput("p_07_state", width = "70%", height = 800),
  
  plotOutput("p_00_country", width = "70%", height = 800),
  plotOutput("p_01_country", width = "70%", height = 800),
  plotOutput("p_02_country", width = "70%", height = 800),
  plotOutput("p_03_country", width = "70%", height = 800),
  
  plotOutput("p_00_county", width = "70%", height = 800),
  plotOutput("p_01_county", width = "70%", height = 800)
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    source("./state_plots.R")
    source("./global_plots.R")
    source("./county_plots.R")
    
    output$header <- renderText({
        sample(headers, 1)
    })
    
    # render plots here
    output$p_00_state <- renderPlot({
        p_00_state
    })
    
    output$p_01_state <- renderPlot({
        p_01_state
    })
    
    output$p_02_state <- renderPlot({
        p_02_state
    })
    
    output$p_03_state <- renderPlot({
        p_03_state
    })
    
    output$p_04_state <- renderPlotly({
        p_04_state
    })
    
    output$p_05_state <- renderPlot({
        p_05_state
    })
    
    output$p_07_state <- renderPlot({
        p_07_state
    })
    
    output$p_00_country <- renderPlot({
        p_00_country
    })
    
    output$p_01_country <- renderPlot({
        p_01_country
    })
    
    output$p_02_country <- renderPlot({
        p_02_country
    })
    
    output$p_03_country <- renderPlot({
        p_03_country
    })
    
    output$p_00_county <- renderPlot({
      p_00_county
    })
    
    output$p_01_county <- renderPlot({
      p_01_county
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
