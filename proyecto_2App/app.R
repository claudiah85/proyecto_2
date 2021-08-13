library(shiny)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(corona_estados)

ui <- fluidPage(
  # Application title
  titlePanel("coronaMEX"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      ## Código para seleccionar paises
      selectizeInput( 'name', label = h3("Seleccionar estados"), 
                      choices = levels(gapminder$name), multiple=TRUE),
      sliderInput( 'ylms', label = h3("Mes"), min = 1, 
                   max = 4, value = c(05-01-2020, 24-04-2020) ),
      radioButtons( 'colorBy', label=h3("Colorear por"), 
                    choices = c(estado="state", country="Nacional"), 
                    selected = "Nacional" )
    ),
    mainPanel(
      plotOutput("lifeExpPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    gapminder %>%
      filter( state %in% input$states ) %>%
      ggplot( aes( year, life_expectancy, col=get(input$colorBy), group=state )) +
      geom_point( size=0.3 ) +
      geom_line( ) +
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2])) +
      labs(col=input$colorBy)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
