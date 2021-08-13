library(shiny)
library(proyecto_2)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(datos)

ui <- fluidPage(
  # Application title
  titlePanel("coronaMEX"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      ## Código para seleccionar paises
      selectizeInput( 'states', label = h3("Seleccionar estados"), 
                      choices = levels(proyecto_2$country), multiple=TRUE),
      sliderInput( 'ylms', label = h3("Años contemplados"), min = 2020, 
                   max = 2020, value = c(05-01-2020, 24-04-2020) ),
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
    proyecto_2 %>%
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
