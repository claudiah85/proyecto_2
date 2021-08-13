library(shiny)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(here)
library(readr)
library(tidyr)

  datos <- read_csv(here("datos","corona_estados.csv"))

  fecha_minima <- as.Date("05/01/2020", "%d/%m/%Y")
  fecha_maxima <- as.Date("24/04/2020", "%d/%m/%Y")

  datos_2 <- pivot_longer(datos, cols=4:114, names_to = "Fechas", values_to = "casos")
  datos_2$Fechas <- as.Date(datos_2$Fechas,"%d/%m/%Y")


  ui <- fluidPage(
    # Application title
    titlePanel("coronaMEX"),
    
    # Sidebar for parameters
    sidebarLayout(
      sidebarPanel(
        ## Código para seleccionar paises
        selectizeInput( 'nombre', label = h3("Seleccionar estados"), 
                      choices = levels(as.factor(datos$nombre)), multiple=TRUE),##
        sliderInput( 'ylms', label = h3("Dìas"), min = fecha_minima, 
                   max = fecha_maxima, value = c(fecha_minima, fecha_maxima)),
        
        downloadButton("output$downloadData", "Descargar html")
      
    ),#SidebarPanel
    
      mainPanel(
        plotOutput("lifeExpPlot")
      )
    )
  )#ui

# Define server logic
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    datos_2 %>%
      filter( nombre %in% input$nombre ) %>%
      ggplot( aes( x= Fechas ,y = casos, group = nombre, color = nombre)) +
      geom_point( size=0.3 ) +
      geom_line( )+
      title("Visualización de casos confirmados de Covid-19 por estado en México")+
      labs(color = "Estado(s):")+
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2]))
  })
  
}#server

# Run the application 
shinyApp(ui = ui, server = server)
##grafica
