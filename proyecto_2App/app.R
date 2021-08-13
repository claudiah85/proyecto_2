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
#View(datos)
#data(datos/corona_estados.csv)
fecha_minima <- as.Date("05/01/2020", "%d/%m/%Y")
fecha_maxima <- as.Date("24/04/2020", "%d/%m/%Y")
#colnames(datos)
#str(datos)
datos_2 <- pivot_longer(datos, cols=4:114, names_to = "Fechas", values_to = "casos")
datos_2$Fechas <- as.Date(datos_2$Fechas,"%d/%m/%Y")
#view(datos_2)

ui <- fluidPage(
  # Application title
  titlePanel("Visualización de casos confirmados de Covid-19 por estado en México"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      
      ## Código para seleccionar paises
      selectizeInput( 'nombre', label = h3("Seleccionar estados"), 
                      choices = levels(as.factor(datos$nombre)), multiple=TRUE),##
      sliderInput( 'ylms', label = h3("Dìas"), min = fecha_minima, 
                   max = fecha_maxima, value = c(fecha_minima, fecha_maxima)),
      
      
       # radioButtons( 'colorBy', label=h3("Colorear por"),
       #               choices = c(estado="nombre"), 
       #               selected = "nombre")
    ),
    mainPanel(
      plotOutput("lifeExpPlot")
    )
  )
)


# Define server logic
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    datos_2 %>%
      filter( nombre %in% input$nombre ) %>%
      ggplot( aes( x= Fechas ,y = casos, group = nombre, color = nombre)) +
      geom_point( size=0.3 ) +
      geom_line( ) +
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2]))
  })
    
  output$plotLegend <- renderText({
    sprintf("Datos de esperanza de vida del año %d al año %d. Cada color
    representa un %s. Los siguientes países estan representados en el gráfico: %s.",
            input$ylms[1], input$ylms[2],
            ifelse(input$colorBy == "continent", "continente", "país"),
            paste(input$countries, collapse=", "))
  })
  
  }#server

# Run the application 
shinyApp(ui = ui, server = server)
##grafica
