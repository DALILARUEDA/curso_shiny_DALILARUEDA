# curso_shiny_DALILARUEDA
#install.packages("shiny")
#install.packages("DT")
#library(DT)
#library(shiny)
#iris
#datos <-iris

ui <- fluidPage(
  fluidRow(
    column(12,
           h1("Título de la aplicación"),
           align= "center" 
    )
  ),
  hr(),
  fluidRow(
    column(width = 3,
           
           h2("Filtros"),
           selectInput(
             inputId = "selectSpecie",
             label = "Seleccionar especie:",
             choices = unique(datos$Species),
             selected = "setosa"
           ),
           
           align="center"),
    column(width = 6,
           h2("Título 2 de la col 2"),
           align="center")
  )
)

server <- function(input, output, session) {
  
  output$tabla = DT::renderDataTable({
    especieSeleccionada = input$selectSpecie
    
    datos = datos[datos$Species == especieSeleccionada,]
    
    DT::datatable(datos)
    
  })

shinyApp(ui, server)
