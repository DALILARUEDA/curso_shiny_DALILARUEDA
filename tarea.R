
##ESTRUCTURA##
niveles_prov <- unique(bd$prov)

# Defino UI para mi aplicación
ui <- fluidPage(
  # Titulo
  titlePanel("Mortalidad infantil en Argentina")
  selectInput(
    inputId = "niveles_prov",
    label = "Seleccionar provincia:",
    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_title(text = "Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021") %>%
      hc_xAxis(title = list(text = "Año")) %>%
      hc_yAxis(title = list(text = "TMI ")) %>%
      hc_exporting(enabled = TRUE) # enable exporting option  
    # Agrega una serie de datos para cada nivel de "prov"
    for (nivel in niveles_prov) {
      data_serie <- bd[bd$prov == nivel,]
      hc <- hc %>%
        hc_add_series(
          data_serie,
          "line",
          hcaes(x = ano, y = TMI),
          name = nivel,
          marker = list(radius = 4)
        )
    }
    
    ## imprimo el grafico
    #print(hc)
    hc
    
  )
  
  )


#highchartOutput() ## para el UI


# Defino server
server <- function(input, output,session) {
  renderHighchart(expr, env = parent.frame(), quoted = FALSE)
  
  renderHighchart() ## para el server  
  
}

selectInput(
  inputId = "niveles_prov",
  label = "Seleccionar provincia:",
  hc <- renderHighchart() %>%
    hc_chart(type = "line") %>%
    hc_title(text = "Serie de tiempo de TMI por 1000 nacidos vivos, Argentina, periodo 1990-2021") %>%
    hc_xAxis(title = list(text = "Año")) %>%
    hc_yAxis(title = list(text = "TMI ")) %>%
    hc_exporting(enabled = TRUE) # enable exporting option  
  # Agrega una serie de datos para cada nivel de "prov"
  for (nivel in niveles_prov) {
    data_serie <- bd[bd$prov == nivel,]
    hc <- hc %>%
      hc_add_series(
        data_serie,
        "line",
        hcaes(x = ano, y = TMI),
        name = nivel,
        marker = list(radius = 4)
      ),
    
  }
  
  ## imprimo el grafico
  #print(hc)
  hc

shinyApp(ui, server)
# Corro la application
shinyApp(ui = ui, server = server)