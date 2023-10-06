
library(shiny)
library(tidyverse)
library(tidyr)
library(lubridate)
library(plotly)
library(shinyWidgets)
library(highcharter)
library(gt)
library(gtExtras)
library(sparkline)
library(svglite)
library(DT)
library(glue)
library(highcharter)
library(bslib)

#descarga del dataset: no funciona xq el archivo esta comprimido
#temp <- tempfile()
#download.file(
#  "http://datos.salud.gob.ar/dataset/c1643775-18e1-40fd-9e7f-0cebb5b1abe6/resource/41691186-72d2-4c3b-a353-7e93fd50a7ed/download/arg_def_15_21.rar",
#  temp
#)
#data <- read.csv(temp)
#unlink(temp)

###LEVANTO DATASET PREVIAMENTE DESCOMPRIMIDO EN PC
library(readr)
datos <- read_csv("arg_def_15_21.csv")
#datos<-arg_def_15_21

############ PROCESO LOS DATOS ##########################
names(datos)
sum(datos$cantidad)

#renombrar variables
#jurisdicciones
table(datos$jurisdiccion)
datos$Jurisdiccion<- factor(datos$jurisdiccion, 
                             labels = c("Cordoba", "Corrientes", "CABA",
                                        "Chaco", "Entre Rios",
                                        "Formosa", "Mendoza", "Misiones",
                                        "Buenos Aires", "Santa Fe", "Tucuman", "Sin dato"))

table(datos$Jurisdiccion)

#sexo
table(datos$sexo_nombre)
datos$Sexo<- factor(datos$sexo_nombre, 
                            labels = c("Varones", "Mujeres", "Sin dato","Sin dato"
                                       ))
table(datos$Sexo)

#grupo de edad (menor 60, 60 y mas)
table(datos$grupo_etario)
datos$Edad <- factor(datos$grupo_etario, 
                    labels = c("Menor de 60 años", "Menor de 60 años", "Menor de 60 años","Menor de 60 años",
                    "60 años y mas","60 años y mas", "60 años y mas", "Sin dato"))
table(datos$Edad)

#agrupo las causas de defuncion (TOTAL DEFUNCIONES)
#agrego la jurisdiccion TOTAL PAIS
datos2<-datos  %>% group_by ( Jurisdiccion,mes_anio_defuncion, mes_def, anio_def, Sexo, Edad )%>% summarise(Total_def =n())
sum(datos2$Total_def)

total2<-datos2 %>% group_by (mes_anio_defuncion, mes_def, anio_def, Sexo, Edad )%>% summarise(Total_def =sum(Total_def))
Jurisdiccion<-rep("Total Argentina",781)
sum(total2$Total_def)

total2<-cbind(Jurisdiccion,total2)
total2<-total2 %>% rename(Jurisdiccion = '...1')
datosj<-rbind(datos2,total2)

#agrego total edad
total3<-datosj %>% group_by (Jurisdiccion, mes_anio_defuncion, mes_def, anio_def, Sexo )%>% summarise(Total_def =sum(Total_def))
Edad<-rep("Total",2711)
sum(total3$Total_def)
total3<-cbind(Edad,total3)
total3<-total3 %>% rename(Edad = '...1')
datose<-rbind(datosj,total3)
table(datose$Edad)
#chequeo<-filter(datose, Edad == "Total" & Jurisdiccion == "Total Argentina")
#sum(chequeo$Total_def)

#agrego total sexo
total4<-datose %>% group_by (Jurisdiccion, mes_anio_defuncion, mes_def, anio_def, Edad )%>% summarise(Total_def =sum(Total_def))
Sexo<-rep("Total",3779)
total4<-cbind(Sexo,total4)
total4<-total4 %>% rename(Sexo = '...1')
datoss<-rbind(datose,total4)
table(datoss$Sexo)
chequeo<-filter(datoss, Edad == "Total" & Jurisdiccion == "Total Argentina" & Sexo == "Total")
sum(chequeo$Total_def)


datosf<-datoss  ####base final a utilizar: "datosf"
rm(chequeo,datos,datos2,datose, datosj, datoss, total2, total3, total4)

##INPUTS
#filtro para seleccionar año (y se pueda seleccionar + de 1)
#filtro para seleccionar jurisdiccion
#filtro para seleccionar edad agrupada (optativo)
#filtro para seleccionar sexo  (optativo)


#OUTPUTS
#tabla con un boton para poder descargar los datos
#grafico de barra



##############  Defino UI para mi aplicación

ui <- fluidPage(useShinyjs(),
  # titulo
  fluidRow(
    column(width = 12,
           h1(strong("Defunciones en Argentina")),
           align = "center"
    )
  ),
  
  hr(),
  
  # primera fila de la ui
  fluidRow(
    column(width = 3,
   ##inputs        
           h2("Filtrar Año"),
           selectInput(
             inputId = "selectAño",
             label = "Seleccionar Año",
             choices = (datosf$anio_def),
             multiple = TRUE,
             selected = "2021"
           ),
   br(),
  
           h2("Filtrar Jurisdicción"),
           selectInput(
             inputId = "selectJuri",
             label = "Seleccionar Jurisdicción",
             choices = unique(datosf$Jurisdiccion),
             selected = "Total Argentina"
           ),
   br(),
    h2("Filtrar grupos de edad"),
           selectInput(
          inputId = "selectEdad",
          label = "Seleccionar Grupos de edad",
          choices = (datosf$Edad),
          multiple = TRUE,
          selected = "Total"
           ),
   br(),
  
           h2("Filtrar Sexo"),
           selectInput(
             inputId = "selectSex",
             label = "Seleccionar Sexo",
             choices = (datosf$Sexo),
             multiple = TRUE,
             selected = "Total"
           ),
           
     downloadButton(
     "descargar",
     "Descargar"
   ),
    ),
   
    column(width = 9),
    highcharter:: highchartOutput("grafico"),
    
   titlePanel(
     "Defunciones en Argentina"
   ),
   DT::DTOutput("Tabla")
  )
)
   
  

################### Defino server

server <- function(input, output, session) {
  
  datosProcesados = reactive({
    AñoSeleccionado = input$selectAño
    datos = datosf[datosf$anio_def == añoSeleccionado,]
    datos
  })
  
 ###OUTPUTS
  #Tabla
  
  output$Tabla = DT::renderDataTable({
    
    datos = datosProcesados()
    DT::datatable(datos, caption = paste0("Año seleccionado: ", input$selectAño))
    
  })
  
  #Grafico
  
  
  output$grafico= renderHighchart({
    año=input$selectAño
    
    niveles_año <- unique(año)
    niveles_mes <- seq(1,12,by=1)
    
    # armo el grafico con highchart
    hc2 <- highchart() %>% 
      hc_chart(type = "column")  %>% 
      hc_title(text = "Defunciones mensuales en Argentina") %>%
      hc_xAxis(categories = niveles_mes,title = list(text = "Mes")) %>%
      hc_yAxis(title = list(text = "Defunciones ")) %>%
      hc_exporting(enabled = TRUE) # enable exporting option
    
    # Agrega una serie de datos para cada nivel de "juri"
    for (nivel in niveles_año) {
      data_serie <- datosProcesados[datosf$Jurisdiccion == nivel,]
      hc2 <- hc2 %>%
        hc_add_series(
          data_serie,
          "column",
          hcaes(x = mes_def, y = Total_def),
          name = nivel,
          marker = list(radius = 4)
        )
      
      # Agrega una serie de datos para cada nivel de "edad"
      for (nivel in niveles_año) {
        data_serie <- datosProcesados[datosf$Edad == nivel,]
        hc2 <- hc2 %>%
          hc_add_series(
            data_serie,
            "column",
            hcaes(x = mes_def, y = Total_def),
            name = nivel,
            marker = list(radius = 4)
          )
        # Agrega una serie de datos para cada nivel de "sexo"
        for (nivel in niveles_año) {
          data_serie <- datosProcesados[datosf$Sexo == nivel,]
          hc2 <- hc2 %>%
            hc_add_series(
              data_serie,
              "column",
              hcaes(x = mes_def, y = Total_def),
              name = nivel,
              marker = list(radius = 4)
            )
      
    }
    print(hc2)
  })
  output$descargar = downloadHandler(
    
    # nombre del archivo a descargar
    
    filename = function() {
      paste('Defunciones', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      
      # procesamiento de los datos para descargar
      datosParaDescargar = datosProcesados
      
      # descarga de los datos
      write.csv(datosParaDescargar, file, row.names = F, na = "")
    }
  )
}
  
  
  ######### Corro la applicacion##############
  shinyApp(ui = ui, server = server)

