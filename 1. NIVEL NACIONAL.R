
library(shiny)
library(leaflet)
library(writexl)
library(haven)
library(sf)
library(rgdal)
library(sp)
library(htmlwidgets)
library(dplyr)
library(RColorBrewer)
library(leaflet.extras)
library(tidyr)
library(gt)
library(ggplot2)
library(gridExtra)
library(gghighlight)
library(shinyWidgets)
library(shinydashboard)
library(plotly)
valid_codes <- c("1","2","3","4","5","6","7","8","9","10","11","14_1","14_2","14_3","31")
valid_codes2 <- c("12","13","15","16","17","18","19","20","21","22_1","22_2","22_3","22_4","22_5","22_6","22_7","23","24","25","26","27","28_1","28_2","28_3","28_4","28_5","28_6","28_7","28_8","28_9","28_10","29_1","29_2","29_3","29_4","29_5","29_6","29_7","29_8","30","32")

datos_nacionales <-readxl::read_excel("C:/Users/Lenovo/Desktop/z. Prueba para sistemas/NN-DATOS/Datos_NACIONALES.xlsx")
datos_nacionales<- datos_nacionales %>%
  filter(anio >= 2017 & anio <= 2021)
datos_provinciales <-readxl::read_excel("C:/Users/Lenovo/Desktop/z. Prueba para sistemas/NN-DATOS/Datos_PROVINCIALES.xlsx")
datos_provinciales <- datos_provinciales %>%
  mutate(provincia_nombre = ifelse(provincia_nombre == "Ciudad Autónoma de Buenos Aires", "CABA", provincia_nombre))
datos_provinciales <- datos_provinciales %>%
  mutate(provincia_nombre = ifelse(provincia_nombre== "Tierra del Fuego, Antártida e Islas del Atlántico Sur", "Tierra del Fuego", provincia_nombre))
geo_provincias <- read_sf("C:/Users/Lenovo/Desktop/z. Prueba para sistemas/NN-DATOS/SHP Argentina Prov-Dpto/Provincias.shp")%>% filter(!(ID %in% c(100)))
geo_departamentos <- read_sf("C:/Users/Lenovo/Desktop/z. Prueba para sistemas/NN-DATOS/SHP Argentina Prov-Dpto/Dpto segun SNIC.shp")
codificacion_snic<- readxl::read_excel("C:/Users/Lenovo/Desktop/z. Prueba para sistemas/NN-DATOS/codificacion_snic.xlsx")

ui <- fluidPage(
  tags$div(
    style = "background-color: #00CCFF; color: #333535; padding: 11px; font-size: 24px; font-weight: bold; text-align: left; font-family: Encode Sans;",
    " TABLERO SNIC",
    tags$a(href = "https://www.argentina.gob.ar/seguridad/estadisticascriminales", target = "_blank",
           tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Minseguridadarg.png/250px-Minseguridadarg.png",width = "140px", height = "47px",style = "float:right; margin-top: -0.15cm;")
    )),
  tabsetPanel(
    # Primera pestaña
    tabPanel("NIVEL NACIONAL",
             fluidRow(
               column(width = 2, selectInput("año", "Seleccione el año", choices = c(2021, 2020, 2019, 2018, 2017)),
                      tags$head(tags$style(HTML(".selectize-input {height: 30px; width: 200px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
               column(width = 2, selectInput("codigo", "Seleccione el código", choices = codificacion_snic$Delito),
                      tags$head(tags$style(HTML(".selectize-input {height: 30px; width: 200px; font-size: 14px;} .selectize-dropdown {z-index: 9999;}")))),
               
               tags$head(
                 tags$style(
                   HTML(".mi-caja { margin-left: 3cm; margin-top: -0.5cm; background-color: #00CCFF; width: 150px; height: 70px; text-align: center; border-radius: 10px;}")
                 )
               ),
               column(width = 2, style = "padding-right: 1cm;", box(
                 title = "",
                 class = "mi-caja",
                 div(
                   style = "display: flex; flex-direction: column; align-items: center; font-size: 19px; font-family: Encode Sans;",
                   div(style = "margin-top: 1.5mm;","Total Nacional"),
                   div(style = "display: flex; justify-content: center; align-items: center;", textOutput(outputId = "total1"))
                 )
               )),
               column(width = 2, style = "padding-left: 1cm;", box(
                 title = "",
                 class = "mi-caja",
                 div(
                   style = "display: flex; flex-direction: column; align-items: center; font-size: 19px; font-family: Encode Sans;",
                   div(style = "margin-top: 1.5mm;","Tasa Nacional"),
                   div(style = "display: flex; justify-content: center; align-items: center;", textOutput(outputId = "tasa1"))
                 )
               )),
               column(width = 2, style = "padding-left: 1cm;", box(
                 title = "",
                 class = "mi-caja",
                 div(
                   style = "display: flex; flex-direction: column; align-items: center; font-size: 19px; font-family: Encode Sans;",
                   div(style = "margin-top: 1.5mm;","Variación inter."),
                   div(style = "display: flex; justify-content: center; align-items: center;", textOutput(outputId = "variacion1"))
                 )
               ))),
             
             
             fluidRow(
               column(width = 4, leafletOutput(outputId ="mapa", height = "500px")),
               column(width = 4, plotlyOutput(outputId ="grafico", height = "500px")),
               column(width= 4, plotlyOutput(outputId ="grafico2", height = "500px")))
    ),
    tabPanel("NIVEL PROVINCIAL"),
    tabPanel("NIVEL DEPARTAMENTAL")),
  tags$div(
    style = "background-color: #00CCFF; color: #333535; padding: 6px; font-size: 12px; font-weight: bold; text-align: left; font-family: Encode Sans; margin-top: 0.3cm;",
    "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC -SAT), Ministerio de Seguridad de la Nación e INDEC. ")
)

server <- function(input, output, session) {
  output$mapa <- renderLeaflet({
    tasa_delito1 <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    #join de la tabla "geo_provincias 2" y "tasa_delito1" en donde los codigos del INDEC coinciden
    shape_tasa <- left_join(geo_provincias, tasa_delito1, by = c("ID" = "provincia_id"))
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      shape_tasa_filtrado <- subset(shape_tasa, tasa_victimas > 0 & pob_tot>=50000)
      n_filas <- nrow(shape_tasa_filtrado)
      
      if (n_filas == 0) {
        colors <- c('white', 'grey')
        levels <- c('no data', 'population =< 50000')
        labels <- factor((c('no data', 'population =< 50000')), levels)
        shape_tasa$color <- "white"
        shape_tasa$color[shape_tasa$tasa_victimas > 0 & shape_tasa$pob_tot < 50000] <- "grey"
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
          addPolygons(data = shape_tasa, 
                      color = ~color,
                      weight = 1,
                      fillOpacity= 1)%>%
          addPolygons(data = geo_provincias, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5, 
                      popup= paste ("<div style='height:45px'>Jurisdicción:", shape_tasa$provincia_nombre,"<br>Víctimas:",format(shape_tasa$victimas, big.mark = "."),"<br>Tasa:",format(round(shape_tasa$tasa_victimas, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),"</div>"))
        #%>%
        #addLegend("bottomright", colors=colors, labels=labels, title= "coloresv", opacity = 10)
        
      } else {
        shape_tasa_filtrado$cuartil <- cut(shape_tasa_filtrado$tasa_victimas, include.lowest=TRUE, breaks = 4) 
        pal <- colorFactor("Blues", shape_tasa_filtrado$cuartil)
        colors <- c(pal(unique(sort(shape_tasa_filtrado$cuartil))), 'white', 'grey')
        levels <- c(levels(shape_tasa_filtrado$cuartil), 'no data', 'population =< 50000')
        labels <- factor(append(as.character(unique(sort(shape_tasa_filtrado$cuartil))), c('no data', 'population =< 50000')), levels)
        
        shape_tasa$color [shape_tasa$tasa_victimas==0]<- "white"
        shape_tasa$color[shape_tasa$tasa_victimas > 0 & shape_tasa$pob_tot >= 50000] <-pal(shape_tasa_filtrado$cuartil)
        shape_tasa$color[shape_tasa$tasa_victimas > 0 & shape_tasa$pob_tot < 50000] <- "grey"
        
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
          addPolygons(data = shape_tasa, 
                      color = ~color,
                      weight = 1,
                      fillOpacity= 1)%>%
          addPolygons(data = geo_provincias, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5, 
                      popup= paste ("<div style='height:45px'>Jurisdicción:", shape_tasa$provincia_nombre,"<br>Víctimas:",format(shape_tasa$victimas, big.mark = "."),"<br>Tasa:",format(round(shape_tasa$tasa_victimas, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),"</div>"))
        #%>%
        #addLegend("bottomright", colors=colors, labels=labels, title= "coloresv", opacity = 10)
        
      }
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      shape_tasa_filtrado <- subset(shape_tasa, tasa_hechos > 0 & pob_tot>=50000)
      n_filas <- nrow(shape_tasa_filtrado) 
      if (n_filas == 0) {
        colors <- c('white', 'grey')
        levels <- c('no data', 'population =< 50000')
        labels <- factor((c('no data', 'population =< 50000')), levels)
        shape_tasa$color <- "white"
        shape_tasa$color[shape_tasa$tasa_hechos > 0 & shape_tasa$pob_tot < 50000] <- "grey"
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
          addPolygons(data = shape_tasa, 
                      color = ~color,
                      weight = 1,
                      fillOpacity= 1)%>%
          addPolygons(data = geo_provincias, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5, 
                      popup= paste ("<div style='height:45px'>Jurisdicción:", shape_tasa$provincia_nombre,"<br>Hechos:",format(shape_tasa$hechos, big.mark = "."),"<br>Tasa:",format(round(shape_tasa$tasa_hechos, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),"</div>"))
        #%>%
        #addLegend("bottomright", colors=colors, labels=labels, title= "coloresh", opacity = 10)
        
      } else {
        shape_tasa_filtrado$cuartil <- cut(shape_tasa_filtrado$tasa_hechos, include.lowest=TRUE, breaks = 4) 
        pal <- colorFactor("Blues", shape_tasa_filtrado$cuartil)
        colors <- c(pal(unique(sort(shape_tasa_filtrado$cuartil))), 'white', 'grey')
        levels <- c(levels(shape_tasa_filtrado$cuartil), 'no data', 'population =< 50000')
        labels <- factor(append(as.character(unique(sort(shape_tasa_filtrado$cuartil))), c('no data', 'population =< 50000')), levels)
        
        shape_tasa$color [shape_tasa$tasa_hechos==0]<- "white"
        shape_tasa$color[shape_tasa$tasa_hechos > 0 & shape_tasa$pob_tot >= 50000] <-pal(shape_tasa_filtrado$cuartil)
        shape_tasa$color[shape_tasa$tasa_hechos > 0 & shape_tasa$pob_tot < 50000] <- "grey"
        
        leaflet() %>%
          addTiles(urlTemplate = "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/mapabase_gris@EPSG%3A3857@png/{z}/{x}/{-y}.png") %>%
          addPolygons(data = shape_tasa, 
                      color = ~color,
                      weight = 1,
                      fillOpacity= 1)%>%
          addPolygons(data = geo_provincias, weight = 1, color = "black", fillColor="transparent", fillOpacity= 0.5,     
                      popup= paste ("<div style='height:45px'>Jurisdicción:", shape_tasa$provincia_nombre,"<br>Hechos:",format(shape_tasa$hechos, big.mark = "."),"<br>Tasa:",format(round(shape_tasa$tasa_hechos, 1), nsmall = 1, decimal.mark = ",", big.mark = "."),"</div>"))
        #%>%
        #addLegend("bottomright", colors=colors, labels=labels, title= "coloresh", opacity = 10)
        
      }
    } else {print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  output$grafico <- renderPlotly({
    colores <- c("#00CCFF")
    df_filtrado <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y añ
      plot_ly(
        df_filtrado, 
        x = df_filtrado$victimas, 
        y = reorder(df_filtrado$provincia_nombre, df_filtrado$victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de víctimas por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Víctimas", 
            titlefont = list(color = "grey"), 
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 11), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 120) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = df_filtrado$victimas,
          y = reorder(df_filtrado$provincia_nombre, df_filtrado$victimas),
          text = format(df_filtrado$victimas, big.mark="."),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xshift = 23# Ajustar la posición del texto dentro de las barras
        )
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {  
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y año
      plot_ly(
        df_filtrado, 
        x = df_filtrado$hechos, 
        y = reorder(df_filtrado$provincia_nombre, df_filtrado$hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Total de hechos por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Hechos",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "", tickfont = list(size = 11), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 120) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = df_filtrado$hechos,
          y = reorder(df_filtrado$provincia_nombre, df_filtrado$hechos),
          text = format(df_filtrado$hechos, big.mark="."),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xshift = 23# Ajustar la posición del texto dentro de las barras
        )  
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  output$grafico2 <- renderPlotly({
    colores <- c("#00CCFF")
    df_filtrado <- subset(datos_provinciales, codigo %in% codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] & anio %in% c(input$año))
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      #Filtrar y agrupar por tipo de codigo y año. Sumar los hechos por codigo y añ
      df_filtrado$tasa_victimas <- round(df_filtrado$tasa_victimas, 1)
      plot_ly(
        df_filtrado, 
        x = df_filtrado$tasa_victimas, 
        y = reorder(df_filtrado$provincia_nombre, df_filtrado$tasa_victimas),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de víctimas por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 habitantes",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "",
                       tickfont = list(size = 11), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 120) # Ajustar el margen izquierdo para dar espacio al texto dentro de las barras
        ) %>% 
        add_annotations(
          x = df_filtrado$tasa_victimas,
          y = reorder(df_filtrado$provincia_nombre, df_filtrado$tasa_victimas),
          text = format(df_filtrado$tasa_victimas, big.mark = ".", decimal.mark = ",") ,
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xshift = 23 # Ajustar la posición del texto dentro de las barras
        )
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {  
      df_filtrado$tasa_hechos <- round(df_filtrado$tasa_hechos, 1)
      plot_ly(
        df_filtrado, 
        x = df_filtrado$tasa_hechos, 
        y = reorder(df_filtrado$provincia_nombre, df_filtrado$tasa_hechos),
        type = "bar", 
        orientation = "h", 
        marker = list(color = colores, textposition = "outside")
      ) %>% 
        layout(
          title = list(text = "Tasa de hechos por jurisdicción", font = list(color = "grey")),
          xaxis = list(
            title = "Tasa cada 100.000 habitantes",
            titlefont = list(color = "grey"),
            showgrid = FALSE,
            tickmode = "array",
            tickvals = NULL,
            showticklabels = FALSE
          ),
          yaxis = list(title = "",tickfont = list(size = 11), showgrid = FALSE),
          showlegend = FALSE,
          margin = list(l = 120) # Ajustar el margen inferior para evitar que se muestren los números del eje x
        ) %>% 
        add_annotations(
          x = df_filtrado$tasa_hechos,
          y = reorder(df_filtrado$provincia_nombre, df_filtrado$tasa_hechos),
          text = format(df_filtrado$tasa_hechos, big.mark = ".", decimal.mark = ","),
          font = list(size = 12, color = "grey"), # Ajustar el tamaño de la fuente dentro de las barras
          showarrow = FALSE,
          xshift = 23# Ajustar la posición del texto dentro de las barras
        )
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  output$total1 <- renderText({
    df_filtrado1 <- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      format(df_filtrado1$cantidad_victimas, big.mark = ".", decimal.mark = ",")
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      format(df_filtrado1$cantidad_hechos, big.mark = ".", decimal.mark = ",")
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }
  })
  
  
  output$tasa1 <- renderText({
    df_filtrado <- subset(datos_nacionales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
    df_filtrado$tasa_hechos <- round(df_filtrado$tasa_hechos, 1)  
    df_filtrado$tasa_victi<- round(df_filtrado$tasa_victi, 1)
    
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      format(df_filtrado$tasa_victi, big.mark = ".", decimal.mark = ",")
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) {
      format(df_filtrado$tasa_hechos, big.mark = ".", decimal.mark = ",")
      
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }   
  })
  
  
  output$variacion1 <- renderText({
    if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes) {
      datos_provinciales <- datos_nacionales %>% 
        arrange(anio) %>%
        group_by(codigo_delito_snic_id) %>%
        mutate(variacion_anual_vic = paste0(round(((tasa_victi/lag(tasa_victi)-1)*100), digits = 1)))
      datos_provinciales$variacion_anual_vic <- ifelse(datos_provinciales$variacion_anual_vic == "Inf" | datos_provinciales$variacion_anual_vic == "NA", "-", datos_provinciales$variacion_anual_vic)
      variacion <- subset(datos_provinciales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
      
      variacion$variacion_anual_vic <- paste0(format(as.numeric(variacion$variacion_anual_vic), nsmall = 1, decimal.mark = ",", big.mark = "."), "%")
      variacion$variacion_anual_vic <- ifelse(variacion$variacion_anual_vic == "Inf%" | variacion$variacion_anual_vic == "NA%", "-", variacion$variacion_anual_vic)
      print(variacion$variacion_anual_vic)
      
    } else if (codificacion_snic$codigo[codificacion_snic$Delito == input$codigo] %in% valid_codes2) { 
      datos_provinciales <- datos_nacionales %>% 
        arrange(anio) %>%
        group_by(codigo_delito_snic_id) %>%
        mutate(variacion_anual_vic = paste0(round(((tasa_hechos/lag(tasa_hechos)-1)*100), digits = 1)))
      datos_provinciales$variacion_anual_vic <- ifelse(datos_provinciales$variacion_anual_vic == "Inf" | datos_provinciales$variacion_anual_vic == "NA", "-", datos_provinciales$variacion_anual_vic)
      variacion <- subset(datos_provinciales, codigo_delito_snic_id %in% c(codificacion_snic$codigo[codificacion_snic$Delito == input$codigo]) & anio %in% c(input$año))
      variacion$variacion_anual_vic <- paste0(format(as.numeric(variacion$variacion_anual_vic), nsmall = 1, decimal.mark = ",", big.mark = "."), "%")
      variacion$variacion_anual_vic <- ifelse(variacion$variacion_anual_vic == "Inf%" | variacion$variacion_anual_vic == "NA%", "-", variacion$variacion_anual_vic)
      print(variacion$variacion_anual_vic)
    } else {
      print("El valor ingresado no corresponde a un delito.")
    }    
  })
}

shinyApp(ui, server)

