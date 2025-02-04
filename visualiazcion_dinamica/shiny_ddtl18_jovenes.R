## Shiny de prueba para ver mapa y tabla

## Cargamos paquetes ------------
if(!require("pacman")) install.packages("pacman")

p_load(bslib, DT, ggplot2, gridlayout, leaflet,sf, shiny, tidyverse)

# library(shiny)
# library(ggplot2)
# library(gridlayout)
# library(bslib)
# library(DT)
# library(leaflet) # Asegúrate de tener la biblioteca leaflet cargada
# library(tidyverse)


# caragamos datos ------
# dbf de secciones: información obtenida de DataMexico
secciones <- sf::st_read("../shp_2021/SECCION.dbf")

## Filtramos y creamos solo secicones de cdmx
secciones_cdmx <- # cdmx %>% st_join(secciones) %>%
  secciones %>% 
  filter(#municipio <=17,
    entidad == 9) %>% 
  select(c(-distrito_l))

## Cargamos distrito local 18
# distrito_local_18 <- readr::read_csv("C:\\Users\\LENOVO\\Documents\\emprendimientos\\Consultorías políticas\\Cerberus\\Mapeos\\01_datos\\distrito_local_18.csv")

distrito_local_18 <- readr::read_csv("01_datos\\distrito_local_18.csv")

# comida <- readr::read_csv("C:\\Users\\LENOVO\\Documents\\emprendimientos\\Consultorías políticas\\Cerberus\\Mapeos\\01_datos\\comida.csv")

comida <- readr::read_csv("01_datos\\comida.csv")

distrito_local_18 <- distrito_local_18 %>% 
  left_join(comida, by =c("seccion" = "secc")) %>% 
  filter(!is.na(entidad)) #%>% 
  # mutate(joven = case_match(joven,
  #                           "no" ~ "No",
  #                           "yes" ~ "Sí"))

unique(distrito_local_18$joven)

## Options shiny ----
# Reload the Shiny app when a sourced R file changes
devmode(devmode = T)
register_devmode_option(
  "shiny.autoreload",
  "Turning on shiny autoreload. To disable, call `options(shiny.autoreload = FALSE)`",
  devmode_default = TRUE
)
options(shiny.minified = TRUE)
options(shiny.autoreload = TRUE)
# options(shiny.reactlog = TRUE) # PAra ver el esquema reactivo con ctrl + F3


## Shiny -----


## Pansar qué UI usaré y cómo modificar las UI
# ui <- fluidPage(
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         inputId = "Joven",
#         label = "¿Es joven al momento?",
#         choices = c("-", "Sí", "No") 
#       ),
#       selectInput(
#         inputId = "Seleccionador",
#         label = "Selecciona",
#         choices = c("-", unique(distrito_local_18$seccion))
#       )
#     ),
#     mainPanel(
#       leafletOutput("mapa"),
#       DTOutput("tabla")
#     )
#   )
# )

ui <- grid_page(
  layout = c(
    "header  header   ",
    "sidebar mapa", 
    "area    area     "
  ),
  row_sizes = c(
    "15px",
    "1fr",
    "1fr"
  ),
  col_sizes = c(
    "205px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Elecciones de información"),
    card_body(
      selectInput(
        inputId = "Joven",
        label = "¿Es joven al momento?",
        choices = c("-", "Sí", "No") 
      ),
      selectInput(
        inputId = "Seleccionador",
        label = "Selecciona",
        choices = c("-", unique(distrito_local_18$seccion))
      ),
      actionButton("reset", "Resetear filtro")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Visualizador de información de militancia",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(area = "mapa", 
                 card_body(leafletOutput(outputId = "mapa"))
                 ), # Cambia "linePlots" a "mapa"
  grid_card(
    area = "area",
    card_body(DTOutput(outputId = "tabla", width = "100%"))
  )
)

server <- function(input, output) {
  
  reactive_data <- reactive({
    if (input$Joven == "-") {
      # Si se selecciona "-", no se aplica ningún filtro
      filtered_data <- distrito_local_18
    } else {
      # Filtrar datos según la selección del usuario
      if (input$Joven == "Sí") {
        filtered_data <- distrito_local_18 %>% 
          filter(joven == "yes")
      } else {
        filtered_data <- distrito_local_18 %>% 
          filter(joven == "no")
      }
    }
    
    # Filtrar datos según la selección del usuario en "Seleccionador"
    if (input$Seleccionador != "-") {
      filtered_data <- filtered_data %>% filter(seccion == input$Seleccionador)
    }
    
    return(filtered_data)
  })
  
  
  output$mapa <- renderLeaflet({
    
    # # Para actualizar el mapa cuando se selecciona una fila en la tabla
    # q <- secciones_cdmx %>%
    #   right_join(distrito_local_18, by = c("seccion")) %>%
    #   sf::st_transform( "+init=epsg:4326")
    
    p <- reactive_data() %>% 
      count(nombre_alcaldia, seccion) %>% 
      rename("jovenes" = n) %>% 
      tibble() %>% 
      mutate(seccion = as.integer(seccion),
             distrito_local = 18)
    
    p <- secciones_cdmx %>%  
      right_join(p) %>% 
      mutate(color = case_when(
        jovenes <= 5 ~ "black",
        jovenes %in% 6:10 ~ "gray",
        jovenes %in% 11:15 ~ "darkgray",
        jovenes %in% 16:20 ~ "#93003B",
        jovenes %in% 21:25 ~ "darkred",
        TRUE ~ "gray"  
      )) %>% 
      sf::st_transform( "+init=epsg:4326")
    
    # Crear el mapa con Leaflet
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     minZoom = 14, maxZoom = 20)) %>%
      addTiles() %>%
      addPolygons(data = p,
                  fillColor = ~color,
                  color = "black",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3),
                  label = ~jovenes,
                  labelOptions = labelOptions(noHide = FALSE,
                                              style = list("font-weight" = "bold")),
                  stroke = TRUE,
                  layerId = ~seccion)
    
  })
  
  
  # Renderizar la tabla DataTable
  output$tabla <- renderDT({
    datatable(
      data = reactive_data(),
      options = list(scrollY = '250px'),
      selection = list(mode = "single", target = "row")
    ) 
  })
  
  
  # Observar el clic en el mapa Leaflet
  observeEvent(input$mapa_shape_click, {
    seccion_seleccionada <- input$mapa_shape_click$id
    if (!is.null(seccion_seleccionada)) {
      showModal(modalDialog(
        title = "Sección Seleccionada",
        paste("Has seleccionado la sección:", seccion_seleccionada)
      ))
    }
    # Filtrar la tabla basada en la ciudad seleccionada en el mapa
    output$tabla <- renderDT({
      datatable(
        reactive_data()[reactive_data()$seccion == seccion_seleccionada, ], #Como datable
        options = list(scrollY = '250px')
      )
    })
  })
  
  
  # Botón para resetear el filtro
  observeEvent(input$reset, {
    output$tabla <- renderDT({
      datatable(
        reactive_data(), options = list(scrollY = '250px'))
    })
  })
  
  
  # RElacion click tabla-mapa
  observeEvent(input$tabla_rows_selected, {
    selected_row <- input$tabla_rows_selected
    if (length(selected_row) == 0) {
      return(NULL)
    } else {
      # Obtener el polígono seleccionado
      seccion_selected <- reactive_data()[selected_row, ]$seccion
      
      # Actualizar el mapa
      p <- reactive_data() %>% 
        count(nombre_alcaldia, seccion) %>% 
        rename("jovenes" = n) %>% 
        tibble() %>% 
        mutate(seccion = as.integer(seccion),
               distrito_local = 18)
      
      p <- secciones_cdmx %>%  
        right_join(p) %>% 
        mutate(color = case_when(
          jovenes <= 5 ~ "black",
          jovenes %in% 6:10 ~ "gray",
          jovenes %in% 11:15 ~ "darkgray",
          jovenes %in% 16:20 ~ "#93003B",
          jovenes %in% 21:25 ~ "darkred",
          TRUE ~ "gray"  
        )) %>% 
        sf::st_transform( "+init=epsg:4326")
      
      # Centrar el mapa en la zona seleccionada
      # coords <- secciones_cdmx[secciones_cdmx$seccion == seccion_selected, ] %>%
      #   st_centroid() %>%
      #   st_coordinates()
      
      leafletProxy("mapa") %>%
        clearShapes() %>%
        addPolygons(data = p[p$seccion == seccion_selected,],
                    fillColor = ~color,
                    color = "black",
                    weight = 1,
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 3),
                    label = ~jovenes,
                    labelOptions = labelOptions(noHide = FALSE,
                                                style = list("font-weight" = "bold")),
                    stroke = TRUE,
                    layerId = ~seccion) #%>%
        # setView(lng = coords[, 1], lat = coords[, 2], zoom = 15)
    }
  })
  
  # Mostrar información sobre la fila seleccionada
  # output$selected_row <- renderText({
  #   selected_row <- input$tabla_rows_selected
  #   if (length(selected_row) == 0) {
  #     return("Selecciona una fila para ver detalles.")
  #   } else {
  #     paste("Fila seleccionada:", selected_row)
  #   }
  # })
  
  
}
  
  
  


shinyApp(ui, server)


# seccion_geom <- p[p$seccion == 4472,]
# leaflet() %>%
#  addTiles() %>%
#   addPolygons(data = seccion_geom)


