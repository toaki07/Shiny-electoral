# Shiny elecciones pasadas

## Código elaborado por Toaki HOz CAnabal 


## Setup -------
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica

## Cargamos paquetes -----
if(!require('pacman'))  install.packages('pacman')
p_load(bslib, DT, ggplot2, gridlayout, leaflet,sf, shiny, tidyverse)


## Cargamos datos -------

## Cargamos votos de alcaldias 2021, link: https://www.iecm.mx/www/estadisticaresultadospelo2021/consultas/resultados.php?mod=3
alcaldias_2021 <- readxl::read_excel("01_datos/Alcaldias/alcaldias_2021.xls") %>% 
  janitor::clean_names() %>%
  Hmisc::cleanup.import()

## CArgamos secciones

secciones <- sf::st_read("01_datos/shp_2021/SECCION.dbf")


### Convertimos datos ------
alcaldias_2021 <- alcaldias_2021 %>%
  select(demarcacion_territorial, seccion, casilla:pt_morena, vn:lista_nominal) %>%
  group_by(seccion) %>%
  mutate(across(c(pan:lista_nominal), \(x) sum(x, na.rm = TRUE))) %>%
  # ungroup() %>%
  select(-casilla) %>% 
  distinct_all() %>%  # o distinct(seccion, .keep_all = TRUE)
  mutate(frente = sum(c_across(contains(c("pan", "pri", "prd"))), 
                      na.rm = TRUE),
         juntos = sum(c_across(contains(c("morena", "pvem", "pt"))), 
                      na.rm = TRUE),
         `dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total) %>% 
  select(demarcacion_territorial:fxm, vn:last_col()) %>% 
  mutate(demarcacion_territorial = case_when(demarcacion_territorial == "MAGDALENA CONTRERAS" ~ "LA MAGDALENA CONTRERAS",
                                             demarcacion_territorial == "CUAJIMALPA" ~ "CUAJIMALPA DE MORELOS",
                                             TRUE ~ demarcacion_territorial)) %>% 
  pivot_longer(cols = c(pan:vn), names_to = "partido", values_to = "votos") %>% 
  mutate(absoluto_ganador = max(votos),
         ganador = partido[which.max(votos)],
         segundo_absoluto = sort(votos, decreasing = TRUE)[2],
         segundo = partido[order(-votos)[2]]
  ) %>%
  pivot_wider(names_from = partido, values_from = votos) %>% 
  mutate(color = case_match(ganador,
                            "morena" ~ "#b62118",
                            "pan" ~ "blue",
                            "pri" ~ "green",
                            "prd" ~ "yellow",
                            "fxm" ~ "pink")) %>% 
  mutate(`Dif_G-P` = absoluto_ganador-segundo_absoluto,
         porcentaje = (`Dif_G-P`/lista_nominal)*100
  ) %>% 
  select(demarcacion_territorial,votacion_total, lista_nominal, seccion, pan, 
         morena,pri, prd, fxm, frente, juntos, `dif_J-F`, `dif_M-PAN`, 
         absoluto_ganador, ganador, segundo_absoluto, segundo, `Dif_G-P`, porcentaje, color)
  
## Creamos sf objetos -----
secciones <- # cdmx %>% st_join(secciones) %>%
  secciones %>% filter(#municipio <=17,
    entidad == 9)

# Unimos secciones y alcaldias
alcaldias_2021 <- secciones %>%
  right_join(alcaldias_2021, by = c("seccion")) %>% 
  select(demarcacion_territorial,votacion_total, lista_nominal, seccion, pan, 
         morena,pri, prd, fxm, frente, juntos, `dif_J-F`, `dif_M-PAN`, 
         absoluto_ganador, ganador, segundo_absoluto, segundo, `Dif_G-P`, porcentaje, color)

## Creamos Shiny ---------

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
        inputId = "Ganador",
        label = "¿Quién ganó en 2021?",
        choices = c("-", "MORENA", "PAN", "PRI", "PRD", "FxM") 
      ),
      selectInput(
        inputId = "Seleccionador",
        label = "Selecciona",
        choices = c("-", unique(alcaldias_2021$seccion))
      ),
      actionButton("reset", "Resetear filtro")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Visualizador de información por sección 2021",
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
    if (input$Ganador == "-") {
      # Si se selecciona "-", no se aplica ningún filtro
      filtered_data <- alcaldias_2021
    } else {
      # Filtrar datos según la selección del usuario
      filtered_data <- switch(input$Ganador,
                              "MORENA" = alcaldias_2021 %>% filter(ganador == "morena"),
                              "PAN" = alcaldias_2021 %>% filter(ganador == "pan"),
                              "PRI" = alcaldias_2021 %>% filter(ganador == "pri"),
                              "PRD" = alcaldias_2021 %>% filter(ganador == "prd"),
                              "FxM" = alcaldias_2021 %>% filter(ganador == "fxm"),
                              NULL)  # Manejar casos no especificados
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
      mutate(seccion = as.integer(seccion))
    
    p <- p %>% 
      sf::st_transform( "+init=epsg:4326")
    
    
    # Leyenda tooltip
    labels_votos <- sprintf(
      "<strong>Delegación: %s</strong><br/> Lista nominal: %s <br> Primer lugar: %s<br> Votos del ganador: %s<br> Diferencia con respecto al padrón: %s<br> Segundo lugar: %s",
      p$demarcacion_territorial, scales::comma(p$lista_nominal),p$ganador , scales::comma(p$absoluto_ganador), paste0(round(p$porcentaje, digits = 2), "%"), p$segundo
    ) %>% 
      lapply(htmltools::HTML)
    
    # Crear el mapa con Leaflet
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = p,
                  fillColor = ~color,
                  fillOpacity = 0.7,  # Aquí puedes ajustar el nivel de transparencia (0 es completamente transparente, 1 es completamente opaco)
                  color = "black",
                  weight = 1,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 3),
                  label = ~ labels_votos,
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
        reactive_data(), 
        options = list(scrollY = '250px'))
    })
  })
  
  
}

shinyApp(ui, server)
