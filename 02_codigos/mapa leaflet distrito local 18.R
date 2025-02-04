## Mapeo de CDMX 
# Código elaborado por Toaki Hoz Canabal 


## Cargamos paquetes ------------
pacman::p_load(foreign, htmltools, leaflet, miniUI, readxl, rJava, sf, shiny, tabulapdf, tidyverse)
# remotes::install_github(c("ropensci/tabulizerjars", "ropensci/tabulizer"), INSTALL_opts = "--no-multiarch")
# install.packages("BiocManager")


## Cargamos datos ---------

# dbf de entidades
entidades <- st_read("01_datos/shp_2021/ENTIDAD.dbf")

# dbf de secciones: información obtenida de DataMexico
secciones <- st_read("01_datos/shp_2021/SECCION.dbf")

# Lista de alcaldías y clave/ número de alcaldías
(alcaldias <- tibble(
  claveDT = c(2L,3L,4L,5L,6L,7L,8L,9L,
              10L,11L,12L,13L,14L,15L,16L,17L),
  nombreDT = c("AZCAPOTZALCO","COYOACÁN",
               "CUAJIMALPA DE MORELOS","GUSTAVO A. MADERO","IZTACALCO",
               "IZTAPALAPA","LA MAGDALENA CONTRERAS","MILPA ALTA",
               "ÁLVARO OBREGÓN","TLÁHUAC","TLALPAN","XOCHIMILCO",
               "BENITO JUÁREZ","CUAUHTÉMOC","MIGUEL HIDALGO",
               "VENUSTIANO CARRANZA")) %>%  
  janitor::clean_names() %>% 
  mutate(nombre_dt = iconv(nombre_dt, to = "ASCII//TRANSLIT"))
  )

colonias <- st_read("01_datos/shp_colonias/colonias_iecm.dbf")

## Analizar tablas con {tabulizer} requiere rJava, miniUI, shiny
# Sacamos datos del rango de secciones de Benito Juárez 
# locate_areas("C:\\Users\\LENOVO\\Documents\\emprendimientos\\Consultorías políticas\\Cerberus\\Rango de secciones de alcaldias\\14. Benito Juarez.pdf") # Para enmarcar el área de las tablas y da coordenadas: c(178.27306, 74.63469, 736.47232, 539.31365)

secciones_benito_juarez <- extract_tables("01_datos/Rango de secciones de alcaldias\\14. Benito Juarez.pdf", #
                                          guess = FALSE,
                                          area = list(c(178.27306, 74.63469, 736.47232, 539.31365) )) %>% 
  lapply(function(matriz) as_tibble(matriz[3:nrow(matriz), ]) %>% 
           rename(demarcacion = 1,
                  nombre_alcaldia = 2, 
                  distrito_local = 3,
                  circunscripcion = 4,
                  seccion = 5)) %>%
  bind_rows() %>% 
  # rename(demarcacion = 1,
  #        nombre_alcaldia = 2, 
  #        distrito_local = 3,
  #        circunscripcion = 4,
  #        seccion = 5) %>% 
  mutate(across(c(demarcacion, distrito_local,circunscripcion , seccion), as.numeric))%>% 
  Hmisc::cleanup.import() # funciona para limpiar los datos quitnado double por L # funciona para limpiar los datos quitnado double por L

 # Sacamos datos del rango de secciones de Alvaro obregon dtto 18
# locate_areas("C:\\Users\\LENOVO\\Documents\\emprendimientos\\Consultorías políticas\\Cerberus\\Rango de secciones de alcaldias\\10. Alvaro Obregin dtto 18.pdf") # Para enmarcar el área de las tablas y da coordenadas: c(179.76654, 50.73152, 748.85603, 568.45914 )

secciones_AO_18 <- extract_tables("01_datos/Rango de secciones de alcaldias\\10. Alvaro Obregin dtto 18.pdf", # 
                                  guess = FALSE,
                                  area = list(c(179.76654, 50.73152, 748.85603, 568.45914 ))) %>% 
  lapply(function(matriz) as_tibble(matriz[3:nrow(matriz), ])) %>%
  bind_rows() %>% 
  rename(demarcacion = 1,
         nombre_alcaldia = 2, 
         distrito_local = 3,
         circunscripcion = 4,
         seccion = 5) %>% 
  mutate(across(c(demarcacion, distrito_local,circunscripcion , seccion), as.numeric)) %>% 
  Hmisc::cleanup.import()

## Cargo datos de elecciones 2021
prep_general_2021 <- 
  read_excel("01_datos/Prep_2021\\prep_general_2021.xls", 
             col_types = c("numeric", "numeric", "text", 
                           "numeric", "text", "numeric", "numeric", 
                           "text", "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric"),
             skip= 2) %>% 
  janitor::clean_names() %>% 
  group_by(demarcacion_territorial, seccion) %>% 
  transmute(distrito_local, clave_dem, 
            demarcacion_territorial,
            across(pan:last_col(), sum)) %>% 
  distinct(seccion, .keep_all = T) %>%
  ungroup() %>% 
  Hmisc::cleanup.import()


prep_general_2021_trabajado <- prep_general_2021 %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna
  mutate(frente = sum(c_across(contains(c("pan", "pri", "prd"))), # Puedo prescindir del rowwise porque c_across ya realiza un rowwise internamente. La diferencia entre c_across y across es 1 para filas, 2 para columnas
                      na.rm = TRUE),
         juntos = sum(c_across(contains(c("morena", "pvem", "pt"))), 
                      na.rm = TRUE),
         `dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total) %>% 
  ungroup() %>% ## Para no seguir usando el rowwise
  dplyr::select(1, 3,4, pan, pri, prd, pvem, pt, mc, morena, 
                frente, juntos, `dif_J-F`,`dif_M-PAN`, 
                p_morena, votacion_total, lista_nominal)



### prep por edad 
prep_p_edad_2021 <- read_excel("01_datos/prep_por edad/prep_2021.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(demarcacion_territorial = str_to_upper(demarcacion)) %>% 
  select(ent, demarcacion_territorial, everything(), -demarcacion) %>% 
  pivot_longer(6:last_col(), 
               names_to = "sexo_edad",
               values_to = "total") %>% 
  Hmisc::cleanup.import()


## Filtramos datos ----------

# Mapeamos ciudad de mexico -----
# cdmx <- entidades %>% 
#   janitor::clean_names() %>% 
#   filter(entidad == 9)


# Mapeamos secciones de cdmx -----------
secciones_cdmx <- # cdmx %>% st_join(secciones) %>%
  secciones %>% filter(#municipio <=17,
    entidad == 9)


secciones_cdmx %>% 
  # across("municipio", .funs = as.factor) %>% 
  ggplot() +
  # geom_sf(data = data, aes(fill = entidad)) +
  geom_sf() 


### Mapeamos secciones de BENITO JUAREZ y ALVARO OBREGON --------
secciones_cdmx %>% 
  left_join(alcaldias, by = c("municipio" = "clave_dt")) %>% 
  filter(nombre_dt == "BENITO JUAREZ" | nombre_dt == "ALVARO OBREGON") %>% 
  ggplot()+ 
  geom_sf()


#  Graficamos con leaflet
# Cargar los datos en el formato adecuado
p <- secciones_cdmx %>% 
  left_join(alcaldias, by = c("municipio" = "clave_dt")) %>% 
  filter(nombre_dt == "BENITO JUAREZ" | nombre_dt == "ALVARO OBREGON") %>%
  st_transform( "+init=epsg:4326")

# Crear un objeto leaflet
leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p, fillColor = "#93003B", 
              color = "black",weight = 1)



leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p,
              fillColor = "#93003B",
              color = "black",
              weight = 2,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 3),
              label = ~seccion,
              labelOptions = labelOptions(noHide = F,
                                          style = list("font-weight" = "bold")),
              stroke = T)



### Creamos capa en kml para google-maps  --------
# Carga la librería sf si aún no está cargada
library(sf)

# Supongamos que tus datos están en el objeto "p"
# Convierte el objeto a un data.frame
p_df <- as.data.frame(p)

# Guarda los datos en un archivo KML
st_write(p_df, "mi_mapa.kml", driver = "KML")


### Mapeamos dummy del nuevo distrito local 18 --------- 
# unimos primero las secciones con rbind()

distrito_local_18 <- secciones_benito_juarez %>% 
  rbind(secciones_AO_18) %>% 
  distinct(demarcacion, nombre_alcaldia,distrito_local, seccion) %>% 
  filter(distrito_local == 18)

# Graficamos con ggplot 
secciones_cdmx %>% 
  left_join(distrito_local_18, by = "seccion") %>% 
  filter(!is.na(demarcacion)) %>% 
  ggplot()+ 
  geom_sf()


# Graficamos con leaflet 
p <- secciones_cdmx %>% 
  left_join(distrito_local_18, by = "seccion") %>% 
  filter(!is.na(demarcacion)) %>% 
  st_transform( "+init=epsg:4326")


leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p,
              fillColor = "#93003B",
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 3),
              label = ~seccion,
              labelOptions = labelOptions(noHide = F,
                                          style = list("font-weight" = "bold")),
              stroke = T)


### Mapeamos el distrito local 18 con votacion trabajada --------

# Secciones seleccionadas del distrito local 18
distrito_local_18 <- secciones_benito_juarez %>% 
  rbind(secciones_AO_18) %>% 
  distinct(demarcacion, nombre_alcaldia,distrito_local, seccion) %>% 
  filter(distrito_local == 18)

## Unimos para que tenga geometry
p <- secciones_cdmx %>% 
  right_join(distrito_local_18, by = "seccion") %>% 
  left_join(prep_general_2021_trabajado, by = "seccion") 


## GRaficamos con ggplot 
p %>% 
  ggplot()+ 
  geom_sf(aes(fill = `dif_M-PAN`)) +
  scale_fill_gradient2(low = "skyblue",mid = "white" ,high = "#b62118")


## Graficamos con leaflet ------
p_1 <- p %>% 
  st_transform( "+init=epsg:4326") %>% 
  select(-c(demarcacion,distrito_l, distrito_f, clave_dem, demarcacion_territorial))


library(RColorBrewer)

# Definir los límites mínimo y máximo
min_value <- min(p_1$`dif_M-PAN`)
max_value <- max(p_1$`dif_M-PAN`)

# Crear la paleta personalizada
custom_palette <- colorRampPalette(c("skyblue", "#b62118"))(8)

 # Crear el mapa con la paleta personalizada
leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p_1,
              fillColor = ~custom_palette[findInterval(`dif_M-PAN`, seq(min_value, max_value, length.out = 5))],
              fillOpacity = 1,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", weight = 3),
              label = ~seccion,
              labelOptions = labelOptions(noHide = F, style = list("font-weight" = "bold")),
              stroke = TRUE)




### Este es el funcional --------
labels_votos <- sprintf(
  "<strong>Sección: %s</strong><br/> L.N: %g <br> V. Morena: %g<br/> V. PAN: %g<br/> Diferencia: %g",
  p$seccion,p$lista_nominal, p$morena, p$pan, p$`dif_M-PAN`) %>% 
  lapply(htmltools::HTML)

min_value <- min(p_1$`dif_M-PAN`)
max_value <- max(p_1$`dif_M-PAN`)

# Crear la paleta personalizada
custom_palette <- c(colorRampPalette(c("blue", "blue"))(4), colorRampPalette(c("skyblue", "#b62118"))(4))

# Crear el mapa con la paleta personalizada
leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p_1, 
              fillColor = ~custom_palette[findInterval(`dif_M-PAN`, 
                                                       seq(min_value, max_value, length.out = 8))],
              fillOpacity = 0.5,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 3, 
                                                  bringToFront = TRUE),
              label = labels_votos,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold")),
              stroke = TRUE) %>% 
  addLegend("bottomright",  # https://stackoverflow.com/questions/38161073/manually-adding-legend-values-in-leaflet
            colors = c("#B62118",  "#A65A5E", "#9694A4", 
                      "#87CEEB", "#0000FF"),
            labels = c("Morena (max: 308)", "", "", "", "PAN (max: 1233)"),
            title = "Diferencia de votos Morena-PAN",
            opacity = 1)

p %>% 
  select(`dif_M-PAN`) %>% 
  filter(`dif_M-PAN`< 0) %>% 
  summarise(mean_1 = mean(`dif_M-PAN`))




### Esta es otra opcion funcional con la leyenda modificada con html ------
leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = p_1, 
              fillColor = ~custom_palette[findInterval(`dif_M-PAN`, seq(min_value, max_value, length.out = 8))],
              fillOpacity = 0.5,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE),
              label = labels_votos,
              labelOptions = labelOptions(noHide = F, style = list("font-weight" = "bold")),
              stroke = TRUE) %>% 
  addControl(html = "<div class='legend' style='font-size: 12px; line-height: 16px;'>
                      <strong>Diferencia de votos Morena-PAN</strong><br>
                      <i class='circle' style='background: #B62118;'></i> Morena (max: 308)<br>
                      <i class='circle' style='background: #A65A5E;'></i><br>
                      <i class='circle' style='background: #9694A4;'></i><br>
                      <i class='circle' style='background: #87CEEB;'></i><br>
                      <i class='circle' style='background: #0000FF;'></i> PAN (max: 1233)
                    </div>",
             position = "bottomright")


### colonias cdmx ----------
## Filtramos datos de colonias

colonias <- colonias %>% 
  janitor::clean_names() %>% 
  filter(nomdt %in% c("BENITO JUAREZ", "ALVARO OBREGON" )) %>% 
  select(-c(ent, cvedt, nomdt, dttoloc, cveut)) %>% 
  st_transform("+init=epsg:4326") 

labels_colonias <-  sprintf(
  "<strong>Colonia: %s</strong>",
  colonias$nomut) %>% 
  lapply(htmltools::HTML)



colonias %>% filter(nomut == "ACACIAS")


p_1 <- p_1 %>% 
  st_transform( "+init=epsg:4326")

colonias %>% as_tibble()

# intersect(tibble(p_1), tibble(colonias))

# Creamos el perimetro del distrito local 18 -------------
# Crear un objeto sf a partir de tus datos con la columna "geometry"
sf_data <- st_as_sf(p_1)

# Unir todas las geometrías en una sola entidad
union <- st_union(sf_data)
# Crear el perímetro del conjunto unificado
perimetro <- st_boundary(union)

class(colonias)
class(colonias_sobrelapadas)



### Agregamos prep por edad ----------
prep_p_edad_2021<- p %>% 
  left_join(prep_p_edad_2021, 
            by = c("seccion" = "sec")) %>% 
  st_transform( "+init=epsg:4326")

## Filtramos por los grupos etareos que queremos 
#variables
variable <- c("h1819", "h2024", "h2529", "m1819", "m2024", "m2529")

# Bucle for
for (i in variable) {
  # Crear el nombre del objeto
  nombre_objeto <- paste0("v_", i)
  
  # Asignar el filtro al objeto
  assign(nombre_objeto, filter(prep_p_edad_2021, sexo_edad == i))
}

# Verificar los objetos creados
ls(pattern = "^v_")

# Obtener el mínimo y máximo de la variable v_h1819
# Rango del color
min_v <- min(v_h1819$total)
max_v <- max(v_h1819$total)

# Crear una rampa de colores basada en la variable v_h1819
# Color 
color_v_h1819 <- colorNumeric(palette = "Oranges", domain = c(min_v, max_v))

# Label de v_h1819
labels_votos_v_h1819 <- sprintf(
  "<strong>Total de votos de hombres 18-19 años: </strong> <br/> &emsp;&emsp;
  &emsp;&emsp;&emsp;&emsp;votos: %g <br>
",
  v_h1819$total) %>% 
  lapply(htmltools::HTML)




leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = perimetro,
              fill = F, weight = 7, color = "red", group = "Límites", opacity = 1) %>%
  addPolygons(data = colonias, color = "black", group = "colonias",
              label = labels_colonias,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold"))) %>%
  addPolygons(data = p_1, 
              fillColor = ~custom_palette[findInterval(`dif_M-PAN`, 
                                                       seq(min_value, max_value, length.out = 8))],
              fillOpacity = 0.5,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 3, 
                                                  bringToFront = TRUE),
              label = labels_votos,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold")),
              stroke = TRUE,
              group = "Votos p/secciones") %>% 
  addPolygons(data = v_h1819, 
              fillColor = ~color_v_h1819(total),
              fillOpacity = 0.7,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 3, 
                                                  bringToFront = TRUE),
              label = labels_votos_v_h1819,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold")),
              stroke = TRUE,
              group = "v_h1819") %>% 
  addLegend("bottomright",  # https://stackoverflow.com/questions/38161073/manually-adding-legend-values-in-leaflet
            colors = c("#B62118",  "#A65A5E", "#9694A4", 
                                "#87CEEB", "#0000FF"),
                                labels = c("Morena (max: 308)", "", "", "", "PAN (max: 1233)"),
            title = "Diferencia de votos Morena-PAN",
            opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Votos p/secciones", "colonias", "Límites", "v_h1819"),
    options = layersControlOptions(collapsed = F, selected = "Votos p/secciones")
  ) %>% 
  hideGroup("colonias") %>%
  hideGroup("Límites") %>% 
  hideGroup("v_h1819") 


### Creamos capa de frente vs hagamos historia

# Crear la paleta personalizada
labels_votos_frentes <- sprintf(
  "<strong> Sección: %s </strong> <br> Juntos: %g  <br/> Frente: %g <br/> Diferencia: %g <br/>",
  p_1$seccion, p_1$juntos, p_1$frente, p_1$`dif_J-F`) %>% 
  lapply(htmltools::HTML)



custom_palette_frentes <- c(colorRampPalette(c("blue", "blue"))(4), 
                            colorRampPalette(c("skyblue", "#b62118"))(4))
min_value_frentes <- min(p_1$dif_J-F)
max_value_frentes <- max(p_1$dif_J-F)

leaflet() %>%
  addProviderTiles(provider = "OpenStreetMap") %>%
  addPolygons(data = perimetro,
              fill = F, weight = 7, color = "red", group = "Límites", opacity = 1) %>%
  addPolygons(data = colonias, color = "black", group = "colonias",
              label = labels_colonias,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold"))) %>%
  addPolygons(data = p_1, 
              fillColor = ~custom_palette[findInterval(`dif_M-PAN`, 
                                                       seq(min_value, max_value, length.out = 8))],
              fillOpacity = 0.5,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 3, 
                                                  bringToFront = TRUE),
              label = labels_votos,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold")),
              stroke = TRUE,
              group = "Votos p/secciones") %>% 
  addPolygons(data = p_1, 
              fillColor = ~custom_palette_frentes[findInterval(`dif_J-F`, 
                                                               seq(min_value, max_value, length.out = 8))],
              fillOpacity = 0.5,
              weight = 1,
              color = "black",
              label = labels_votos_frentes,
              group = "Frentes") %>% 
  addPolygons(data = v_h1819, 
              fillColor = ~color_v_h1819(total),
              fillOpacity = 0.7,
              color = "black",
              weight = 1,
              highlightOptions = highlightOptions(color = "white", 
                                                  weight = 3, 
                                                  bringToFront = TRUE),
              label = labels_votos_v_h1819,
              labelOptions = labelOptions(noHide = F, 
                                          style = list("font-weight" = "bold")),
              stroke = TRUE,
              group = "v_h1819") %>% 
  addLegend("bottomright",  # https://stackoverflow.com/questions/38161073/manually-adding-legend-values-in-leaflet
            colors = c("#B62118",  "#A65A5E", "#9694A4", 
                                "#87CEEB", "#0000FF"),
                                labels = c("Morena (max: 308)", "", "", "", "PAN (max: 1233)"),
            title = "Diferencia de votos Morena-PAN",
            opacity = 1) %>%
  # Layers control
  addLayersControl(
    overlayGroups = c("Votos p/secciones", "colonias", "Límites", "v_h1819", "Frentes"),
    options = layersControlOptions(collapsed = F, selected = "Votos p/secciones")
  ) %>% 
  hideGroup("colonias") %>%
  hideGroup("Límites") %>% 
  hideGroup("v_h1819") %>% 
  hideGroup("Frentes") 



