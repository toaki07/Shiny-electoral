### Mapa CDXM comparativo 2018 - 2021

## Código elaborado por Toaki Hoz Canabal

### El objetivo de este código es crear un mapa con la media de las elecciones entre 2018 y 2021 por partido para intentar aproximar la votación ganadora y las secciones más rentables 

## setup --------

## Cargamos paquetes ----------------------

pacman::p_load(sf, 
               leaflet,
               tidyverse)


### Cargamos datos ---------------


## Cargo datos de elecciones 2018 para diputados (verificado)
votacion_dip_2018 <- readxl::read_excel("01_datos/diputaciones_MR/Diputaciones_MR_2018.xls", sheet = "bd2018dmrsec") %>% 
  janitor::clean_names() %>% 
  Hmisc::cleanup.import() %>% 
  select(-c(votacion_total_pt_mor_pes, votacion_total_prd_mc_dtto5, votacion_total_pan_prd_mc )) %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna
  mutate(frente = sum(c_across(contains(c("pan", "mc", "prd"))), 
                      na.rm = TRUE),
         juntos = sum(c_across(contains(c("morena", "pes", "pt", "mor"))), 
                      na.rm = TRUE),
         todosxM = sum(c_across(contains(c("pri", "pvem", "panal"))),
                       na.rm = T),
         `dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/vt,
         p_morena = morena/vt) %>% 
  select(1:5, frente, juntos, morena, pan, vt, ln)
  

## Cargo datos de elecciones 2021
prep_general_2021 <- readxl::read_excel("01_datos/diputaciones_MR/Diputaciones_MR_2021.xls", sheet = "ResultadosSecciones") %>% 
  janitor::clean_names() %>% 
  Hmisc::cleanup.import() %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna
  mutate(frente = sum(c_across(contains(c("pan", "pri", "prd"))), 
                      na.rm = TRUE),
         juntos = sum(c_across(contains(c("morena", "pvem", "pt"))), 
                      na.rm = TRUE),
         `dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total) %>% 
  select(1:5, frente, juntos, morena, pan, votacion_total, lista_nominal)

## Cargamos secciones distrito local 18
distrito_local_18 <- read.csv("01_datos/distrito_local_18.csv") %>% 
  tibble()

# dbf de secciones: información obtenida de DataMexico
secciones <- st_read("01_datos/shp_2021/SECCION.dbf")

# secciones <- st_read("../shp_2021\\SECCION.dbf")

## Modificamos datos para unir--------

# Nombres de tibbles
names(votacion_dip_2018) <- names(prep_general_2021)


## Corroboramos
votacion_dip_2018
prep_general_2021

# Creamos secciones_cdmx
secciones_cdmx <- # cdmx %>% st_join(secciones) %>%
  secciones %>% filter(#municipio <=17,
    entidad == 9)
# Removemos datos innecesarios 
rm(secciones)

# Union de distrito local con secciones
distrito_local_18 <- secciones_cdmx %>% 
  left_join(distrito_local_18, by = "seccion") %>% 
  filter(!is.na(demarcacion))


## Graficamos 2018  (estático)------
p <-  distrito_local_18 %>%
  left_join(votacion_dip_2018 %>%
              select(-distrito_local), by = "seccion") %>%
  mutate(`dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total)

lista_nominal <- mean(x = p$lista_nominal)
promedio_victoria <- round(mean(abs(p$`dif_J-F`)))

(g_2018 <- distrito_local_18 %>% 
  left_join(votacion_dip_2018 %>% 
              select(-distrito_local), by = "seccion") %>% 
  mutate(`dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total) %>% 
  select(-c(1:5,tipo,demarcacion, demarcacion_territorial)) %>% 
  mutate(color = case_when(`dif_J-F` >= 0   & `dif_J-F`<= 169 ~    "morena1",
                           `dif_J-F` > 169 & `dif_J-F` <= 338 ~   "morena2",
                           `dif_J-F` > 338 & `dif_J-F` <= 510 ~   "morena3", 
                           `dif_J-F` < 0   & `dif_J-F` >= -186 ~  "pan1",
                           `dif_J-F` < -186 & `dif_J-F` >= -372 ~ "pan2",
                           `dif_J-F` < -372 & `dif_J-F` >= -558 ~ "pan3")) %>% 
  mutate(color = factor(color, ordered = T, levels = c("morena3", "morena2", 
                                                       "morena1", "pan1", "pan2", "pan3"))) %>%
  ggplot(aes(fill = color)) +
  geom_sf() +
  scale_fill_manual(name = "Rangos de votación", 
                    values = c("morena1" = "#FF5733",
                               "morena2" = "#FF0000",
                               "morena3" = "#800000",
                               "pan1" = "#66B2FF",
                               "pan2" = "#0000FF",
                               "pan3" = "#000080"), 
                    labels = c("morena3" = "Entre 339 y 510",
                               "morena2" = "Entre 170 y 338",
                               "morena1" = "Entre 0 y 169",
                               "pan1" = "Entre -1 y -186",
                               "pan2" = "Entre -187 y -372",
                               "pan3" = "Entre -373 y -558")) +
  theme_bw() +
  labs(title = "Votación por diputación distrito local 18",
       subtitle = "2018",
       caption = paste0("Por un solo partido se necesitaron 170 casillistas\n", "Promedio  ", round(lista_nominal), " personas en la lista nominal por sección\n", "Promedio en diferencia de votos para ganar: ", promedio_victoria)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
 )
  


## Graficamos 2021 (estático) ---------
# p <- distrito_local_18 %>% 
#   left_join(prep_general_2021 %>% 
#               select(-distrito_local), by = "seccion") %>% 
#   mutate(`dif_J-F` = juntos - frente,
#          `dif_M-PAN` = morena - pan,
#          p_pan = pan/votacion_total,
#          p_morena = morena/votacion_total) %>% 
#   select(-c(1:5,tipo,demarcacion, demarcacion_territorial)) %>% 
#   mutate(color = case_when(`dif_J-F` >= 0   & `dif_J-F`<= 19 ~    "morena1",
#                            `dif_J-F` > 19 & `dif_J-F` <= 38 ~   "morena2",
#                            `dif_J-F` > 38 & `dif_J-F` <= 57 ~   "morena3", 
#                            `dif_J-F` > 57 & `dif_J-F` <= 76 ~ "morena4",
#                            `dif_J-F` < 0   & `dif_J-F` >= -493 ~  "pan1",
#                            `dif_J-F` < -493 & `dif_J-F` >= -986 ~ "pan2",
#                            `dif_J-F` < -986 & `dif_J-F` >= -1479 ~ "pan3",
#                            `dif_J-F` < 1479 & `dif_J-F` >= -1972 ~ "pan4")) %>% 
#   mutate(color = factor(color, ordered = T, levels = c("morena4", "morena3", "morena2", 
#                                                        "morena1", "pan1", "pan2", "pan3", "pan4")))
# 
# morena_gana <- p %>% filter(`dif_J-F` >= 0)
# morena_gana %>% 
#   ggplot(aes(fill = color)) +
#   geom_sf()
# range(morena_gana$`dif_J-F`)
# 76/4
# 
# pan_gana <- p %>% filter(`dif_J-F` <= 0)
# range(pan_gana$`dif_J-F`)
# 1972/4
# 
# pan_gana %>% filter(`dif_J-F` == min(`dif_J-F`))
j <- distrito_local_18 %>% 
  left_join(prep_general_2021 %>% 
              select(-distrito_local), by = "seccion") %>% 
  mutate(`dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total)

promedio_victoria <- round(mean(abs(j$`dif_J-F`)))
promedio_lista <- round(mean(j$lista_nominal))

(g_2021 <- distrito_local_18 %>% 
  left_join(prep_general_2021 %>% 
              select(-distrito_local), by = "seccion") %>% 
  mutate(`dif_J-F` = juntos - frente,
         `dif_M-PAN` = morena - pan,
         p_pan = pan/votacion_total,
         p_morena = morena/votacion_total) %>% 
  select(-c(1:5,tipo,demarcacion, demarcacion_territorial)) %>% 
  mutate(color = case_when(`dif_J-F` >= 0   & `dif_J-F`<= 19 ~    "morena1",
                           `dif_J-F` > 19 & `dif_J-F` <= 38 ~   "morena2",
                           `dif_J-F` > 38 & `dif_J-F` <= 57 ~   "morena3", 
                           `dif_J-F` > 57 & `dif_J-F` <= 76 ~ "morena4",
                           `dif_J-F` < 0   & `dif_J-F` >= -493 ~  "pan1",
                           `dif_J-F` < -493 & `dif_J-F` >= -986 ~ "pan2",
                           `dif_J-F` < -986 & `dif_J-F` >= -1479 ~ "pan3",
                           `dif_J-F` < 1479 & `dif_J-F` >= -1972 ~ "pan4")) %>% 
  mutate(color = factor(color, ordered = T, 
                        levels = c("morena4", "morena3", "morena2", 
                                   "morena1", "pan1", "pan2", "pan3", "pan4"))) %>%
  ggplot(aes(fill = color)) +
  geom_sf() +
  scale_fill_manual(name = "Rangos de votación", 
                    values = c("morena1" = "#FFA07A",
                               "morena2" = "#FF5733",
                               "morena3" = "#FF0000",
                               "morena4" = "#800000", 
                               "pan1" = "#ADD8E6",
                               "pan2" = "#66B2FF",
                               "pan3" = "#0000FF",
                               "pan4" = "#0000FF"), 
                    labels = c("morena4" = "Entre 58 y 76",
                               "morena3" = "Entre 39 y 57",
                               "morena2" = "Entre 20 y 38",
                               "morena1" = "Entre 0 y 19",
                               "pan1" = "Entre -1 y -493",
                               "pan2" = "Entre -494 y -986",
                               "pan3" = "Entre -987 y -1479",
                               "pan4" = "Entre -1480 y -1972")) +
  theme_bw() +
  labs(title = "Votación por diputación distrito local 18",
       subtitle = "2021",
       caption = paste0("Promedio ",promedio_lista, " personas en la lista nominal por sección\n" ,"Promedio en diferencia de votos para ganar: ", promedio_victoria)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
 )

j %>% 
  count(`dif_J-F` > 0)

## Graficamos propuesta de proyección (unimos datos) (estático) --------------
votos_electorales <- votacion_dip_2018 %>% 
  left_join(prep_general_2021,
            by = "seccion",
            suffix = c("_2018", "_2021"))


# p <-                           De aquí sale p para los códigos de rango

q <- distrito_local_18 %>% 
  left_join(votos_electorales,
            by = "seccion") %>% 
  select(seccion, distrito_local, contains(c("juntos", "frente", "total", "lista"))) %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna. Recuerda que para volver a hacer operaciones por columna es necesario un ungroup()
  mutate(promedio_juntos = mean(c(juntos_2018, juntos_2021)),
         promedio_frente = mean(c(frente_2018, frente_2021)),
         promedio_lista = mean(c(lista_nominal_2018, lista_nominal_2021)),
         diferencia_d_promedios = promedio_juntos - promedio_frente) %>% 
  select(seccion, contains(c("juntos", "frente")), diferencia_d_promedios, everything()) %>% 
  ungroup()

promedio_victoria <- round(mean(abs(q$diferencia_d_promedios)))
promedio_lista <- round(mean(q$promedio_lista))


(g_proyeccion <- distrito_local_18 %>% 
  left_join(votos_electorales,
            by = "seccion") %>% 
  select(seccion, distrito_local, contains(c("juntos", "frente", "total", "lista"))) %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna. Recuerda que para volver a hacer operaciones por columna es necesario un ungroup()
  mutate(promedio_juntos = mean(c(juntos_2018, juntos_2021)),
         promedio_frente = mean(c(frente_2018, frente_2021)),
         diferencia_d_promedios = promedio_juntos - promedio_frente) %>% 
  select(seccion, contains(c("juntos", "frente")), diferencia_d_promedios, everything()) %>% 
  ungroup() %>% 
  mutate(color = case_when(diferencia_d_promedios >= 0 & diferencia_d_promedios <= 59 ~ "morena1",
                           diferencia_d_promedios > 59 & diferencia_d_promedios <= 118 ~ "morena2",
                           diferencia_d_promedios > 118 & diferencia_d_promedios <= 177 ~ "morena3",
                           diferencia_d_promedios > 177 & diferencia_d_promedios <= 236 ~ "morena4",
                           diferencia_d_promedios < 0 & diferencia_d_promedios >= -227 ~ "pan1",
                           diferencia_d_promedios < -227 & diferencia_d_promedios >= -454 ~ "pan2",
                           diferencia_d_promedios < -454 & diferencia_d_promedios >= -681 ~ "pan3",
                           diferencia_d_promedios < -681 & diferencia_d_promedios >= -908 ~ "pan4")) %>% 
  mutate(color = factor(color, ordered = T, 
                        levels = c("morena4", "morena3", "morena2", 
                                   "morena1", "pan1", "pan2", "pan3", "pan4"))) %>% 
  ggplot(aes(fill = color)) +
  geom_sf() +
  scale_fill_manual(name = "Rangos de votación", 
                    values = c("morena1" = "#FFA07A",
                               "morena2" = "#FF5733",
                               "morena3" = "#FF0000",
                               "morena4" = "#800000", 
                               "pan1" = "#ADD8E6",
                               "pan2" = "#66B2FF",
                               "pan3" = "#0000FF",
                               "pan4" = "#0000FF"), 
                    labels = c("morena4" = "Entre 178 y 236",
                               "morena3" = "Entre 119 y 177",
                               "morena2" = "Entre 60 y 118",
                               "morena1" = "Entre 0 y 59",
                               "pan1" = "Entre -1 y -227",
                               "pan2" = "Entre -228 y -454",
                               "pan3" = "Entre -455 y -681",
                               "pan4" = "Entre -682 y -908")) +
  theme_bw() +
  labs(title = "Votación por diputación distrito local 18",
       subtitle = "Proyección 1 al 2024",
       caption = paste0("Promedio en diferencia de votos para ganar: ", promedio_victoria)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
  )
  


q %>% 
  count(diferencia_d_promedios > 0)

q %>% tibble() %>% 
  select(diferencia_d_promedios) %>% 
  summarise(promedio_gano_morena_2024 =mean(case_when(diferencia_d_promedios > 0 ~ diferencia_d_promedios),
                                            na.rm = T),
            promedio_gano_pan_2024 = mean(case_when(diferencia_d_promedios < 0 ~ diferencia_d_promedios),
                                          na.rm = T) )




### Ver rangos de la misma columna con filtros. Usar summarise con filtros
# 
# morena_gana <- p %>% ungroup() %>% 
#   as_tibble() %>% 
#   summarise(rango_general = range(diferencia_d_promedios), 
#             rango_morena = range(.data[[as_tibble(p)[p$diferencia_d_promedios < 0, "diferencia_d_promedios"]]]))
# range(p$diferencia_d_promedios)
# 
# p %>%
#   ungroup() %>%
#   as_tibble() %>%
#   summarise(
#     rango_general = range(diferencia_d_promedios),
#     rango_morena = range(case_when(diferencia_d_promedios > 0 ~ diferencia_d_promedios), na.rm = T),
#     rango_pan = range(case_when(diferencia_d_promedios < 0 ~ diferencia_d_promedios), na.rm = T)
#   )
# 
# ## HAcerlo a manita
# rango_general <- range(p$diferencia_d_promedios)
# rango_morena <- range(p$diferencia_d_promedios[p$diferencia_d_promedios > 0])
# rango_pan <- range(p$diferencia_d_promedios[p$diferencia_d_promedios < 0])
# 
# (resultados <- tibble(
#   rango_general = rango_general,
#   rango_morena = rango_morena,
#   rango_pan = rango_pan)
#   )
# Calculos para ver subgrupos de rango
# 236%%4
# 
# 236/4
# seq(0, 236 ,59)
# 
# 906%%4
# 
# 908/4
# seq(0, 908, 227)

## Unimos 3 mapas -----------
layout_matrix <- matrix(c(1, 1, 2, 2, 4, 3, 3, 4), nrow = 2, byrow = TRUE)
gridExtra::grid.arrange(g_2018, g_2021, g_proyeccion, layout_matrix = layout_matrix)


## Graficamos 2018 (dinámico) ----------

## Graficamos 2021 (dinámico) ----------

## Graficamos propuesta de proyección (dinámico) ---------

# Hacemos mapa conjunto (con leaflet.extra o por capas distintas)
## Información a destacar ----------------
p <- distrito_local_18 %>% 
  left_join(votos_electorales,
            by = "seccion") %>% 
  select(seccion, distrito_local, contains(c("juntos", "frente", "total", "lista"))) %>% 
  rowwise() %>% # se aplica rowwise() a un tibble, las operaciones subsecuentes se aplican 
  # a nivel de fila. Esto significa que las funciones de agregación, como sum(), 
  # se calculan por fila en lugar de por columna. Recuerda que para volver a hacer operaciones por columna es necesario un ungroup()
  mutate(promedio_juntos = mean(c(juntos_2018, juntos_2021)),
         promedio_frente = mean(c(frente_2018, frente_2021)),
         promedio_lista = mean(c(lista_nominal_2018, lista_nominal_2021)),
         diferencia_d_promedios = promedio_juntos - promedio_frente,
         diferencias_2018 = juntos_2018 - frente_2018,
         diferencias_2021 = juntos_2021 - frente_2021) %>% 
  select(seccion, contains(c("juntos", "frente")),diferencias_2018, diferencias_2021, diferencia_d_promedios, everything()) %>% 
  ungroup()
  


# Minimos
p %>%
  as_tibble() %>%
  summarise(
    minimo_juntos_2018 = min(juntos_2018),
    minimo_juntos_2021 = min(juntos_2021),
    minimo_frente_2018 = min(frente_2018),
    minimo_frente_2021 = min(frente_2021)
  )

## Máximos
p %>%
  as_tibble() %>%
  summarise(
    maximo_juntos_2018 = max(juntos_2018),
    maximo_juntos_2021 = max(juntos_2021),
    maximo_frente_2018 = max(frente_2018),
    maximo_frente_2021 = max(frente_2021)
    )
  
## Promedios de votacion
p %>%
  as_tibble() %>%
  summarise(
    promedio_juntos_2018 = round(mean(juntos_2018)),
    promedio_juntos_2021 = round(mean(juntos_2021)),
    promedio_frente_2018 = round(mean(frente_2018)),
    promedio_frente_2021 = round(mean(frente_2021))
  )

## Con respecto a la comparativa de voto
p %>% 
  as_tibble() %>% 
  summarise(diferencia_2018_frentes = juntos_2018 - frente_2018,
            diferencia_2021_frentes = juntos_2021 - frente_2021) %>% 
  summarise(promedio_gano_morena_2018 =mean(case_when(diferencia_2018_frentes > 0 ~ diferencia_2018_frentes),
                                       na.rm = T),
            promedio_gano_morena_2021 =mean(case_when(diferencia_2021_frentes > 0 ~ diferencia_2021_frentes),
                                       na.rm = T),
            promedio_gano_pan_2018 = mean(case_when(diferencia_2018_frentes < 0 ~ diferencia_2018_frentes),
                                          na.rm = T),
            promedio_gana_pan_2021 = mean(case_when(diferencia_2021_frentes < 0 ~ diferencia_2021_frentes),
                                          na.rm = T))

# p %>%
#   ungroup() %>%
#   as_tibble() %>%
#   summarise(
#     rango_general = range(diferencia_d_promedios),
#     rango_morena = range(case_when(diferencia_d_promedios > 0 ~ diferencia_d_promedios), na.rm = T),
#     rango_pan = range(case_when(diferencia_d_promedios < 0 ~ diferencia_d_promedios), na.rm = T)
#   )