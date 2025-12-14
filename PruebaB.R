###CÓDIGO REALIZADO POR PALOMA GUEVARA (apguevara.ap@gmail.com) CON LOS  MICRODATOS EXTRAÍDOS DE LAS ESTADÍSTICAS DE DEFUNCIONES REGISTRADAS (EDR), DE INEGI, 2024, https://www.inegi.org.mx/programas/edr/#microdatos 
###PARTE B DE LA PRUEBA TÉCNICA PARA LA VACANTE COMO ANALISTA DE DATOS SR. DE DATA CÍVICA, DICIEMBRE DE 2025.

rm(list = ls())
setwd("C:/PruebaTecnica")
library(tidyverse)
library(ggplot2)
library(foreign)
library(dplyr)
library(tidyr)
library(gt)
library(sf)
library(rnaturalearth)
library(treemapify)



edr24 <- read.dbf("C:/PruebaTecnica/PruebaB/defunciones_base_datos_2024_dbf/DEFUN24.dbf")

# ANÁLISIS EXPLORATORIO DE LOS HOMICIDIOS REGISTRADOS EN MÉXICO DURANTE 2024 ----

# 1. TASAS DE HOMICIDIO A NIVEL NACIONAL ----
#¿Cuáles son las entidades con tasas de homicidio más elevadas para hombres y mujeres?

# No se puede hacer una comparación entre entidades en términos de presuntos homicidios absolutos dado que la diferencia de volumen de habitantes entre estados hace que los resultados sean engañosos. Por eso, es necesario calcular las tasas (presuntos homicidios/población total * 100 000) y, dado que estamos trabajando con datos de INEGI y la información que tenemos disponible corresponde a los censos (además de por una cuestión de tiempo, pues podría proyectarse la población 2024 y tener tasas más precisas), trabajaremos con la población total registrada al 2020.  

## 1.1 data frame de población total ----
pob_ent <- tibble::tibble(
  ENT_OCURR = c(
    "AGUASCALIENTES", "BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "CAMPECHE",
    "COAHUILA", "COLIMA", "CHIAPAS", "CHIHUAHUA", "CIUDAD DE MEXICO",
    "DURANGO", "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO",
    "ESTADO DE MEXICO", "MICHOACAN", "MORELOS", "NAYARIT",
    "NUEVO LEON", "OAXACA", "PUEBLA", "QUERETARO", "QUINTANA ROO",
    "SAN LUIS POTOSI", "SINALOA", "SONORA", "TABASCO", "TAMAULIPAS",
    "TLAXCALA", "VERACRUZ", "YUCATAN", "ZACATECAS"
  ),
  pob_tot = c( #fuente de población total por entidad: https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=Poblacion_Poblacion_01_e60cd8cf-927f-4b94-823e-972457a12d4b 
    1425607, 3769020, 798447, 928363, 3146771, 731391, 5543828,
    3741869, 9209944, 1832650, 6166934, 3540685, 3082841,
    8348151, 16992418, 4748846, 1971520, 1235456,
    5784442, 4132148, 6583278, 2368467, 1857985,
    2822255, 3026943, 2944840, 2402598, 3527735,
    1342977, 8062579, 2320898, 1622138
  )
)

## 1.2 data frame de homicidios ----
df_homicidios <- edr24 %>%
  filter(TIPO_DEFUN == 2) %>%      # Solo homicidios
  group_by(ENT_OCURR) %>%
  summarise(
    homicidios = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ENT_OCURR = recode(as.character(ENT_OCURR),
                       "01"  = "AGUASCALIENTES",
                       "02"  = "BAJA CALIFORNIA",
                       "03"  = "BAJA CALIFORNIA SUR",
                       "04"  = "CAMPECHE",
                       "05"  = "COAHUILA",
                       "06"  = "COLIMA",
                       "07"  = "CHIAPAS",
                       "08"  = "CHIHUAHUA",
                       "09"  = "CIUDAD DE MEXICO",
                       "10" = "DURANGO",
                       "11" = "GUANAJUATO",
                       "12" = "GUERRERO",
                       "13" = "HIDALGO",
                       "14" = "JALISCO",
                       "15" = "ESTADO DE MEXICO",
                       "16" = "MICHOACAN",
                       "17" = "MORELOS",
                       "18" = "NAYARIT",
                       "19" = "NUEVO LEON",
                       "20" = "OAXACA",
                       "21" = "PUEBLA",
                       "22" = "QUERETARO",
                       "23" = "QUINTANA ROO",
                       "24" = "SAN LUIS POTOSI",
                       "25" = "SINALOA",
                       "26" = "SONORA",
                       "27" = "TABASCO",
                       "28" = "TAMAULIPAS",
                       "29" = "TLAXCALA",
                       "30" = "VERACRUZ",
                       "31" = "YUCATAN",
                       "32" = "ZACATECAS",
                       "99" = "INDETERMINADO"
    )
  ) 


## 1.3 Unir bases y calcular tasas ----

#cambiar el formato de la base de homicidios para poder hacer left join
df_tasas <- df_homicidios %>%
  left_join(pob_ent, by = "ENT_OCURR") %>%
  mutate(
    tasas = (homicidios / pob_tot) * 100000
  )

## 1.4 Creación de mapa ----
geom_sf(aes(fill = tasas))

mapa <- ne_states(
  country = "Mexico",
  returnclass = "sf"
)

mapa <- mapa %>%
  mutate(
    ENT_OCURR = toupper(name)
  )

mapa_homicidios <- mapa %>%
  left_join(df_tasas, by = "ENT_OCURR")

ggplot(mapa_homicidios) +
  geom_sf(aes(fill = tasas), color = "white", size = 0.2) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    na.value = "lightgrey",
    name = "Tasa x 100 mil \n
    habitantes"
  ) +
  labs(
    title = "Tasa de homicidios por entidad federativa",
    caption = "Fuente: 1. Estadísticas de Defunciones Registradas, INEGI, 2024. \n 2.Censo de Población y vivienda, INEGI, 2020. \n 3.Tasa de homicidios calculada como: (Presuntos homicidios totales (2024) / Población total (2020)) * 100,000"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,      
      face = "bold",     
      size = 14
    ),
    plot.caption = element_text(
      hjust = 0,          
      size = 9,
    ),
    axis.title = element_blank(),   
    axis.text  = element_blank(),  
    axis.ticks = element_blank()
  )

# 2. CAUSAS DE DEFUNCIÓN ----
# ¿Cuáles son las causas de defunción más frecuentes en los homicidios para cada sexo?

## 2.1 data frame causas de defunción por sexo ----
## Se muestran las 10 causas más frecuentes para cada sexo
df_causas_sexo <- edr24 %>%
  filter(
    TIPO_DEFUN == 2,
    SEXO %in% c(1, 2),    
    !is.na(CAUSA_DEF)
  ) %>%
  group_by(SEXO, CAUSA_DEF) %>%
  summarise(
    homicidios = n(),
    .groups = "drop"
  ) %>%
  group_by(SEXO) %>%
  arrange(desc(homicidios)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  mutate(
    SEXO = recode(SEXO,
                  `1` = "Hombres",
                  `2` = "Mujeres")
  )

## 2.2 data frame con columnas separadas para hombres y mujeres ----
## renombrar causas de defunción con base en los anexos del CIE 10

df_CS_sep <- df_causas_sexo %>%
  mutate(
    CAUSA_DEF = recode(as.character(CAUSA_DEF),
                       "X954" = "ARMAS DE FUEGO, EN CALLES Y CARRETERAS",
                       "X959" = "ARMAS DE FUEGO, EN LUGAR NO ESPECIFICADO",
                       "X950" = "ARMAS DE FUEGO, EN VIVIENDA",
                       "X958" = "ARMAS DE FUEGO, EN OTRO LUGAR ESPECIFICADO",
                       "X994" = "OBJETO CORTANTE, EN CALLES Y CARRETERAS",
                       "X990" = "OBJETO CORTANTE, EN VIVIENDA",
                       "X999" = "OBJETO CORTANTE, EN LUGAR NO ESPECIFICADO",
                       "X914" = "AHORCAMIENTO, ESTRANGULAMIENTO Y SOFOCACION, EN CALLES Y CARRETERAS",
                       "X910" = "AHORCAMIENTO, ESTRANGULAMIENTO Y SOFOCACION, EN VIVIENDA",
                       "X919" = "AHORCAMIENTO, ESTRANGULAMIENTO Y SOFOCACION, EN LUGAR NO ESPECIFICADO",
                       "Y094" = "MEDIOS NO ESPECIFICADOS, EN CALLES Y CARRETERAS",
                       "Y099" = "MEDIOS NO ESPECIFICADOS, EN LUGAR NO ESPECIFICADO",
                       "X955" = "ARMAS DE FUEGO, EN COMERCIO Y AREA SERVICIOS"
    )
  ) %>%
  arrange(SEXO, desc(homicidios)) %>%
  group_by(SEXO) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = SEXO,
    values_from = c(homicidios, CAUSA_DEF),
    names_glue = "{SEXO}_{.value}"
  ) %>%
  select(
    Hombres = Hombres_homicidios,
    Causas_H = Hombres_CAUSA_DEF,
    Mujeres = Mujeres_homicidios,
    Causas_M = Mujeres_CAUSA_DEF
  ) %>%
  rename(
    `Total homicidios hombres` = Hombres,
    `Causas de presunto homicidio` = Causas_H,
    `Total homicidios mujeres` = Mujeres,
    `Causas de presunto homicidio  ` = Causas_M
  )

## 2.3 Crear cuadro para presentar (html)----

cuadro_causa <- gt(df_CS_sep) %>%
  tab_header(
    title = "Causas principales de homicidio en México, 2024",
    subtitle = "Comparación entre hombres y mujeres",
    ) %>% 
  tab_footnote(footnote = "Fuente: Estadísticas de Defunciones Registradas, INEGI, 2024."
)

# 3. OCUPACIÓN Y HOMICIDIO ----
# Ocupaciones que tienen los registros más frecuentes de presunto homicidio, para hombres y mujeres

## 3.1 data frame de ocupaciones por sexo ----
## Se muestran las 6 ocupaciones más frecuentes con presuntos homicidios registrados para cada sexo

df_ocu <- edr24 %>%
  filter(
    TIPO_DEFUN == 2,
    SEXO %in% c(1, 2),    
    !is.na(OCUPACION),
    OCUPACION != 998,
    OCUPACION != 999,
    OCUPACION != 997
  ) %>%
  group_by(SEXO, OCUPACION) %>%
  summarise(
    homicidios = n(),
    .groups = "drop"
  ) %>%
  group_by(SEXO) %>%
  arrange(desc(homicidios)) %>%
  slice_head(n = 6) %>%
  ungroup() %>%
  mutate(
    SEXO = recode(SEXO,
                  `1` = "Hombres",
                  `2` = "Mujeres")
  )

## 3.2 data frame con columnas separadas para hombres y mujeres ----
## renombrar ocupaciones con base en SINCO y CIUO-08

df_ocu_sep <- df_ocu %>%
  mutate(
    OCUPACION = recode(as.character(OCUPACION),
                       "110" = "Oficiales de las fuerzas armadas",
                       "041" = "Comerciantes en establecimientos",
                       "071" = "Trabajadores en la extracción y la edificación de construcciones",
                       "061" = "Trabajadores en actividades agrícolas y ganaderas",
                       "083" = "Conductores de transporte y de maquinaria móvil",
                       "026" = "Auxiliares y técnicos en ciencias exactas, biológicas, ingeniería, informática y en telecomunicaciones",
                       "096" = "Trabajadores domésticos, de limpieza, planchadores y otros trabajadores de limpieza",
                       "052" = "Trabajadores en cuidados personales y del hogar",
                       "051" = "Trabajadores en la preparación y servicio de alimentos y bebidas, así como en servicios de esparcimiento y de hoteleríaM"
    )
  ) %>%
  arrange(SEXO, desc(homicidios)) %>%
  group_by(SEXO) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    names_from = SEXO,
    values_from = c(homicidios, OCUPACION),
    names_glue = "{SEXO}_{.value}"
  ) %>%
  select(
    Hombres = Hombres_homicidios,
    Causas_H = Hombres_OCUPACION,
    Mujeres = Mujeres_homicidios,
    Causas_M = Mujeres_OCUPACION
  ) %>%
  rename(
    `Total homicidios hombres` = Hombres,
    `Ocupación del fallecido` = Causas_H,
    `Total homicidios mujeres` = Mujeres,
    `Ocupación de la fallecida  ` = Causas_M
  )

## 3.3 Treemap ----

##Hombres
df_treemap_h <- df_ocu_sep %>%
  select(
    Ocupacion = `Ocupación del fallecido`,
    Homicidios = `Total homicidios hombres`
  ) %>%
  filter(!is.na(Ocupacion))


ggplot(
  df_treemap_h,
  aes(
    area = Homicidios,
    fill = Ocupacion,
    label = paste0(Ocupacion, "\n", Homicidios)
  )
) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    reflow = TRUE,
    min.size = 3
  ) +
  labs(
    title = "Ocupaciones con más registros de presuntos homicidios en hombres, 2024",
    caption = "Fuente: Estadísticas de Defunciones Registradas, INEGI, 2024"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    )
  )



##Mujeres
df_treemap_m <- df_ocu_sep %>%
  select(
    Ocupacion = `Ocupación de la fallecida  `,
    Homicidios = `Total homicidios mujeres`
  ) %>%
  filter(!is.na(Ocupacion))


ggplot(
  df_treemap_m,
  aes(
    area = Homicidios,
    fill = Ocupacion,
    label = paste0(Ocupacion, "\n", Homicidios)
  )
) +
  geom_treemap() +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    reflow = TRUE,
    min.size = 3
  ) +
  labs(
    title = "Ocupaciones con más registros de presuntos homicidios en mujeres, 2024",
    caption = "Fuente: Estadísticas de Defunciones Registradas, INEGI, 2024"
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      hjust = 0.5,
      size = 15
    )

  )
