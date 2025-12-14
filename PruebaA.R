###CÓDIGO REALIZADO POR PALOMA GUEVARA (apguevara.ap@gmail.com) CON DATOS EXTRAÍDOS A PARTIR DEL REPOSITORIO DE IRVING MORALES (https://github.com/irvingfisica/reqrnpdno/tree/master) PARA EL WEB SCRAPING DE LA VERSIÓN PÚBLICA RNPDNO (https://versionpublicarnpdno.segob.gob.mx/Dashboard/Index), CON UN NIVEL DE EXTRACCIÓN POR ESTADOS. 
###PARTE A DE LA PRUEBA TÉCNICA PARA LA VACANTE COMO ANALISTA DE DATOS SR. DE DATA CÍVICA, DICIEMBRE DE 2025.

rm(list = ls())
setwd("C:/Users/paomy/OneDrive/Escritorio/PruebaTecnica_Guevara")
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# 1. EXTRACCIÓN Y LIMPIEZA DE DATOS ----

## 1.1 Carpeta y df de estados ----
carpeta <- "C:/Users/paomy/OneDrive/Escritorio/PruebaTecnica_Guevara/reqrnpdno/salida/estados/"
archivos <- list.files(carpeta, pattern = "\\.json$", full.names = TRUE)
lista_estados <- map(archivos, fromJSON)

## 1.2 Renombrar cada estado ----
names(lista_estados) <- gsub("\\.json$", "", basename(archivos))
nombres_estados <- c(
  "0"  = "NACIONAL",
  "1"  = "AGUASCALIENTES",
  "2"  = "BAJA CALIFORNIA",
  "3"  = "BAJA CALIFORNIA SUR",
  "4"  = "CAMPECHE",
  "5"  = "COAHUILA",
  "6"  = "COLIMA",
  "7"  = "CHIAPAS",
  "8"  = "CHIHUAHUA",
  "9"  = "CIUDAD DE MEXICO",
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
  "33" = "INDETERMINADO"
)
names(lista_estados) <- nombres_estados[names(lista_estados)]

#2. BASE CON DATOS PRINCIPALES: EDAD Y ENTIDAD ----

## 2.1 Función para extraer datos de entidad por edad simple ----
edadXentidad <- function(entidad_lista, nombre_entidad) {
  edad_data <- entidad_lista$por_edad #extraer edad
  df <- imap(edad_data, ~ #convertir sexo a tibble
               tibble(
                 edad = names(.x),
                 total = unlist(.x),
                 sexo = .y
               )
  ) %>% 
    list_rbind()
  df <- df %>% mutate( #edad como entero
    edad = as.integer(edad),
    entidad = nombre_entidad
  )
  return(df)
}

## 2.2 Aplicar función a cada estado ----
df_edad_edo <- imap(lista_estados, edadXentidad) %>%
  list_rbind()

## 2.3 Crear grupos de edad ----
df_edad_edo <- df_edad_edo %>%
  mutate(
    grupo_edad = case_when(
      edad >= 0  & edad <= 9  ~ "Infancias (0-9)",
      edad >= 10 & edad <= 19 ~ "Adolescentes (10-19)",
      edad >= 20 & edad <= 35 ~ "Jovenes (20-35)",
      edad >= 36 & edad <= 59 ~ "Adultos (36-59)",
      edad >= 60              ~ "Adultos mayores (60+)",
      TRUE                    ~ NA_character_
    )
  )

## 2.4 Ordenar por grupos de edad ----
df_edad_edo <- df_edad_edo %>%
  mutate(
    grupo_edad = factor(
      grupo_edad,
      levels = c(
        "Infancias (0-9)",
        "Adolescentes (10-19)",
        "Jovenes (20-35)",
        "Adultos (36-59)",
        "Adultos mayores (60+)"
      )
    )
  ) %>%
  arrange(entidad, grupo_edad, sexo)

# 3. DATA FRAME FINAL Y EVALUACIÓN ----

## 3.1 df final ----
df_final <- df_edad_edo %>%
  group_by(entidad, sexo, grupo_edad) %>%
  summarise(total = sum(total), .groups = "drop")

## 3.2 Evaluación de datos ----
# Para un posterior manejo de datos, es necesario saber el volumen de los registros perdidos, por lo que nos preguntamos cuántos estados tienen registros con sexo indeterminado.
df_indeterminado <- df_final %>% 
  filter(
    sexo == "Indeterminado",
    total != 0,
  ) %>% 
  select(entidad, sexo, grupo_edad, total)
# Los resultados indican que, a nivel nacional, hay únicamente 17 personas desaparecidas sin sexo determinado, de las cuales: 4 se registraron en la CDMX, 1 en Hidalgo, 1 en Michoacán, 1 en Puebla, 1 en Sonora y 9 en Tabasco.  

#En segundo lugar, nos preguntamos cuántas personas sin edad registrada hay por entidad.
df_NA <- df_final %>% 
  filter(
    is.na(grupo_edad),
    total != 0,
  ) %>% 
  select(entidad, sexo, grupo_edad, total)
# Los resultados indican que, a nivel nacional, hay 791 personas desaparecidas sin edad registrada, de las cuales 422 son hombres, 8 no tienen sexo determinado y 361 son mujeres. Además, destaca el caso de Tabasco: sólo esta entidad concentra 227 hombres sin edad registrada, 241 mujeres sin edad registrada y a las 8 personas con sexo indeterminado sin edad registrada. 

## 3.3 Guardar como CSV ----

write.csv(
  df_final,
  "C:/Users/paomy/OneDrive/Escritorio/PruebaTecnica_Guevara/df_final.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

