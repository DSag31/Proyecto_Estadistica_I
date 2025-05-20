#install.packages(c("arrow", "dplyr", "ggplot2"))
# Instalar librerías si hace falta
install.packages(c("arrow","dplyr","ggplot2","readr","sf", "tidyr", "RColorBrewer","kableExtra","scales","knitr","lubridate","viridis","stringr"))  # si no se tienen
# Cargar librerías importantes
library(arrow)
library(dplyr) #Util para eliminar columnas
library(ggplot2) # para graficar
library(readr)
library(sf) 
library(tidyr)    
library(RColorBrewer)  # para paletas extra
library(kableExtra)
library(scales)
library(viridis)
library(knitr)
library(lubridate)
library(stringr)
##################################Carga de Datos#########################################################

datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet") 


##################################Pregunta 1#########################################################
#Análisis del número de afectados entre los años 1985 a 2018 (homicidios)
#Gráfica : Histograma.


# 1. Filtrar el rango de años de interés y contar víctimas por año
df_por_ano <- datos %>%
  filter(yy_hecho >= 1985, yy_hecho <= 2018) %>%
  group_by(yy_hecho) %>%
  summarise(n_victimas = n(), .groups = "drop")


# datos %>%:
# Inicia una secuencia de comandos usando el operador pipe (%>%),
# que permite leer y aplicar comandos en orden.

# filter(yy_hecho >= 1985, yy_hecho <= 2018):
# Filtra los registros del DataFrame datos.
# 
# - Condición: Solo mantiene las filas donde el año (yy_hecho) 
#   está entre 1985 y 2018.
# - Esto asegura que solo analizamos los datos del periodo de interés.

# group_by(yy_hecho):
# Agrupa los datos por año (yy_hecho).

# summarise(n_victimas = n(), .groups = "drop"):
# Para cada año (yy_hecho), cuenta el número de filas (número de víctimas).
# 
# - El resultado es una tabla con dos columnas:
#   - yy_hecho: El año.
#   - n_victimas: El número total de víctimas en ese año.
# - .groups = "drop" evita que el resultado sea un objeto agrupado 


# 2. Crear el histograma (barras por año)
ggplot(df_por_ano, aes(x = yy_hecho, y = n_victimas)) +
  geom_col(width = 0.8) +
  scale_x_continuous(breaks = seq(1985, 2018, by = 1)) +
  labs(
    title    = "Número de víctimas de homicidio por año (1985–2018)",
    x        = "Año",
    y        = "Número de víctimas"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    panel.grid.minor = element_blank()
  )
##################################Pregunta 2#########################################################
#Mapa de calor: Referencias de los departamentos afectados por  la problemática. ¿Qué departamento es el más afectado?¿En qué año se pudo ver más activa la violencia en los departamentos

casos_dept <- datos %>%
  mutate(dept_code_hecho = sprintf("%02d", as.integer(dept_code_hecho))) %>%
  group_by(dept_code_hecho) %>%
  summarise(n_victimas = n(), .groups = "drop")

# 1. Lee el GeoPackage de departamentos de Colombia
gdf_dept <- st_read("C:/Datos_limpios/gadm41_COL.gpkg", layer = "ADM_ADM_1") %>%
  st_transform(4326)

# 2. Diccionario nombre → código DANE
nombre_codigos <- c(
  "Amazonas"                     = "91",
  "Antioquia"                    = "05",
  "Arauca"                       = "81",
  "Atlántico"                    = "08",
  "Bogotá D.C."                  = "11",
  "Bolívar"                      = "13",
  "Boyacá"                       = "15",
  "Caldas"                       = "17",
  "Caquetá"                      = "18",
  "Casanare"                     = "85",
  "Cauca"                        = "19",
  "Cesar"                        = "20",
  "Chocó"                        = "27",
  "Córdoba"                      = "23",
  "Cundinamarca"                 = "25",
  "Guainía"                      = "94",
  "Guaviare"                     = "95",
  "Huila"                        = "41",
  "La Guajira"                   = "44",
  "Magdalena"                    = "47",
  "Meta"                         = "50",
  "Nariño"                       = "52",
  "Norte de Santander"           = "54",
  "Putumayo"                     = "86",
  "Quindío"                      = "63",
  "Risaralda"                    = "66",
  "San Andrés y Providencia"     = "88",
  "Santander"                    = "68",
  "Sucre"                        = "70",
  "Tolima"                       = "73",
  "Valle del Cauca"              = "76",
  "Vaupés"                       = "97",
  "Vichada"                      = "99"
)

# 3. Asigna el código a cada polígono
#    Sustituye NAME_1 por el nombre correcto de la columna de tu GPKG
gdf_dept <- gdf_dept %>%
  mutate(dept_code_hecho = nombre_codigos[as.character(NAME_1)])

# 4. Une y completa NA con cero
map_data <- gdf_dept %>%
  left_join(casos_dept, by = "dept_code_hecho") %>%
  replace_na(list(n_victimas = 0))

# 5. Dibuja el mapa

ggplot(map_data) +
  geom_sf(aes(fill = n_victimas), color = "gray80", size = 0.2) +
  
  # ←   DESCOMENTA UNA DE ESTAS 3 OPCIONES DE ESCALA   ←

  #scale_fill_viridis_c(name = "N° víctimas", option = "magma", direction = -1) +

  scale_fill_gradientn(
    name = "N° víctimas",
    colours = rev(rainbow(10)),    # 10 colores del arcoíris
    na.value = "white"
  ) +
  
  labs(
    title    = "Mapa de calor victimas de homicidio por departamento (1985–2018)"
  ) +
  theme_void() +
  theme(
    plot.title    = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "right"
  )
