#install.packages(c("arrow", "dplyr", "ggplot2"))
library(arrow)
library(dplyr) #Util para eliminar columnas
library(ggplot2) # para graficar
library(readr)

##################################Pregunta 1#########################################################
#Análisis del número de afectados entre los años 1985 a 2018 (homicidios)
#Gráfica : Histograma.

# 1. Leer el archivo Parquet de homicidios
df_hom <- read_parquet("C:/datosLimpios/datosLimpios-homicidio-R100.parquet") 

# 2. Filtrar el rango de años de interés y contar víctimas por año
df_por_ano <- df_hom %>%
  filter(yy_hecho >= 1985, yy_hecho <= 2018) %>%
  group_by(yy_hecho) %>%
  summarise(n_victimas = n(), .groups = "drop")


# df_hom %>%:
# Inicia una secuencia de comandos usando el operador pipe (%>%),
# que permite leer y aplicar comandos en orden.

# filter(yy_hecho >= 1985, yy_hecho <= 2018):
# Filtra los registros del DataFrame df_hom.
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


# 3. Crear el histograma (barras por año)
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



##################################Pregunta 4#########################################################
#¿Qué registros de municipios (en Colombia ) principalmente están afectados  (mínimo 5)?

# 1. Asegurar tipo numérico
df_hom <- df_hom %>%
  mutate(muni_code_hecho = as.numeric(muni_code_hecho))

# 2. Top 5 municipios con más homicidios
top_municipios <- df_hom %>%
  group_by(muni_code_hecho) %>%
  summarise(total_homicidios = n()) %>%
  arrange(desc(total_homicidios)) %>%
  slice_head(n = 5)

# 3. Leer archivo CSV de municipios
municipios_dane <- read_delim("C:/Datos_limpios/CodigosDaneDepartamentoMunicipio/Departamentos_Municipios.csv",
                              delim = ";", show_col_types = FALSE)

# 4. Limpiar código DANE del municipio
municipios_dane <- municipios_dane %>%
  mutate(`CÓDIGO DANE DEL MUNICIPIO` = gsub("\\.", "", `CÓDIGO DANE DEL MUNICIPIO`),
         `CÓDIGO DANE DEL MUNICIPIO` = trimws(`CÓDIGO DANE DEL MUNICIPIO`),
         `CÓDIGO DANE DEL MUNICIPIO` = as.numeric(`CÓDIGO DANE DEL MUNICIPIO`))

# 5. Join para obtener nombres de municipios
top_municipios_nombres <- top_municipios %>%
  left_join(municipios_dane, by = c("muni_code_hecho" = "CÓDIGO DANE DEL MUNICIPIO"))

# 6. Graficar resultados
ggplot(top_municipios_nombres, aes(x = reorder(MUNICIPIO, -total_homicidios), y = total_homicidios, fill = DEPARTAMENTO)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 5 municipios con más homicidios (1985–2018)",
    x = "Municipio",
    y = "Cantidad de homicidios"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
##################################Pregunta 5#########################################################


##################################Pregunta 6#########################################################


##################################Pregunta 7#########################################################


##################################Pregunta 8#########################################################


##################################Pregunta 9#########################################################


##################################Pregunta 10#########################################################


##################################Pregunta 11#########################################################


##################################Pregunta 12#########################################################


##################################Pregunta 13#########################################################
