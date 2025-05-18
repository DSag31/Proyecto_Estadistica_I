#install.packages(c("arrow", "dplyr", "ggplot2"))
# Instalar librerías si hace falta
install.packages(c("arrow","dplyr","ggplot2","readr","sf", "tidyr", "RColorBrewer","kableExtra","scales","knitr"))  # si no se tienen
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
library(knitr)
##################################Pregunta 1#########################################################
#Análisis del número de afectados entre los años 1985 a 2018 (homicidios)
#Gráfica : Histograma.

# 1. Leer el archivo Parquet de homicidios
datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet") 

# 2. Filtrar el rango de años de interés y contar víctimas por año
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
#Mapa de calor: Referencias de los departamentos afectados por  la problemática. ¿Qué departamento es el más afectado?¿En qué año se pudo ver más activa la violencia en los departamentos
# 1. Lee los datos de homicidios y cuenta por departamento
ddatos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet")

casos_dept <- datos %>%
  mutate(dept_code_hecho = sprintf("%02d", as.integer(dept_code_hecho))) %>%
  group_by(dept_code_hecho) %>%
  summarise(n_victimas = n(), .groups = "drop")

# 2. Lee el GeoPackage de departamentos de Colombia
gdf_dept <- st_read("C:/Datos_limpios/gadm41_COL.gpkg", layer = "ADM_ADM_1") %>%
  st_transform(4326)

# 3. Diccionario nombre → código DANE
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

# 4. Asigna el código a cada polígono
#    Sustituye NAME_1 por el nombre correcto de la columna de tu GPKG
gdf_dept <- gdf_dept %>%
  mutate(dept_code_hecho = nombre_codigos[as.character(NAME_1)])

# 5. Une y completa NA con cero
map_data <- gdf_dept %>%
  left_join(casos_dept, by = "dept_code_hecho") %>%
  replace_na(list(n_victimas = 0))

# 6. Dibuja el mapa

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
##################################Pregunta 3#########################################################
#¿Qué tal está la situación de Bucaramanga? (mapa de calor de los municipios de santander, enfocándonos en la región de santander)
# 1. Lee los datos de homicidios y cuenta por municipio
datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet")

casos_mun <- datos %>%
  mutate(muni_code_hecho = sprintf("%05d", as.integer(muni_code_hecho))) %>%
  group_by(muni_code_hecho) %>%
  summarise(n_victimas = n(), .groups = "drop")

# 2. Lee la capa de municipios de Colombia y filtra solo Santander
gdf_mun <- st_read("C:/Datos_limpios/gadm41_COL.gpkg", layer = "ADM_ADM_2") %>%
  st_transform(4326)

# Asegúrate de que en tu GPKG la columna del departamento se llame NAME_1
gdf_santander <- gdf_mun %>%
  filter(NAME_1 == "Santander")

# 3. Diccionario nombre de municipio → código DANE de 5 dígitos
nombre_codigos <- c(
  "Bucaramanga"               = "68001",
  "Aguada"                    = "68013",
  "Albania"                   = "68020",
  "Aratoca"                   = "68051",
  "Barbosa"                   = "68077",
  "Barichara"                 = "68079",
  "Barrancabermeja"           = "68081",
  "Betulia"                   = "68092",
  "Bolívar"                   = "68101",
  "Cabrera"                   = "68121",
  "California"                = "68132",
  "Capitanejo"                = "68147",
  "Carcasí"                   = "68152",
  "Cepitá"                    = "68160",
  "Cerrito"                   = "68162",
  "Charalá"                   = "68167",
  "Charta"                    = "68169",
  "Chima"                     = "68176",
  "Chipatá"                   = "68179",
  "Cimitarra"                 = "68190",
  "Concepción"                = "68207",
  "Confines"                  = "68209",
  "Contratación"              = "68211",
  "Coromoro"                  = "68217",
  "Curití"                    = "68229",
  "El Carmen de Chucurí"      = "68235",
  "El Guacamayo"              = "68245",
  "El Peñón"                  = "68250",
  "El Playón"                 = "68255",
  "Encino"                    = "68264",
  "Enciso"                    = "68266",
  "Florián"                   = "68271",
  "Floridablanca"             = "68276",
  "Galán"                     = "68296",
  "Gámbita"                   = "68298",
  "Girón"                     = "68307",
  "Guaca"                     = "68318",
  "Guadalupe"                 = "68320",
  "Guapotá"                   = "68322",
  "Guavatá"                   = "68324",
  "Güepsa"                    = "68327",
  "Hato"                      = "68344",
  "Jesús María"               = "68368",
  "Jordán"                    = "68370",
  "La Belleza"                = "68377",
  "Landázuri"                 = "68385",
  "La Paz"                    = "68397",
  "Lebrija"                   = "68406",
  "Los Santos"                = "68418",
  "Macaravita"                = "68425",
  "Málaga"                    = "68432",
  "Matanza"                   = "68444",
  "Mogotes"                   = "68464",
  "Molagavita"                = "68468",
  "Ocamonte"                  = "68498",
  "Oiba"                      = "68500",
  "Onzaga"                    = "68502",
  "Palmar"                    = "68522",
  "Palmas del Socorro"        = "68524",
  "Páramo"                    = "68533",
  "Piedecuesta"               = "68547",
  "Pinchote"                  = "68549",
  "Puente Nacional"           = "68572",
  "Puerto Parra"              = "68573",
  "Puerto Wilches"            = "68575",
  "Rionegro"                  = "68615",
  "Sabana de Torres"          = "68655",
  "San Andrés"                = "68669",
  "San Benito"                = "68673",
  "San Gil"                   = "68679",
  "San Joaquín"               = "68682",
  "San José de Miranda"       = "68684",
  "San Miguel"                = "68686",
  "San Vicente de Chucurí"    = "68689",
  "Santa Bárbara"             = "68705",
  "Santa Helena del Opón"     = "68720",
  "Simacota"                  = "68745",
  "Socorro"                   = "68755",
  "Suaita"                    = "68770",
  "Sucre"                     = "68773",
  "Surata"                    = "68780",
  "Tona"                      = "68820",
  "Valle de San José"         = "68855",
  "Vélez"                     = "68861",
  "Vetas"                     = "68867",
  "Villanueva"                = "68872",
  "Zapatoca"                  = "68895"
)

# 4. Asigna el código DANE a cada municipio (columna NAME_2 en tu GPKG)
gdf_santander <- gdf_santander %>%
  mutate(muni_code_hecho = nombre_codigos[as.character(NAME_2)])

# 5. Une los conteos y rellena con cero donde falte
map_data <- gdf_santander %>%
  left_join(casos_mun, by = "muni_code_hecho") %>%
  replace_na(list(n_victimas = 0))

# 6. Dibuja el mapa sólo de Santander, con escala invertida (rojo = más víctimas)
ggplot(map_data) +
  geom_sf(aes(fill = n_victimas), color = "gray80", size = 0.2) +
  scale_fill_gradientn(
    name     = "N° víctimas",
    colours  = rev(rainbow(10)),  # colores invertidos, extremos rojos
    na.value = "white"
  ) +
  labs(
    title = "Mapa de calor de homicidios en municipios de Santander\n(1985–2018)"
  ) +
  theme_void() +
  theme(
    plot.title     = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "right"
  )


##################################Pregunta 4#########################################################
#¿Qué registros de municipios (en Colombia ) principalmente están afectados  (mínimo 5)?
datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet")
# 1. Asegurar tipo numérico
datos <- datos %>%
  mutate(muni_code_hecho = as.numeric(muni_code_hecho))

# 2. Top 5 municipios con más homicidios
top_municipios <- datos %>%
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
#De que edad a que edad han estado las personas mas afectadas (mayor frecuencia absoluta del intervalo)
  #a. la infancia van de 0 a 14, los adolescentes de 15 a 19,  adultez desde los 20 en adelante

# Contar la frecuencia absoluta de cada categoría de edad
conteo_edades <- datos %>%
  count(edad_categoria, sort = TRUE)

# Ver las primeras filas para encontrar la categoría con más homicidios
print(conteo_edades)

# Si deseas mostrar solo la categoría más frecuente (el modo)
edad_mas_afectada <- conteo_edades %>%
  slice_max(n, n = 1)

print(edad_mas_afectada)

ggplot(conteo_edades, aes(x = reorder(edad_categoria, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Frecuencia de homicidios por categoría de edad",
       x = "Rango de Edad",
       y = "Cantidad de Homicidios") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################Pregunta 6#########################################################
#Que tipo de etnias existen y cuales han sido las más afectadas
# 1. Leer los datos
datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet")

# 2. Agrupar por etnia y calcular porcentaje preciso
conteo_etnias <- datos %>%
  filter(!is.na(etnia) & etnia != "") %>%
  count(etnia, sort = TRUE) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = paste0(etnia, " (", percent(porcentaje, accuracy = 0.001), ")")  # Precisión de 0.001%
  )

# 3. Tabla de resumen con porcentaje con 3 decimales
tabla_etnias <- conteo_etnias %>%
  mutate(porcentaje = round(porcentaje * 100, 3)) %>%
  rename(
    "Grupo Étnico" = etnia,
    "Número de Homicidios" = n,
    "Porcentaje (%)" = porcentaje
  )

# Mostrar tabla en consola
kable(tabla_etnias, format = "markdown", align = "c")

# 4. Diagrama de pastel con etiquetas precisas
ggplot(conteo_etnias, aes(x = "", y = n, fill = etnia)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = etiqueta), position = position_stack(vjust = 0.5), size = 3) +
  labs(
    title = "Distribución de homicidios por grupo étnico (Pastel)",
    fill = "Etnia"
  ) +
  theme_void() +
  theme(legend.position = "none")

# 5. Diagrama de barras con porcentaje más exacto
ggplot(conteo_etnias, aes(x = reorder(etnia, -n), y = n, fill = etnia)) +
  geom_col() +
  geom_text(aes(label = percent(porcentaje, accuracy = 0.001)), vjust = -0.5) +
  labs(
    title = "Número de homicidios por grupo étnico (Barras)",
    x = "Grupo Étnico",
    y = "Número de Homicidios"
  ) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylim(0, max(conteo_etnias$n) * 1.1)
  #a. Cuantos casos estan asociados con desplazamiento forzado (relacion de la columna is_forced_dis)
##################################Pregunta 7#########################################################
#¿Qué guerrilla es la que está afectando más a estos ciudadanos?
# 1. Leer datos
datos <- read_parquet("C:/Datos_limpios/datosLimpios-homicidio-R100.parquet")

# 2. Contar casos por guerrilla (columna p_str)
conteo_guerrilla <- datos %>%
  filter(!is.na(p_str) & p_str != "") %>%
  count(p_str, sort = TRUE)

# 3. Mostrar tabla con conteo total por guerrilla
print(conteo_guerrilla)

# 4. Graficar top 10 guerrillas que más afectan (por número de casos)
top10_guerrilla <- head(conteo_guerrilla, 10)

ggplot(top10_guerrilla, aes(x = reorder(p_str, n), y = n, fill = p_str)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Guerrillas que más afectan a los ciudadanos",
    x = "Guerrilla",
    y = "Número de casos"
  ) +
  theme_minimal()
##################################Pregunta 8#########################################################
#¿Años en que mister violencia afecto mas a estas comunidades? cual fue el año que en teoria existio mas tranquilidad entre los ciudadanos

##################################Pregunta 9#########################################################
#En un promedio general, que se ven mas afectados, los hombres o las mujeres?

##################################Pregunta 10#########################################################
#¿Existen meses del año con picos recurrentes de víctimas? (osea hacer el analisis de los meses desde el 85 al 2018 donde se han presentado mas delitos (los meses mas movidos ) 

##################################Pregunta 11#########################################################
#¿En cuales departamentos se vieron   más afectados infancia/adolescencia//adultez de cada problematica?

##################################Pregunta 12#########################################################
#¿La distribución de las edades esta sesgada mas a la parte de jóvenes o adultos?

##################################Pregunta 13#########################################################
#Si conviertes edad categoría a numérica, ¿cuál es la edad promedio de las víctimas?
  #a. en promedio, que edades han sido afectadas
  #b. cual es la edad más afectada (moda)
  #c. varianza y desviacion estandar
  #d. Aplicando los cuartiles, ¿que podemos deducir?
  #e. y su sesgo?
  #f. que hay de su kurtosis?
