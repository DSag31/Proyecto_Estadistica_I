# Asegúrate de tener arrow instalado
install.packages("arrow")  # Solo si no lo habías hecho antes

# Cargar la librería
library(arrow)

# Leer el archivo .parquet
df <- read_parquet("C:/datos_filtrados.parquet")

head(df)       # Muestra las primeras filas
str(df)        # Estructura de columnas y tipos
summary(df)    # Resumen estadístico básico
