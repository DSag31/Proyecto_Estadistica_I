install.packages("arrow")
library(arrow)
library(dplyr) #Util para eliminar columnas

datos <- read_parquet("C:/verdata-desaparicion-R100.parquet") #Leemos el archivo el cual queremos modificar 

# Definir columnas a conservar
columnas_a_conservar <- c(
  "replica", "match_group_id", "dept_code_hecho", "edad_categoria", 
  "edad_jep", "etnia", "is_conflict", "is_forced_dis", 
  "muni_code_hecho", "p_str", "sexo", "yy_hecho", "yymm_hecho"
)


#Esta funcion elimina todas las columnas exceptuando las de la variable columnas_a_conservar
datos_filtrados <- datos %>% 
  select(all_of(columnas_a_conservar)) 

# Guardar el nuevo archivo limpio
write_parquet(datos_filtrados, "C:/datos_filtrados.parquet")

