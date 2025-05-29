# Proyecto_Estadistica_I
Proyecto de Estadística Integración de datos y estimación estadística de víctimas en el marco del conflicto armado
## IMPORTANTE
Si se desea que los archivos R script y el R markdown funcione, es necesario mover la carpeta Datos_limpios al disco local C/: 
## Explicacion de las Carpetas
### Victimas_Conflicto_Armado_Datos:
En esta carpeta se almacenan los datos Base sobre los cuales se va a trabajar, estos datos tienen formato .parquet, son los datos completos obtenidos directamente de la pagina https://microdatos.dane.gov.co/index.php/catalog/795 sobra decir que estos datos vienen de manera un poco desorganizada y un poco confusa

### R
En esta capeta se encuentran 3 subcarpetas:
- Codigos_R_Problematica: Esta carpeta contiene los codigo R funcionales para que muestren graficas y resultados, archivos que se van a tratar en la problematica como lo son Desapariciones, Homicidios, Reclutamiento y secuestros
- R Markdown: Esta capeta contiene los archivos para mostrar un informe en tipo .html, un informe bien estructurado y visual donde se muestran los datos
- R para modificar el parquet: Una carpeta no tan importante, pero escencial para modificar los datos originales, es decir los que estaban en la capeta Victimas_Conflicto_Armado_Datos
