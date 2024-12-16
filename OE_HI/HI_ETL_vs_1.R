rm(list=ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(RPostgreSQL)
<<<<<<< HEAD

df_hi <- read.xlsx("C:/Users/marcelochavez/Documents/MDI/VISUALIZADOR/ARCHIVOS/DATA_LAKE_MDI/DB/HI/HI_2014_2024.xlsx",
                   detectDates = T,
                   sheet = "MDI_HomicidiosIntencionales_PM")

# Función para limpiar nombres de columnas
remove_tildes_and_dots <- function(column_name) {
  # Reemplazar tildes (tanto en mayúsculas como minúsculas)
  column_name <- gsub('[Áá]', 'A', column_name)
  column_name <- gsub('[Éé]', 'E', column_name)
  column_name <- gsub('[Íí]', 'I', column_name)
  column_name <- gsub('[Óó]', 'O', column_name)
  column_name <- gsub('[Úú]', 'U', column_name)
  
  # Reemplazar la 'ñ' con 'ni' y 'Ñ' con 'NI'
  column_name <- gsub('ñ', 'ni', column_name)
  column_name <- gsub('Ñ', 'NI', column_name)
  
  # Reemplazar puntos (.) con guiones bajos (_) en todos los casos
  column_name <- gsub('\\.', '_', column_name)
  
  # Reemplazar espacios con guiones bajos (_) para mantener el formato limpio
  column_name <- gsub(' ', '_', column_name)
  
  # Eliminar guiones bajos repetidos generados por múltiples reemplazos
  column_name <- gsub('_+', '_', column_name)
  
  # Eliminar guiones bajos (_) que se encuentren al final del nombre de la columna
  column_name <- gsub('_$', '', column_name)
  
  # Convertir a minúsculas para uniformidad
  column_name <- tolower(column_name)
  
  return(column_name)
}

# Aplicar la función a los nombres de columnas
colnames(df_hi) <- sapply(colnames(df_hi), remove_tildes_and_dots)

df_hi <- df_hi %>%
  mutate(
    edad = trimws(edad),  # Eliminar espacios en blanco al inicio y al final
    edad = case_when(
      edad == "SIN_DATO" ~ 9999,                         # Reemplazar "SIN_DATO" por 9999
      grepl("^[0-9]+$", edad) ~ as.numeric(edad),        # Convertir solo si el valor es numérico
      TRUE ~ NA_real_                                    # Asignar NA a cualquier otro valor no reconocido
    )
  )

# Definir manualmente los tipos de datos
field_types <- c(
    "tipo_muerte" = "text",
    "zona" = "text",
    "subzona" = "text",
    "distrito" = "text",
    "circuito" = "text",
    "provincia" = "text",
    "codigo_provincia" = "integer",
    "canton" = "text",
    "codigo_canton" = "text",
    "area_hecho" = "text",
    "lugar" = "text",
    "tipo_lugar" = "text",
    "fecha_infraccion" = "date",
    "hora_infraccion" = "text",
    "arma" = "text",
    "tipo_arma" = "text",
    "presun_motiva" = "text",
    "presun_motiva_obser" = "text",
    "probable_causa_m" = "text",
    "edad" = "integer",
    "med_edad" = "text",
    "sexo" = "text",
    "genero" = "text",
    "etnia" = "text",
    "estado_civil" = "text",
    "nacionalidad" = "text",
    "discapacidad" = "text",
    "prof_reg_civ" = "text",
    "instruccion" = "text",
    "antecedentes" = "text"
)

# Conectar a PostgreSQL
con <- dbConnect(
=======
library(pool)

# Conexión a PostgreSQL:
postgres <- dbConnect(
>>>>>>> e994a1eeedcc7516534a9aa331380593e8c2fd80
    RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",  # Cambiar según configuración
    port = 5432,         # Puerto de PostgreSQL
    user = "postgres",   # Usuario de PostgreSQL
    password = "marce"  # Contraseña de PostgreSQL
)

<<<<<<< HEAD
# Guardar la tabla en PostgreSQL con los tipos de columnas especificados
dbWriteTable(
    con,
=======
dpa_2024 <- read.xlsx("CATALOGOS/CODIFICACION_2024.xlsx",
                      startRow = 2, 
                      cols = 2:ncol(read.xlsx("CATALOGOS/CODIFICACION_2024.xlsx")),
                      sheet = "PROVINCIAS") %>% 
    rename("codigo_provincia"="DPA_PROVIN",
           "provincia"="DPA_DESPRO")

df_hi <- read.xlsx("HI/DB/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                   detectDates = T,
                   sheet = "MDI_HomicidiosIntencionales_PM") %>% 
mutate(rango_edad = case_when(
    edad >= 0 & edad <= 10 ~ "0-10 años",
    edad >= 11 & edad <= 20 ~ "11-20 años",
    edad >= 21 & edad <= 30 ~ "21-30 años",
    edad >= 31 & edad <= 40 ~ "31-40 años",
    edad >= 41 & edad <= 50 ~ "41-50 años",
    edad >= 51 & edad <= 60 ~ "51-60 años",
    edad >= 61 & edad <= 70 ~ "61-70 años",
    edad >= 71 & edad <= 80 ~ "71-80 años",
    edad >= 81 ~ "81 años en adelante"
),
codigo_provincia = case_when(
    nchar(codigo_provincia) == 1 & codigo_provincia %in% 1:9 ~ paste0("0", as.character(codigo_provincia)),
    TRUE ~ as.character(codigo_provincia)),
total=1,
mayores_menores = ifelse(edad>=18,"Mayores de edad","Menores de edad"),
sexo = case_when(
  sexo == "HOMBRE" ~ "Hombres",
  sexo == "MUJER" ~ "Mujeres",
  TRUE ~ "Sin determinar" 
  )) %>% 
    select(-provincia)

df_hi <- merge(df_hi,
               dpa_2024,
               by="codigo_provincia")

df_hi_map <- df_hi %>%
    group_by(fecha_infraccion,
             provincia) %>% 
    summarise(total_hi = n(), .groups = "drop")

# Definir los tipos de datos
field_types <- c(
    "tipo_muerte" = "VARCHAR",                       # 1
    "zona" = "VARCHAR",                              # 2
    "subzona" = "VARCHAR",                           # 3
    "distrito" = "VARCHAR",                          # 4
    "circuito" = "VARCHAR",                          # 5
    "codigo_subcircuito" = "VARCHAR",                # 6
    "subcircuito" = "VARCHAR",                       # 7
    "provincia" = "VARCHAR",                         # 8
    "codigo_provincia" = "INTEGER",                  # 9
    "canton" = "VARCHAR",                            # 10
    "codigo_canton" = "VARCHAR",                     # 11
    "coordenada_y" = "FLOAT(2)",                     # 12
    "coordenada_x" = "FLOAT(2)",                     # 13
    "area_hecho" = "VARCHAR",                        # 14
    "lugar" = "VARCHAR",                             # 15
    "tipo_lugar" = "VARCHAR",                        # 16
    "fecha_infraccion" = "DATE",                     # 17
    "hora_infraccion" = "FLOAT(2)",                  # 18
    "arma" = "VARCHAR",                              # 19
    "tipo_arma" = "VARCHAR",                         # 20
    "presunta_motivacion" = "VARCHAR",               # 21
    "presunta_motivacion_observada" = "VARCHAR",     # 22
    "probable_causa_motivada" = "VARCHAR",           # 23
    "edad" = "FLOAT(2)",                             # 24
    "medida_edad" = "VARCHAR",                       # 25
    "sexo" = "VARCHAR",                              # 26
    "genero" = "VARCHAR",                            # 27
    "etnia" = "VARCHAR",                             # 28
    "estado_civil" = "VARCHAR",                      # 29
    "nacionalidad" = "VARCHAR",                      # 30
    "discapacidad" = "VARCHAR",                      # 31
    "profesional_registro_civil" = "VARCHAR",        # 32
    "instruccion" = "VARCHAR",                       # 33
    "antecedentes" = "VARCHAR",                      # 34
    "rango_edad" = "VARCHAR",                        # 35
    "total" = "numeric(0)"
)
    
# Guarda la tabla df_hi en PostgreSQL
dbWriteTable(
    conn = postgres,
>>>>>>> e994a1eeedcc7516534a9aa331380593e8c2fd80
    name = DBI::Id(schema = "data_lake",
                   table = "hi_2024"),
    value = df_hi,
    overwrite = TRUE,
<<<<<<< HEAD
    row.names = FALSE,
    field.types = field_types
)

# Cerrar la conexión
dbDisconnect(con)

source("C:/Users/marcelochavez/Documents/MDI/VISUALIZADOR/ARCHIVOS/DATA_LAKE_MDI/SCRIPTS/exploratorio.R")

df_exploratorio <- exploratorio(df_hi)












=======
    row.names = FALSE
)

# Guarda la tabla df_hi_map en PostgreSQL
dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_2024_map"),
    value = df_hi_map,
    overwrite = TRUE,
    row.names = FALSE
)

# Cerrar la conexión
dbDisconnect(postgres)
>>>>>>> e994a1eeedcc7516534a9aa331380593e8c2fd80
