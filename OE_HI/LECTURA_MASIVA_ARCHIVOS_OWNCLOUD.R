# Limpiar el espacio de trabajo
rm(list = ls(all.names = TRUE))

library(readr)
library(dplyr)
library(purrr)
library(glue)
library(lubridate)
library(stringr)
library(RPostgreSQL)
library(openxlsx)

# Conexión a PostgreSQL:
postgres <- dbConnect(
    RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",  # Cambiar según configuración
    port = 5432,         # Puerto de PostgreSQL
    user = "postgres",   # Usuario de PostgreSQL
    password = "marce"  # Contraseña de PostgreSQL
)

cargar_archivos <- function(file_list) {
    resultados <- list()  # Lista para almacenar los data frames procesados
    
    for (i in seq_along(file_list$id)) {
        file_id <- file_list$id[i]
        nombre_archivo <- file_list$name[i]
        
        # Extraer identificador único del archivo (últimos 4 dígitos o nombre base)
        tipo_coip <- str_extract(nombre_archivo, "\\d{4}$")
        if (is.na(tipo_coip)) {
            tipo_coip <- str_remove(nombre_archivo, "\\.xlsx$")
        }
        
        # Descargar archivo temporalmente
        temp_file <- tempfile(fileext = ".xlsx")
        drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
        
        # Leer nombres de hojas
        nombres_hojas <- openxlsx::getSheetNames(temp_file)
        if (length(nombres_hojas) == 0) {
            warning(paste("El archivo no contiene hojas accesibles:", nombre_archivo))
            unlink(temp_file)
            next
        }
        
        # Filtrar hojas con nombres que contienen 4 dígitos
        hojas_validas <- nombres_hojas[grepl("\\d{4}", nombres_hojas)]
        if (length(hojas_validas) == 0) {
            warning(paste("No se encontraron hojas válidas en el archivo:", nombre_archivo))
            unlink(temp_file)
            next
        }
        
        # Combinar datos de hojas válidas en un solo data frame
        df_combined <- map_dfr(hojas_validas, function(hoja) {
            df <- openxlsx::read.xlsx(temp_file, sheet = hoja, colNames = TRUE)
            df %>%
                mutate(
                    anio_coip = hoja,
                    tipo_coip = tipo_coip
                )
        })
        
        # Guardar el data frame en la lista con un nombre específico
        nombre_df <- paste0("da_", tipo_coip)
        resultados[[nombre_df]] <- df_combined
        
        # Eliminar archivo temporal
        unlink(temp_file)
    }
    
    return(resultados)  # Devolver la lista de data frames
}

# Llamar a la función para cargar los archivos
dataframes <- cargar_archivos_homicidios(url_base, fecha_inicio, fecha_fin)

# Función para hacer rbind y manejar columnas faltantes
rbind_multiple <- function(...) {
    # Listar todos los data frames que se pasan a la función
    dfs <- list(...)
    
    # Encontrar todas las columnas únicas presentes en todos los data frames
    todas_columnas <- unique(unlist(lapply(dfs, colnames)))
    
    # Asegurar que cada data frame tenga todas las columnas, llenando con NA donde falten
    dfs_completados <- lapply(dfs, function(df) {
        # Añadir cualquier columna faltante como NA
        faltantes <- setdiff(todas_columnas, colnames(df))
        df[faltantes] <- NA
        return(df)
    })
    
    # Usar bind_rows para combinar todos los data frames
    return(bind_rows(dfs_completados))
}

df_hi_14_23 <- read.xlsx("OE_HI/DB/mdi_homicidios_intencionales_pm_2014_2023.xlsx",
                         sheet = "MDI_HomicidiosIntencionales_PM", 
                         detectDates = T)

df_hi_2024 <- read.xlsx("OE_HI/DB/mdi_homicidiosintencionales_pm_2024_enero-septiembre.xlsx",
                        sheet = "MDI_HomicidiosIntencionales_PM", 
                        detectDates = T)

df_hi_total <- rbind(df_hi_14_23, df_hi_2024)


dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_total"),
    value = df_hi_total,
    overwrite = TRUE,
    row.names = FALSE
)

# Usar do.call para aplicar la función a la lista de dataframes
df_cs_hi <- do.call(rbind_multiple, df_hi_total)


# Guarda la tabla df_hi_map en PostgreSQL
dbWriteTable(
    conn = postgres,
    name = DBI::Id(schema = "data_lake",
                   table = "hi_total"),
    value = df_hi_total,
    overwrite = TRUE,
    row.names = FALSE
)