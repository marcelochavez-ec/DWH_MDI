# Limpiar entorno
rm(list = ls(all.names = TRUE))

# Cargar librerías necesarias
library(googledrive)
library(openxlsx)
library(tidyverse)
library(stringr)
library(glue)

# Autenticación con Google Drive
drive_auth(email = "marcelo.chavez@interior.gob.ec")

# ID de la carpeta de Detenidos Aprehendidos
folder_id <- "1RbRmnjR5kHhpEK6O_b5MjYQ0Vc3clCS1"

# Listar los archivos con extensión .xlsx en la carpeta
file_list <- drive_ls(as_id(folder_id)) %>%
    filter(str_detect(name, "\\.xlsx$"))

# Verificar si hay archivos en la lista
if (nrow(file_list) == 0) {
    stop("No se encontraron archivos con extensión .xlsx en la carpeta.")
}

# Función para procesar cada archivo
procesar_archivo <- function(file_id, file_name) {
    # Extraer el año del nombre del archivo
    anio <- str_extract(file_name, "\\d{4}")
    if (is.na(anio)) {
        warning(glue("No se pudo extraer el año del archivo: {file_name}. Será omitido."))
        return(NULL)
    }
    
    # Descargar archivo temporalmente
    temp_file <- tempfile(fileext = ".xlsx")
    drive_download(as_id(file_id), path = temp_file, overwrite = TRUE)
    
    # Verificar si existe la hoja 'BDD_DA'
    nombres_hojas <- openxlsx::getSheetNames(temp_file)
    if (!"BDD_DA" %in% nombres_hojas) {
        warning(glue("El archivo {file_name} no contiene la hoja 'BDD_DA'. Será omitido."))
        unlink(temp_file)
        return(NULL)
    }
    
    # Leer la hoja 'BDD_DA'
    data <- openxlsx::read.xlsx(temp_file, sheet = "BDD_DA", colNames = TRUE)
    
    # Agregar columna con el año y devolver el data frame
    data <- data %>%
        mutate(anio = anio)
    
    # Eliminar archivo temporal
    unlink(temp_file)
    
    return(data)
}

# Procesar todos los archivos y combinarlos en una lista de data frames
resultados <- map2(file_list$id, file_list$name, procesar_archivo)

# Filtrar resultados no nulos y asignar nombres dinámicos a cada data frame
resultados <- resultados[!map_lgl(resultados, is.null)]
nombres_dfs <- map_chr(resultados, ~ glue("da_{unique(.x$anio)}"))

# Asignar cada data frame con un nombre dinámico
walk2(nombres_dfs, resultados, assign, envir = .GlobalEnv)




