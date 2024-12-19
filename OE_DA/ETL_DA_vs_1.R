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

# Listar los archivos dentro de la carpeta
file_list <- drive_ls(as_id(folder_id))

# Buscar el archivo específico
archivo_objetivo <- file_list %>% filter(name == "DETENIDOS_APREHENDIDOS_2016.xlsx")

# Verificar si el archivo fue encontrado
if (nrow(archivo_objetivo) == 0) {
    stop("El archivo 'DETENIDOS_APREHENDIDOS_2016.xlsx' no fue encontrado en la carpeta.")
}

# Imprimir el nombre del archivo para verificar
cat("Nombre del archivo encontrado:", archivo_objetivo$name, "\n")

# Ajustar extracción del año
anio <- str_extract(archivo_objetivo$name, "\\d{4}")
if (is.na(anio)) {
    stop("No se pudo extraer el año del nombre del archivo.")
}

# Descargar el archivo temporalmente
temp_file <- tempfile(fileext = ".xlsx")
drive_download(as_id(archivo_objetivo$id), path = temp_file, overwrite = TRUE)

# Verificar si el archivo fue descargado correctamente
if (!file.exists(temp_file)) {
    stop("El archivo no se descargó correctamente.")
}

# Leer las hojas del archivo
nombres_hojas <- openxlsx::getSheetNames(temp_file)
cat("Nombres de hojas en el archivo:\n", nombres_hojas, "\n")

# Leer todas las hojas y combinarlas en un solo data frame
data_combined <- map_dfr(nombres_hojas, function(hoja) {
    openxlsx::read.xlsx(temp_file, sheet = hoja, colNames = TRUE) %>%
        mutate(anio_hoja = hoja)  # Agregar una columna con el nombre de la hoja
})

# Asignar el data frame combinado a un nombre dinámico
nombre_df <- glue("da_{anio}")
assign(nombre_df, data_combined)

