rm(list = ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(skimr)
library(pool)
library(glue)

# Vector con nombres de meses en español
meses_es <- c(
    "enero",
    "febrero",
    "marzo",
    "abril",
    "mayo", 
    "junio",
    "julio",
    "agosto", 
    "septiembre",
    "octubre", 
    "noviembre",
    "diciembre"
)

# Crear un pool de conexión
pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",  # Cambiar según configuración
    port = 5432,         # Puerto de PostgreSQL
    user = "postgres",   # Usuario de PostgreSQL
    password = "marce"   # Contraseña de PostgreSQL
)

hi_14_23 <- read.xlsx("OE_HI/DB/mdi_homicidiosintencionales_pm_2014-2023.xlsx",
                      sheet = "MDI_HomicidiosIntencionales_PM",
                      detectDates = T)

hi_14_23$coordenada_x <- as.numeric(hi_14_23$coordenada_x)
hi_14_23$coordenada_y <- as.numeric(hi_14_23$coordenada_y)
hi_14_23$hora_infraccion <- as.numeric(hi_14_23$hora_infraccion)
hi_14_23$edad <- as.numeric(hi_14_23$edad)

hi_24 <- read.xlsx("OE_HI/DB/mdi_homicidiosintencionales_pm_2024_enero-octubre.xlsx",
                   sheet = "MDI_HomicidiosIntencionales_PM",
                   detectDates = T)

hi_total <- bind_rows(hi_14_23, hi_24) %>% 
    mutate(
        # Reemplazar "SIN_DATO" por "SD" en las columnas de texto
        across(where(is.character), ~ ifelse(. == "SIN_DATO", "SD", .)),
        
        # Asegurarse que 'codigo_provincia' sea de tipo character y agregar cero si es necesario
        codigo_provincia = as.character(codigo_provincia),
        codigo_provincia = ifelse(nchar(codigo_provincia) == 1, glue("0{codigo_provincia}"), codigo_provincia),
        
        # Asignar la región según 'codigo_provincia'
        region = case_when(
            codigo_provincia %in% c("01", "02", "03", "04", "05", "06", "10", "11", "17", "18") ~ "SIERRA",  # Provincias de la Sierra
            codigo_provincia %in% c("07", "08", "09", "12", "13", "23", "24") ~ "COSTA",  # Provincias de la Costa
            codigo_provincia %in% c("14", "15", "16", "19", "21", "22") ~ "AMAZÓNICA",  # Provincias de la Amazonía
            codigo_provincia == "20" ~ "INSULAR",  # Galápagos
            codigo_provincia == "90" ~ "ZONA NO DELIMITADA",  # Zona en estudio
            TRUE ~ "OTRAS"  # En caso de que no se cumpla ninguna condición
        ),
        
        # Extraer el año y mes de la fecha de infracción
        anio = year(fecha_infraccion),
        mes = month(fecha_infraccion),
        
        # Convertir el mes a nombre en español (ordenado)
        mes_nombre = factor(meses_es[mes], levels = meses_es),
        
        # Calcular los días del mes
        dias_mes = days_in_month(fecha_infraccion)) %>% 
    rename("latitud"="coordenada_y",
           "longitud"="coordenada_x") %>% 
    mutate(latitud=round(latitud,2),
           longitud=round(longitud,2))

field_types <- c(
    "tipo_muerte" = "varchar(50)", #01
    "zona" = "varchar(50)", #02
    "subzona" = "varchar(50)", #03
    "distrito" = "varchar(50)", #04
    "circuito" = "varchar(50)", #05
    "codigo_subcircuito" = "varchar(20)", #06
    "subcircuito" = "varchar(50)", #07
    "provincia" = "varchar(50)", #08
    "codigo_provincia" = "integer", #09
    "canton" = "varchar(50)", #10
    "codigo_canton" = "varchar(10)", #11
    "latitud" = "numeric", #12
    "longitud" = "numeric", #13
    "area_hecho" = "varchar(50)", #14
    "lugar" = "varchar(100)", #15
    "tipo_lugar" = "varchar(50)", #16
    "fecha_infraccion" = "timestamp", #17
    "hora_infraccion" = "varchar(100)", #18
    "arma" = "varchar(50)", #19
    "tipo_arma" = "varchar(50)", #20
    "presunta_motivacion" = "varchar(100)", #21
    "presun_motiva_observada" = "varchar(200)", #22
    "probable_causa_motivada" = "varchar(200)", #23
    "edad" = "integer", #24
    "medida_edad" = "varchar(10)", #25
    "sexo" = "varchar(20)", #26
    "genero" = "varchar(20)", #27
    "etnia" = "varchar(50)", #28
    "estado_civil" = "varchar(50)", #29
    "nacionalidad" = "varchar(50)", #30
    "discapacidad" = "varchar(50)", #31
    "profesion_registro_civil" = "varchar(100)", #32
    "instruccion" = "varchar(50)", #33
    "antecedentes" = "varchar(50)", #34
    "region" = "varchar(50)", #35
    "anio" = "integer", #36
    "mes" = "integer", #37
    "mes_nombre" = "varchar(20)", #38
    "dias_mes" = "integer" #39
)

# Guardar la tabla en PostgreSQL con los tipos de columnas especificados
dbWriteTable(
    pool,
    name = DBI::Id(schema = "data_lake", table = "hi_historico_da"),
    value = hi_total,
    overwrite = TRUE,
    row.names = FALSE,
    field.types = field_types
)

# Calcular el promedio mensual ajustado por número de días en el mes
promedio_mensual_anio <- hi_total %>%
    group_by(provincia, anio, mes, mes_nombre, dias_mes, region) %>% # Agrupar por provincia, año, mes y días del mes
    summarise(total_casos = n(), .groups = "drop") %>%       # Contar homicidios por mes
    mutate(promedio_mensual = round(total_casos / dias_mes,2)) %>%    # Calcular promedio ajustado
    arrange(provincia, anio, mes)
