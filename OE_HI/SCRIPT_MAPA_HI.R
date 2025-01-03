rm(list = ls(all.names = TRUE))

library(tidyverse)
library(leaflet)
library(sf)
library(RPostgreSQL)
library(pool)

# Configuración regional en español
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

# Crear conexión a la base de datos
pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "mdi_dwh",
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = "XXX"
)

hi_14_24 <- dbGetQuery(pool,
"SELECT latitud, longitud, tipo_muerte
  FROM data_lake.hi_historico_da
  WHERE EXTRACT(YEAR FROM fecha_infraccion) >= 2024")

poolClose(pool)

# Convertir data.frame a objeto sf
hi_14_24 <- st_as_sf(hi_14_24, coords = c("longitud", "latitud"), crs = 4326)

# Crear una paleta de colores basada en tipo_muerte
paleta_colores <- colorFactor(
    palette = "Set1",  # Escoge una paleta de colores
    domain = hi_14_24$tipo_muerte  # Basada en las categorías de tipo_muerte
)

leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
        data = hi_14_24,
        color = ~paleta_colores(tipo_muerte),  # Asigna el color según tipo_muerte
        fillOpacity = 0.7,
        popup = ~paste(
            "Tipo de Muerte: ", tipo_muerte
        ),
        group = "Incidentes 2024"
    ) %>%
    addLayersControl(
        overlayGroups = c("Incidentes 2024"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    setView(lng = -78.5, lat = -1.5, zoom = 7)  # Ajusta el zoom más amplio para Ecuador






