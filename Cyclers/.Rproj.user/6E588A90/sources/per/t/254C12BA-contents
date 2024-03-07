library(sf)
library(osrm)
library(tidyverse)
library(dplyr)
library(purrr)

data_trip <- subtrip_gpspoints

routes <- map_dfr(unique(data_trip$subtrip_id), function(i) {
  subtrip_data <- data_trip[data_trip$subtrip_id == i, ]
  
  # Obtenir les coordonnées de début et de fin du sous-trajet
  src <- c(subtrip_data$long[1], subtrip_data$lat[1])  # Point de départ
  dst <- c(tail(subtrip_data$long, 1), tail(subtrip_data$lat, 1))  # Point de destination
  
  # Tracer le trajet pour le sous-trajet actuel
  route <- tryCatch(
    osrmRoute(src = src, dst = dst),
    error = function(e) NULL
  )
  
  # Si la route est calculée avec succès, retourner l'identifiant du sous-trajet avec la route calculée
  if (!is.null(route)) {
    route$subtrip_id = i
    return(route)
  }
})

routes

# leaflet(data=routes) %>%
#   addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
#   addPolylines(
#     color = rgb(50, 149, 115, maxColorValue = 255),
#     weight = 2
#   )

save.image("routes")
saveRDS(routes,"Data/routes.rds")

routes = readRDS("Data/routes.rds")

# routes_df <- bind_rows(routes) 
# routes_df <- data.frame(routes_df)

# save.image("routes_df")
# saveRDS(routes,"Data/routes_df.rds")
