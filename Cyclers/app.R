library(readr)
library(leaflet)
library(tidyverse)
library(osrm)
library(sf)
library(dplyr)
library(hms)
library(factoextra)
library(ggplot2)
library(lubridate)
library(tidyr)
library(cowplot)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(shinydashboard)
library(plotly)

# Fichiers avec toutes les fonctions
source("fonctions.R")

# Chargement des données
subtrip_gpspoints = readRDS("Data/subtrip_gpspoints.rds")
gps_subtrip_mtp = readRDS("Data/gps_subtrip_mtp.rds")
donnees_fac = readRDS("Data/donnees_fac.rds")
trip_subtrip = readRDS("Data/trip_subtrip.rds")
trip_subtrip = trip_subtrip[, -1] #Supprimer la première colonne inutile
trip_subtrip_aberrant = readRDS("Data/trip_subtrip_aberrant.rds")

# Dataframe pour le cluster
trip_cluster = trip_subtrip #On ne considère que les données du Luxembourg

trip_subtrip_mtp = readRDS("Data/trip_subtrip_mtp.rds")

# Concaténer les données du Luxembourg et de Montpellier
subtrip_gpspoints = bind_rows(
  subtrip_gpspoints %>% 
    mutate(Etude= "Luxembourg"), 
  gps_subtrip_mtp %>% 
    mutate(Etude= "Montpellier")) 
trip_subtrip = bind_rows(
  trip_subtrip %>% 
    mutate(Etude= "Luxembourg"), 
  trip_subtrip_mtp %>% 
    mutate(Etude= "Montpellier")) 

trip_subtrip_coherent <- trip_subtrip

routes = readRDS("Data/routes.rds")

# Projeter les coordonnées en latitude et longitude (WGS 84)
data_polygon <- st_transform(donnees_fac, crs = 4326)  # EPSG:4326 est le code EPSG pour le système de coordonnées WGS 84

# Traitement utile pour le temps total et la distance totale par trajet (histogrammes)
trip_subtrip$start_time_datetime <- as.POSIXct(trip_subtrip$start_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip$day_of_week <- weekdays(trip_subtrip$start_time_datetime)
trip_subtrip$day_type <- ifelse(trip_subtrip$day_of_week %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Week", "Weekend")
trip_subtrip$season <- sapply(trip_subtrip$start_time_datetime, get_season)

# Convertir les colonnes de temps en format datetime si nécessaire
trip_subtrip_aberrant$start_time_x <- as.POSIXct(trip_subtrip_aberrant$start_time_x, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_aberrant$end_time_x <- as.POSIXct(trip_subtrip_aberrant$end_time_x, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_aberrant$start_time_y <- as.POSIXct(trip_subtrip_aberrant$start_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_aberrant$end_time_y <- as.POSIXct(trip_subtrip_aberrant$end_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Convertir la durée en heures
trip_subtrip_aberrant$duration_seconds <- as.numeric(difftime(trip_subtrip_aberrant$end_time_x, trip_subtrip_aberrant$start_time_x, units = "secs"))
# Convertir les secondes en objets hms
trip_subtrip_aberrant$duration_x <- as_hms(trip_subtrip_aberrant$duration_seconds)

# Convertir les colonnes de temps en format datetime si nécessaire
trip_subtrip_coherent$start_time_x <- as.POSIXct(trip_subtrip_coherent$start_time_x, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_coherent$end_time_x <- as.POSIXct(trip_subtrip_coherent$end_time_x, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_coherent$start_time_y <- as.POSIXct(trip_subtrip_coherent$start_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")
trip_subtrip_coherent$end_time_y <- as.POSIXct(trip_subtrip_coherent$end_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Convertir la durée en heures
trip_subtrip_coherent$duration_seconds <- as.numeric(difftime(trip_subtrip_coherent$end_time_x, trip_subtrip_coherent$start_time_x, units = "secs"))
# Convertir les secondes en objets hms
trip_subtrip_coherent$duration_x <- as_hms(trip_subtrip_coherent$duration_seconds)



# Définition des couleurs
my_colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9")

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(
    title = span(tags$img(src="Log_Muv.png", width="55px"), "Cyclers")
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Analyses descriptives", tabName = "analyse", icon = icon("chart-line")),
      menuItem("Analyses exploratoires", tabName = "analyse_desc", icon = icon("chart-line")),
      menuItem("Carte des chemins parcourus", tabName = "carte", icon = icon("chart-pie")),
      menuItem("Clustering avec K-Means", tabName = "cluster", icon = icon("chart-bar")),
      menuItem("A propos", tabName = "apropos", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML(".main-header .navbar, .main-header .logo {
            background-color: #C6DFF6 !important;
          }
          .main-sidebar {
            background-color: #78A5D8 !important;
          }
          .main-sidebar .sidebar-menu .active > a {
            background-color: #C6DFF6 !important;
            color: #FFFFFF !important;
          }
          .main-sidebar .sidebar-menu a:hover {
            background-color: #C6DFF6 !important;
            color: #FFFFFF !important;
          }
          .main-sidebar .sidebar-menu a {
            color: #FFFFFF !important;
          }
          .sidebar-toggle:hover {
            background-color: #78A5D8 !important;
          }
          .sidebar-collapse .sidebar-menu {
            width: 50px !important;
          }
          .center-content {
            text-align: center;
          }
          #altrium-container {
            position: relative;
            margin: 0;
            padding: 0;
            overflow: hidden;
            height: calc(100vh - 80px);
          }
          #altrium-image {
            width: 100%;
            height: 100%;
            object-fit: cover;
            margin: 0;
            padding: 0;
          }
          #altrium-text {
            position: absolute;
            top: 2%;
            left: 50%;
            transform: translateX(-50%);
            color: #FC346F;
            text-align: center;
          }
          #altrium-text h1 {
            font-size: 4em;
          }
          #altrium-text p {
            font-size: 2em;
          }
          #propos-text {
              position: absolute;
              top: 100px;
              left: 50px;
              color: white;
              z-index: 1;
              font-size: 24px; /* Modifiez cette valeur selon vos besoins */
          }
        ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "accueil",
        div(id = "altrium-container",
            tags$img(id = "altrium-image", src = "altrium_fond.png"),
            div(id = "altrium-text",
                h1(class = "center-content", "MOBILITÉ CHALLENGE")
            )
        )
      ),
      tabItem(
        tabName = "analyse",
        tabPanel("Analyses descriptives",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("graph_type", label = "Choisir le type de graphique :", 
                                 choices = c("Nombre d'utilisateurs", "Nombre total de kilomètres par mode de transport", "Moyenne du temps passé par mode de transport","Calories brûlées par mode de transport", "Vitesse moyenne par mode de transport","Durée par mode de transport" ),
                                 selected = "Nombre d'utilisateurs"),
                     selectInput("dataset", label = "Choisir le jeu de données :",
                                 choices = c("Avec valeurs aberrantes", "Sans valeurs aberrantes")
                     ),
                     selectInput("etude_des", "Localisation :",
                                 choices = c("Luxembourg", "Montpellier"),
                                 selected = "Montpellier"),
                     conditionalPanel(
                       condition = "input.graph_type == 'Nombre d\\'utilisateurs'",  
                       selectInput("time_unit", label = "Choisir l'unité de temps :", 
                                   choices = c("Semaine", "Mois"), selected = "Semaine")
                     )
                   ),
                   mainPanel(
                     fluidRow(
                       column(width = 12, plotlyOutput("selected_plot", height = "800px")))
                   )
                 )
        )
      ),
      tabItem(tabName = "analyse_desc",
              tabPanel("Analyses exploratoires",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("etude_plot", "Localisation :",
                                       choices = c("Luxembourg", "Montpellier"),
                                       selected = "Montpellier"),
                           selectInput("typeSelect_plot", "Mode de transport :",
                                       choices = unique(trip_subtrip$subtrip_type), multiple = TRUE, selected = "Walking"),
                           selectInput("filterSelect_plot", "Moment de la journée :",
                                       choices = c("Morning, Afternoon, Evening, Night", "More detailed", "Per hour"),
                                       selected = "More detailed"),
                           selectInput("dayTypeSelect_plot", "Période de la semaine :",
                                       choices = c("Week", "Weekend"), multiple = TRUE, selected = "Weekend"),
                           selectInput("plotType", "Choisir le type de plot :",
                                       choices = c("Temps total", "Distance totale"),
                                       selected = "Distance totale"),
                           selectInput("seasonSelect_plot", "Saison :",
                                       choices = c("Spring", "Summer", "Autumn", "Winter"), multiple = TRUE, selected = c("Spring", "Summer", "Autumn", "Winter"))
                         ),
                         mainPanel(
                           plotOutput("descripPlot", width = "100%", height = "800px")
                         )
                       )
              )
      ),
      tabItem(tabName = "carte",
              tabPanel("Carte des chemins parcourus",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("mode_transport", "Mode de transport :",
                                       choices = c("Tous", "Walking", "Bus", "Biking", "Metro", "CarElectric", "Train", "Scooter", "BikeElectric", "MotoElectric"),
                                       selected = "Biking"),
                           selectInput("etude", "Localisation :",
                                       choices = c("Luxembourg", "Montpellier"),
                                       selected = "Montpellier"),
                           sliderInput("distance_range", "Distance (km) :", min = 0, max = 100, value = c(0, 5), step = 0.1),
                           plotOutput("distribution_plot", height = "400px")
                         ),
                         mainPanel(
                           tabsetPanel(
                             tabPanel("Carte", 
                                      fluidRow(column(width = 12, leafletOutput("carte", height = "600px"))))
                           )
                         )
                       )
              )
      ),
      tabItem(tabName = "cluster",
              tabPanel("Clustering avec K-Means",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput("k", "Nombre de clusters :", min = 1, max = 10, value = 3),
                           selectInput("x_axis", "Dimension :", choices = c("1", "2", "3"), selected = "1"),
                           selectInput("y_axis", "Dimension :", choices = c("1", "2", "3"), selected = "2"),
                           plotOutput("wss_plot")
                         ),
                         mainPanel(
                           plotOutput("clustering_plot", height = "300px"),
                           uiOutput("distPlotsUI")             )
                       )
              )
      ),
      tabItem(
        tabName = "apropos",
        div(
          id = "altrium-container",
          tags$img(id = "altrium-image", src = "altrium_neutre.png"),
          div(id = "propos-text",
              p("MIASHS : Traitement et analyse de données de l'application MUV, et développement d'une application afin de présenter les visualisations de données"),
              p("INFOCOM CNO : Promouvoir le challenge et faire la communication afin d'engager la cible"),
              p("Équipe MIASHS : Audric Girondin, Jonathan Duckes, Maéva Maïo, Houria Sayah "),
              p("Équipe CNO : Salomé Armatol, Louise Armengol, Lucas Déjean")
          )
        )
      )
    )
  )
)

# Définition du serveur
server <- function(input, output, session) {
  
  # Carte interactive
  output$carte <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(minZoom = 10, maxZoom = 18)) %>%
      setView(lng = if (input$etude == "Luxembourg") 6.1319346 else 3.862038,
              lat = if (input$etude == "Luxembourg") 49.611621 else 43.62505,
              zoom = if (input$etude == "Luxembourg") 14 else 13) %>%
      addPolygons(
        data = data_polygon,
        fillColor = "red",    # Couleur de remplissage
        fillOpacity = 0.5,    # Opacité du remplissage
        color = "black",      # Couleur de la bordure
        weight = 2            # Épaisseur de la bordure
      ) %>%
      addPolylines(
        data = routes%>%filter(subtrip_id %in% unique(filtered_data()$subtrip_id)),
        color = rgb(50, 149, 115, maxColorValue = 255),
        weight = 2
      )
  })
  
  # Définition de filtered_data réactive
  filtered_data <- reactive({
    # Filtrer en fonction du mode de transport
    filtered <- switch(input$mode_transport,
                       "Tous" = subtrip_gpspoints,
                       "Walking" = filter(subtrip_gpspoints, subtrip_type_x == "Walking"),
                       "Bus" = filter(subtrip_gpspoints, subtrip_type_x == "Bus"),
                       "Biking" = filter(subtrip_gpspoints, subtrip_type_x == "Biking"),
                       "Metro" = filter(subtrip_gpspoints, subtrip_type_x == "Metro"),
                       "CarElectric" = filter(subtrip_gpspoints, subtrip_type_x == "CarElectric"),
                       "Train" = filter(subtrip_gpspoints, subtrip_type_x == "Train"),
                       "Scooter" = filter(subtrip_gpspoints, subtrip_type_x == "Scooter"),
                       "BikeElectric" = filter(subtrip_gpspoints, subtrip_type_x == "BikeElectric"),
                       "MotoElectric" = filter(subtrip_gpspoints, subtrip_type_x == "MotoElectric")
    )
    
    # Filtrer en fonction de l'étude
    filtered <- switch(input$etude,
                       "Tous" = filtered,
                       "Montpellier" = filter(filtered, Etude == "Montpellier"),
                       "Luxembourg" = filter(filtered, Etude == "Luxembourg")
    )
    
    # Filtrer en fonction de la distance
    filtered <- filtered %>%
      filter(distance_x >= input$distance_range[1] & distance_x <= input$distance_range[2])
    
    #max_value <- ifelse(exists("filtered") && !is.null(filtered$distance_x), ceiling(max(filtered$distance_x)), 10)
    #updateSliderInput(session, "distance_range", max = ceiling(max(filtered$distance_x)))
    
    return(filtered)
  })
  
  
  # Distribution plot
  output$distribution_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = distance_x)) +
      geom_histogram(bins = 30, aes(y = ..density..), fill = c("#ffffb3"), color = "black") +  # Histogramme avec densité
      geom_density(alpha = 0.5, color = rgb(252, 52, 111, maxColorValue = 255)) +  # Courbe de densité
      labs(x = "Distance", y = "Densité de probabilité", title = "Distribution du nombre de chemins en fonction de la distance")
  })
  
  ## Partie cluster
  # Sélection des variables d'intérêt
  cluster_selected <- trip_cluster[c("trip_id","subtrip_id","distance_x", "subtrip_type", "duration_seconds", "avg_speed")]
  
  # Traitement des données
  colnames(cluster_selected)[colnames(cluster_selected) == "distance_x"] <- "Distance"
  colnames(cluster_selected)[colnames(cluster_selected) == "duration_seconds"] <- "Durée"
  colnames(cluster_selected)[colnames(cluster_selected) == "avg_speed"] <- "Vitesse"
  
  cluster_onehot <- model.matrix(~ subtrip_type - 1, cluster_selected)
  cluster_onehot <- as.data.frame(cluster_onehot)
  
  cluster_final <- cbind(cluster_selected[c("trip_id","subtrip_id","Distance", "Durée", "Vitesse")], cluster_onehot)
  cluster_final_reduced <- subset(cluster_final, select = -c(trip_id, subtrip_id))
  cluster_final_reduced
  
  
  output$wss_plot <-  renderPlot({
    set.seed(123) # Pour la reproductibilité
    cluster_final_scale <- scale(cluster_final_reduced, center = TRUE, scale = TRUE)
    wss <- sapply(1:10, function(k) {kmeans(cluster_final_reduced, k, nstart = 10)$tot.withinss})
    plot(1:10, wss, type = "b", pch = 19, xlab = "Nombre de clusters", ylab = "Somme des carrés intra-clusters")
  })
  
  # Ajustement des marges pour afficher le graphique (trop grand)
  par(mar = c(5, 4, 4, 2) + 0.1) # marges (bas, gauche, haut, droite) + un petit ajustement
  
  # Clustering avec K-Means
  output$clustering_plot <- renderPlot({
    set.seed(123)
    cluster_final_scale <- scale(cluster_final_reduced, center = TRUE, scale = TRUE)
    kmeans_result <- kmeans(cluster_final_scale, centers = input$k, nstart = 10)
    
    x_index <- match(input$x_axis, c("1", "2", "3"))
    y_index <- match(input$y_axis, c("1", "2", "3"))
    fviz_cluster(kmeans_result, data = cluster_final, geom = "point", 
                 stand = TRUE, ellipse = TRUE, ellipse.type = "norm", axes = c(x_index, y_index),palette = "Set3")
  })
  
  
  output$distPlotsUI <- renderUI({
    req(input$k) # Assurez-vous que k est défini
    
    # Générez dynamiquement un output de plot pour chaque cluster
    plot_output_list <- lapply(1:input$k, function(i) {
      plot_name <- paste0("plot", i)
      plotOutput(plot_name)
    })
    
    # Retournez tous les plotOutput dans une liste
    do.call(tagList, plot_output_list)
  })
  
  # Générez les graphiques pour chaque plotOutput défini précédemment
  observe({
    cluster_final_scale <- scale(cluster_final_reduced, center = TRUE, scale = TRUE)
    kmeans_result <- kmeans(cluster_final_scale, centers = input$k, nstart = 10)
    cluster_final$cluster <- as.factor(kmeans_result$cluster)
    cluster_final$subtrip_type <- trip_cluster$subtrip_type
    
    freq_data <- cluster_final %>%
      group_by(cluster, subtrip_type) %>%
      summarise(freq = n()) %>%
      mutate(freq_pct = freq / sum(freq)) %>%
      ungroup() %>%
      arrange(cluster, desc(freq))
    
    color_palette <- brewer.pal(n = length(unique(cluster_final$subtrip_type)), name = "Set3")
    
    for (i in 1:input$k) {
      local({
        local_i <- i
        output[[paste0("plot", local_i)]] <- renderPlot({
          # Sous-ensemble de données pour le cluster actuel
          cluster_data <- freq_data %>% filter(cluster == as.character(local_i))
          
          # Créez le graphique pour le cluster actuel
          ggplot(cluster_data, aes(x = subtrip_type, y = freq, fill = subtrip_type)) +
            geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.75) +
            theme_minimal() +
            scale_fill_manual(values = color_palette) +
            labs(title = paste("Cluster", local_i, "Distribution par Type de Transport"),
                 x = "Type de Transport",
                 y = "Fréquence",
                 fill = "Type de Transport")
        })
      })
    }
  })
  
  
  output$descripPlot <- renderPlot({
    if (input$plotType == "Temps total") {
      if (length(input$typeSelect_plot) == 0) {
        # Si aucun type de transport n'est sélectionné, afficher un message d'erreur.
        return(ggplot() +
                 labs(x = "Moment de la journée", y = "Temps total de trajet (heures)", fill = "Mode de transport"))
      } else {
        # Filtrer les données selon la sélection de l'utilisateur
        filtered_data <- trip_subtrip %>%
          filter(subtrip_type %in% input$typeSelect_plot & day_type %in% input$dayTypeSelect_plot & season %in% input$seasonSelect_plot) %>%
          mutate(hour = hour(start_time_datetime))
        
        filtered_data <- switch(input$etude_plot,
                                "Tous" = filtered_data,
                                "Montpellier" = filter(filtered_data, Etude == "Montpellier"),
                                "Luxembourg" = filter(filtered_data, Etude == "Luxembourg")
        )
        
        # Appliquer la fonction part_of_day pour catégoriser les moments de la journée
        filtered_data$part_of_day <- sapply(filtered_data$hour, part_of_day, input$filterSelect_plot)
        
        # Sélectionner la palette de couleurs selon le nombre de types de transport sélectionnés
        selected_palette <- color_palettes[[min(length(input$typeSelect_plot), length(color_palettes))]]
        
        if(input$filterSelect_plot == "Per hour") {
          # Diviser les données en deux groupes : de 0 à 12 heures et de 13 à 23 heures
          filtered_data_0_to_12 <- filtered_data %>%
            filter(hour < 13)
          
          filtered_data_13_to_23 <- filtered_data %>%
            filter(hour >= 13)
          
          # Ajuster l'ordre des moments de la journée pour chaque groupe
          filtered_data_0_to_12$part_of_day <- factor(filtered_data_0_to_12$part_of_day, levels = c(
            "0-00:59", "1-1:59", "2-2:59", "3-3:59", "4-4:59", "5-5:59",
            "6-6:59", "7-7:59", "8-8:59", "9-9:59", "10-10:59", "11-11:59", "12-12:59"
          ))
          filtered_data_13_to_23$part_of_day <- factor(filtered_data_13_to_23$part_of_day, levels = c(
            "13-13:59", "14-14:59", "15-15:59", "16-16:59",
            "17-17:59", "18-18:59", "19-19:59", "20-20:59", "21-21:59",
            "22-22:59", "23-23:59"
          ))
          
          # Créer deux graphiques séparés pour mieux visualiser
          plot_0_to_12 <- ggplot(filtered_data_0_to_12, aes(x = part_of_day, fill = subtrip_type)) +
            geom_histogram(stat = "count", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Temps total(heures)", fill = "Mode de transport") +
            ggtitle("0 to 12 Hours")
          
          plot_13_to_23 <- ggplot(filtered_data_13_to_23, aes(x = part_of_day, fill = subtrip_type)) +
            geom_histogram(stat = "count", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Temps total (heures)", fill = "Mode de transport") +
            ggtitle("13 to 23 Hours")
          
          # Afficher les deux graphiques l'un en haut de l'autre pour utiliser toute la largeur
          grid.arrange(plot_0_to_12, plot_13_to_23, ncol = 1, heights = c(6, 6))
          
        } else {
          # Ajuster l'ordre des moments de la journée pour les autres filtres
          filtered_data$part_of_day <- factor(filtered_data$part_of_day, levels = c(
            "Morning (6-11:59)", "Afternoon (12-17:59)", "Evening (18-20:59)", "Night (21-5:59)", "0-5:59", "6-9:59",
            "10-11:59", "12-13:59", "14-15:59", "16-19:59", "20-23:59", "Unknown"
          ))
          # Plot
          ggplot(filtered_data, aes(x = part_of_day, fill = subtrip_type)) +
            geom_histogram(stat = "count", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Temps total (heures)", fill = "Mode de transport")
        }
      }
    } else if (input$plotType == "Distance totale") {
      if (length(input$typeSelect_plot) == 0) {
        # Si aucun type de transport n'est sélectionné ou aucun type de jour n'est sélectionné, afficher plot vide.
        return(ggplot() +
                 labs(x = "Moment de la journée", y = "Distance totale de trajet (km)", fill = "Mode de transport"))
      } else {
        # Palette qualitative car variable type de transport nominale
        color_palettes <- list(
          c("#8dd3c7"),
          c("#8dd3c7", "#ffffb3"),
          c("#8dd3c7", "#ffffb3", "#bebada"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5"),
          c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fdb462", "#b3de69", "#fccde5", "#d9d9d9")
        )
        
        # Filtrer les données selon la sélection de l'utilisateur
        filtered_data <- trip_subtrip %>%
          filter(subtrip_type %in% input$typeSelect_plot & day_type %in% input$dayTypeSelect_plot & season %in% input$seasonSelect_plot) %>%
          mutate(hour = hour(start_time_datetime))
        
        filtered_data <- switch(input$etude_plot,
                                "Tous" = filtered_data,
                                "Montpellier" = filter(filtered_data, Etude == "Montpellier"),
                                "Luxembourg" = filter(filtered_data, Etude == "Luxembourg")
        )
        
        # Appliquer la fonction part_of_day pour catégoriser les moments de la journée
        filtered_data$part_of_day <- sapply(filtered_data$hour, part_of_day, input$filterSelect_plot)
        
        # Sélectionner la palette de couleurs selon le nombre de types de transport sélectionnés
        selected_palette <- color_palettes[[min(length(input$typeSelect_plot), length(color_palettes))]]
        
        if(input$filterSelect_plot == "Per hour") {
          # Diviser les données en deux groupes : de 0 à 12 heures et de 13 à 23 heures
          filtered_data_0_to_12 <- filtered_data %>%
            filter(hour < 13)
          
          filtered_data_13_to_23 <- filtered_data %>%
            filter(hour >= 13)
          
          # Ajuster l'ordre des moments de la journée pour chaque groupe
          filtered_data_0_to_12$part_of_day <- factor(filtered_data_0_to_12$part_of_day, levels = c(
            "0-00:59", "1-1:59", "2-2:59", "3-3:59", "4-4:59", "5-5:59",
            "6-6:59", "7-7:59", "8-8:59", "9-9:59", "10-10:59", "11-11:59", "12-12:59"
          ))
          filtered_data_13_to_23$part_of_day <- factor(filtered_data_13_to_23$part_of_day, levels = c(
            "13-13:59", "14-14:59", "15-15:59", "16-16:59",
            "17-17:59", "18-18:59", "19-19:59", "20-20:59", "21-21:59",
            "22-22:59", "23-23:59"
          ))
          
          # Créer deux graphiques séparés pour mieux visualiser
          plot_0_to_12 <- ggplot(filtered_data_0_to_12, aes(x = part_of_day, y = distance_y, fill = subtrip_type)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Distance totale parcourue (km)", fill = "Mode de transport") +
            ggtitle("0 to 12 Hours")
          
          plot_13_to_23 <- ggplot(filtered_data_13_to_23, aes(x = part_of_day, y = distance_y, fill = subtrip_type)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Distance totale parcourue (km)", fill = "Mode de transport") +
            ggtitle("13 to 23 Hours")
          
          # Afficher les deux graphiques l'un en haut de l'autre pour utiliser toute la largeur
          grid.arrange(plot_0_to_12, plot_13_to_23, ncol = 1, heights = c(6, 6))
          
        } else {
          # Ajuster l'ordre des moments de la journée pour les autres filtres
          filtered_data$part_of_day <- factor(filtered_data$part_of_day, levels = c(
            "Morning (6-11:59)", "Afternoon (12-17:59)", "Evening (18-20:59)", "Night (21-5:59)", "0-5:59", "6-9:59",
            "10-11:59", "12-13:59", "14-15:59", "16-19:59", "20-23:59", "Unknown"
          ))
          # Plot
          ggplot(filtered_data, aes(x = part_of_day, y = distance_y, fill = subtrip_type)) +
            geom_bar(stat = "identity", position = position_dodge()) +
            scale_fill_manual(values = selected_palette) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
                  axis.text.y = element_text(size = 13),
                  axis.title.x = element_text(size = 16),
                  axis.title.y = element_text(size = 16),
                  legend.position = "bottom",
                  panel.background = element_rect(fill = "white")) +
            labs(x = "Moment de la journée", y = "Distance totale parcourue (km)", fill = "Mode de transport")
        }
      }
    }
  })
  
  ## Partie Analyse des données de déplacements
  output$selected_plot <- renderPlotly({
    if (input$dataset == "Avec valeurs aberrantes") {
      graph_type <- input$graph_type 
      
      if (graph_type == "Nombre d'utilisateurs") {
        if (input$time_unit == "Semaine") {
          users_data <- trip_subtrip_aberrant %>%
            group_by(time_unit = floor_date(start_time_x, "week")) %>%
            summarise(unique_users = n_distinct(id_utente_x))
          
          x_label <- "Semaine"
        } else {
          users_data <- trip_subtrip_aberrant %>%
            group_by(time_unit = floor_date(start_time_x, "month")) %>%
            summarise(unique_users = n_distinct(id_utente_x))
          
          x_label <- "Mois"
        }
        
        p <- ggplot(users_data, aes(x = time_unit, y = unique_users, text = paste("Nombre d'utilisateurs :", unique_users))) +
          geom_col(fill = "#78A5D8") +
          labs(title = paste("Nombre d'utilisateurs", x_label),
               x = x_label,
               y = "Nombre d'utilisateurs") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p, tooltip = "text"))
        
      } else if (graph_type == "Nombre total de kilomètres par mode de transport") {
        total_distance <- trip_subtrip_aberrant %>%
          group_by(subtrip_type) %>%
          summarise(total_distance = sum(distance_x))
        
        p <- ggplot(total_distance, aes(x = subtrip_type, y = total_distance, fill = subtrip_type, text = paste("Kilomètres parcourus :", total_distance))) +
          geom_col() +
          labs(title = "Nombre de kilomètres parcourus par mode de transport",
               x = "Moyen de transport",
               y = "Kilomètres parcourus") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p, tooltip = "text"))
        
      } else if (graph_type == "Moyenne du temps passé par mode de transport") {
        mean_duration_by_transport <- trip_subtrip_aberrant %>%
          group_by(subtrip_type) %>%
          summarise(mean_duration_minutes = mean(as.numeric(duration_x) / 60, na.rm = TRUE))
        
        p <- ggplot(mean_duration_by_transport, aes(x = subtrip_type, y = mean_duration_minutes, fill = subtrip_type)) +
          geom_bar(stat = "identity") +
          labs(title = "Moyenne du temps passé par mode de transport",
               x = "Mode de transport",
               y = "Moyenne du temps passé (minutes)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      } else if (graph_type == "Calories brûlées par mode de transport") {
        p <- ggplot(trip_subtrip_aberrant, aes(x = subtrip_type, y = calories_x, fill = subtrip_type)) +
          geom_bar(stat = "summary", fun = "sum", position = "dodge") +
          labs(title = "Nombre de calories brûlées par mode de transport",
               x = "Moyen de transport",
               y = "Calories brûlées") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)+
          scale_y_continuous(labels = scales::comma)
        
        return(ggplotly(p))
      } else if (graph_type == "Vitesse moyenne par mode de transport") {
        avg_speed_ordered <- trip_subtrip_aberrant %>%
          group_by(subtrip_type) %>%
          summarise(avg_speed = median(avg_speed)) %>%
          arrange(avg_speed) %>%
          pull(subtrip_type)
        
        p <- ggplot(trip_subtrip_aberrant, aes(x = factor(subtrip_type, levels = avg_speed_ordered), y = avg_speed, fill = subtrip_type)) +
          geom_violin(trim = FALSE, scale = "width") +
          labs(title = "Vitesse moyenne par mode de transport",
               x = "Moyen de transport",
               y = "Vitesse moyenne (km/h)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      } else if (graph_type == "Durée par mode de transport") {
        # Convertir la colonne de durée en heures décimales
        trip_subtrip_aberrant$duration_hours <- as.numeric(trip_subtrip_aberrant$duration_x) / 3600
        
        # Assurez-vous que subtrip_type est un facteur avec les mêmes niveaux que ceux définis dans my_colors
        trip_subtrip_aberrant$subtrip_type <- factor(trip_subtrip_aberrant$subtrip_type, levels = unique(trip_subtrip_aberrant$subtrip_type))
        
        # Tracer le violin plot de la durée par mode de transport
        p <- ggplot(trip_subtrip_aberrant, aes(x = subtrip_type, y = duration_hours, fill = subtrip_type)) +
          geom_violin(trim = FALSE, scale = "width") +
          labs(title = "Durée par mode de transport",
               x = "Moyen de transport",
               y = "Durée (en heures)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      }
    } else if (input$dataset == "Sans valeurs aberrantes") {
      graph_type <- input$graph_type 
      
      if (graph_type == "Nombre d'utilisateurs") {
        if (input$time_unit == "Semaine") {
          users_data <- trip_subtrip_coherent %>%
            group_by(time_unit = floor_date(start_time_x, "week")) %>%
            summarise(unique_users = n_distinct(id_utente_x))
          
          x_label <- "Semaine"
        } else {
          users_data <- trip_subtrip_coherent %>%
            group_by(time_unit = floor_date(start_time_x, "month")) %>%
            summarise(unique_users = n_distinct(id_utente_x))
          
          x_label <- "Mois"
        }
        
        p <- ggplot(users_data, aes(x = time_unit, y = unique_users, text = paste("Nombre d'utilisateurs :", unique_users))) +
          geom_col(fill = "#78A5D8") +
          labs(title = paste("Nombre d'utilisateurs", x_label),
               x = x_label,
               y = "Nombre d'utilisateurs") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p, tooltip = "text"))
        
      } else if (graph_type == "Nombre total de kilomètres par mode de transport") {
        total_distance <- trip_subtrip_coherent %>%
          group_by(subtrip_type) %>%
          summarise(total_distance = sum(distance_x))
        
        p <- ggplot(total_distance, aes(x = subtrip_type, y = total_distance, fill = subtrip_type, text = paste("Kilomètres parcourus :", total_distance))) +
          geom_col() +
          labs(title = "Nombre de kilomètres parcourus par mode de transport",
               x = "Moyen de transport",
               y = "Kilomètres parcourus") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p, tooltip = "text"))
        
      } else if (graph_type == "Moyenne du temps passé par mode de transport") {
        mean_duration_by_transport <- trip_subtrip_coherent %>%
          group_by(subtrip_type) %>%
          summarise(mean_duration_minutes = mean(as.numeric(duration_x) / 60, na.rm = TRUE))
        
        p <- ggplot(mean_duration_by_transport, aes(x = subtrip_type, y = mean_duration_minutes, fill = subtrip_type)) +
          geom_bar(stat = "identity") +
          labs(title = "Moyenne du temps passé par mode de transport",
               x = "Mode de transport",
               y = "Moyenne du temps passé (minutes)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))+
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      } else if (graph_type == "Calories brûlées par mode de transport") {
        p <- ggplot(trip_subtrip_coherent, aes(x = subtrip_type, y = calories_x, fill = subtrip_type)) +
          geom_bar(stat = "summary", fun = "sum", position = "dodge") +
          labs(title = "Nombre de calories brûlées par mode de transport",
               x = "Moyen de transport",
               y = "Calories brûlées") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)+
          scale_y_continuous(labels = scales::comma)
        
        return(ggplotly(p))
      } else if (graph_type == "Vitesse moyenne par mode de transport") {
        avg_speed_ordered <- trip_subtrip_coherent %>%
          group_by(subtrip_type) %>%
          summarise(avg_speed = median(avg_speed)) %>%
          arrange(avg_speed) %>%
          pull(subtrip_type)
        
        p <- ggplot(trip_subtrip_coherent, aes(x = factor(subtrip_type, levels = avg_speed_ordered), y = avg_speed, fill = subtrip_type)) +
          geom_violin(trim = FALSE, scale = "width") +
          labs(title = "Vitesse moyenne par mode de transport",
               x = "Moyen de transport",
               y = "Vitesse moyenne (km/h)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      } else if (graph_type == "Durée par mode de transport") {
        # Convertir la colonne de durée en heures décimales
        trip_subtrip_coherent$duration_hours <- as.numeric(trip_subtrip_coherent$duration_x) / 3600
        
        # Assurez-vous que subtrip_type est un facteur avec les mêmes niveaux que ceux définis dans my_colors
        trip_subtrip_coherent$subtrip_type <- factor(trip_subtrip_coherent$subtrip_type, levels = unique(trip_subtrip_coherent$subtrip_type))
        
        # Tracer le violin plot de la durée par mode de transport
        p <- ggplot(trip_subtrip_coherent, aes(x = subtrip_type, y = duration_hours, fill = subtrip_type)) +
          geom_violin(trim = FALSE, scale = "width") +
          labs(title = "Durée par mode de transport",
               x = "Moyen de transport",
               y = "Durée (en heures)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                panel.background = element_rect(fill = "white")) +
          scale_fill_manual(values = my_colors)
        
        return(ggplotly(p))
      }
    }
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
