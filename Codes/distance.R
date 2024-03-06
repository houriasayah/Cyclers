library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(shiny)
library(gridExtra)

# Lecture des données
trip_subtrip <- read.csv("trip_subtrip.csv", sep = ";")

# Conversion du format de start_time_y en date heure
trip_subtrip$start_time_datetime <- as.POSIXct(trip_subtrip$start_time_y, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Extraire le nom du jour de la semaine
trip_subtrip$day_of_week <- weekdays(trip_subtrip$start_time_datetime)

# Créer une nouvelle colonne pour indiquer si c'est un jour de semaine ou de week-end
trip_subtrip$day_type <- ifelse(trip_subtrip$day_of_week %in% c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi"), "Semaine", "Weekend")

# Traduction des noms des jours en anglais
trip_subtrip$day_of_week <- recode(trip_subtrip$day_of_week, "Samedi" = "Saturday", "Dimanche" = "Sunday", "Lundi" = "Monday", "Mardi" = "Tuesday", "Mercredi" = "Wednesday", "Jeudi" = "Thursday", "Vendredi" = "Friday")

# Traduction en anglais dans day_type
trip_subtrip$day_type <- recode(trip_subtrip$day_type, "Semaine" = "Week")

# Fonction pour catégoriser les moments de la journée (agissant avec un filtre)
part_of_day <- function(hour, filter_type) {
  if (is.na(hour)) {
    return('Unknown')
  }
  # En quatre périodes de la journée
  if (filter_type == "Morning, Afternoon, Evening, Night") {
    if (hour >= 6 & hour <= 11) {
      return('Morning (6-11:59)')
    } else if (hour >= 12 & hour <= 17) {
      return('Afternoon (12-17:59)')
    } else if (hour >= 18 & hour <= 20) {
      return('Evening (18-20:59)')
    } else {
      return('Night (21-5:59)')
    }
  # Plus de détails en voyant par regroupement d'heures
  } else if (filter_type == "More detailed") {
    if (hour >= 0 & hour <= 5) {
      return('0-5:59')
    } else if (hour >= 6 & hour <= 9) {
      return('6-9:59')
    } else if (hour >= 10 & hour <= 11) {
      return('10-11:59')
    } else if (hour >= 12 & hour <= 13) {
      return('12-13:59')
    } else if (hour >= 14 & hour <= 15) {
      return('14-15:59')
    } else if (hour >= 16 & hour <= 19) {
      return('16-19:59')
    } else {
      return('20-23:59')
    }
  # Par heure
  } else if (filter_type == "Per hour") {
    if (hour >= 0 & hour < 1) {
      return('0-00:59')
    } else if (hour >= 1 & hour < 2) {
      return('1-1:59')
    } else if (hour >= 2 & hour < 3) {
      return('2-2:59')
    } else if (hour >= 3 & hour < 4) {
      return('3-3:59')
    } else if (hour >= 4 & hour < 5) {
      return('4-4:59')
    } else if (hour >= 5 & hour < 6) {
      return('5-5:59')
    } else if (hour >= 6 & hour < 7) {
      return('6-6:59')
    } else if (hour >= 7 & hour < 8) {
      return('7-7:59')
    } else if (hour >= 8 & hour < 9) {
      return('8-8:59')
    } else if (hour >= 9 & hour < 10) {
      return('9-9:59')
    } else if (hour >= 10 & hour < 11) {
      return('10-10:59')
    } else if (hour >= 11 & hour < 12) {
      return('11-11:59')
    } else if (hour >= 12 & hour < 13) {
      return('12-12:59')
    } else if (hour >= 13 & hour < 14) {
      return('13-13:59')
    } else if (hour >= 14 & hour < 15) {
      return('14-14:59')
    } else if (hour >= 15 & hour < 16) {
      return('15-15:59')
    } else if (hour >= 16 & hour < 17) {
      return('16-16:59')
    } else if (hour >= 17 & hour < 18) {
      return('17-17:59')
    } else if (hour >= 18 & hour < 19) {
      return('18-18:59')
    } else if (hour >= 19 & hour < 20) {
      return('19-19:59')
    } else if (hour >= 20 & hour < 21) {
      return('20-20:59')
    } else if (hour >= 21 & hour < 22) {
      return('21-21:59')
    } else if (hour >= 22 & hour < 23) {
      return('22-22:59')
    } else {
      return('23-23:59')
    }
  }
}

# Fonction pour catégoriser les saisons en fonction de la date
get_season <- function(date) {
  if (is.na(date)) {
    return("Unknown")
  }
  
  month <- as.numeric(format(date, "%m"))
  day <- as.numeric(format(date, "%d"))
  
  if ((month == 3 & day >= 20) | month %in% c(4, 5) | (month == 6 & day < 21)) {
    return("Spring")
  } else if ((month == 6 & day >= 21) | month %in% c(7, 8) | (month == 9 & day < 23)) {
    return("Summer")
  } else if ((month == 9 & day >= 23) | month %in% c(10, 11) | (month == 12 & day < 21)) {
    return("Autumn")
  } else {
    return("Winter")
  }
}

# Ajouter une colonne de saison au DataFrame
trip_subtrip$season <- sapply(trip_subtrip$start_time_datetime, get_season)

# UI
ui <- fluidPage(
  titlePanel("Distance Covered by Type of Transport and Part of Day for Luxembourg"),
  fluidRow(
    column(width = 3,
           # Filtre sur type de mobilité
           selectInput("typeSelect", "Choose a Type of Transport:",
                       choices = unique(trip_subtrip$subtrip_type), multiple = TRUE)
    ),
    column(width = 3,
           # Filtre sur le choix de la manière dont est calculé Part of day
           selectInput("filterSelect", "Choose the Type of the Part of Day :",
                       choices = c("Morning, Afternoon, Evening, Night", "More detailed", "Per hour"))
    ),
    column(width = 3,
           # Filtre pour le type de jour
           selectInput("dayTypeSelect", "Choose Type of Day:",
                       choices = c("Week", "Weekend"), multiple = TRUE)
    ),
    column(width = 3,
           # Filtre pour la saison
           selectInput("seasonSelect", "Choose a Season :",
                       choices = c("Spring", "Summer", "Autumn", "Winter"), multiple = TRUE)
    )
  ),
  plotOutput("distancePlot", width = "100%", height = "800px")
)

# Server
server <- function(input, output) {
  output$distancePlot <- renderPlot({
    if (length(input$typeSelect) == 0 | length(input$dayTypeSelect) == 0) {
      # Si aucun type de transport n'est sélectionné ou aucun type de jour n'est sélectionné, afficher plot vide.
      return(ggplot() + 
               labs(x = "Part of Day", y = "Total Distance Covered (km)", fill = "Type of Transport") +
               ggtitle("Please select a type of transport and type of day to view data."))
    } else {
      # Palette qualitative car variable type de transport nominale
      color_palettes <- list(
        c("#e41a1c"),
        c("#e41a1c", "#4daf4a"),
        c("#e41a1c", "#4daf4a", "#377eb8"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3", "#ff7f00"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3", "#ff7f00", "#ffff33"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3", "#ff7f00", "#ffff33", "#a65628"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf"),
        c("#e41a1c", "#4daf4a", "#377eb8", "#984ea3", "#ff7f00", "#ffff33", "#a65628", "#f781bf", "#999999")
      )
      
      # Filtrer les données selon la sélection de l'utilisateur
      filtered_data <- trip_subtrip %>%
        filter(subtrip_type %in% input$typeSelect & day_type %in% input$dayTypeSelect & season %in% input$seasonSelect) %>%
        mutate(hour = hour(start_time_datetime))
      
      # Appliquer la fonction part_of_day pour catégoriser les moments de la journée
      filtered_data$part_of_day <- sapply(filtered_data$hour, part_of_day, input$filterSelect)
      
      # Sélectionner la palette de couleurs selon le nombre de types de transport sélectionnés
      selected_palette <- color_palettes[[min(length(input$typeSelect), length(color_palettes))]]
      
      if(input$filterSelect == "Per hour") {
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
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom") +
          labs(x = "Part of Day", y = "Total Distance Covered (km)", fill = "Type of Transport") +
          ggtitle("0 to 12 Hours")
        
        plot_13_to_23 <- ggplot(filtered_data_13_to_23, aes(x = part_of_day, y = distance_y, fill = subtrip_type)) +
          geom_bar(stat = "identity", position = position_dodge()) +
          scale_fill_manual(values = selected_palette) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom") + 
          labs(x = "Part of Day", y = "Total Distance Covered (km)", fill = "Type of Transport") +
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
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                legend.position = "bottom") +
          labs(x = "Part of Day", y = "Total Distance Covered (km)", fill = "Type of Transport")
      }
    }
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)