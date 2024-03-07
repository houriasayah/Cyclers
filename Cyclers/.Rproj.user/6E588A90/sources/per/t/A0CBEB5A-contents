## Fichiers avec les fonctions

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
