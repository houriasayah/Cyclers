data <- read.csv("trip_subtrip_cohérente.csv", sep = ";")
#data <- read_csv("trip_subtrip_mtp.csv")

# Sélectionner les variables d'intérêt
data_selected <- data[c("trip_id","subtrip_id","distance_x", "subtrip_type", "duration_seconds", "avg_speed")]

# Assurez-vous que toutes les valeurs sont numériques et sans NA/NaN/Inf
data_onehot <- model.matrix(~ subtrip_type - 1, data_selected)
data_onehot <- as.data.frame(data_onehot)

# Fusionner les données encodées avec les autres variables numériques
data_final <- cbind(data_selected[c("trip_id","subtrip_id","distance_x", "duration_seconds", "avg_speed")], data_onehot)
data_final_reduced <- subset(data_final, select = -c(trip_id, subtrip_id))
data_final_reduced
# Vérifier la structure des données finale

set.seed(123) # Pour la reproductibilité
wss <- sapply(1:10, function(k){kmeans(data_final_reduced, k, nstart = 10)$tot.withinss})
plot(1:10, wss, type="b", pch = 19, xlab="Nombre de clusters", ylab="Somme des carrés intra-clusters")

set.seed(123)
data_final_scale <- scale(data_final_reduced,center = TRUE,scale = TRUE)
k <- 10 # Remplacez par le nombre de clusters choisi
kmeans_result <- kmeans(data_final_scale, centers = k, nstart = 10)
# Afficher les résultats
print(kmeans_result$size) # Taille des clusters
print(kmeans_result$centers) # Centres des clusters

library(factoextra)
fviz_cluster(kmeans_result, data = data_final, geom = "point", 
             stand = TRUE, frame.type = "norm", axes = c(1,2))

fviz_cluster(kmeans_result, data = data_final, geom = "point", 
             stand = TRUE, frame.type = "norm", axes = c(1,3))

fviz_cluster(kmeans_result, data = data_final, geom = "point", 
             stand = TRUE, frame.type = "norm", axes = c(2,3))

library(tidyverse)

data_final$cluster <- kmeans_result$cluster
data_final$subtrip_type <- data$subtrip_type


####################################################################################################
data_final %>%
  group_by(cluster)%>%
  summarise(moy=mean(avg_speed))

pivot_longer(data = data_final,cols = c("avg_speed","distance_x","duration_seconds"),names_to = "Type",values_to = "Valeurs")%>%
  ggplot(aes( as.factor(cluster),Valeurs ))+
  geom_boxplot()+facet_wrap("Type",scales = "free_y")

library(ggplot2)

ggplot(data_final, aes(x = cluster, fill = subtrip_type)) +
  geom_bar(position = "dodge",width = 0.75) +
  theme_minimal() +
  labs(title = "Distribution des clusters par type de transport",
       x = "Cluster",
       y = "Nombre d'observations",
       fill = "Type de Transport")
# Assurez-vous que les packages nécessaires sont chargés
library(dplyr)
library(ggplot2)
library(RColorBrewer)


# Calcul des fréquences pour chaque combinaison de cluster et de subtrip_type
freq_data <- data_final %>%
  group_by(cluster, subtrip_type) %>%
  summarise(freq = n()) %>%
  mutate(freq_pct = freq / sum(freq)) %>%
  ungroup() %>%
  arrange(cluster, desc(freq))

# Nombre de clusters
color_palette <- brewer.pal(n = length(unique(data_final$subtrip_type)), name = "Set3")

# Boucle pour générer un rapport pour chaque cluster
for (i in 1:k) {
  # Filtrer les données pour le cluster courant
  cluster_data <- freq_data[freq_data$cluster == i, ]
  
  # Calculer la fréquence des observations par type de transport pour le cluster courant
  freq_summary <- aggregate(freq ~ subtrip_type, data = cluster_data, sum)
  
  # Afficher le rapport pour le cluster courant
  print(paste("Rapport pour le cluster:", i))
  print(freq_summary)
  
  # Création du graphique ggplot
  p<- ggplot(cluster_data, aes(x = subtrip_type, y = freq, fill = subtrip_type)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single"),width = 0.75) +
    theme_minimal() +scale_fill_manual(values = color_palette) + # Utilisation de la palette de couleurs
    labs(title = "Distribution des clusters par type de transport",
         x = "Cluster",
         y = "Fréquence des observations",
         fill = "Type de Transport")
  
  print(p)
}


