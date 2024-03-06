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
k <- 3 # Remplacez par le nombre de clusters choisi
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
data_final %>%
  group_by(cluster)%>%
  summarise(moy=mean(avg_speed))

pivot_longer(data = data_final,cols = c("avg_speed","distance_x","duration_seconds"),names_to = "Type",values_to = "Valeurs")%>%
  ggplot(aes( as.factor(cluster),Valeurs ))+
  geom_boxplot()+facet_wrap("Type",scales = "free_y")

data_final$subtrip_type <- data$subtrip_type
library(ggplot2)

ggplot(data_final, aes(x = cluster, fill = subtrip_type)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Distribution des clusters par type de transport",
       x = "Cluster",
       y = "Nombre d'observations",
       fill = "Type de Transport")
