# Chargement des données depuis le fichier CSV. Le fichier est appelé "ss.csv", 
# il est présumé que ce fichier est présent dans le répertoire de travail actuel.
# Les données sont séparées par des virgules (sep=",") et les décimales sont séparées par un point (dec=".").
sommeil = read.csv("Sleep_data.csv", h = T, sep = ",", dec = ".")

# Affiche la structure des données (types de variables, dimension du jeu de données, etc.) 
# pour mieux comprendre ce que contient le fichier CSV.
str(sommeil)

# Affiche le répertoire de travail actuel, utile pour vérifier où le fichier "ss.csv" est situé.
getwd()

# Ces lignes sont commentées et servent à modifier le répertoire de travail si nécessaire 
# pour pointer vers l'emplacement où le fichier CSV est stocké, 
# et redémarrer R si une mise à jour du répertoire est nécessaire.
#setwd("C:/Users/murie/Downloads/archive (1)")
#.rs.restartR()

# Sélection des variables quantitatives pertinentes pour l'ACP : 
# - Durée de sommeil
# - Qualité du sommeil
# - Niveau d'activité physique
# - Niveau de stress
# - Pression artérielle
# - Fréquence cardiaque
# - Nombre de pas quotidiens
data_sommeil_quanti = sommeil[, c("Sleep.Duration", "Quality.of.Sleep", 
                                  "Physical.Activity.Level", "Stress.Level",
                                  "Blood.Pressure", "Heart.Rate", "Daily.Steps")]

# Vérification de la structure des variables quantitatives sélectionnées (type de données et dimensions).
str(data_sommeil_quanti)

# Génération de l'histogramme pour chaque variable afin d'explorer leur distribution. 
# Les graphiques sont affichés dans une grille 3x3 pour avoir une vue d'ensemble rapide.
par(mfrow = c(3, 3))
sapply(data_sommeil_quanti, hist)

# Calcul de la matrice de corrélation entre les variables quantitatives.
# Cela permet d’identifier les relations linéaires entre les variables avant de procéder à l'ACP.
cor(data_sommeil_quanti)

# Application de l'ACP sur les données quantitatives avec centrage et réduction des variables 
# (scale.unit = TRUE). Cela permet de normaliser les variables avant l'ACP.
ACP = PCA(data_sommeil_quanti, scale.unit = TRUE)

# Affichage d'un résumé des résultats de l'ACP : 
# - Valeurs propres (pour savoir combien de variance chaque composante explique),
# - Proportions de variance expliquées par chaque composante principale, etc.
summary(ACP)

# Affichage des variables dans l'espace des deux premières composantes principales.
# Cela montre comment les variables se répartissent selon ces dimensions.
plot.PCA(ACP, choix = "var", cex = 0.5)

# Visualisation graphique avancée des variables dans l'espace factoriel. 
# Les variables sont affichées avec des étiquettes (repel = TRUE pour éviter le chevauchement) 
# et les axes sont les deux premières composantes principales.
library(factoextra)
fviz_pca_var(ACP, col.var = "black", repel = TRUE, title = "Graphique des variables (axes factoriels 1 et 2)")

# Sélection des mêmes variables quantitatives que précédemment, mais avec l'ajout de la variable qualitative "Genre".
quanti_gender = sommeil[, c("Sleep.Duration", "Quality.of.Sleep", 
                            "Physical.Activity.Level", "Stress.Level",
                            "Blood.Pressure", "Heart.Rate", "Daily.Steps", "Gender")]

# Transformation de la variable "Gender" en facteur afin de l'utiliser dans l'ACP.
quanti_gender$Gender <- as.factor(quanti_gender$Gender)

# Vérification de la structure des données après transformation.
str(quanti_gender)

# Application de l'ACP avec la variable "Gender" comme variable qualitative supplémentaire (quali.sup = 8).
ACP_gender = PCA(quanti_gender, quali.sup = 8)

# Affichage des valeurs propres et des résultats des variables pour l'ACP avec "Gender".
ACP_gender$eig
ACP_gender$var

# Affichage des variables dans l'espace des deux premières composantes principales.
plot(ACP_gender, choix = "var", cex = 0.5)

# Affichage des individus dans l'espace factoriel, avec la variable "Gender" utilisée pour colorier les points.
plot(ACP_gender, choix = "ind", habillage = 8, label = "quali")

# Sélection des mêmes variables quantitatives que précédemment, mais avec l'ajout de la variable qualitative "BMI.Category".
quanti_BMI.Category = sommeil[, c("Sleep.Duration", "Quality.of.Sleep", 
                                  "Physical.Activity.Level", "Stress.Level",
                                  "Blood.Pressure", "Heart.Rate", "Daily.Steps", "BMI.Category")]

# Transformation de la variable "BMI.Category" en facteur afin de l'utiliser dans l'ACP.
quanti_BMI.Category$BMI.Category <- as.factor(quanti_BMI.Category$BMI.Category)

# Vérification de la structure des données après transformation.
str(quanti_BMI.Category)

# Application de l'ACP avec la variable "BMI.Category" comme variable qualitative supplémentaire (quali.sup = 8).
ACP_BMI.Category = PCA(quanti_BMI.Category, quali.sup = 8)

# Affichage des valeurs propres et des résultats des variables pour l'ACP avec "BMI.Category".
ACP_BMI.Category$eig
ACP_BMI.Category$var

# Affichage des variables dans l'espace des deux premières composantes principales.
plot(ACP_BMI.Category, choix = "var", cex = 0.5)

# Affichage des individus dans l'espace factoriel, avec la variable "BMI.Category" utilisée pour colorier les points.
plot(ACP_BMI.Category, choix = "ind", habillage = 8, label = "quali")





#Explication générale : RESUME
#Chargement des données : Le fichier CSV contenant les données sur le sommeil, l’activité physique, et la santé est chargé dans R.

#Exploration des données : Après avoir vérifié la structure du jeu de données, des histogrammes sont générés pour mieux comprendre la distribution des différentes variables.

#Analyse de corrélation : La corrélation entre les variables quantitatives est calculée pour identifier d’éventuelles relations linéaires avant de procéder à l'ACP.

#ACP (Analyse en Composantes Principales) : L’ACP est réalisée sur les variables quantitatives afin de réduire la dimensionnalité des données et de détecter les structures sous-jacentes.

#Incorporation de variables qualitatives (Genre et IMC) : Des variables qualitatives, telles que le genre et la catégorie d'IMC, sont ajoutées pour examiner leurs effets sur les comportements de sommeil et les paramètres de santé.

#Visualisations : Des graphiques sont générés pour visualiser les résultats de l'ACP, notamment la projection des variables et des individus dans l’espace des deux premières composantes principales.