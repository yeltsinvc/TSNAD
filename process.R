# Installer les bibliothèques si ce n'est pas déjà fait
install.packages("dplyr")
install.packages("stringr")

# Charger les bibliothèques
library(dplyr)
library(stringr)

# Définir le chemin vers le fichier
fichier <- "data/EDGT34_TOTAL_PERSONNES.TXT"

# Lire les lignes du fichier
lignes <- readLines(fichier)

# Fonction pour extraire les variables à partir d'une ligne donnée
extraire_variables <- function(ligne) {
  variables <- data.frame(
    PP1 = str_sub(ligne, 1, 1),
    TIRA = str_sub(ligne, 2, 4),
    PP2 = str_sub(ligne, 5, 7),
    ECH = str_sub(ligne, 8, 10),
    PER = str_sub(ligne, 11, 12),
    DATE = str_sub(ligne, 13, 18),
    PENQ = str_sub(ligne, 19, 19),
    JOURDEP = str_sub(ligne, 20, 20),
    P2 = str_sub(ligne, 21, 21),
    P3 = str_sub(ligne, 22, 22),
    P4 = str_sub(ligne, 23, 24),
    P7 = str_sub(ligne, 25, 25),
    P8 = str_sub(ligne, 26, 26),
    P9 = str_sub(ligne, 27, 27),
    P14 = str_sub(ligne, 28, 28),
    P15 = str_sub(ligne, 29, 34),
    P16 = str_sub(ligne, 35, 35),
    P17 = str_sub(ligne, 36, 36),
    P18 = str_sub(ligne, 37, 37),
    P19 = str_sub(ligne, 38, 38),
    P20 = str_sub(ligne, 39, 39),
    P21 = str_sub(ligne, 40, 40),
    P22 = str_sub(ligne, 41, 41),
    P23 = str_sub(ligne, 42, 42),
    P24A = str_sub(ligne, 43, 43),
    P11A = str_sub(ligne, 44, 44),
    P25 = str_sub(ligne, 45, 45),
    P26 = str_sub(ligne, 46, 46),
    PL27 = str_sub(ligne, 47, 47),
    PL28 = str_sub(ligne, 48, 48),
    P12A = str_sub(ligne, 49, 49),
    P12B = str_sub(ligne, 50, 50),
    P12C = str_sub(ligne, 51, 51),
    P12D = str_sub(ligne, 52, 52),
    P12E = str_sub(ligne, 53, 53),
    P12F = str_sub(ligne, 54, 54),
    P1 = str_sub(ligne, 55, 55),
    COE1 = str_sub(ligne, 63, 70),
    COEP = str_sub(ligne, 71, 78),
    DP13 = str_sub(ligne, 77, 82),
    PFIN = str_sub(ligne, 78, 78)
  )
  return(variables)
}

# Appliquer la fonction à toutes les lignes
donnees <- lapply(lignes, extraire_variables)

# Convertir la liste en data.frame
df <- do.call(rbind, donnees)

# Afficher un aperçu des données
print(head(df))

# Afficher le data frame entier si nécessaire
View(df)

# Nettoyage des espaces blancs
df <- df %>%
  mutate(across(everything(), str_trim))

# Convertir les variables en numérique ou factor si nécessaire
df$P4 <- as.numeric(df$P4)  # Convertir l'âge en numérique
df$P2 <- factor(df$P2)      # Convertir le sexe en facteur


# Résumé statistique pour toutes les variables numériques
summary(df)

# Résumé statistique spécifique pour l'âge (P4)
summary(as.numeric(df$P4))

# Comptage de la distribution du sexe (P2)
table(df$P2)

# Comptage de la possession du permis de conduire (P7)
table(df$P7)


# Moyenne de l'âge (en supprimant les valeurs manquantes)
mean(as.numeric(df$P4), na.rm = TRUE)

# Pourcentage de personnes avec un permis de conduire (P7)
prop.table(table(df$P7)) * 100




# Installer ggplot2 si nécessaire
install.packages("ggplot2")

# Charger ggplot2
library(ggplot2)

# Créer un histogramme de l'âge (P4)
ggplot(df, aes(x = as.numeric(P4))) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Répartition des âges", x = "Âge", y = "Nombre de personnes") +
  theme_minimal()

# Diagramme à barres pour le sexe (P2)
ggplot(df, aes(x = P2)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Distribution du sexe", x = "Sexe", y = "Nombre de personnes") +
  theme_minimal()

# Diagramme à barres pour la possession du permis de conduire (P7)
ggplot(df, aes(x = P7)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Possession du permis de conduire", x = "Permis de conduire", y = "Nombre de personnes") +
  theme_minimal()

# Boxplot de l'âge par sexe
ggplot(df, aes(x = P2, y = as.numeric(P4), fill = P2)) +
  geom_boxplot() +
  labs(title = "Répartition de l'âge par sexe", x = "Sexe", y = "Âge") +
  theme_minimal()




# Mapper le sexe
df$P2 <- factor(df$P2, 
                levels = c("1", "2"), 
                labels = c("Masculin", "Féminin"))

# Mapper le permis de conduire
df$P7 <- factor(df$P7, 
                levels = c("1", "2", "3"), 
                labels = c("Oui", "Non", "Conduite accompagnée"))

# Mapper le jour des déplacements
df$JOURDEP <- factor(df$JOURDEP, 
                     levels = c("1", "2", "3", "4", "5"), 
                     labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi"))



# Distribution des niveaux d'études
table(df$P8)

# Pourcentage des personnes ayant un permis de conduire
prop.table(table(df$P7)) * 100

# Diagramme à barres pour le niveau d'études
ggplot(df, aes(x = P8)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution du niveau d'études", x = "Niveau d'études", y = "Nombre de personnes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Diagramme à barres pour le jour des déplacements
ggplot(df, aes(x = JOURDEP)) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Jour des déplacements", x = "Jour", y = "Nombre de déplacements") +
  theme_minimal()

# Boxplot de l'âge par possession de permis de conduire
ggplot(df, aes(x = P7, y = as.numeric(P4), fill = P7)) +
  geom_boxplot() +
  labs(title = "Répartition de l'âge par possession de permis", x = "Permis de conduire", y = "Âge") +
  theme_minimal()

# S'assurer que l'âge est numérique
df$P4 <- as.numeric(df$P4)

# Supprimer les lignes avec des valeurs manquantes
df_clean <- na.omit(df[, c("P4", "P2", "P8")])

# Modèle ANCOVA: effet du sexe (P2) sur l'âge (P4) avec contrôle du niveau d'études (P8)
ancova_model <- lm(P4 ~ P2 + P8, data = df_clean)

# Résumé du modèle ANCOVA
summary(ancova_model)

# Bibliothèque pour les visualisations
library(ggplot2)

# Visualisation des moyennes ajustées
ggplot(df_clean, aes(x = P2, y = P4, color = P8)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Effet du sexe sur l'âge en fonction du niveau d'études", x = "Sexe", y = "Âge") +
  theme_minimal()



# Supposons que chaque jour où une personne a fait un déplacement soit compté comme 1 déplacement
# On peut créer une variable "nb_deplacements" qui compte combien de jours une personne a voyagé

# Si JOURDEP est le jour du déplacement (ex. 1 = lundi, etc.), nous considérons qu'il y a 1 déplacement par jour
df$nb_deplacements <- ifelse(!is.na(df$JOURDEP), 1, 0)

# Résumer pour obtenir le nombre total de déplacements par personne
# Par exemple, en cumulant sur plusieurs jours si c'est un échantillon qui contient des multiples jours
# Convertir l'âge en numérique
df$P4 <- as.numeric(df$P4)

# Assurer que "nb_deplacements" est numérique
df$nb_deplacements <- as.numeric(df$nb_deplacements)

# Supprimer les valeurs manquantes
df_clean <- na.omit(df[, c("P4", "nb_deplacements")])

# Modèle de régression linéaire : âge (P4) prédit le nombre de déplacements
regression_model <- lm(nb_deplacements ~ P4, data = df_clean)

# Résumé du modèle
summary(regression_model)


# Graphique de régression linéaire pour l'âge et le nombre de déplacements
ggplot(df_clean, aes(x = P4, y = nb_deplacements)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relation entre l'âge et le nombre de déplacements", x = "Âge", y = "Nombre de déplacements") +
  theme_minimal()


# Lire le fichier de données de déplacements
df_deplacement <- read.fwf("data/EDGT34_TOTAL_DEPLACEMENTS.TXT", 
                           widths = c(1, 3, 3, 3, 2, 2, 2, 2, 6, 2, 2, 2, 2, 6, 2, 2, 3, 2, 1, 1, 2, 2, 8, 8, 8, 1, 1, 1), 
                           header = FALSE)

# Assigner des noms de colonnes
colnames(df_deplacement) <- c("DP1", "TIRA", "DP2", "ECH", "PER", "NDEP", "D2", "D2A", "D3", 
                              "D4A", "D4B", "D5", "D5A", "D7", "D8A", "D8B", "D9", "D6", 
                              "D10", "D10Z", "MODP", "MOIP", "DOIB", "DIST", "DISP", "DFIN", 
                              "D11", "ND11")


# Convertir certaines variables en numérique
df_deplacement$D9 <- as.numeric(df_deplacement$D9)  # Durée du déplacement
df_deplacement$DIST <- as.numeric(df_deplacement$DIST)  # Distance parcourue
df_deplacement$DOIB <- as.numeric(df_deplacement$DOIB)  # Distance vol d'oiseau

# Résumé statistique de la durée et de la distance parcourue
summary(df_deplacement$D9)  # Durée des déplacements
summary(df_deplacement$DIST)  # Distance parcourue


# Créer un histogramme de la durée des déplacements
library(ggplot2)
ggplot(df_deplacement, aes(x = D9)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Distribution de la durée des déplacements", x = "Durée (minutes)", y = "Fréquence") +
  theme_minimal()


# Créer un histogramme de la distance parcourue
ggplot(df_deplacement, aes(x = DIST)) +
  geom_histogram(binwidth = 500, fill = "green", color = "black") +
  labs(title = "Distribution de la distance parcourue", x = "Distance (mètres)", y = "Fréquence") +
  theme_minimal()


# Régression linéaire pour prédire la durée en fonction de la distance
regression_model <- lm(D9 ~ DIST, data = df_deplacement)

# Résumé du modèle
summary(regression_model)

# Graphique de la régression linéaire
ggplot(df_deplacement, aes(x = DIST, y = D9)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relation entre la durée et la distance parcourue", 
       x = "Distance parcourue (mètres)", y = "Durée (minutes)") +
  theme_minimal()


# Moyenne de la durée des déplacements par mode de transport urbain principal
aggregate(D9 ~ MODP, data = df_deplacement, mean)

# Boxplot de la durée par mode de transport
ggplot(df_deplacement, aes(x = as.factor(MODP), y = D9)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Durée des déplacements par mode de transport urbain", x = "Mode de transport", y = "Durée (minutes)") +
  theme_minimal()

# Créer un mappage des codes de modes de transport
transport_mapping <- c(
  "11" = "Conducteur de vélo", "12" = "Passager de vélo", 
  "13" = "Conducteur de 2/3 roues <50cm3", "14" = "Passager de 2/3 roues <50cm3", 
  "15" = "Conducteur de 2/3 roues >=50cm3", "16" = "Passager de 2/3 roues >=50cm3", 
  "21" = "Conducteur de véhicule particulier", "22" = "Passager de véhicule particulier", 
  "31" = "Passager bus ou car urbain", "32" = "Passager tramway", 
  "39" = "Passager autre réseau urbain", "41" = "Passager transports collectifs départementaux", 
  "42" = "Passager autres autocars", "51" = "Passager train", 
  "61" = "Passager taxi", "71" = "Transport employeur", 
  "81" = "Conducteur de fourgon", "82" = "Passager de fourgon", 
  "91" = "Transport fluvial ou maritime", "92" = "Avion", 
  "93" = "Roller, skate, trottinette", "94" = "Fauteuil roulant", 
  "95" = "Autres modes"
)

# Appliquer le mappage aux variables MODP et MOIP
df_deplacement$MODP_desc <- transport_mapping[as.character(df_deplacement$MODP)]
df_deplacement$MOIP_desc <- transport_mapping[as.character(df_deplacement$MOIP)]


# Diagramme à barres de la distribution des modes de transport urbains
ggplot(df_deplacement, aes(x = MODP_desc)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution des modes de transport urbains", x = "Mode de transport", y = "Nombre de déplacements") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculer la durée moyenne des déplacements par mode de transport
moyenne_duree_transport <- aggregate(D9 ~ MODP_desc, data = df_deplacement, mean)

# Afficher les résultats
print(moyenne_duree_transport)

# Graphique boxplot de la durée par mode de transport
ggplot(df_deplacement, aes(x = MODP_desc, y = D9)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Durée des déplacements par mode de transport urbain", x = "Mode de transport", y = "Durée (minutes)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculer la distance moyenne parcourue par mode de transport
moyenne_distance_transport <- aggregate(DIST ~ MODP_desc, data = df_deplacement, mean)

# Afficher les résultats
print(moyenne_distance_transport)

# Graphique boxplot de la distance parcourue par mode de transport
ggplot(df_deplacement, aes(x = MODP_desc, y = DIST)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distance parcourue par mode de transport urbain", x = "Mode de transport", y = "Distance (mètres)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






