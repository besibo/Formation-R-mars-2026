# Script 1
# Prise en main de R : objets, indexation, import/export et premiers graphiques.

# Mise en mémoire des packages
library(palmerpenguins)
library(tidyverse)


# Les vecteurs ----
# Cette première partie présente les principaux types de vecteurs en R ainsi
# que quelques opérations de base appliquées à ces objets.

# Vecteurs numériques
1:10 # Suite d'entiers

# Vecteur numérique quelconque
vec <- c(21, 3, 76, 65, 2, 1000)
vec # Affichage de vec
mean(vec) # Affichage de la moyenne de vec

# Vecteurs de chaînes de caractères
c("rouge", "vert", "jaune", "rouge")
couleurs <- c("rouge", "vert", "jaune", "rouge")

# Vecteurs de vrais/faux (vecteurs logiques/booléens)
vf <- c(TRUE, T, T, FALSE, FALSE, T, F)
vf

# Pour lister les objets de l'environnement de travail
ls()

# Pour supprimer les objets de l'environnement de travail
# rm(vf)

# Combiner des vecteurs
vec
c(vec, vec)
c(vec, vf)
sum(vf)
mean(vf)
c(vec, couleurs)

# Pour connaitre le type de vecteur
class(couleurs)
class(vf)
class(vec)

# Opérations sur les vecteurs
vec
mean(vec) # Moyenne
sum(vec) # Somme
sd(vec) # Écart-type
var(vec) # Variance
log(vec) # Log naturel
exp(vec) # Exponentielle
rank(vec) # Rang
order(vec) # Ordre des permutation nécessaires pour trier vec
sort(vec) # Tri de vec par ordre croissant

length(vec) # Nombre d'éléments
dim(vec) # Dimension du vecteur

# La notion de recyclage
a <- c(1, 2, 3, 4)
b <- c(3, 6, 9)
d <- c(10, 100)


a + b
a * c(10)
a + d

# Quelques fonctions qui permettent de créer des séquences régulières
# Ces fonctions sont très utiles pour générer rapidement des suites de valeurs.

# L'opérateur ":"
2:10
c(2:10) # c() est inutile dans ce cas
c(c(c(2:10))) # idem

# La différence entre , et .
c(1, 5:10, 5)
1.5:10.4

-2:5
5:-3

# La fonction seq()
seq(from = 1, to = 20, by = 2)
seq(from = 20, to = 2, by = -2)
seq(from = 0, to = 1, by = 0.05)
seq(from = 0, to = 100, length.out = 21)

# La fonction rep() : pour répliquer des vecteurs
rep(couleurs, times = 3)
serie <- rep(couleurs, each = 3)


# Les facteurs ----
# Les facteurs servent à représenter des variables qualitatives / catégorielles.

mois <- c(
       "jan",
       "fev",
       "mar",
       "avr",
       "avr",
       "mai",
       "jui",
       "juil",
       "aou",
       "sep",
       "oct",
       "nov",
       "dec"
)

mois
mois_fac <- factor(mois)

levels(mois_fac)
levels(mois_fac) <- c(8, 4, 12, 2, 1, 6, 7, 5, 3, 11, 10, 9)
mois_fac

mean(mois_fac)


# Pour imposer un ordre particulier aux niveaux d'un facteur, il faut spécifier
# les niveaux dans l'ordre souhaité lors de la création du facteur.
cat <- c(
       "jan",
       "fev",
       "mar",
       "avr",
       "mai",
       "jui",
       "juil",
       "aou",
       "sep",
       "oct",
       "nov",
       "dec"
)

mois_fac_ok <- factor(mois, levels = cat)
mois_fac_ok

# Les tableaux ou data.frame ----
# Les data frames et tibbles permettent de stocker des données tabulaires.

df <- data.frame(
       Jour = c("Lundi", "Mardi", "Jeudi", "Dimanche"),
       Vent = c(15, 23, NA, 45),
       Soleil = c(TRUE, TRUE, TRUE, FALSE),
       ident = a
)

df
class(df)

# La fonction summary est une fonction générique qui fournit
# un résumé statistique différent selon le type d'objet fourni.
summary(vec)
summary(mois_fac)
summary(vf)
summary(df)
str(df)

# Les tibbles : des data.frames particuliers
class(palmerpenguins::penguins)
palmerpenguins::penguins

class(datasets::penguins)
datasets::penguins
penguins


# L'indexation ----
# L'indexation permet d'extraire des éléments par position, par nom ou via une
# condition logique.

# L'indexation par position
couleurs
couleurs[2]
couleurs[c(4, 3, 2, 1)]
couleurs[c(1, 2, 2, 2, 3, 4)]
couleurs[5]
couleurs[-2]

df[1, 1] # Lignes , colonnes
df[c(1, 2), 1]
df[c(1, 2), ]
df[, -4]

df[, 2]
df[, "Vent"]
df$Vent

# L'indexation par condition
vec
vec > 50
vec[vec > 50]

couleurs == "rouge"
couleurs[couleurs != "rouge"]

df[df$Soleil == TRUE, ]

# Pour récupérer les longueurs de bec des manchots mâles de l'espèce Gentoo
penguins[penguins$sex == "male" & penguins$species == "Gentoo", 3]
penguins[penguins$species == "Gentoo" & penguins$sex == "male", 3]

summary(penguins)
levels(penguins$species)

# La façon la plus élégante de faire ce type de manipulation est d'utiliser les fonctions du tidyverse.
# Voir script 2 pour une présentation plus détaillée de ces fonctions.
penguins %>%
       filter(species == "Gentoo", sex == "male") %>%
       select(bill_length_mm)


df

# À ce stade, on passe de la manipulation en mémoire à la lecture/écriture de
# fichiers externes.

# Exportation de données
write.csv(df, "meteo.csv")
write.csv2(df, "meteo2.csv", row.names = FALSE)

# Importation de données.
# Attention à bien copier-coller dans le script les commandes de l'assistant d'importation
dauphin <- read_delim(
       "Data/dauphin.csv",
       delim = ";",
       escape_double = FALSE,
       locale = locale(decimal_mark = ","),
       trim_ws = TRUE
)
View(dauphin)
dauphin

# Si l'assistant d'importation ne fonctionne pas :
dauph <- read.table("Data/dauphin.csv", header = TRUE, dec = ",", sep = ";")

dauph <- as_tibble(dauph)
dauph

identical(dauphin, dauph)
class(dauph)
class(dauphin)

# pour les fichiers Excel, il faut utiliser la fonction read_excel du package readxl
library(readxl)
dauph_excel <- read_excel("Data/dauphin.xls", na = "*", skip = 9)

dauph_excel


# Graphiques avec ggplot2 ----
# Les graphiques suivants illustrent la logique de ggplot2 :
# 1. définir les données ;
# 2. déclarer les variables esthétiques avec aes() ;
# 3. ajouter une ou plusieurs couches géométriques.

# Exemples de nuages points et illustration du paradoxe de Simpson
ggplot(data = penguins, mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
       geom_point()

# Changement de la couleur de tous les points
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
       geom_point(color = "red")

# Association de la couleur à une variable catégorielle
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
       geom_point()

# Association de la couleur à une variable numérique continue
ggplot(
       penguins,
       aes(x = bill_length_mm, y = bill_depth_mm, color = flipper_length_mm)
) +
       geom_point()

# Changement du type de point et de la transparence, et ajout de courbes de tendance
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, fill = sex)) +
       geom_point(alpha = 0.7, shape = 21) +
       geom_smooth(se = FALSE)

# Le paradoxe de Simpson : la pente de la regression linéaire est décroissante pour les données globales...
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
       geom_point() +
       geom_smooth(method = "lm")

# ... mais elle est croissante pour chacune des espèces prises séparément :
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
       geom_point() +
       geom_smooth(method = "lm")


# L'importance du placement de aes() : si on place aes(color = species) dans geom_point()
# et pas dans ggplot(), alors la courbe de tendance est calculée sur l'ensemble des données et
# non séparément pour chaque espèce.
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
       geom_point(aes(color = species)) +
       geom_smooth(method = "lm")
