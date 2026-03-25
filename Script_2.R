# Script 2
# Visualisation de données avec ggplot2 puis manipulation de tableaux avec dplyr.

# Mise en mémoire des packages
library(palmerpenguins)
library(tidyverse)
library(scales) # Pour la mise en forme des axes

# Importation de données
dauphin <- read_delim(
  "Data/dauphin.csv",
  delim = ";",
  escape_double = FALSE,
  locale = locale(decimal_mark = ","),
  trim_ws = TRUE
)


# Stripchart ----
# Un nuage de points avec jitter permet de visualiser chaque observation tout
# en limitant les superpositions.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7)

# Même graphique, mais avec les axes inversés
ggplot(penguins, aes(y = species, x = body_mass_g)) +
  geom_jitter(width = 0, height = 0.2, alpha = 0.7)


# Nuage de points et courbes de tendance
# Ici, on représente la relation entre âge et taille, avec une couleur par sexe
# et une droite de tendance ajoutée à chaque groupe.
ggplot(dauphin, aes(x = Age, y = Taille, color = Sexe)) +
  geom_point() +
  geom_smooth(se = FALSE, linewidth = 0.5, linetype = 2)

# Sauvegarde avec ggsave()
ggsave("Ma_belle_figure.png", width = 7, height = 4, dpi = 300)

# Les histogrammes ----
# Les histogrammes et densités sont adaptés à l'exploration de distributions.

# Premier histogramme simple : on regarde la distribution globale des masses
# corporelles, sans distinguer les espèces.
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(
    fill = "steelblue",
    color = "grey30",
    bins = 20
  )

# Même variable, mais cette fois avec un remplissage par espèce pour comparer
# visuellement la contribution des groupes. L'utilisation des facets permettra d'y voir plus clair
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(
    color = "grey30",
    binwidth = 250
  )

# Graphique des densités,, qui évite la difficulté du choix des classes
# et facilite les comparaisons entre groupes au prix d'un axe des y plus difficile à interpréter
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_density(
    color = "grey30",
    alpha = 0.4
  )

# Les boîtes à moustaches ----
# Les boxplots et violin plots résument la distribution tout en facilitant la
# comparaison entre groupes.
# Ce premier graphique superpose un boxplot et les points individuels, avec une
# couleur par sexe pour voir la dispersion à l'intérieur de chaque espèce.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot(notch = TRUE) +
  geom_jitter(width = 0.3, height = 0, alpha = 0.4, aes(color = sex))

# Variante du bloc précédent : on retire les sexes manquants et on utilise le
# remplissage des boîtes pour faire apparaître les groupes plus directement.
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(notch = TRUE, alpha = 0.5) +
  geom_jitter(
    shape = 21,
    position = position_jitterdodge()
  ) # Pour que les points soient alignés avec les boîtes

# Version minimale du boxplot, sans points ajoutés ni distinction par sexe.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()

# On change ensuite de géométrie : le violon montre la forme de la distribution :
# c'est un mélange entre boxplot et graphique de densité.
# Ici, on ajoute une ligne pour indiquer la médiane et les quartiles
ggplot(penguins, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_violin(quantile.linetype = 1)

# Enfin, on combine violon et boxplot pour résumer à la fois la densité et les
# statistiques de position.
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_violin() +
  geom_boxplot(width = 0.2)

# Les facets ----
# Les facettes produisent un panneau par modalité d'un facteur afin de comparer visuellement
# les mêmes relations dans plusieurs sous-groupes.
# On reprend l'histogramme des masses, mais en séparant les espèces en panneaux
# distincts empilés verticalement.
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(color = "grey30", bins = 20, show.legend = FALSE) +
  facet_wrap(~species, ncol = 1, scales = "free_y")

# Même idée, mais avec une grille croisant sexe et espèce pour affiner la
# comparaison.
ggplot(penguins, aes(x = body_mass_g, fill = species)) +
  geom_histogram(color = "grey30", bins = 20, show.legend = FALSE) +
  facet_grid(sex ~ species)

# Ici, on quitte les distributions univariées pour un nuage de points avec
# régression linéaire, facetté par espèce.
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~species, ncol = 2, scales = "free")


# Exercice sur les dauphins : correction ----
# Retour au jeu de données des dauphins avec le même principe : nuage de points
# âge-taille et tendance lissée, colorés par sexe.
ggplot(dauphin, aes(x = Age, y = Taille, color = Sexe)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Même chose mais avec une seule courbe de tendance :
# la couleur est portée par les points seulement, mais pas par la courbe de tendance
ggplot(dauphin, aes(x = Age, y = Taille)) +
  geom_point(aes(color = Sexe)) +
  geom_smooth(se = FALSE)

# Cette troisième version ajoute aussi la forme des points et le type de ligne
# pour renforcer la distinction entre sexes.
ggplot(dauphin, aes(x = Age, y = Taille, shape = Sexe, linetype = Sexe)) +
  geom_point() +
  geom_smooth(se = FALSE)

# Diagrammes bâtons ----
# Les diagrammes en bâtons servent ici à représenter des effectifs.
# Comptage simple du nombre d'individus par espèce chez les manchots.
ggplot(penguins, aes(x = species)) +
  geom_bar()

# Même principe appliqué à la variable Repro du tableau dauphin.
ggplot(dauphin, aes(x = Repro)) +
  geom_bar()

# On réordonne ici les espèces de la plus fréquente à la moins fréquente pour
# améliorer la lecture du diagramme.
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar(color = "grey30", fill = "steelblue")

# Diagramme bâtons empilé
# On ajoute maintenant le sexe en remplissage : les effectifs sont ventilés à
# l'intérieur de chaque barre.
ggplot(penguins, aes(x = fct_infreq(species), fill = sex)) +
  geom_bar(color = "grey30")

# Diagramme bâtons juxtaposé
# Même décomposition par sexe, mais cette fois avec des barres côte à côte pour
# comparer plus directement les effectifs.
ggplot(penguins, aes(x = fct_infreq(species), fill = sex)) +
  geom_bar(
    color = "grey30",
    position = position_dodge(
      preserve = "single" # Pour que la largueur des barres soit la même partout
    )
  )

# Diagramme bâtons normalisés
# On normalise ensuite chaque barre pour comparer des proportions plutôt que
# des effectifs bruts.
ggplot(penguins, aes(x = fct_infreq(species), fill = sex)) +
  geom_bar(color = "grey30", position = position_fill())

# Système de coordonnées ----
# coord_flip() inverse les axes sans modifier les données.
# Ce bloc reprend le diagramme normalisé précédent en orientation horizontale.
ggplot(penguins, aes(x = fct_infreq(species), fill = sex)) +
  geom_bar(color = "grey30", position = position_fill()) +
  coord_flip()

# D'autres systèmes de corrdonnées existent. Consulter les diapos de la formation pour en savoir plus.

# Les labels ----
# Une figure destinée à être diffusée doit comporter des labels explicites,
# un titre et, si nécessaire, une source.
# On enrichit ici un graphique déjà vu avec des labels, un thème et une palette
# personnalisée pour obtenir une figure publiable.
# Attention : si vous ne disposez pas de la police Optima-Regular (ou si elle n'est pas accessible par R),
# le graphique risque d'être mal généré
penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(notch = TRUE, alpha = 0.5) +
  geom_jitter(
    shape = 21,
    position = position_jitterdodge()
  ) +
  labs(
    x = "Espèce",
    y = "Masse corporelle (g)",
    fill = "Sexe",
    title = "Masse de 3 espèces de manchots",
    subtitle = "Le dimorphisme sexuel est marqué",
    caption = "Source : palmerpenguins"
  ) +
  theme_bw(base_size = 10, base_family = "Optima-Regular") +
  theme(axis.title.x = element_text(hjust = 0)) +
  scale_fill_manual(values = c("purple", "orange"))


# Exemple de changement d'échelle de couleur continue :
# on utilise une palette de viridis, plus lisible que les palettes classiques,
# et on inverse la direction pour faire apparaître les valeurs les plus élevées en foncé.
ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, color = flipper_length_mm)
) +
  geom_point() +
  scale_color_viridis_c(option = "D", direction = -1)

# Variante du bloc précédent : la variable continue est portée par le
# remplissage des points, avec une autre palette et une personnalisation de l'axe y :
# on fournit 3 couleurs et la valeur qui correspond à la valeur médiane de la
# variable pour faire apparaître la couleur intermédiaire à ce niveau.
ggplot(
  penguins,
  aes(x = bill_length_mm, y = bill_depth_mm, fill = flipper_length_mm)
) +
  geom_point(shape = 21, color = "grey30") +
  scale_fill_gradient2(
    low = "steelblue",
    high = "darkorchid",
    mid = "white",
    midpoint = 190
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  theme_bw()

# ------------------------------------------------------------------------

# Manipuler des tableaux avec dplyr ----
# Deuxième partie : sélection, filtrage, tri, création de variables et résumés.

# La fonction filter() ----
# filter() conserve uniquement les lignes répondant à une ou plusieurs
# conditions logiques.

# On commence par ne garder que les individus de l'espèce Gentoo.
penguins %>%
  filter(species == "Gentoo")

# Inversement, ce bloc retire les Gentoo pour conserver toutes les autres espèces.
penguins %>%
  filter(species != "Gentoo")

# À partir de dplyr version 1.2, la fonction filter_out permet de retirer
# les lignes répondant à une condition, ce qui peut être plus lisible que
# de devoir écrire une condition négative.
penguins %>%
  filter_out(species == "Gentoo")

# Ici, on isole les lignes où le sexe est manquant afin de repérer les NA.
penguins %>%
  filter(is.na(sex)) %>%
  View()

# Même logique, mais cette fois on conserve uniquement les lignes pour lesquelles le sexe est connu.
penguins %>%
  filter(!is.na(sex))

# filter_out() produit ici le même résultat que le bloc précédent, en retirant
# explicitement les lignes avec sexe manquant.
penguins %>%
  filter_out(is.na(sex))

# On sélectionne maintenant deux espèces à l'aide de %in%, plus lisible quand
# plusieurs modalités sont autorisées.
penguins %>%
  filter(species %in% c("Gentoo", "Adelie"))

# Même filtre que précédemment, mais écrit avec l'opérateur logique |.
penguins %>%
  filter(species == "Gentoo" | species == "Adelie")

# summary() donne un aperçu rapide de toutes les variables du tableau.
summary(penguins)

# Manchots Chinstrap mâles dont le bec est plus long que 50 mm
# Première écriture de la condition, avec des opérateurs logiques explicites.
penguins %>%
  filter(species == "Chinstrap" & sex == "male" & bill_length_mm > 50)

# Même sélection, mais avec plusieurs arguments séparés dans filter(), souvent
# plus lisible.
penguins %>%
  filter(species == "Chinstrap", sex == "male", bill_length_mm > 50)

# Manchots Chinstrap mâles dont le bec est compris entre 50 et 52 mm inclus
# On ajoute ici une borne inférieure et une borne supérieure écrites séparément.
penguins %>%
  filter(
    species == "Chinstrap",
    sex == "male",
    bill_length_mm >= 50,
    bill_length_mm <= 52
  )

# Même intervalle que précédemment, mais avec between() pour condenser l'écriture.
penguins %>%
  filter(species == "Chinstrap", sex == "male", between(bill_length_mm, 50, 52))

# À partir d'ici, on combine souvent plusieurs verbes/fonctions dplyr dans un pipeline
# pour décrire une chaîne complète de traitement.

# Exemple de pipeline mixte : filtrer les Adélie puis construire directement un
# graphique sur le sous-ensemble obtenu.
penguins %>%
  filter(species == "Adelie") %>%
  ggplot(aes(x = body_mass_g, y = bill_length_mm, colour = sex)) +
  geom_point(alpha = 0.8) +
  theme_bw()

# Trier avec arrange() ----
# arrange() trie les lignes d'un tableau selon une ou plusieurs variables.

# Tri croissant sur la masse corporelle.
penguins %>%
  arrange(body_mass_g) # ordre croissant

# Même opération, mais avec un tri décroissant puis un second critère de tri pour
# départager les exæquo.
penguins %>%
  arrange(
    desc(body_mass_g), # desc() : ordre décroissant
    flipper_length_mm
  )

# Chez les Gentoo, quelles sont les plus grandes longueurs de nageoires ?
# On filtre d'abord les Gentoo, puis on trie pour faire remonter les plus
# grandes longueurs de nageoires.
penguins %>%
  filter(species == "Gentoo") %>%
  arrange(desc(flipper_length_mm)) %>%
  print(n = Inf) # Pour afficher toutes les données, pas seulement les 10 premières


# Sélectionner des colonnes avec select() ----
# select() permet de garder, retirer ou réordonner des variables.

# On commence par extraire un petit sous-tableau avec trois variables.
# On lui donne un nom pour pouvoir le réutiliser ensuite si besoin.
small <- penguins %>%
  select(species, body_mass_g, sex)

small

# Ici, on enlève explicitement deux colonnes jugées inutiles.
penguins %>%
  select(-island, -year)

# Cette écriture conserve toutes les colonnes comprises entre species et
# flipper_length_mm dans l'ordre du tableau.
penguins %>%
  select(species:flipper_length_mm)

# Sélection des variables dont le nom se termine par "_mm".
penguins %>%
  select(ends_with("_mm"))

# Sélection des variables dont le nom commence par "bill".
penguins %>%
  select(starts_with("bill"))

# Sélection des variables dont le nom contient le motif "length".
penguins %>%
  select(contains("length"))

# On choisit ici quelques variables à placer en tête, puis everything() ajoute
# toutes les autres à la suite.
penguins %>%
  select(species, sex, body_mass_g, everything())

# Créer ou modifier des colonnes avec mutate() ----
# mutate() crée de nouvelles variables ou modifie des variables existantes.

# On crée ici un indicateur de forme du bec (bill_shape), immédiatement utilisé dans un
# graphique.
penguins %>%
  mutate(bill_shape = bill_depth_mm / bill_length_mm) %>%
  ggplot(aes(x = flipper_length_mm, y = bill_shape, color = species)) +
  geom_point()

# Même principe, mais avec une variable logique indiquant si le bec dépasse un seuil.
penguins %>%
  mutate(long_bill = bill_length_mm > 48.5) %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm, colour = long_bill)) +
  geom_point()

# Ici, mutate() est combiné à rename() et relocate() pour modifier, renommer et
# réorganiser les colonnes.
penguins %>%
  mutate(body_mass_g = body_mass_g / 1000) %>%
  rename(mass = body_mass_g, bill_length = bill_length_mm) %>%
  relocate(year, .after = island)

# Dernier exemple simple : on crée un identifiant correspondant à l'ordre inverse
# des lignes.
penguins %>%
  mutate(id = rev(row_number()))


# Les résumés statistiques avec summarise() ----
# summarise() produit des tableaux de synthèse, souvent après un group_by().

# Résumé global de la longueur du bec sur l'ensemble du tableau.
penguins %>%
  summarise(
    moyenne = mean(bill_length_mm, na.rm = TRUE),
    variance = var(bill_length_mm, na.rm = TRUE),
    mediane = median(bill_length_mm, na.rm = TRUE)
  )

# Comparaison utile : summary() fournit aussi un résumé, mais avec un format
# standard sur toutes les variables.
summary(penguins)


# On passe ensuite à des résumés pour chaque combinaison d'espèce et de sexe.
penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarise(
    moyenne = mean(bill_length_mm, na.rm = TRUE),
    variance = var(bill_length_mm, na.rm = TRUE),
    mediane = median(bill_length_mm, na.rm = TRUE),
    nombre = n()
  )

# Le package skimr
# skimr fournit un aperçu statistique plus riche qu'un simple summary().
library(skimr)
# Même logique de résumé par groupes, mais avec une sortie plus détaillée grâce
# à skimr.
penguins %>%
  group_by(species, sex) %>%
  skim(bill_depth_mm, bill_length_mm)

# On trie ici les données pour les inspecter visuellement dans l'ordre.
penguins %>%
  arrange(species, body_mass_g) %>%
  View()

# Ce bloc filtre les mâles puis, dans chaque espèce, conserve seulement les
# individus plus lourds que la moyenne de leur groupe.
penguins %>%
  filter(sex == "male") %>%
  group_by(species) %>%
  filter(body_mass_g > mean(body_mass_g, na.rm = TRUE))

# On crée une variable logique, puis on calcule la proportion de becs longs par
# espèce en prenant la moyenne de VRAI/FAUX.
penguins %>%
  mutate(long_bec = bill_length_mm > 48) %>%
  group_by(species) %>%
  summarise(prop_long = mean(long_bec, na.rm = TRUE))

# Même résultat mais sans variable intermédiaire : la condition est écrite
# directement dans summarise().
penguins %>%
  group_by(species) %>%
  summarise(prop_long = mean(bill_length_mm > 48, na.rm = TRUE))

# Exercice ----
# Créer une colonne bec_long, (V/F), qui contient vrai pour les individus
# qui ont les becs les plus longs (>= Q3) pour chaque espèce.
# On fait un graphique, pour faire apparaître ces individus en couleur,
# et les autres en gris sur un nuage de point de la longueur des nageoires en fonction
# de la masse corporelle (1 graph par espèce).
# Ce dernier bloc combine plusieurs notions vues plus haut :
# group_by(), mutate(), filtre des NA, facettes et personnalisation graphique.

# Première version : la définition de bec_long se fait à l'échelle de l'espèce
# uniquement, puis le résultat est visualisé avec des facettes.

penguins %>%
  group_by(species) %>%
  mutate(
    bec_long = bill_length_mm >= quantile(bill_length_mm, 0.75, na.rm = TRUE)
  ) %>%
  filter(!is.na(bec_long)) %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm, fill = bec_long)) +
  geom_point(shape = 21, alpha = 0.6) +
  facet_wrap(~species, ncol = 1, scales = "free") +
  labs(fill = "") +
  scale_fill_manual(
    values = c("grey", "red"),
    label = c("Becs courts", "Becs longs")
  ) +
  scale_x_continuous(labels = scales::number_format(big.mark = " ")) +
  theme_bw()

# On peut utiliser des conditions directement dans aes(), par exemple pour
# distinguer les Gentoo des autres espèces sans faire de facettes.
penguins %>%
  ggplot(aes(
    x = body_mass_g,
    y = flipper_length_mm,
    fill = species == "Gentoo"
  )) +
  geom_point(shape = 21, alpha = 0.6) +
  scale_fill_manual(values = c("grey30", "red")) +
  theme_bw() +
  labs(fill = "Gentoo ?")
