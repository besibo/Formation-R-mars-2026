# Script 3
# Introduction à quelques analyses statistiques avec tidyverse et rstatix :
# vérification des conditions, test de Student, alternative non paramétrique,
# régression linéaire simple/multiple et remise en forme de tableaux.

# Mise en mémoire des packages
library(palmerpenguins)
library(tidyverse)
library(car) # Pour le test de Levene
library(rstatix) # Pour tous les autres tests

penguins

# On souhaite comparer la moyenne des masses corporelles entre mâles et femelles de l'espèce Adélie

# Objectif, faire un test de Student avec les hypothèses suivantes :
# H0 : les moyennes sont égales entre mâles et femelles dans la population générale
# H1 : les moyennes sont différentes entre mâles et femelles dans la population générale

# Tous les tests seront faits au seuil alpha = 0.05
# La logique suivie est la suivante :
# 1. vérifier les conditions d'application ;
# 2. réaliser le test ;
# 3. interpréter le résultat ;
# 4. comparer avec une alternative non paramétrique.

# Étape 1 : test de normalité de Shapiro ----
# (il faudrait commencer par de l'exploration graphique et statistique)
# On commence par vérifier les effectifs disponibles dans chaque groupe.
penguins %>%
  filter(species == "Adelie") %>%
  group_by(sex) %>%
  summarise(n_obs = n())

penguins %>%
  filter(species == "Adelie") %>%
  count(sex)

# Création de deux vecteurs de masse corporelle pour les mâles et les femelles
# Cette étape est utile pour utiliser la syntaxe "historique" des tests.
masses_males <- penguins %>%
  filter(species == "Adelie", !is.na(sex), sex == "male") %>%
  pull(body_mass_g)

masses_femelles <- penguins %>%
  filter(species == "Adelie", !is.na(sex), sex == "female") %>%
  pull(body_mass_g)

# Affichage des données
masses_males
masses_femelles

# Calcul des moyennes
mean(masses_femelles)
mean(masses_males)

# Test de Shapiro-Wilk
# H0 : les données suivent une distribution normale
# H1 : les données ne suivent pas une distribution normale

shapiro.test(masses_males)
# p > alpha : donc, on ne peut pas rejeter H0. La masse des mâles suit une distribution normale

shapiro.test(masses_femelles)
# p > alpha : donc, on ne peut pas rejeter H0. La masse des femelles suit une distribution normale

# Les deux groupes suivent une distribution normale, on peut donc comparer les variances.

# Alternative à la première méthode classique : test de Shapiro-Wilk "façon tidyverse"
# Même vérification, mais appliquée directement dans un pipeline.
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  group_by(sex) %>%
  shapiro_test(body_mass_g)

# Les tests sont les mêmes, les résultats obtenus sont strictement identiques,
# mais la syntaxe est plus concise, plus facile à lire et elle évite de devoir
# créer des objets intermédiaires (comme masses_males et masses_femelles).

# Etape 2 : homogénéité des variances ----
# H0 : les variances sont homogènes
# H1 : les variances ne sont pas homogènes

# test F : var.test()
# test de Bartlett : bartlett.test()
# test de Levene : car::leveneTest() ou rstatix::levene_test()
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  levene_test(body_mass_g ~ sex)
# p > alpha : on ne peut pas rejeter H0, les variances sont homogènes

# on peut avoir un aperçu des variabilités grâce à skim()
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  group_by(sex) %>%
  skimr::skim(body_mass_g)

# On peut donc faire le test de Student ----
# H0 : les moyennes des 2 populations sont égales : µ_males - µ_femelles = 0
# H1 : les moyennes des 2 populations sont différentes : µ_males - µ_femelles ≠ 0
# Le premier test est réalisé "façon base R historique", puis avec rstatix.

t.test(masses_males, masses_femelles, var.equal = TRUE)
# Les moyennes sont différentes (Test de Student, t = 13.1, ddl = 144, p < 0.001)

# Student façon tidyverse
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  t_test(body_mass_g ~ sex, detailed = TRUE, var.equal = TRUE)

# Les résultats sont identiques, mais attention à l'ordre des séries :
# Dans la syntaxe "historique", on fournit les séries dans l'ordre "mâles" puis femelles".
# Dans la syntaxe "tidyverse", les séries sont fournies dans l'ordre akphabétique, donc "femelles" puis "mâles".
# Ici, la masse des mâles est plus élevée que celle des femelles, donc la différence de moyenne est positive dans
# le test "historique" et négative dans le test "tidyverse". Même chose pour les intervalles de confiance à 95% de
# la différence de moyennes.

# Si on souhaite faire un test unilatéral, il suffit de préciser l'argument alternative = "less" ou "greater" dans t_test().
# Attention, ici, on n'a pas vraiment le droit de le faire, car on n'a pas d'hypothèse a priori sur le sens de la différence,
# mais c'est juste pour montrer la syntaxe.
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  t_test(
    body_mass_g ~ sex,
    var.equal = TRUE,
    detailed = TRUE,
    alternative = "less"
  )

# Si on n'ajoute pas l'argument var.equal = TRUE, le test de Welch est réalisé,
# qui suppose la normalité, mais pas l'homogénéité des variances. Sa puissance est donc intermédiare
# entre le test de Student et une alternative non paramétrique (test de Wilcoxon).
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  t_test(body_mass_g ~ sex, detailed = TRUE)

# Enfin, si les conditions d'application du test de Student ne sont pas respectées,
# on peut réalise le test non paramétrique de Wilcoxon.
penguins %>%
  filter(species == "Adelie", !is.na(sex)) %>%
  wilcox_test(body_mass_g ~ sex, detailed = TRUE)

# --------------------------------------------------------------------

# Deuxième exemple : relation entre deux variables quantitatives.
# Quelle est la relation entre masse et longueur des nageoires chez les Chinstrap ?
# Exploration graphique
penguins %>%
  filter(species == "Chinstrap") %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm")

# Réalisation de la régression
res <- penguins %>%
  filter(species == "Chinstrap") %>%
  lm(flipper_length_mm ~ body_mass_g + bill_length_mm, data = .)

# Analyse des résidus (vérif conditions d'application)
plot(res)

# Examen des résultats
summary(res)

# nageoire = 0.0119 * masse + 151.4

confint(res)


# penguins %>%
#   filter(species == "Adelie",
#          !is.na(sex)) %>%
#   ggplot(aes(sample = body_mass_g)) +
#   geom_qq() +
#   geom_qq_line()
#
#
# penguins %>%
#   filter(species == "Adelie",
#          !is.na(sex)) %>%
#   ggplot(aes(x = body_mass_g)) +
#   geom_density(fill = "red",
#                alpha = 0.3,
#                color = "red") +
#   geom_histogram(aes(y = after_stat(density))) +
#   facet_wrap(~sex, ncol = 1)
#

# Pivot_longer et pivot_wider
# Dernière partie : quelques opérations de mise en forme de tableaux.
# Elles sont utiles pour préparer des données avant analyse ou visualisation.

masses_long <- penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarise(masse_max = max(body_mass_g, na.rm = TRUE))

masses_larges <- masses_long %>%
  pivot_wider(names_from = species, values_from = masse_max)

masses_larges %>%
  pivot_longer(
    cols = Adelie:Gentoo,
    names_to = "Espece",
    values_to = "max_masses"
  )

# unite() et separate()
# On termine avec un exemple classique de "tidy data" à partir du jeu whoTB.

whoTB <- read_csv("Data/whoTB.csv")

who_clean <- whoTB %>%
  select(-iso2, -iso3) %>%
  pivot_longer(
    cols = starts_with("new"),
    names_to = "code",
    values_to = "cases"
  ) %>%
  filter(!is.na(cases)) %>%
  separate(col = code, into = c("emergence", "type", "sexe_age")) %>%
  select(-emergence) %>%
  separate(col = sexe_age, into = c("sexe", "age"), sep = 1)

who_clean %>%
  filter(country %in% c("India", "China")) %>%
  ggplot(aes(x = year, y = cases, color = age)) +
  geom_line() +
  facet_wrap(~sexe) +
  theme_bw()

who_clean %>%
  filter(country %in% c("India", "China")) %>%
  ggplot(aes(x = country, y = cases, fill = type)) +
  geom_col(position = position_dodge())
