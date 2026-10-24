# ==============================================================================
# Simulation de jeu de données pédagogique (N = 100) - v11 (Vin & Profil Sujets)
# Objectif : Ajout des variables sujets quantitatives continues (Oenologie)
# Auteur: Pascal Schlich
# ==============================================================================

library(openxlsx)

set.seed(8000) # Seed sélectionnée pour préserver la structure des préférences

# ------------------------------------------------------------------------------
# 1. PRODUITS ET ATTRIBUTS SENSORIELS STRICTEMENT INCHANGÉS (Feuille: Products)
# ------------------------------------------------------------------------------
product_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")

df_products <- data.frame(
  Product = product_names,
  `Floral`      = c(8.5, 6.0, 3.0, 1.5, 1.0, 2.0, 2.5, 4.0, 4.5, 3.5),
  `Sweet`       = c(7.5, 9.0, 4.0, 2.0, 1.5, 1.0, 3.0, 5.0, 5.5, 4.0),
  `Red fruits`  = c(5.0, 7.0, 8.5, 3.0, 2.0, 1.5, 2.0, 4.5, 7.0, 4.0),
  `Acidic`      = c(3.0, 4.0, 9.0, 7.5, 4.0, 2.0, 2.5, 3.5, 6.5, 3.0),
  `Citrus`      = c(2.0, 3.0, 7.0, 8.5, 5.0, 3.0, 3.5, 3.0, 5.0, 4.0),
  `Bitter`      = c(1.0, 1.5, 2.0, 5.5, 9.0, 7.5, 7.0, 3.5, 2.5, 4.5),
  `Woody`       = c(2.5, 1.0, 1.5, 3.0, 7.5, 8.5, 6.0, 3.0, 2.0, 4.0),
  `Astringent`  = c(3.0, 1.5, 2.0, 4.0, 6.0, 9.0, 5.5, 3.0, 2.0, 3.5),
  check.names = FALSE
)

# ------------------------------------------------------------------------------
# 2. CONSUMERS & SUBJECT VARIABLES (Feuille: Subjects)
# ------------------------------------------------------------------------------
n_c1 <- 40; n_c2 <- 35; n_c3 <- 25
n_total <- n_c1 + n_c2 + n_c3

cons_ids <- paste0("S", 1:n_total)
cluster_assignment <- c(rep("C1", n_c1), rep("C2", n_c2), rep("C3", n_c3))

# Indicatrices d'âge équilibrées
age_18_30_vec <- c(rep(1, 25), rep(0, 15),  rep(0, 35),  rep(0, 25))
age_31_50_vec <- c(rep(0, 25), rep(1, 15),  rep(1, 13), rep(0, 22), rep(1, 15), rep(0, 10))
age_gt50_vec  <- 1 - (age_18_30_vec + age_31_50_vec)

# Genre
female_vec <- c(
  rbinom(n_c1, 1, 0.65), # C1
  rbinom(n_c2, 1, 0.20), # C2
  rbinom(n_c3, 1, 0.50)  # C3
)
male_vec <- 1 - female_vec

# --- NOUVELLES VARIABLES SUJETS QUANTITATIVES CONTINUES ---

# 1. Income_year (12,000 à 120,000 €, Médiane ~24,000, Moyenne ~30,000, Hommes > Femmes)
raw_inc <- 11000 + exp(rnorm(n_total, mean = 9.4, sd = 0.85)) + male_vec * 7000
income_year_vec <- round(pmax(12000, pmin(120000, raw_inc)), -2)

# 2. Glass_week (0 à 35 verres, Médiane ~5, Moyenne ~7, Hommes nettement > Femmes)
raw_glass <- exp(rnorm(n_total, mean = 1.4, sd = 0.8)) - 1 + male_vec * 3.5
glass_week_vec <- round(pmax(0, pmin(35, raw_glass)))

# 3. Bottles_home (0 à 200 bouteilles, Médiane ~20, Moyenne ~30, >50 ans très élevé)
raw_bottles <- exp(rnorm(n_total, mean = 2.6, sd = 0.85)) - 2 + male_vec * 6 + age_gt50_vec * 38
bottles_home_vec <- round(pmax(0, pmin(200, raw_bottles)))

df_subjects <- data.frame(
  Consumer     = cons_ids,
  Male         = male_vec,
  Female       = female_vec,
  Age_18_30    = age_18_30_vec,
  Age_31_50    = age_31_50_vec,
  Age_gt50     = age_gt50_vec,
  Income_year  = income_year_vec,
  Glass_week   = glass_week_vec,
  Bottles_home = bottles_home_vec,
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 3. MATRICE HÉDONIQUE AVEC PRÉFÉRENCES INDIVIDUELLES VARIÉES
# ------------------------------------------------------------------------------
mat_liking <- matrix(NA, nrow = 10, ncol = n_total, dimnames = list(product_names, cons_ids))

for (j in 1:n_total) {
  c_type <- cluster_assignment[j]

  if (c_type == "C1") {
    w_sweet  <- runif(1, 0.3, 1.2)
    w_acid   <- runif(1, 0.2, 1.0)
    w_bitter <- runif(1, -1.0, -0.3)
    base_scores <- 4 + w_sweet * df_products$`Sweet` + w_acid * df_products$`Acidic` + w_bitter * df_products$`Bitter`
  } else if (c_type == "C2") {
    w_bitter <- runif(1, 0.4, 1.2)
    w_wood   <- runif(1, 0.2, 1.0)
    w_sweet  <- runif(1, -1.0, -0.3)
    base_scores <- 4 + w_bitter * df_products$`Bitter` + w_wood * df_products$`Woody` + w_sweet * df_products$`Sweet`
  } else {
    w_bal    <- runif(1, 0.6, 1.2)
    w_diff   <- runif(1, -0.4, 0.4)
    base_scores <- c(A=4.0, B=4.8, C=4.2, D=5.2, E=3.8, F=3.2, G=4.6, H=8.2 + w_diff, I=5.2, J=8.2 - w_diff)
  }

  scores_noisy <- round(base_scores + rnorm(10, mean = 0, sd = 1.8))
  scores_noisy <- pmax(1, pmin(9, scores_noisy))
  mat_liking[, j] <- scores_noisy
}

df_liking <- data.frame(Product = product_names, mat_liking, check.names = FALSE)

# ------------------------------------------------------------------------------
# 4. EXPORT EXCEL (3 FEUILLES)
# ------------------------------------------------------------------------------
wb <- createWorkbook()

addWorksheet(wb, "Liking")
writeData(wb, "Liking", df_liking)

addWorksheet(wb, "Products")
writeData(wb, "Products", df_products)

addWorksheet(wb, "Subjects")
writeData(wb, "Subjects", df_subjects)

file_out <- "dev/Simulated_Dataset.xlsx"
saveWorkbook(wb, file = file_out, overwrite = TRUE)
cat(sprintf("--> Fichier créé avec succès : %s (Variables quantitatives sujets intégrées)\n", file_out))
