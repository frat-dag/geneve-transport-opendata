# =============================================================
# PROJET TPG OPEN DATA - Script 00 — Exploration générale
# Fichier  : 00_exploration_generale.R
# Objectif : Analyse exploratoire complète (EDA) de tous
#            les datasets TPG avant toute modélisation
#            — inventaire des datasets
#            — qualité des données (NA, doublons, outliers)
#            — cohérence entre datasets
#            — statistiques descriptives formelles
#            — rapport de qualité
# Date     : avril 2026
# Note     : Ce script consolide l'analyse exploratoire (EDA)
#            conduite au fil du projet. Dans une démarche
#            exploratoire, l'EDA formelle émerge naturellement
#            après une première phase d'exploration qui permet
#            de cibler les vérifications sur les dimensions
#            réellement pertinentes.
#            À exécuter avant toute nouvelle analyse.
# =============================================================

# --- 1. Packages ---------------------------------------------
library(httr2)       # Appels API REST
library(readr)       # Lecture CSV
library(dplyr)       # Manipulation données
library(tidyr)       # Mise en forme
library(ggplot2)     # Visualisations
library(lubridate)   # Dates
library(janitor)     # Nettoyage noms colonnes

# --- 2. Fonction utilitaire — rapport qualité dataset --------
# Cette fonction calcule les indicateurs de qualité pour
# n'importe quel dataset — on l'utilisera pour tous les datasets

rapport_qualite <- function(df, nom_dataset) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("DATASET :", nom_dataset, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  cat("Dimensions       :", nrow(df), "lignes x", ncol(df), "colonnes\n")
  cat("Taille mémoire   :", format(object.size(df), units = "MB"), "\n\n")
  
  cat("--- Structure des colonnes ---\n")
  for (col in names(df)) {
    n_na      <- sum(is.na(df[[col]]))
    pct_na    <- round(n_na / nrow(df) * 100, 1)
    n_unique  <- n_distinct(df[[col]])
    type_col  <- class(df[[col]])[1]
    cat(sprintf("  %-35s [%s] NA: %d (%.1f%%) Unique: %d\n",
                col, type_col, n_na, pct_na, n_unique))
  }
  
  # Doublons
  n_doublons <- nrow(df) - nrow(distinct(df))
  cat("\n--- Doublons ---\n")
  cat("Lignes dupliquées :", n_doublons,
      ifelse(n_doublons == 0, "✓ Aucun doublon", "⚠ ATTENTION"), "\n")
  
  cat("\n")
}

# --- 3. Téléchargement de tous les datasets ------------------

message("Téléchargement des datasets TPG...")

# Fonction générique de téléchargement
telecharger_dataset <- function(dataset_id) {
  url <- paste0(
    "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/",
    dataset_id, "/exports/csv"
  )
  reponse <- request(url) |>
    req_url_query(lang = "fr", delimiter = ";",
                  timezone = "Europe/Zurich") |>
    req_perform()
  resp_body_string(reponse) |>
    read_delim(delim = ";", locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)
}

# Téléchargement de chaque dataset
message("1/5 — Arrêts...")
arrets_raw <- telecharger_dataset("arrets")

message("2/5 — Fréquentation journalière...")
freq_jour_raw <- telecharger_dataset("montees-par-arret-par-ligne")

message("3/5 — Fréquentation mensuelle...")
freq_mois_raw <- telecharger_dataset("montees-mensuelles-par-arret-par-ligne")

message("4/5 — Fréquentation horaire...")
freq_heure_raw <- telecharger_dataset("frequentation-journaliere-par-tranche-horaire")

message("5/5 — Kilomètres produits...")
km_raw <- telecharger_dataset("kilometres-produits-journaliers-par-ligne")

message("✓ Tous les datasets téléchargés.")

# --- 4. Rapport de qualité — dataset par dataset -------------

rapport_qualite(arrets_raw,     "Arrêts du réseau")
rapport_qualite(freq_jour_raw,  "Fréquentation journalière par arrêt/ligne")
rapport_qualite(freq_mois_raw,  "Fréquentation mensuelle par arrêt/ligne")
rapport_qualite(freq_heure_raw, "Fréquentation par tranche horaire")
rapport_qualite(km_raw,         "Kilomètres produits par ligne")

# --- 5. Statistiques descriptives formelles ------------------

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("STATISTIQUES DESCRIPTIVES FORMELLES\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# 5.1 — Arrêts
cat("\n--- Arrêts ---\n")
cat("Total arrêts historiques :", nrow(arrets_raw), "\n")
cat("Arrêts actifs (Y)        :", sum(arrets_raw$actif == "Y", na.rm = TRUE), "\n")
cat("Arrêts inactifs (N)      :", sum(arrets_raw$actif == "N", na.rm = TRUE), "\n")
cat("Communes distinctes      :", n_distinct(arrets_raw$commune), "\n")
cat("Pays                     :", paste(unique(arrets_raw$pays), collapse = ", "), "\n")
cat("Arrêts sans coordonnées  :", sum(is.na(arrets_raw$coordonnees)), "\n")

# 5.2 — Fréquentation journalière
cat("\n--- Fréquentation journalière ---\n")
freq_jour_def <- freq_jour_raw |> filter(donnees_definitives == TRUE)
cat("Période couverte  :", format(min(freq_jour_def$date)),
    "→", format(max(freq_jour_def$date)), "\n")
cat("Nb jours couverts :", n_distinct(freq_jour_def$date), "\n")
cat("Nb lignes actives :", n_distinct(freq_jour_def$ligne), "\n")
cat("Nb arrêts couverts:", n_distinct(freq_jour_def$arret), "\n")
cat("Montées — Min     :", round(min(freq_jour_def$nb_de_montees, na.rm = TRUE), 1), "\n")
cat("Montées — Max     :", round(max(freq_jour_def$nb_de_montees, na.rm = TRUE), 1), "\n")
cat("Montées — Moyenne :", round(mean(freq_jour_def$nb_de_montees, na.rm = TRUE), 1), "\n")
cat("Montées — Médiane :", round(median(freq_jour_def$nb_de_montees, na.rm = TRUE), 1), "\n")
cat("Données définitives :", sum(freq_jour_raw$donnees_definitives, na.rm = TRUE),
    "/", nrow(freq_jour_raw),
    paste0("(", round(mean(freq_jour_raw$donnees_definitives, na.rm = TRUE) * 100, 1), "%)"), "\n")

# 5.3 — Fréquentation mensuelle
cat("\n--- Fréquentation mensuelle ---\n")
freq_mois_def <- freq_mois_raw |> filter(donnees_definitives == TRUE)
cat("Période couverte  :", min(freq_mois_def$annee), "→", max(freq_mois_def$annee), "\n")
cat("Nb mois couverts  :", n_distinct(paste(freq_mois_def$annee,
                                            freq_mois_def$indice_du_mois)), "\n")
cat("Nb lignes actives :", n_distinct(freq_mois_def$ligne), "\n")

# 5.4 — Kilomètres produits
cat("\n--- Kilomètres produits ---\n")
cat("Colonnes disponibles :\n")
print(colnames(km_raw))
print(head(km_raw, 3))

# --- 6. Cohérence entre datasets -----------------------------

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("COHÉRENCE ENTRE DATASETS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# 6.1 — Arrêts présents dans fréquentation mais pas dans arrets
arrets_freq <- unique(freq_jour_raw$arret_code_long)
arrets_ref   <- unique(arrets_raw$arretcodelong)
arrets_orphelins <- setdiff(arrets_freq, arrets_ref)

cat("\nArrêts dans fréquentation mais absents du référentiel :",
    length(arrets_orphelins), "\n")
if (length(arrets_orphelins) > 0 & length(arrets_orphelins) <= 20) {
  cat("Codes concernés :", paste(arrets_orphelins, collapse = ", "), "\n")
}

# 6.2 — Lignes communes entre datasets journalier et mensuel
lignes_jour <- unique(freq_jour_raw$ligne)
lignes_mois <- unique(freq_mois_raw$ligne)
cat("\nLignes dans journalier :", length(lignes_jour), "\n")
cat("Lignes dans mensuel    :", length(lignes_mois), "\n")
cat("Lignes communes        :",
    length(intersect(lignes_jour, lignes_mois)), "\n")
cat("Lignes uniquement dans journalier :",
    length(setdiff(lignes_jour, lignes_mois)), "\n")
cat("Lignes uniquement dans mensuel    :",
    length(setdiff(lignes_mois, lignes_jour)), "\n")

# --- 7. Détection des outliers — fréquentation journalière ---

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("DÉTECTION DES OUTLIERS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Méthode IQR — valeurs au-delà de Q1 - 1.5*IQR ou Q3 + 1.5*IQR
freq_jour_def_agg <- freq_jour_def |>
  group_by(date) |>
  summarise(total_montees = sum(nb_de_montees, na.rm = TRUE),
            .groups = "drop")

Q1  <- quantile(freq_jour_def_agg$total_montees, 0.25)
Q3  <- quantile(freq_jour_def_agg$total_montees, 0.75)
IQR <- Q3 - Q1

outliers_bas  <- freq_jour_def_agg |>
  filter(total_montees < Q1 - 1.5 * IQR) |>
  arrange(total_montees)

outliers_haut <- freq_jour_def_agg |>
  filter(total_montees > Q3 + 1.5 * IQR) |>
  arrange(desc(total_montees))

cat("\nSeuil outlier bas  :", round(Q1 - 1.5 * IQR), "montées/jour\n")
cat("Seuil outlier haut :", round(Q3 + 1.5 * IQR), "montées/jour\n")
cat("\nJours anormalement bas :", nrow(outliers_bas), "\n")
if (nrow(outliers_bas) > 0) print(head(outliers_bas, 10))
cat("\nJours anormalement élevés :", nrow(outliers_haut), "\n")
if (nrow(outliers_haut) > 0) print(head(outliers_haut, 5))

# --- 8. Visualisation — qualité des données ------------------

# Graphique : taux de données définitives par mois
taux_definitif <- freq_jour_raw |>
  mutate(mois = floor_date(date, "month")) |>
  group_by(mois) |>
  summarise(
    taux_def = mean(donnees_definitives, na.rm = TRUE) * 100,
    .groups  = "drop"
  )

ggplot(taux_definitif, aes(x = mois, y = taux_def)) +
  geom_line(color = "#E30613", linewidth = 0.8) +
  geom_hline(yintercept = 95, color = "#4A6FA5",
             linetype = "dashed", linewidth = 0.5) +
  annotate("text", x = min(taux_definitif$mois),
           y = 96, label = "Seuil 95%", color = "#4A6FA5",
           size = 3, hjust = 0) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Taux de données définitives par mois — fréquentation journalière",
    subtitle = "Un taux < 95% indique des données encore provisoires",
    x        = NULL,
    y        = "% données définitives",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/00_qualite_donnees_definitives.png",
       width = 12, height = 5, dpi = 150)
message("✓ Graphique qualité données sauvegardé")

# --- 9. Résumé des décisions de nettoyage -------------------

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("DÉCISIONS DE NETTOYAGE — RÉSUMÉ\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("1. FILTRAGE données définitives :\n")
cat("   filter(donnees_definitives == TRUE)\n")
cat("   Raison : les données provisoires sont sujettes à révision\n\n")
cat("2. SÉPARATION coordonnées :\n")
cat("   sub(',.*', '', coordonnees) → latitude\n")
cat("   sub('.*, ', '', coordonnees) → longitude\n")
cat("   Raison : colonne mixte dans le dataset arrêts\n\n")
cat("3. FILTRAGE arrêts sans coordonnées :\n")
cat("   filter(!is.na(latitude), !is.na(longitude))\n")
cat("   Raison : arrêts non cartographiables\n\n")
cat("4. SÉPARATION type de jour :\n")
cat("   filter(horaire_type %in% c('NORMAL','SAMEDI','DIMANCHE'))\n")
cat("   Raison : exclure jours VACANCES pour analyses par jour\n\n")
cat("5. CONVERSION tranche horaire :\n")
cat("   as.integer(horaire_tranche_stop_theo)\n")
cat("   Raison : heure début de tranche en format numérique\n\n")
cat("   NAs introduits pour valeurs non-numériques → exclus par filter(!is.na(heure))\n")

message("\n✓ Exploration générale terminée — rapport complet affiché.")