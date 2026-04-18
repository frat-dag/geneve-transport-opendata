# Nettoyage de l'environnement
#rm(list = ls())

# Répertoire de travail
#setwd("D:/Frat/Documents/IA/Claude/Projet TPG/tpg-opendata-analysis/R")

# ============================================================
# SCRIPT 00 — EXPLORATION GÉNÉRALE DES DONNÉES TPG
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Inventaire complet de tous les datasets avant
# toute analyse. Les résultats de ce script guident tous
# les scripts suivants.
# ============================================================


# ── 1. PACKAGES ─────────────────────────────────────────────

library(httr2)      # Requêtes HTTP vers l'API TPG
library(readr)      # Lecture et parsing des CSV
library(dplyr)      # Manipulation de données
library(tidyr)      # Nettoyage et restructuration
library(janitor)    # Nettoyage des noms de colonnes


# ── 2. PARAMÈTRES GLOBAUX ───────────────────────────────────

# URL de base de l'API TPG (endpoint exports — pas de limite de lignes)
BASE_URL <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets"

# Paramètres communs à toutes les requêtes
PARAMS <- list(
  lang      = "fr",
  delimiter = ";",
  timezone  = "Europe/Zurich"
)

# ── 3. FONCTION DE TÉLÉCHARGEMENT ───────────────────────────

# Télécharge un dataset TPG complet via l'endpoint exports/csv
# Paramètres :
#   dataset_id : identifiant du dataset dans l'API (ex: "arrets")
#   n_max      : nombre max de lignes (Inf = tout télécharger)
# Retourne : un tibble, ou arrête le script avec un message d'erreur clair

fetch_dataset <- function(dataset_id, n_max = Inf) {
  
  url <- paste0(BASE_URL, "/", dataset_id, "/exports/csv")
  
  message("Téléchargement : ", dataset_id, " ...")
  
  result <- tryCatch({
    read_delim(
      url,
      delim         = ";",
      locale        = locale(encoding = "UTF-8",
                             tz       = "Europe/Zurich"),
      n_max         = n_max,
      show_col_types = FALSE
    )
  }, error = function(e) {
    stop("Erreur sur le dataset '", dataset_id, "' : ", e$message)
  })
  
  message("  -> ", nrow(result), " lignes x ", ncol(result), " colonnes")
  return(result)
}

# ── 4. TÉLÉCHARGEMENT — ÉCHANTILLON (1000 lignes) ───────────
# Objectif : inspecter la structure de chaque dataset
# avant de télécharger l'intégralité

arrets_raw        <- fetch_dataset("arrets",                                        n_max = 1000)
journalier_raw    <- fetch_dataset("montees-par-arret-par-ligne",                   n_max = 1000)
mensuel_raw       <- fetch_dataset("montees-mensuelles-par-arret-par-ligne",        n_max = 1000)
horaire_raw       <- fetch_dataset("frequentation-journaliere-par-tranche-horaire", n_max = 1000)
mn_raw            <- fetch_dataset("mn_montees-par-arret-par-ligne-par-tranchehoraire", n_max = 1000)
km_prod_raw       <- fetch_dataset("kilometres-produits-journaliers-par-ligne",     n_max = 1000)
collisions_raw    <- fetch_dataset("collisions-tpg-avec-tiers",                     n_max = 1000)

# ── 5. INSPECTION DE LA STRUCTURE ───────────────────────────
# Objectif : pour chaque dataset, afficher les colonnes,
# leur type, et un aperçu des valeurs — sans encore analyser

inspecter <- function(df, nom) {
  cat("\n", strrep("=", 60), "\n")
  cat("DATASET :", nom, "\n")
  cat(strrep("=", 60), "\n")
  cat("Dimensions :", nrow(df), "lignes x", ncol(df), "colonnes\n\n")
  
  # Pour chaque colonne : type + 3 premières valeurs distinctes
  for (col in names(df)) {
    vals <- unique(df[[col]])
    vals <- vals[!is.na(vals)]
    apercu <- paste(head(vals, 3), collapse = " | ")
    cat(sprintf("  %-45s [%s]  %s\n",
                col,
                class(df[[col]])[1],
                apercu))
  }
}

inspecter(arrets_raw,     "arrets")
inspecter(journalier_raw, "montees-journalier")
inspecter(mensuel_raw,    "montees-mensuel")
inspecter(horaire_raw,    "frequentation-horaire")
inspecter(mn_raw,         "mn-lignes-MN")
inspecter(km_prod_raw,    "km-produits")
inspecter(collisions_raw, "collisions")

# ── 6. VALEURS MANQUANTES ────────────────────────────────────
# Objectif : quantifier les NA par colonne et par dataset
# Un NA ignoré maintenant = un biais silencieux plus tard

compter_na <- function(df, nom) {
  cat("\n", strrep("=", 60), "\n")
  cat("DATASET :", nom, "\n")
  cat(strrep("=", 60), "\n")
  
  na_counts <- df %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(),
                 names_to  = "colonne",
                 values_to = "nb_na") %>%
    mutate(pct_na = round(nb_na / nrow(df) * 100, 1)) %>%
    filter(nb_na > 0) %>%
    arrange(desc(nb_na))
  
  if (nrow(na_counts) == 0) {
    cat("  Aucun NA détecté sur cet échantillon\n")
  } else {
    for (i in seq_len(nrow(na_counts))) {
      cat(sprintf("  %-45s %d NA  (%s%%)\n",
                  na_counts$colonne[i],
                  na_counts$nb_na[i],
                  na_counts$pct_na[i]))
    }
  }
}

compter_na(arrets_raw,     "arrets")
compter_na(journalier_raw, "montees-journalier")
compter_na(mensuel_raw,    "montees-mensuel")
compter_na(horaire_raw,    "frequentation-horaire")
compter_na(mn_raw,         "mn-lignes-MN")
compter_na(km_prod_raw,    "km-produits")
compter_na(collisions_raw, "collisions")


# ── 7. BLOC DÉCISION ─────────────────────────────────────────
# Ce qu'on a appris — ce que ça implique pour les scripts suivants
# Ce bloc est la mémoire de l'EDA. On le met à jour si on découvre
# de nouvelles anomalies dans les scripts suivants.

# DÉCISION 1 — codedidoc (arrets, ~16% NA)
# Impact Phase 1-3 : aucun — on n'utilise pas ce code dans ces phases
# Impact Phase 4   : critique — les arrêts sans codedidoc ne pourront
#                    pas être joints aux données GTFS-RT temps réel
# Action           : inventorier ces arrêts (actifs ou inactifs ?)
#                    avant de démarrer la Phase 4

# DÉCISION 2 — coordonnees (arrets, 3.3% NA)
# Impact           : ces arrêts seront exclus de toutes les cartes
# Action           : exclusion silencieuse documentée dans script 01
#                    vérifier si ces arrêts sont actifs ou inactifs

# DÉCISION 3 — ligne (mensuel, 2.8% NA)
# Impact           : lignes non identifiables — exclure des analyses
#                    par ligne. Ne PAS exclure des totaux globaux
#                    sans vérifier si ces NA représentent un volume
#                    significatif de montées
# Action           : investiguer en script 03 (évolution temporelle)

# DÉCISION 4 — type de colonne "ligne" incohérent entre datasets
# journalier : numeric | mensuel : character | collisions : character
# Impact           : toute jointure entre ces datasets produira des
#                    NA silencieux si on ne convertit pas d'abord
# Action           : dans chaque script de jointure, convertir "ligne"
#                    en character AVANT la jointure — convention retenue

# DÉCISION 5 — sens (collisions, 9.2% NA)
# Hypothèse        : collisions hors ligne (dépôt, manœuvre)
# Impact           : analyses par sens de circulation biaisées
# Action           : traiter ces NA comme une catégorie "hors ligne"
#                    dans le script collisions — ne pas les exclure

# DÉCISION 6 — données provisoires (donnees_definitives == FALSE)
# Vu dans horaire : TRUE et FALSE coexistent
# Action           : filtrer sur donnees_definitives == TRUE
#                    dans TOUS les scripts d'analyse — sans exception


# ── 8. TÉLÉCHARGEMENT COMPLET ────────────────────────────────
# On télécharge maintenant l'intégralité de chaque dataset
# La structure est connue — on sait ce qu'on va recevoir

message("Début du téléchargement complet — patience...")

arrets        <- fetch_dataset("arrets")
journalier    <- fetch_dataset("montees-par-arret-par-ligne")
mensuel       <- fetch_dataset("montees-mensuelles-par-arret-par-ligne")
horaire       <- fetch_dataset("frequentation-journaliere-par-tranche-horaire")
mn            <- fetch_dataset("mn_montees-par-arret-par-ligne-par-tranchehoraire")
km_prod       <- fetch_dataset("kilometres-produits-journaliers-par-ligne")
collisions    <- fetch_dataset("collisions-tpg-avec-tiers")

message("Téléchargement complet terminé.")


# ── 9. VÉRIFICATION DES PÉRIODES COUVERTES ──────────────────
# Objectif : confirmer les dates de début et fin de chaque dataset
# Les dimensions ne suffisent pas — on veut savoir QUAND

verifier_periode <- function(df, nom, col_date) {
  if (!col_date %in% names(df)) {
    cat(nom, ": colonne", col_date, "absente\n")
    return()
  }
  dates <- df[[col_date]]
  cat(sprintf("%-30s  du %s  au %s  (%d lignes)\n",
              nom,
              format(min(dates, na.rm = TRUE)),
              format(max(dates, na.rm = TRUE)),
              nrow(df)))
}

cat("\n=== PÉRIODES COUVERTES ===\n\n")
verifier_periode(arrets,     "arrets",      "actif")       # pas de date
verifier_periode(journalier, "journalier",  "date")
verifier_periode(mensuel,    "mensuel",     "mois")
verifier_periode(horaire,    "horaire",     "date")
verifier_periode(mn,         "mn",          "date")
verifier_periode(km_prod,    "km_prod",     "date")
verifier_periode(collisions, "collisions",  "jour")


# ── 10. MISE À JOUR BLOC DÉCISION — PÉRIODES ────────────────

# DÉCISION 7 — Journalier limité à 1.75M lignes (avr 2023 → avr 2026)
# Le dataset journalier ne remonte pas à fév 2023 comme documenté initialement
# et semble limité à 1.75M de lignes par l'API.
# Impact : les analyses de fréquentation fine (par arrêt × ligne × jour)
# ne couvrent que 3 ans. Pour les tendances longues → utiliser le mensuel.
# Règle : journalier = analyses détaillées récentes
#          mensuel   = tendances historiques (2016-2026)

# DÉCISION 8 — Dataset mn : périmètre restreint (CCG, mar 2024 → avr 2026)
# Ce dataset ne couvre que 2 ans et uniquement le réseau CCG.
# Il ne peut PAS être utilisé pour des analyses représentatives du réseau TPG.
# Usage limité : analyse spécifique des lignes M et N du pays de Gex.

# DÉCISION 9 — Collisions : le dataset le plus long (2015-2026, 11 ans)
# C'est le seul dataset qui remonte avant 2016.
# Potentiel fort pour les analyses de tendance sécurité long terme.


# ── 11. VALEURS MANQUANTES — DONNÉES COMPLÈTES ──────────────
# Objectif : confirmer que les NA observés sur l'échantillon
# se retrouvent sur l'intégralité des données

cat("\n=== VALEURS MANQUANTES — DONNÉES COMPLÈTES ===\n")

compter_na(arrets,     "arrets")
compter_na(journalier, "montees-journalier")
compter_na(mensuel,    "montees-mensuel")
compter_na(horaire,    "frequentation-horaire")
compter_na(mn,         "mn-lignes-MN")
compter_na(km_prod,    "km-produits")
compter_na(collisions, "collisions")


# ══════════════════════════════════════════════════════════════
# BLOC DÉCISION FINAL — SCRIPT 00
# Ce qu'on a appris. Ce qu'on fait ensuite. Pourquoi.
# ══════════════════════════════════════════════════════════════

# ── CE QU'ON A ÉTABLI ────────────────────────────────────────

# PÉRIODES RÉELLES (vs documentation initiale)
# journalier : avr 2023 → avr 2026  (3 ans)   ⚠ pas depuis fév 2023
# mensuel    : jan 2016 → mar 2026  (10 ans)   ✅ référence historique
# horaire    : jan 2019 → avr 2026  (7 ans)    ✅ couvre pré/post COVID
# mn         : mar 2024 → avr 2026  (2 ans)    ⚠ CCG uniquement
# km_prod    : jan 2016 → avr 2026  (10 ans)   ✅
# collisions : jan 2015 → avr 2026  (11 ans)   ✅ le plus long

# NA CONFIRMÉS SUR DONNÉES COMPLÈTES
# arrets     / codedidoc   : 14.8% → critique Phase 4 (GTFS-RT)
# arrets     / coordonnees :  3.2% → exclusion cartographie documentée
# journalier / ligne       :  0.1% → exclure des analyses par ligne
# mensuel    / ligne       :  1.1% → quantifier les montées perdues
# km_prod    / ligne       :  0.3% → exclure des analyses par ligne
# collisions / sens        :  9.8% → catégorie "Hors ligne" à créer
# collisions / cat         :  0.0% → anecdotique, documenter

# CONVENTIONS ADOPTÉES (valables pour tous les scripts suivants)
# 1. ligne toujours converti en character avant jointure
# 2. filtre donnees_definitives == TRUE systématique
# 3. journalier = analyses détaillées récentes (3 ans)
#    mensuel    = tendances historiques (10 ans)
# 4. dataset mn : usage restreint — CCG, 2 ans seulement

# ── CE QU'ON FAIT ENSUITE ────────────────────────────────────

# Script 01 — arrets
#   → Séparer coordonnees en latitude / longitude (numeric)
#   → Documenter les 148 arrêts sans coordonnées (actifs ou inactifs ?)
#   → Inventorier les 688 arrêts sans codedidoc (actifs ou inactifs ?)
#   → Carte Leaflet : arrêts actifs vs inactifs

# Script 02 — fréquentation journalière
#   → Appliquer filtre donnees_definitives == TRUE
#   → Convertir ligne en character
#   → Top arrêts et top lignes sur données propres
#   → Distribution des montées — courbe de Lorenz (concentration)

# Script 03 — évolution temporelle
#   → Utiliser le mensuel pour les tendances 2016-2026
#   → Annoter : Léman Express (déc 2019), COVID (mar 2020), gratuité (jan 2025)
#   → Quantifier les montées perdues avec les NA de ligne (mensuel)

# Script 04 — heatmap horaire
#   → Filtre !is.na(horaire_tranche_stop_theo)
#   → Filtre horaire_type %in% c("NORMAL","SAMEDI","DIMANCHE")
#   → Analyser séparément jours NORMAL vs VACANCES (T-002 à reproduire)

# Script 05 — profil journalier
#   → Médiane sur jours NORMAL — jamais la moyenne brute
#   → Tests T-001, T-003, T-005, T-005b à intégrer au bon moment

# Script 06 — saisonnalité
#   → Décomposition STL sur mensuel (2016-2026)
#   → Annoter les ruptures connues

# Script 07 — impact COVID et ruptures
#   → Tests T-002, T-004, T-004b, T-006 à reproduire avec rigueur narrative
#   → Léman Express déc 2019 à tester (Chow)
#   → Gratuité jan 2025 à tester (T-009) si données suffisantes

# Script 08 — collisions
#   → Créer catégorie sens = "Hors ligne" pour les 978 NA
#   → Explorer les 5 indicateurs de gravité
#   → Cartographie directe (latitude/longitude déjà séparés)

# Script 09 — récapitulatif tests statistiques
#   → Dunn post-hoc (complément T-001)
#   → T-007 Gini + bootstrap (concentration arrêts)
#   → T-008 corrélation fréquentation × km produits
#   → T-009 gratuité jeunes jan 2025