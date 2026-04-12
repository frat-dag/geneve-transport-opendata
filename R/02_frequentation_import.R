# =============================================================
# PROJET TPG OPEN DATA - Phase 1, Analyses 1.2 / 1.3 / 1.4
# Fichier  : 02_frequentation_import.R
# Objectif : Télécharger la fréquentation journalière par
#            arrêt et par ligne, produire les tops 10 et
#            analyser la distribution
# Source   : https://opendata.tpg.ch
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
library(httr2)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

# --- 2. Paramètres de l'API ----------------------------------

url_freq <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/montees-par-arret-par-ligne/exports/csv"

# --- 3. Téléchargement ---------------------------------------

message("Téléchargement de la fréquentation journalière...")

reponse <- request(url_freq) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

message("✓ Données reçues.")

# --- 4. Chargement dans R ------------------------------------

freq_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

# --- 5. Exploration initiale ---------------------------------

message("Dimensions : ", nrow(freq_raw), " lignes x ", ncol(freq_raw), " colonnes")
message("Colonnes disponibles :")
print(colnames(freq_raw))
print(head(freq_raw, 5))