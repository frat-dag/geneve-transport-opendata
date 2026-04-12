# =============================================================
# PROJET TPG OPEN DATA - Phase 1, Analyse 1.1
# Fichier  : 01_arrets_import.R
# Objectif : Télécharger les arrêts du réseau TPG,
#            explorer la structure et produire une
#            première carte interactive Leaflet
# Source   : https://opendata.tpg.ch
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
library(httr2)
library(readr)
library(dplyr)
library(janitor)
library(leaflet)
library(htmlwidgets)

# --- 2. Paramètres de l'API ----------------------------------

# URL de l'endpoint export CSV (pas de limite de lignes)
url_arrets <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/arrets/exports/csv"

# --- 3. Téléchargement ---------------------------------------

message("Téléchargement des arrêts TPG...")

reponse <- request(url_arrets) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

message("✓ Données reçues.")

# --- 4. Chargement dans R ------------------------------------

arrets_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

# --- 5. Exploration initiale ---------------------------------

# Nombre de lignes et colonnes
message("Dimensions : ", nrow(arrets_raw), " arrêts x ", ncol(arrets_raw), " colonnes")

# Noms des colonnes — IMPORTANT : on adaptera le code selon ce qu'on voit ici
message("Colonnes disponibles :")
print(colnames(arrets_raw))

# Aperçu des premières lignes
print(head(arrets_raw, 5))

# --- 6. Nettoyage et préparation -----------------------------

arrets <- arrets_raw |>
  # Séparer les coordonnées en latitude et longitude
  mutate(
    latitude  = as.numeric(sub(",.*", "", coordonnees)),
    longitude = as.numeric(sub(".*, ", "", coordonnees))
  ) |>
  # Garder uniquement les arrêts avec coordonnées valides
  filter(!is.na(latitude), !is.na(longitude))

# Résumé rapide
cat("\n=== Résumé du réseau TPG ===\n")
cat("Total arrêts          :", nrow(arrets_raw), "\n")
cat("Arrêts géolocalisés   :", nrow(arrets), "\n")
cat("Arrêts actifs (Y)     :", sum(arrets$actif == "Y"), "\n")
cat("Arrêts inactifs (N)   :", sum(arrets$actif == "N"), "\n")
cat("Communes distinctes   :", n_distinct(arrets$commune), "\n")
cat("Pays représentés      :", paste(unique(arrets$pays), collapse = ", "), "\n")

# --- 7. Sauvegarde des données -------------------------------

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

write_csv2(arrets_raw, "data/raw/arrets_raw.csv")
write_csv2(arrets,     "data/processed/arrets_clean.csv")

message("✓ Données sauvegardées dans data/raw/ et data/processed/")


# --- 8. Carte interactive Leaflet ----------------------------

message("Génération de la carte interactive...")

# On crée deux couches : arrêts actifs et inactifs
arrets_actifs   <- arrets |> filter(actif == "Y")
arrets_inactifs <- arrets |> filter(actif == "N")

carte <- leaflet() |>
  addTiles() |>  # Fond de carte OpenStreetMap
  # Arrêts actifs en rouge TPG
  addCircleMarkers(
    data        = arrets_actifs,
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 5,
    color       = "#E30613",
    fillColor   = "#E30613",
    stroke      = FALSE,
    fillOpacity = 0.8,
    popup       = ~paste0(
      "<b>", nomarret, "</b><br>",
      "Commune : ", commune, "<br>",
      "Pays : ", pays, "<br>",
      "Code : ", arretcodelong, "<br>",
      "<span style='color:green'>● Actif</span>"
    ),
    group = "Arrêts actifs"
  ) |>
  # Arrêts inactifs en gris
  addCircleMarkers(
    data        = arrets_inactifs,
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 3,
    color       = "#4A6FA5",
    fillColor   = "#4A6FA5",
    stroke      = FALSE,
    fillOpacity = 0.6,
    popup       = ~paste0(
      "<b>", nomarret, "</b><br>",
      "Commune : ", commune, "<br>",
      "Pays : ", pays, "<br>",
      "Code : ", arretcodelong, "<br>",
      "<span style='color:red'>● Inactif</span>"
    ),
    group = "Arrêts inactifs"
  ) |>
  # Contrôle des couches — on peut afficher/masquer chaque groupe
  addLayersControl(
    overlayGroups = c("Arrêts actifs", "Arrêts inactifs"),
    options       = layersControlOptions(collapsed = FALSE)
  ) |>
  # Légende
  addLegend(
    position = "bottomright",
    colors   = c("#E30613", "#4A6FA5"),
    labels   = c(
      paste0("Actifs (", nrow(arrets_actifs), ")"),
      paste0("Inactifs (", nrow(arrets_inactifs), ")")
    ),
    title    = "Arrêts TPG"
  )

# Afficher dans RStudio
print(carte)

# Sauvegarder en HTML
dir.create("outputs", showWarnings = FALSE)
saveWidget(carte, "outputs/01_carte_arrets_tpg.html", selfcontained = TRUE)
message("✓ Carte sauvegardée : outputs/01_carte_arrets_tpg.html")