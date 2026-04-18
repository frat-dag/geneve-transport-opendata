# ============================================================
# SCRIPT 01 — IMPORT ET NETTOYAGE DES ARRÊTS TPG
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Charger le dataset arrêts, le nettoyer, produire
# la carte Leaflet de référence du réseau.
# Ce qu'on sait déjà (script 00) :
#   - coordonnees = chaîne unique à séparer en lat/lon
#   - 148 arrêts sans coordonnées → à exclure + documenter
#   - 688 arrêts sans codedidoc → à inventorier
#   - colonne actif = "Y" ou "N" (character)
# ============================================================

# ── 1. PACKAGES ─────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(readr)
library(leaflet)
library(htmlwidgets)

# ── 2. CHARGEMENT ───────────────────────────────────────────
# On charge depuis le .rds sauvegardé en script 00
# Plus rapide que retélécharger — types déjà préservés

arrets <- readRDS("../data/raw/arrets.rds")

cat("Dimensions :", nrow(arrets), "lignes x", ncol(arrets), "colonnes\n")

# ── 3. NETTOYAGE ─────────────────────────────────────────────

# ÉTAPE 3.1 — Séparer coordonnees en latitude + longitude
# La colonne contient "46.220238, 6.226213" — on sépare sur ", "
# separate() de tidyr fait exactement ça proprement

arrets_clean <- arrets %>%
  separate(
    col   = coordonnees,
    into  = c("latitude", "longitude"),
    sep   = ", ",
    convert = TRUE    # convertit automatiquement en numeric
  )

# Vérification : les types sont-ils bien numeric ?
cat("Type latitude  :", class(arrets_clean$latitude), "\n")
cat("Type longitude :", class(arrets_clean$longitude), "\n")

# ÉTAPE 3.2 — Inventaire AVANT exclusion
# On regarde ce qu'on va perdre avant de filtrer
# Règle : ne jamais exclure sans avoir d'abord compté et caractérisé

sans_coords <- arrets_clean %>% filter(is.na(latitude))
avec_coords <- arrets_clean %>% filter(!is.na(latitude))

cat("\n--- Arrêts sans coordonnées ---\n")
cat("Total          :", nrow(sans_coords), "\n")
cat("Dont actifs    :", sum(sans_coords$actif == "Y"), "\n")
cat("Dont inactifs  :", sum(sans_coords$actif == "N"), "\n")

cat("\n--- Arrêts avec coordonnées ---\n")
cat("Total          :", nrow(avec_coords), "\n")
cat("Dont actifs    :", sum(avec_coords$actif == "Y"), "\n")
cat("Dont inactifs  :", sum(avec_coords$actif == "N"), "\n")



# ÉTAPE 3.3 — Exclusion documentée des arrêts sans coordonnées
# Tous inactifs (vérification étape 3.2) → exclusion sans impact analytique

arrets_clean <- arrets_clean %>%
  filter(!is.na(latitude))

cat("Dataset nettoyé :", nrow(arrets_clean), "arrêts\n")
cat("Exclus          : 148 arrêts inactifs sans coordonnées\n")

# ÉTAPE 3.4 — Inventaire codedidoc manquant
# Même logique : compter et caractériser avant de conclure

sans_didoc <- arrets_clean %>% filter(is.na(codedidoc))

cat("\n--- Arrêts sans codedidoc ---\n")
cat("Total         :", nrow(sans_didoc), "\n")
cat("Dont actifs   :", sum(sans_didoc$actif == "Y"), "\n")
cat("Dont inactifs :", sum(sans_didoc$actif == "N"), "\n")
cat("Communes concernées :\n")
print(sort(table(sans_didoc$commune), decreasing = TRUE))

# ÉTAPE 3.5 — Focus sur les 29 arrêts actifs sans codedidoc
# Ce sont les seuls problématiques — les inactifs sans codedidoc
# n'ont aucun impact sur nos analyses

names(arrets_clean)

actifs_sans_didoc <- arrets_clean %>%
  filter(is.na(codedidoc), actif == "Y") %>%
  dplyr::select(arretcodelong, nomarret, commune, pays)

cat("--- 29 arrêts ACTIFS sans codedidoc ---\n")
print(actifs_sans_didoc, n = 29)


# ÉTAPE 3.6 — Documentation des 29 arrêts actifs sans codedidoc
# Deux catégories identifiées :
#   - Points de douane CH/FR (arrêts techniques transfrontaliers)
#   - Dépôts TPG (sites de remisage — pas des arrêts commerciaux)
# Conclusion : absence de codedidoc structurellement justifiée
# Impact Phase 4 : nul — ces arrêts n'apparaissent pas dans GTFS-RT

cat("Répartition par pays — arrêts actifs sans codedidoc :\n")
print(table(actifs_sans_didoc$pays))

depots <- actifs_sans_didoc %>%
  filter(grepl("^D[A-Z]00", arretcodelong))
cat("\nDépôts TPG identifiés :", nrow(depots), "\n")
print(depots$nomarret)

douanes <- actifs_sans_didoc %>%
  filter(!grepl("^D[A-Z]00", arretcodelong))
cat("\nPoints de douane identifiés :", nrow(douanes), "\n")
cat("  Dont côté FR :", sum(douanes$pays == "FR"), "\n")
cat("  Dont côté CH :", sum(douanes$pays == "CH"), "\n")

# ══════════════════════════════════════════════════════════════
# BLOC DÉCISION — SCRIPT 01
# ══════════════════════════════════════════════════════════════

# DÉCISION 1 — 148 arrêts exclus (sans coordonnées)
# Tous inactifs → exclusion sans impact analytique
# Dataset final : 4 486 arrêts géolocalisés

# DÉCISION 2 — 29 arrêts actifs sans codedidoc
# Structurellement justifié — deux catégories :
#   - 26 points de douane transfrontaliers (13 paires CH/FR symétriques)
#   - 3 dépôts TPG (Bachet, En-Chardon, Jonction)
# Ces arrêts n'ont pas vocation à avoir un codedidoc
# Impact Phase 4 : nul — absents du GTFS-RT par nature
# À documenter dans le README : "les arrêts techniques
# (douanes + dépôts) sont inclus dans le dataset mais
# exclus des analyses de fréquentation"

# DÉCISION 3 — Structure du dataset final arrets_clean
# 4 486 lignes x 8 colonnes
# latitude et longitude : numeric ✅
# actif : "Y" (1 928) ou "N" (2 558)
# codedidoc : NA pour arrêts techniques uniquement


# ── 4. CARTE LEAFLET ─────────────────────────────────────────

# Séparation actifs / inactifs
actifs   <- arrets_clean %>% filter(actif == "Y")
inactifs <- arrets_clean %>% filter(actif == "N")

cat("Arrêts actifs   :", nrow(actifs), "\n")
cat("Arrêts inactifs :", nrow(inactifs), "\n")

# Construction de la carte
carte_arrets <- leaflet() %>%
  
  # Fond de carte OpenStreetMap
  addTiles() %>%
  
  # Couche 1 — Arrêts actifs (rouge TPG)
  addCircleMarkers(
    data        = actifs,
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 4,
    color       = "#E30613",
    fillColor   = "#E30613",
    fillOpacity = 0.8,
    weight      = 1,
    popup       = ~paste0(
      "<b>", nomarret, "</b><br>",
      "Commune : ", commune, "<br>",
      "Pays : ", pays, "<br>",
      "Code : ", arretcodelong
    ),
    group = "Arrêts actifs (1 928)"
  ) %>%
  
  # Couche 2 — Arrêts inactifs (bleu ardoise)
  addCircleMarkers(
    data        = inactifs,
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 3,
    color       = "#4A6FA5",
    fillColor   = "#4A6FA5",
    fillOpacity = 0.5,
    weight      = 1,
    popup       = ~paste0(
      "<b>", nomarret, "</b><br>",
      "Commune : ", commune, "<br>",
      "Pays : ", pays, "<br>",
      "Code : ", arretcodelong, "<br>",
      "<i>Arrêt inactif</i>"
    ),
    group = "Arrêts inactifs (2 558)"
  ) %>%
  
  # Contrôle des couches — on/off
  addLayersControl(
    overlayGroups = c("Arrêts actifs (1 928)", "Arrêts inactifs (2 558)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Légende
  addLegend(
    position = "bottomright",
    colors   = c("#E30613", "#4A6FA5"),
    labels   = c("Actifs (1 928)", "Inactifs (2 558)"),
    title    = "Arrêts TPG",
    opacity  = 0.8
  )

# Sauvegarde
dir.create("../outputs", showWarnings = FALSE)
saveWidget(carte_arrets, "../outputs/01_carte_arrets_tpg.html",
           selfcontained = TRUE)

message("Carte sauvegardée : outputs/01_carte_arrets_tpg.html")

carte_arrets


# Vérification — arrêts sans nom dans le dataset final
sans_nom <- arrets_clean %>% filter(is.na(nomarret))
cat("Arrêts sans nom :", nrow(sans_nom), "\n")
cat("Dont actifs     :", sum(sans_nom$actif == "Y"), "\n")
cat("Dont inactifs   :", sum(sans_nom$actif == "N"), "\n")
