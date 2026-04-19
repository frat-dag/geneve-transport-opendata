# ============================================================
# SCRIPT 08 — ANALYSE DES COLLISIONS
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Explorer le dataset collisions TPG avec tiers.
# Cartographie, analyse temporelle, indicateurs de gravité.
# Ce qu'on sait déjà (script 00) :
#   - 9 975 lignes, jan 2015 → avr 2026
#   - latitude/longitude déjà séparés ✅
#   - 978 NA sur sens (9.8%) → "Hors ligne"
#   - 5 indicateurs de gravité distincts
# ============================================================

# ── 1. NETTOYAGE ET PACKAGES ─────────────────────────────────

rm(list = ls())
gc()

setwd("D:/Frat/Documents/IA/Claude/Projet TPG/tpg-opendata-analysis/R")

source("00_palette.R")

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(leaflet)
library(htmlwidgets)

# ── 2. CHARGEMENT ───────────────────────────────────────────

collisions <- readRDS("../data/raw/collisions.rds")

cat("Dimensions :", nrow(collisions), "lignes x",
    ncol(collisions), "colonnes\n")
cat("Période    :", format(min(collisions$jour)),
    "→", format(max(collisions$jour)), "\n")
cat("Colonnes   :\n")
print(names(collisions))

# ── 3. NETTOYAGE ET EXPLORATION ──────────────────────────────

# Traitement des NA et création de variables utiles
collisions <- collisions %>%
  mutate(
    # Catégorie sens — NA = "Hors ligne"
    sens_clean = ifelse(is.na(sens), "Hors ligne", sens),
    # Catégorie événement — NA = "Non catégorisé"
    cat_clean  = ifelse(is.na(cat), "Non catégorisé", cat),
    # Variables temporelles
    annee      = year(jour),
    mois_num   = month(jour),
    heure_num  = as.integer(substr(as.character(heure), 1, 2)),
    jour_semaine = wday(jour, label = TRUE, abbr = FALSE,
                        locale = "fr_FR.UTF-8")
  )

# Statistiques générales
cat("=== EXPLORATION COLLISIONS ===\n\n")

cat("--- Par catégorie ---\n")
print(table(collisions$cat_clean))

cat("\n--- Par sens ---\n")
print(table(collisions$sens_clean))

cat("\n--- Par type de ligne ---\n")
print(sort(table(collisions$ligne_type_hist), decreasing = TRUE))

cat("\n--- Années couvertes ---\n")
print(table(collisions$annee))


# ── 4. ÉVOLUTION TEMPORELLE DES COLLISIONS ──────────────────

# Par année
collisions_annuel <- collisions %>%
  group_by(annee) %>%
  summarise(
    n_collisions       = n(),
    gravite_moy        = round(mean(indicateur_de_severite,
                                    na.rm = TRUE), 3),
    blessures_moy      = round(mean(niv_blessure_humain,
                                    na.rm = TRUE), 3),
    .groups = "drop"
  ) %>%
  filter(annee < 2026)  # 2026 incomplet

p_evolution <- ggplot(collisions_annuel,
                      aes(x = annee, y = n_collisions)) +
  geom_col(fill = TPG_RED, alpha = 0.85) +
  geom_text(aes(label = n_collisions),
            vjust = -0.4, size = 3.2, color = "grey30") +
  
  # COVID
  geom_vline(xintercept = 2020, linetype = "dashed",
             color = COL_NEUTRE, linewidth = 0.5) +
  annotate("text", x = 2020, y = 950,
           label = "COVID", hjust = -0.1,
           size = 2.8, color = COL_NEUTRE) +
  
  scale_x_continuous(breaks = 2015:2025) +
  scale_y_continuous(limits = c(0, 1150)) +
  labs(
    title    = "Évolution annuelle des collisions TPG — 2015 à 2025",
    subtitle = "Nombre de collisions avec tiers par année",
    x        = NULL,
    y        = "Nombre de collisions",
    caption  = "Source : TPG Open Data | 2026 exclu (année incomplète)"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_evolution)

ggsave("../outputs/08_collisions_annuel.png",
       plot = p_evolution, width = 12, height = 6, dpi = 150)

# ── 5. CARTE LEAFLET DES COLLISIONS ─────────────────────────
# Coordonnées déjà séparées — cartographie directe
# Couleur par indicateur de sévérité

# Filtrer les collisions géolocalisées
collisions_geo <- collisions %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter(latitude > 45 & latitude < 47) %>%  # bbox Genève
  filter(longitude > 5.5 & longitude < 7.5)

cat("Collisions géolocalisées :", nrow(collisions_geo),
    "sur", nrow(collisions), "\n")

# Palette de couleurs selon la sévérité
pal_severite <- colorNumeric(
  palette = c("#4A6FA5", "#FFC107", "#E30613"),
  domain  = collisions_geo$indicateur_de_severite,
  na.color = "#888888"
)

carte_collisions <- leaflet(collisions_geo) %>%
  addTiles() %>%
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 4,
    color       = ~pal_severite(indicateur_de_severite),
    fillColor   = ~pal_severite(indicateur_de_severite),
    fillOpacity = 0.7,
    weight      = 1,
    popup       = ~paste0(
      "<b>", format(jour, "%d %B %Y"), "</b><br>",
      "Ligne : ", ligne, " (", ligne_type_hist, ")<br>",
      "Arrêt : ", arret, "<br>",
      "Sens : ", sens_clean, "<br>",
      "Sévérité : ", indicateur_de_severite, "<br>",
      "Blessures : ", niv_blessure_humain
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal_severite,
    values   = ~indicateur_de_severite,
    title    = "Sévérité",
    opacity  = 0.8
  )

carte_collisions

saveWidget(carte_collisions,
           "../outputs/08_carte_collisions.html",
           selfcontained = TRUE)

message("Carte collisions sauvegardée.")

# ── 6. DISTRIBUTION HORAIRE DES COLLISIONS ──────────────────
# À quelle heure les collisions sont-elles les plus fréquentes ?

collisions_heure <- collisions %>%
  filter(!is.na(heure_num)) %>%
  group_by(heure_num) %>%
  summarise(
    n_collisions  = n(),
    gravite_moy   = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
    .groups = "drop"
  )

p_heure <- ggplot(collisions_heure,
                  aes(x = heure_num, y = n_collisions)) +
  geom_col(fill = TPG_RED, alpha = 0.85) +
  geom_line(aes(y = gravite_moy * 200),
            color = "#4A6FA5", linewidth = 0.8) +
  
  scale_x_continuous(breaks = seq(0, 23, by = 2),
                     labels = function(x) paste0(x, "h")) +
  scale_y_continuous(
    name     = "Nombre de collisions",
    sec.axis = sec_axis(~ . / 200,
                        name = "Sévérité moyenne (axe droit)")
  ) +
  labs(
    title    = "Distribution horaire des collisions TPG",
    subtitle = "Barres = nombre de collisions | Ligne bleue = sévérité moyenne",
    x        = "Heure",
    caption  = "Source : TPG Open Data | 2015-2026"
  ) +
  theme_tpg() +
  theme(
    axis.text.x = element_text(angle = 0),
    axis.title.y.right = element_text(color = "#4A6FA5")
  )

print(p_heure)


# Correction — agréger proprement par heure entière
collisions_heure <- collisions %>%
  filter(!is.na(heure_num) & heure_num >= 0 & heure_num <= 23) %>%
  group_by(heure_num) %>%
  summarise(
    n_collisions = n(),
    gravite_moy  = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
    .groups      = "drop"
  )

p_heure <- ggplot(collisions_heure,
                  aes(x = heure_num, y = n_collisions)) +
  geom_col(fill = TPG_RED, alpha = 0.85, width = 0.8) +
  geom_line(aes(y = gravite_moy * 300),
            color = "#4A6FA5", linewidth = 0.8) +
  geom_point(aes(y = gravite_moy * 300),
             color = "#4A6FA5", size = 1.5) +
  
  scale_x_continuous(breaks = 0:23,
                     labels = paste0(0:23, "h")) +
  scale_y_continuous(
    name     = "Nombre de collisions",
    sec.axis = sec_axis(~ . / 300,
                        name   = "Sévérité moyenne",
                        breaks = seq(0, 2, by = 0.5))
  ) +
  labs(
    title    = "Distribution horaire des collisions TPG",
    subtitle = "Barres = nombre | Ligne bleue = sévérité moyenne",
    x        = "Heure",
    caption  = "Source : TPG Open Data | 2015-2026"
  ) +
  theme_tpg() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.y.right = element_text(color = "#4A6FA5")
  )

print(p_heure)

# Diagnostic — que contient heure_num ?
cat("Valeurs distinctes de heure_num :\n")
print(sort(unique(collisions$heure_num)))

cat("\nAperçu colonne heure brute :\n")
print(head(collisions$heure, 10))


# Correction extraction heure — diviser les secondes par 3600
collisions <- collisions %>%
  mutate(
    heure_num = as.integer(as.numeric(heure) / 3600)
  )

cat("Valeurs heure_num après correction :\n")
print(sort(unique(collisions$heure_num)))

# Agrégation corrigée
collisions_heure <- collisions %>%
  filter(!is.na(heure_num) & heure_num >= 0 & heure_num <= 23) %>%
  group_by(heure_num) %>%
  summarise(
    n_collisions = n(),
    gravite_moy  = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
    .groups      = "drop"
  )

cat("\nDistribution par heure :\n")
print(collisions_heure)


p_heure <- ggplot(collisions_heure,
                  aes(x = heure_num, y = n_collisions)) +
  geom_col(fill = TPG_RED, alpha = 0.85, width = 0.8) +
  geom_line(aes(y = gravite_moy * 600),
            color = "#4A6FA5", linewidth = 0.8) +
  geom_point(aes(y = gravite_moy * 600),
             color = "#4A6FA5", size = 1.5) +
  
  scale_x_continuous(breaks = seq(0, 23, by = 1),
                     labels = paste0(0:23, "h")) +
  scale_y_continuous(
    name     = "Nombre de collisions",
    sec.axis = sec_axis(~ . / 600,
                        name   = "Sévérité moyenne",
                        breaks = seq(0, 2, by = 0.5))
  ) +
  labs(
    title    = "Distribution horaire des collisions TPG",
    subtitle = "Barres = nombre | Ligne bleue = sévérité moyenne",
    x        = "Heure",
    caption  = "Source : TPG Open Data | 2015-2026"
  ) +
  theme_tpg() +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8),
    axis.title.y.right = element_text(color = "#4A6FA5")
  )

print(p_heure)

ggsave("../outputs/08_collisions_horaire.png",
       plot = p_heure, width = 12, height = 6, dpi = 150)

# ── 7. BLOC DÉCISION FINAL — SCRIPT 08 ──────────────────────

# RÉSULTATS CLÉS :
# - 9 975 collisions sur 11 ans (2015-2026), 100% géolocalisées
# - 99.9% = accidents — dataset homogène
# - Pic 2025 : 1 024 collisions — record historique
#   Hypothèse : expansion offre + nouveaux conducteurs moins expérimentés
# - Creux 2020 : 708 — COVID = moins de véhicules = moins de collisions
# - Distribution horaire : pic 7h-8h et 16h-17h — corrélé à la fréquentation
# - Sévérité stable (~1.0) sur toute la journée — nuit non plus dangereuse
# - 978 "Hors ligne" (9.8%) = collisions en dépôt/manœuvre
#
# LIMITE PRINCIPALE :
# Le nombre brut de collisions n'est pas normalisé par l'exposition
# (km parcourus). À croiser avec km_prod en Phase 3 pour calculer
# le taux de collision par km — indicateur plus juste
#
# NOTE VIZ : carte Leaflet → fort pour publication élus
# NOTE VIZ : graphique horaire → corrélation avec heatmap fréquentation

message("Script 08 terminé.")