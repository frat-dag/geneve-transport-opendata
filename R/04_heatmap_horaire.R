# =============================================================
# PROJET TPG OPEN DATA - Phase 2, Analyse 2.1
# Fichier  : 04_heatmap_horaire.R
# Objectif : Visualiser la fréquentation par heure et par
#            jour de la semaine — "quand Genève prend le bus"
# Dataset  : frequentation-journaliere-par-tranche-horaire
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
# Nouveaux par rapport aux scripts précédents :
install.packages("viridis")
library(viridis)     # Palettes de couleurs perceptuellement uniformes
# — idéales pour les heatmaps (daltonisme-friendly)

# Déjà utilisés :
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)

# --- 2. Téléchargement ---------------------------------------

url_horaire <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/frequentation-journaliere-par-tranche-horaire/exports/csv"

message("Téléchargement fréquentation par tranche horaire...")

reponse <- request(url_horaire) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

freq_horaire_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

message("✓ Données reçues : ", nrow(freq_horaire_raw), " lignes x ",
        ncol(freq_horaire_raw), " colonnes")
message("Colonnes :")
print(colnames(freq_horaire_raw))
print(head(freq_horaire_raw, 3))

# --- 3. Nettoyage et préparation -----------------------------

freq_horaire <- freq_horaire_raw |>
  filter(donnees_definitives == TRUE) |>
  # Extraire l'heure comme nombre entier
  mutate(
    heure = as.integer(horaire_tranche_stop_theo),
    # Simplifier le nom du jour (supprimer le numéro devant)
    jour  = sub("^[0-9]-", "", jour_semaine),
    # Ordonner les jours correctement (lundi → dimanche)
    jour  = factor(jour, levels = c(
      "Lundi", "Mardi", "Mercredi", "Jeudi",
      "Vendredi", "Samedi", "Dimanche"
    ))
  ) |>
  filter(!is.na(heure), !is.na(jour))

# --- 4. Agrégation heure × jour ------------------------------

# Moyenne des montées par tranche horaire et par jour de la semaine
heatmap_data <- freq_horaire |>
  group_by(jour, heure) |>
  summarise(
    moy_montees = mean(nb_de_montees, na.rm = TRUE),
    .groups     = "drop"
  )

# Aperçu
print(heatmap_data)

# --- 5. Heatmap heure × jour ---------------------------------

ggplot(heatmap_data,
       aes(x = heure, y = jour, fill = moy_montees)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    name   = "Montées\nmoyennes",
    option = "C",      # Palette "inferno" — du sombre au jaune vif
    labels = scales::comma
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    labels = function(x) paste0(x, "h")
  ) +
  labs(
    title    = "Fréquentation TPG par heure et jour de la semaine",
    subtitle = "Moyenne des montées par tranche horaire — réseau complet",
    x        = "Heure de la journée",
    y        = NULL,
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

ggsave("outputs/04_heatmap_horaire.png",
       width = 12, height = 5, dpi = 150)
message("✓ Heatmap sauvegardée : outputs/04_heatmap_horaire.png")