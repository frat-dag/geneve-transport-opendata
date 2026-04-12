# =============================================================
# PROJET TPG OPEN DATA - Phase 1, Analyse 1.5
# Fichier  : 03_evolution_temporelle.R
# Objectif : Analyser l'évolution temporelle de la fréquentation
#            mensuelle sur 3 ans (fév. 2023 – fév. 2026)
#            Identifier saisonnalité, tendances, anomalies
# Dataset  : montees-mensuelles-par-arret-par-ligne
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
# Nouveaux packages par rapport aux scripts précédents :
library(lubridate)   # Manipulation avancée des dates (mois, années, trimestres)
library(scales)      # Formatage des axes (millions, dates lisibles)

# Déjà utilisés dans les scripts précédents :
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)

# --- 2. Téléchargement du dataset mensuel --------------------

url_mensuel <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/montees-mensuelles-par-arret-par-ligne/exports/csv"

message("Téléchargement de la fréquentation mensuelle...")

reponse <- request(url_mensuel) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

freq_mensuel_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

message("✓ Données reçues : ", nrow(freq_mensuel_raw), " lignes x ", ncol(freq_mensuel_raw), " colonnes")
message("Colonnes : ")
print(colnames(freq_mensuel_raw))
print(head(freq_mensuel_raw, 3))

# --- 3. Nettoyage et préparation -----------------------------

freq_mensuel <- freq_mensuel_raw |>
  # Données définitives uniquement
  filter(donnees_definitives == TRUE) |>
  # Créer une vraie date à partir de annee + indice_du_mois
  mutate(
    date_mois = make_date(annee, indice_du_mois, 1)
  )

# Vérification de la période couverte
cat("Période couverte :\n")
cat("Du :", format(min(freq_mensuel$date_mois)), "\n")
cat("Au :", format(max(freq_mensuel$date_mois)), "\n")

# --- 4. Agrégation mensuelle totale --------------------------

# Total des montées par mois — toutes lignes et arrêts confondus
evolution_mensuelle <- freq_mensuel |>
  group_by(date_mois) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(date_mois)

print(evolution_mensuelle)

# --- 5. Visualisation — courbe temporelle --------------------

ggplot(evolution_mensuelle,
       aes(x = date_mois, y = total_montees / 1000000)) +
  geom_line(color = "#E30613", linewidth = 1) +
  geom_point(color = "#E30613", size = 2) +
  # Ligne de tendance lissée
  geom_smooth(
    method = "loess",
    color  = "#666666",
    fill   = "#EEEEEE",
    alpha  = 0.4,
    se     = TRUE
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "M")
  ) +
  labs(
    title    = "Évolution mensuelle de la fréquentation TPG",
    subtitle = "Total des montées par mois — janv. 2016 à fév. 2026",
    x        = NULL,
    y        = "Total montées (en millions)",
    caption  = "Source : opendata.tpg.ch | Courbe grise : tendance lissée (LOESS)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold"),
    plot.subtitle   = element_text(color = "grey50"),
    axis.text.x     = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/03_evolution_mensuelle.png",
       width = 12, height = 6, dpi = 150)
message("✓ Graphique sauvegardé : outputs/03_evolution_mensuelle.png")

# --- 6. Version annotée avec repères historiques -------------

# Identifier le mois le plus bas (COVID)
mois_covid <- evolution_mensuelle |>
  slice_min(total_montees, n = 1)

ggplot(evolution_mensuelle,
       aes(x = date_mois, y = total_montees / 1000000)) +
  geom_line(color = "#E30613", linewidth = 1) +
  geom_point(color = "#E30613", size = 1.5) +
  geom_smooth(
    method = "loess",
    color  = "#666666",
    fill   = "#EEEEEE",
    alpha  = 0.4,
    se     = TRUE
  ) +
  # Zone COVID en gris
  annotate(
    "rect",
    xmin  = as.Date("2020-03-01"),
    xmax  = as.Date("2021-06-01"),
    ymin  = -Inf,
    ymax  = Inf,
    fill  = "#CCCCCC",
    alpha = 0.3
  ) +
  # Label COVID
  annotate(
    "text",
    x     = as.Date("2020-09-01"),
    y     = 21,
    label = "Pandémie\nCOVID-19",
    size  = 3.5,
    color = "grey40",
    fontface = "italic"
  ) +
  # Point le plus bas
  annotate(
    "text",
    x     = mois_covid$date_mois + 60,
    y     = mois_covid$total_montees / 1000000 + 1.5,
    label = paste0("Plancher : ",
                   round(mois_covid$total_montees / 1000000, 1),
                   "M\n(",
                   format(mois_covid$date_mois, "%b %Y"), ")"),
    size  = 3,
    color = "grey30"
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "M")
  ) +
  labs(
    title    = "Évolution mensuelle de la fréquentation TPG",
    subtitle = "Total des montées par mois — janv. 2016 à fév. 2026",
    x        = NULL,
    y        = "Total montées (en millions)",
    caption  = "Source : opendata.tpg.ch | Courbe grise : tendance lissée (LOESS)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/03_evolution_mensuelle_annotee.png",
       width = 12, height = 6, dpi = 150)
message("✓ Graphique annoté sauvegardé : outputs/03_evolution_mensuelle_annotee.png")