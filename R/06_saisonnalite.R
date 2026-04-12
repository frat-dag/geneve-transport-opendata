# =============================================================
# PROJET TPG OPEN DATA - Phase 2, Analyse 2.3
# Fichier  : 06_saisonnalite.R
# Objectif : Analyser la saisonnalité de la fréquentation
#            sur 7 ans (2016-2026)
#            — décomposition STL (tendance + saisonnalité + résidu)
#            — effets vacances scolaires, été, Noël
#            — comparaison des profils annuels
# Dataset  : montees-mensuelles-par-arret-par-ligne
# Date     : avril 2026
# Note     : La décomposition STL (Seasonal and Trend
#            decomposition using Loess) sépare une série
#            temporelle en 3 composantes :
#            1. Tendance (trend) : évolution long terme
#            2. Saisonnalité : pattern qui se répète chaque année
#            3. Résidu : ce qui reste inexpliqué (ex: COVID)
# =============================================================

# --- 1. Packages ---------------------------------------------
# Nouveaux :
library(stats)       # Décomposition STL — inclus dans R de base
library(lubridate)   # Manipulation avancée des dates

# Déjà utilisés :
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# --- 2. Téléchargement ---------------------------------------
# Réutilisation du dataset mensuel (déjà chargé en script 03)

if (!exists("freq_mensuel_raw")) {
  message("Téléchargement fréquentation mensuelle...")
  url_mensuel <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/montees-mensuelles-par-arret-par-ligne/exports/csv"
  reponse <- request(url_mensuel) |>
    req_url_query(lang = "fr", delimiter = ";",
                  timezone = "Europe/Zurich") |>
    req_perform()
  freq_mensuel_raw <- resp_body_string(reponse) |>
    read_delim(delim = ";", locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)
  message("✓ Données reçues.")
} else {
  message("✓ Données déjà en mémoire.")
}

# --- 3. Préparation série temporelle mensuelle ---------------

freq_mensuel <- freq_mensuel_raw |>
  filter(donnees_definitives == TRUE) |>
  mutate(date_mois = make_date(annee, indice_du_mois, 1))

# Agrégation mensuelle totale — toutes lignes et arrêts
evolution_mensuelle <- freq_mensuel |>
  group_by(date_mois, annee, indice_du_mois) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  arrange(date_mois)

cat("Période :", format(min(evolution_mensuelle$date_mois)),
    "→", format(max(evolution_mensuelle$date_mois)), "\n")
cat("Nombre de mois :", nrow(evolution_mensuelle), "\n\n")

# --- 4. Décomposition STL ------------------------------------
# STL nécessite un objet ts (time series) R
# fréquence = 12 (données mensuelles, cycle annuel)

serie_ts <- ts(
  evolution_mensuelle$total_montees,
  start     = c(min(evolution_mensuelle$annee),
                evolution_mensuelle$indice_du_mois[1]),
  frequency = 12   # 12 mois par an = cycle saisonnier annuel
)

# Décomposition STL
# s.window = "periodic" : saisonnalité supposée stable dans le temps
decomp_stl <- stl(serie_ts, s.window = "periodic")

# Extraire les composantes
composantes <- data.frame(
  date_mois    = evolution_mensuelle$date_mois,
  observee     = as.numeric(serie_ts),
  tendance     = as.numeric(decomp_stl$time.series[, "trend"]),
  saisonnalite = as.numeric(decomp_stl$time.series[, "seasonal"]),
  residu       = as.numeric(decomp_stl$time.series[, "remainder"])
)

# Aperçu des composantes
cat("=== Décomposition STL — statistiques ===\n")
cat("Amplitude saisonnière (max-min) :",
    round(max(composantes$saisonnalite) -
            min(composantes$saisonnalite)), "montées\n")
cat("Mois le plus fort (saisonnalité) :",
    month.name[which.max(composantes$saisonnalite[1:12])], "\n")
cat("Mois le plus faible (saisonnalité) :",
    month.name[which.min(composantes$saisonnalite[1:12])], "\n\n")

# --- 5. Visualisation décomposition STL ----------------------

# Palette cohérente avec le projet
couleurs_stl <- c(
  "Observée"      = "#E30613",
  "Tendance"      = "#8B0000",
  "Saisonnalité"  = "#4A6FA5",
  "Résidu"        = "#888888"
)

# Graphique 1 — les 4 composantes empilées
composantes_long <- composantes |>
  tidyr::pivot_longer(
    cols      = c(observee, tendance, saisonnalite, residu),
    names_to  = "composante",
    values_to = "valeur"
  ) |>
  mutate(composante = factor(composante,
                             levels = c("observee", "tendance", "saisonnalite", "residu"),
                             labels = c("Observée", "Tendance", "Saisonnalité", "Résidu")
  ))

ggplot(composantes_long,
       aes(x = date_mois, y = valeur / 1000000,
           color = composante)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~composante, scales = "free_y", ncol = 1) +
  scale_color_manual(values = couleurs_stl, guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  labs(
    title    = "Décomposition STL de la fréquentation TPG",
    subtitle = "Tendance + Saisonnalité + Résidu — jan. 2016 à fév. 2026",
    x        = NULL,
    y        = "Montées (millions)",
    caption  = "Source : opendata.tpg.ch | Méthode : STL (Loess)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave("outputs/06_decomposition_stl.png",
       width = 12, height = 10, dpi = 150)
message("✓ Décomposition STL sauvegardée")

# --- 6. Profil saisonnier moyen ------------------------------
# Quelle est la fréquentation moyenne par mois de l'année ?
# (hors effet COVID et tendance long terme)


ggplot(profil_saisonnier,
       aes(x = mois_nom, y = saisonnalite_moy / 1000,
           fill = couleur)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5) +
  # Ajout des valeurs sur les barres
  geom_text(
    aes(label = paste0(round(saisonnalite_moy / 1000), "k"),
        vjust = ifelse(saisonnalite_moy >= 0, -0.3, 1.2)),
    size  = 3,
    color = "grey30"
  ) +
  scale_fill_manual(
    values = c("positif" = "#E30613", "negatif" = "#4A6FA5"),
    guide  = "none"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "k"),
    limits = c(-2500, 2000)   # Assure que les labels ne sont pas coupés
  ) +
  labs(
    title    = "Profil saisonnier de la fréquentation TPG",
    subtitle = "Composante saisonnière STL — écart à la tendance long terme\nJanvier ≈ 0 : mois le plus neutre de l'année",
    x        = NULL,
    y        = "Écart à la tendance (milliers de montées)",
    caption  = "Source : opendata.tpg.ch | Rouge = au-dessus tendance · Bleu = en-dessous"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    plot.subtitle      = element_text(color = "grey50"),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank()
  )

ggsave("outputs/06_profil_saisonnier.png",
       width = 10, height = 6, dpi = 150)
message("✓ Profil saisonnier mis à jour")