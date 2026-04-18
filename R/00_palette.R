# ── PALETTE OFFICIELLE DU PROJET ────────────────────────────
# À sauvegarder dans R/00_palette.R
# Charger en début de chaque script avec : source("00_palette.R")

# Couleur principale — Rouge TPG officiel
TPG_RED     <- "#E30613"

# Séquence temporelle — du plus ancien (pâle) au plus récent (saturé)
# Logique : l'œil va vers la couleur la plus foncée = année la plus récente
PALETTE_ANNEES <- c(
  "2019" = "#FADBD8",
  "2020" = "#F1948A",
  "2021" = "#E74C3C",
  "2022" = "#C0392B",
  "2023" = "#E30613",
  "2024" = "#A93226",
  "2025" = "#7B241C"
)

# Couleurs par type de ligne
PALETTE_TYPES <- c(
  "PRINCIPAL"  = "#E30613",   # rouge TPG — le réseau dominant
  "SECONDAIRE" = "#4A6FA5",   # bleu ardoise
  "GLCT"       = "#E67E22",   # orange — transfrontalier
  "SCOLAIRE"   = "#27AE60"    # vert — éducation
)

# Couleurs fonctionnelles — annotations et ruptures
COL_LEMAN    <- "#2196F3"   # bleu — Léman Express
COL_COVID    <- "#E30613"   # rouge — COVID
COL_GRATUITE <- "#27AE60"   # vert — gratuité jeunes
COL_NEUTRE   <- "#888888"   # gris — éléments secondaires
COL_REF      <- "#333333"   # gris foncé — lignes de référence

# Thème ggplot2 officiel du projet
theme_tpg <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", color = "#333333"),
      plot.subtitle      = element_text(color = "#666666", size = 10),
      plot.caption       = element_text(color = "#999999", size = 8),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x        = element_text(angle = 45, hjust = 1),
      legend.position    = "bottom"
    )
}