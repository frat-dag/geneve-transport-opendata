# ── PALETTE DU PROJET ───────────────────────────────────────
# Fichier R/00_palette.R, charge par les scripts avec
# source(here::here("R", "00_palette.R")).
#
# T-04 : les conditions d'utilisation des donnees interdisent
# l'usage du nom, du sigle et des marques de l'entreprise hors
# mention de source. Les identifiants theme_tpg() et TPG_RED ont
# donc ete renommes, et la couleur principale a ete remplacee par
# une teinte propre au projet. Ce travail est independant et ne
# cherche pas a reprendre l'identite visuelle de qui que ce soit.

# Couleur principale du projet : rouge brique, choisi pour ce projet.
ROUGE_PRINCIPAL <- "#B23A48"

# Sequence temporelle : du plus ancien (pale) au plus recent (sature).
# L'oeil va vers la couleur la plus foncee, donc vers l'annee la plus recente.
PALETTE_ANNEES <- c(
  "2019" = "#F3DCDE",
  "2020" = "#E0AFB5",
  "2021" = "#CC7F89",
  "2022" = "#B95763",
  "2023" = "#B23A48",
  "2024" = "#8E2A38",
  "2025" = "#651C27"
)

# Couleurs par type de ligne
PALETTE_TYPES <- c(
  "PRINCIPAL"  = "#B23A48",   # couleur principale, le reseau dominant
  "SECONDAIRE" = "#4A6FA5",   # bleu ardoise
  "GLCT"       = "#E67E22",   # orange, transfrontalier
  "SCOLAIRE"   = "#27AE60"    # vert, scolaire
)

# Couleurs fonctionnelles : annotations et ruptures.
# COL_COVID valait auparavant la meme valeur que la couleur
# principale : le repere vertical du COVID se confondait avec la
# courbe elle-meme dans les figures 03, 06, 07 et 08. Il est
# desormais distinct des trois autres reperes.
COL_LEMAN    <- "#2196F3"   # bleu, Leman Express
COL_COVID    <- "#7D3C98"   # violet, COVID
COL_GRATUITE <- "#27AE60"   # vert, gratuite jeunes
COL_NEUTRE   <- "#888888"   # gris, elements secondaires
COL_REF      <- "#333333"   # gris fonce, lignes de reference

# Theme ggplot2 du projet
theme_projet <- function() {
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
