# =============================================================
# PROJET TPG OPEN DATA - Phase 2, Analyse 2.2
# Fichier  : 05_profil_journalier.R
# Objectif : Comparer les profils horaires moyens par type
#            de jour avec rigueur statistique
#            — identifier le jour le plus chargé
#            — mesurer l'impact des fériés/vacances
#            — visualiser les profils représentatifs
# Dataset  : frequentation-journaliere-par-tranche-horaire
# Date     : avril 2026
# Note     : Principe méthodologique — exploration visuelle
#            d'abord, puis validation statistique.
#            Toujours distinguer moyenne vs médiane.
#            Toujours séparer jours NORMAL vs VACANCES.
# =============================================================

# --- 1. Packages ---------------------------------------------
# Nouveau :
library(scales)      # Formatage des axes (virgules, pourcentages)

# Déjà utilisés :
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)

# --- 2. Téléchargement ---------------------------------------
# Si freq_horaire_raw est déjà en mémoire depuis le script 04,
# on ne retélécharge pas — économie de temps et de bande passante

if (!exists("freq_horaire_raw")) {
  message("Données absentes en mémoire — téléchargement...")
  url_horaire <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/frequentation-journaliere-par-tranche-horaire/exports/csv"
  reponse <- request(url_horaire) |>
    req_url_query(lang = "fr", delimiter = ";",
                  timezone = "Europe/Zurich") |>
    req_perform()
  freq_horaire_raw <- resp_body_string(reponse) |>
    read_delim(delim = ";", locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)
  message("✓ Données reçues.")
} else {
  message("✓ Données déjà en mémoire — pas de re-téléchargement.")
}

# --- 3. Préparation ------------------------------------------

freq_horaire <- freq_horaire_raw |>
  filter(donnees_definitives == TRUE) |>
  mutate(
    heure = as.integer(horaire_tranche_stop_theo),
    jour  = sub("^[0-9]-", "", jour_semaine),
    jour  = factor(jour, levels = c(
      "Lundi", "Mardi", "Mercredi", "Jeudi",
      "Vendredi", "Samedi", "Dimanche"
    ))
  ) |>
  filter(!is.na(heure), !is.na(jour))

cat("Période couverte :\n")
cat("Du :", format(min(freq_horaire$date)), "\n")
cat("Au :", format(max(freq_horaire$date)), "\n\n")

# --- 4. Analyse préalable — impact du type de jour -----------
# PRINCIPE : avant toute analyse par jour de semaine, on mesure
# l'impact des jours fériés/vacances sur la fréquentation.
# Intuition à tester (T-002) : les jours VACANCES ont une
# fréquentation significativement différente des jours NORMAL.

impact_type <- freq_horaire |>
  group_by(horaire_type) |>
  summarise(
    moy_montees    = mean(nb_de_montees, na.rm = TRUE),
    median_montees = median(nb_de_montees, na.rm = TRUE),
    nb_jours       = n_distinct(date),
    .groups        = "drop"
  ) |>
  arrange(desc(moy_montees))

cat("=== Impact du type de jour sur la fréquentation ===\n")
print(impact_type)

# Calcul de l'écart NORMAL vs VACANCES
ecart_pct <- round(
  (1 - impact_type$moy_montees[impact_type$horaire_type == "VACANCES"] /
     impact_type$moy_montees[impact_type$horaire_type == "NORMAL"]) * 100, 1
)
cat("\nLes jours VACANCES/fériés ont", ecart_pct,
    "% de fréquentation en moins que les jours NORMAL\n")
cat("=> Décision : on filtre sur NORMAL pour les analyses\n")
cat("   par jour de semaine (évite le biais des fériés)\n\n")
cat("NOTE : Test statistique formel (T-002) à effectuer\n")
cat("       en Phase 2 — test de Wilcoxon/Mann-Whitney\n\n")

# --- 5. Classement des jours — rigueur méthodologique --------
# PROBLÈME IDENTIFIÉ : la moyenne annuelle par jour est biaisée
# par les fériés inégalement répartis dans la semaine.
# Exemple : Jeudi a plus de fériés (Ascension, Jeûne Genevois)
#           Lundi a plus de fériés (Pâques, Pentecôte)
#           => leur moyenne annuelle est artificiellement basse
#
# SOLUTION : utiliser la MÉDIANE sur jours NORMAL uniquement
# La médiane est insensible aux valeurs extrêmes et représente
# mieux le "jour ordinaire typique" vécu par les usagers.

classement_jours <- freq_horaire |>
  filter(horaire_type == "NORMAL") |>
  group_by(jour) |>
  summarise(
    moy_montees    = mean(nb_de_montees, na.rm = TRUE),
    median_montees = median(nb_de_montees, na.rm = TRUE),
    nb_obs         = n(),
    .groups        = "drop"
  ) |>
  arrange(desc(median_montees))

cat("=== Classement des jours — jours NORMAL uniquement ===\n")
cat("(trié par médiane — mesure la plus représentative)\n\n")
print(classement_jours)

cat("\nJour le plus chargé (médiane) :",
    as.character(classement_jours$jour[1]), "\n")
cat("Jour le moins chargé semaine (médiane) :",
    as.character(classement_jours$jour[5]), "\n\n")
cat("NOTE : Test ANOVA + Tukey (T-001) à effectuer\n")
cat("       pour valider si les différences sont\n")
cat("       statistiquement significatives\n\n")

# --- 6. Agrégation par heure et jour -------------------------
# Filtre sur NORMAL uniquement — décision documentée ci-dessus
# Mesure : moyenne des montées par tranche horaire

profil_journalier <- freq_horaire |>
  filter(horaire_type %in% c("NORMAL", "SAMEDI", "DIMANCHE")) |>
  group_by(jour, heure) |>
  summarise(
    moy_montees = mean(nb_de_montees, na.rm = TRUE),
    .groups     = "drop"
  )

# --- 7. Visualisation 1 — facets par jour --------------------

ggplot(profil_journalier,
       aes(x = heure, y = moy_montees)) +
  geom_line(color = "#E30613", linewidth = 0.8) +
  geom_area(fill = "#E30613", alpha = 0.15) +
  facet_wrap(~jour, nrow = 2) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 6),
    labels = function(x) paste0(x, "h")
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Profil horaire de la fréquentation TPG",
    subtitle = "Moyenne des montées par heure — jours NORMAL uniquement (jan. 2019 – fév. 2026)",
    x        = "Heure",
    y        = "Montées moyennes",
    caption  = "Source : opendata.tpg.ch | Jours fériés et vacances exclus"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold", size = 11)
  )

ggsave("outputs/05_profil_journalier_facets.png",
       width = 14, height = 6, dpi = 150)
message("✓ Graphique facets sauvegardé")

# --- 8. Visualisation 2 — 4 jours représentatifs -------------
# Sélection basée sur les données (médiane, jours NORMAL) :
# - Mercredi : jour le plus chargé à la médiane (surprise !)
# - Jeudi    : 2ème jour le plus chargé
# - Samedi   : représentatif du weekend actif
# - Dimanche : représentatif du weekend minimal
# Note : Vendredi exclu — dernier des jours de semaine à la
#        médiane, biais fort des ponts et pré-vacances

profil_selection <- profil_journalier |>
  filter(jour %in% c("Mercredi", "Lundi", "Samedi", "Dimanche"))

couleurs_selection <- c(
  "Mercredi" = "#8B0000",   # Rouge foncé — jour le + chargé
  "Lundi"    = "#E30613",   # Rouge TPG — 2ème jour
  "Samedi"   = "#4A6FA5",   # Bleu — weekend actif
  "Dimanche" = "#888888"    # Gris — weekend minimal
)

ggplot(profil_selection,
       aes(x = heure, y = moy_montees, color = jour)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  # Annotation pic du soir
  annotate("text",
           x = 19, y = max(profil_selection$moy_montees) * 0.98,
           label = "Pic du soir 17h-18h",
           size = 3, color = "grey40", fontface = "italic", hjust = 0) +
  # Annotation pic du matin
  annotate("text",
           x = 8, y = max(profil_selection$moy_montees) * 0.85,
           label = "Pic du matin 8h",
           size = 3, color = "grey40", fontface = "italic", hjust = 0) +
  scale_color_manual(values = couleurs_selection, name = "Jour") +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    labels = function(x) paste0(x, "h")
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Profil horaire TPG — 4 jours représentatifs",
    subtitle = "Mercredi (+ chargé) · Lundi (- chargé semaine) · Samedi · Dimanche — jours NORMAL uniquement",
    x        = "Heure de la journée",
    y        = "Montées moyennes",
    caption  = "Source : opendata.tpg.ch | Sélection basée sur médiane, jours fériés exclus"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    legend.position  = "right"
  )

ggsave("outputs/05_profil_journalier_selection.png",
       width = 12, height = 6, dpi = 150)
message("✓ Graphique sélection sauvegardé")