# =============================================================
# PROJET TPG OPEN DATA - Phase 2, Analyse 2.4
# Fichier  : 07_impact_covid.R
# Objectif : Analyser l'impact du COVID-19 sur la fréquentation
#            — quantifier la baisse et la durée de récupération
#            — comparer les lignes entre elles (toutes affectées ?)
#            — identifier si le niveau pré-COVID est retrouvé
#            — préparer les tests statistiques T-004 et T-006
# Dataset  : montees-mensuelles-par-arret-par-ligne
# Date     : avril 2026
# Note     : Le COVID est visible comme résidu dans la
#            décomposition STL (script 06). Ici on l'analyse
#            directement et quantitativement.
# =============================================================

# --- 1. Packages ---------------------------------------------
# Déjà utilisés — pas de nouveau package
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

# --- 2. Données ----------------------------------------------

if (!exists("freq_mois_raw")) {
  message("Téléchargement fréquentation mensuelle...")
  url_mensuel <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/montees-mensuelles-par-arret-par-ligne/exports/csv"
  reponse <- request(url_mensuel) |>
    req_url_query(lang = "fr", delimiter = ";",
                  timezone = "Europe/Zurich") |>
    req_perform()
  freq_mois_raw <- resp_body_string(reponse) |>
    read_delim(delim = ";", locale = locale(encoding = "UTF-8"),
               show_col_types = FALSE)
  message("✓ Données reçues.")
} else {
  message("✓ Données déjà en mémoire.")
}

# --- 3. Préparation ------------------------------------------

freq_mois <- freq_mois_raw |>
  filter(donnees_definitives == TRUE) |>
  mutate(date_mois = make_date(annee, indice_du_mois, 1))

# Agrégation mensuelle totale
evolution_mensuelle <- freq_mois |>
  group_by(date_mois, annee, indice_du_mois) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  arrange(date_mois)

# --- 4. Définition des périodes COVID ------------------------
# On définit 3 périodes pour structurer l'analyse :
# PRE-COVID  : jan 2016 → fév 2020 (avant le confinement)
# COVID      : mar 2020 → déc 2021 (confinements et restrictions)
# POST-COVID : jan 2022 → fév 2026 (retour à la normale)
# Note : ces bornes sont des hypothèses à tester (T-004, T-006)

evolution_mensuelle <- evolution_mensuelle |>
  mutate(
    periode = case_when(
      date_mois < as.Date("2020-03-01")  ~ "Pré-COVID",
      date_mois <= as.Date("2021-12-31") ~ "COVID",
      TRUE                               ~ "Post-COVID"
    ),
    periode = factor(periode,
                     levels = c("Pré-COVID", "COVID", "Post-COVID"))
  )

# --- 5. Quantification de l'impact ---------------------------

stats_periodes <- evolution_mensuelle |>
  group_by(periode) |>
  summarise(
    moy_montees    = mean(total_montees, na.rm = TRUE),
    median_montees = median(total_montees, na.rm = TRUE),
    min_montees    = min(total_montees, na.rm = TRUE),
    max_montees    = max(total_montees, na.rm = TRUE),
    nb_mois        = n(),
    .groups        = "drop"
  )

cat("=== Statistiques par période ===\n")
print(stats_periodes)

# Calcul de la baisse COVID vs pré-COVID
moy_pre  <- stats_periodes$moy_montees[stats_periodes$periode == "Pré-COVID"]
moy_covid <- stats_periodes$moy_montees[stats_periodes$periode == "COVID"]
moy_post  <- stats_periodes$moy_montees[stats_periodes$periode == "Post-COVID"]

cat("\n--- Impact quantifié ---\n")
cat("Baisse COVID vs pré-COVID     :",
    round((1 - moy_covid / moy_pre) * 100, 1), "%\n")
cat("Récupération post vs pré-COVID :",
    round((moy_post / moy_pre) * 100, 1), "% du niveau initial\n")
cat("Niveau pré-COVID retrouvé ?   :",
    ifelse(moy_post >= moy_pre * 0.95, "OUI (>95%)", "NON (<95%)"), "\n")

# Mois le plus bas — plancher COVID
mois_plancher <- evolution_mensuelle |>
  slice_min(total_montees, n = 1)
cat("\nPlancher absolu :", format(mois_plancher$date_mois, "%B %Y"),
    "—", round(mois_plancher$total_montees / 1000000, 2), "M montées\n")
cat("Soit", round((1 - mois_plancher$total_montees / moy_pre) * 100, 1),
    "% en dessous de la moyenne pré-COVID\n\n")
cat("NOTE : Tests statistiques T-004 (rupture structurelle)\n")
cat("       et T-006 (Mann-Whitney pré vs post) à effectuer\n")
cat("       pour valider ces observations\n\n")

# --- 6. Visualisation 1 — évolution avec périodes colorées ---

couleurs_periodes <- c(
  "Pré-COVID"  = "#4A6FA5",
  "COVID"      = "#E30613",
  "Post-COVID" = "#1D9E75"
)

ggplot(evolution_mensuelle,
       aes(x = date_mois, y = total_montees / 1000000,
           color = periode)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  # Zones colorées en arrière-plan
  annotate("rect",
           xmin = as.Date("2020-03-01"),
           xmax = as.Date("2022-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "#FFCCCC", alpha = 0.3) +
  # Ligne de référence — moyenne pré-COVID
  geom_hline(yintercept = moy_pre / 1000000,
             linetype = "dashed", color = "#4A6FA5",
             linewidth = 0.6) +
  annotate("text",
           x = as.Date("2016-06-01"),
           y = moy_pre / 1000000 + 0.3,
           label = paste0("Moyenne pré-COVID : ",
                          round(moy_pre / 1000000, 1), "M"),
           color = "#4A6FA5", size = 3, hjust = 0) +
  # Annotation plancher
  annotate("text",
           x = mois_plancher$date_mois + 60,
           y = mois_plancher$total_montees / 1000000 + 1.2,
           label = paste0("Plancher : ",
                          round(mois_plancher$total_montees / 1000000, 1),
                          "M\n(",
                          format(mois_plancher$date_mois, "%b %Y"), ")"),
           color = "#E30613", size = 3) +
  scale_color_manual(values = couleurs_periodes, name = "Période") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  labs(
    title    = "Impact du COVID-19 sur la fréquentation TPG",
    subtitle = "Évolution mensuelle 2016-2026 — 3 périodes distinctes",
    x        = NULL,
    y        = "Total montées (millions)",
    caption  = "Source : opendata.tpg.ch | Zone rouge = période COVID (mar. 2020 – déc. 2021)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )

ggsave("outputs/07_impact_covid_evolution.png",
       width = 12, height = 6, dpi = 150)
message("✓ Graphique évolution COVID sauvegardé")

# --- 7. Visualisation 2 — comparaison par type de ligne ------
# Question : toutes les lignes ont-elles été affectées
# de la même façon ?

impact_par_type <- freq_mois |>
  mutate(
    periode = case_when(
      date_mois < as.Date("2020-03-01")  ~ "Pre_COVID",
      date_mois <= as.Date("2021-12-31") ~ "COVID",
      TRUE                               ~ "Post_COVID"
    )
  ) |>
  filter(!is.na(ligne_type_act)) |>
  group_by(ligne_type_act, periode) |>
  summarise(
    moy_montees = mean(nb_de_montees, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from  = periode,
    values_from = moy_montees
  ) |>
  mutate(
    baisse_covid_pct = round((1 - COVID / Pre_COVID) * 100, 1),
    recup_post_pct   = round((Post_COVID / Pre_COVID) * 100, 1)
  ) |>
  arrange(desc(baisse_covid_pct))

cat("=== Impact COVID par type de ligne ===\n")
print(impact_par_type)

# Graphique comparatif
impact_long <- impact_par_type |>
  select(ligne_type_act, Pre_COVID, COVID, Post_COVID) |>
  tidyr::pivot_longer(
    cols      = c(Pre_COVID, COVID, Post_COVID),
    names_to  = "periode",
    values_to = "moy_montees"
  ) |>
  mutate(
    periode = factor(periode,
                     levels = c("Pre_COVID", "COVID", "Post_COVID"),
                     labels = c("Pré-COVID", "COVID", "Post-COVID"))
  ) |>
  filter(!is.na(moy_montees))

ggplot(impact_long,
       aes(x = ligne_type_act, y = moy_montees,
           fill = periode)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c("Pré-COVID"  = "#4A6FA5",
               "COVID"      = "#E30613",
               "Post-COVID" = "#1D9E75"),
    name = "Période"
  ) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "Impact COVID par type de ligne TPG",
    subtitle = "Montées moyennes par arrêt/mois selon le type de ligne",
    x        = "Type de ligne",
    y        = "Montées moyennes",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 30, hjust = 1),
    legend.position  = "top"
  )

ggsave("outputs/07_impact_covid_par_type.png",
       width = 10, height = 6, dpi = 150)
message("✓ Graphique impact par type de ligne sauvegardé")

# --- 8. Notes méthodologiques importantes --------------------

cat(paste(rep("=", 60), collapse = ""), "\n")
cat("NOTES MÉTHODOLOGIQUES\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("1. BIAIS GLCT (SYN-003) :\n")
cat("   Le type GLCT affiche 117% de récupération post-COVID.\n")
cat("   ATTENTION : ce n'est PAS une croissance organique.\n")
cat("   En mai 2023, les TPG ont remporté l'appel d'offres GLCT\n")
cat("   et intégré de nouvelles lignes transfrontalières\n")
cat("   (60, 64, 66, 68 — pays de Gex).\n")
cat("   => Exclure le GLCT des comparaisons pré/post-COVID\n\n")

cat("2. DOUBLE RUPTURE STRUCTURELLE (SYN-002) :\n")
cat("   Deux événements majeurs affectent la comparabilité\n")
cat("   des données sur la période 2016-2026 :\n")
cat("   a) Décembre 2019 : mise en service Léman Express\n")
cat("      → Réorganisation complète du réseau TPG\n")
cat("      → Ligne 12 déchargée, rôle Bel-Air modifié\n")
cat("   b) Mars 2020 : COVID-19\n")
cat("      → Chute brutale de la fréquentation\n")
cat("   => Toute comparaison doit tenir compte de ces deux\n")
cat("      ruptures, pas seulement du COVID\n\n")

cat("3. GRATUITÉ JEUNES (SYN-001) :\n")
cat("   Entrée en vigueur le 1er janvier 2025.\n")
cat("   Les données post-2025 seront structurellement\n")
cat("   différentes pour les lignes scolaires/secondaires.\n")
cat("   => À tester : rupture structurelle jan 2025 sur\n")
cat("      lignes SCOLAIRE et SECONDAIRE\n\n")

# --- 9. Graphique final — évolution avec TOUTES les ruptures -

ggplot(evolution_mensuelle,
       aes(x = date_mois, y = total_montees / 1000000,
           color = periode)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  # Zone COVID
  annotate("rect",
           xmin = as.Date("2020-03-01"),
           xmax = as.Date("2022-01-01"),
           ymin = -Inf, ymax = Inf,
           fill = "#FFCCCC", alpha = 0.3) +
  # Ligne Léman Express — décembre 2019
  geom_vline(xintercept = as.Date("2019-12-01"),
             linetype = "dashed",
             color = "#1D9E75",
             linewidth = 0.8) +
  # Léman Express — déplacé à gauche de la ligne
  annotate("text",
           x = as.Date("2019-09-01"),
           y = 4,
           label = "Léman Express\ndéc. 2019",
           color = "#1D9E75",
           size = 2.8, hjust = 1,
           fontface = "italic") +
  # Ligne gratuité jeunes — janvier 2025
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dashed",
             color = "#FF8C00",
             linewidth = 0.8) +
  # Gratuité jeunes — légende déplacée en bas
  annotate("text",
           x = as.Date("2025-01-01"),
           y = 4,
           label = "Gratuité\njeunes\njan. 2025",
           color = "#FF8C00",
           size = 2.8, hjust = 0.5,
           fontface = "italic") +
  # Ligne moyenne pré-COVID
  geom_hline(yintercept = moy_pre / 1000000,
             linetype = "dashed",
             color = "#4A6FA5",
             linewidth = 0.6) +
  # Moyenne pré-COVID — déplacée plus à droite
  annotate("text",
           x = as.Date("2018-01-01"),
           y = moy_pre / 1000000 + 0.3,
           label = paste0("Moyenne pré-COVID : ",
                          round(moy_pre / 1000000, 1), "M"),
           color = "#4A6FA5", size = 3, hjust = 0) +
  annotate("text",
           x = mois_plancher$date_mois + 60,
           y = mois_plancher$total_montees / 1000000 + 1.2,
           label = paste0("Plancher : ",
                          round(mois_plancher$total_montees / 1000000, 1),
                          "M (", format(mois_plancher$date_mois, "%b %Y"), ")"),
           color = "#E30613", size = 3) +
  scale_color_manual(values = couleurs_periodes, name = "Période") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = function(x) paste0(x, "M")) +
  labs(
    title    = "Fréquentation TPG 2016-2026 — 3 ruptures structurelles majeures",
    subtitle = "Léman Express (déc. 2019) · COVID-19 (mar. 2020) · Gratuité jeunes (jan. 2025)",
    x        = NULL,
    y        = "Total montées (millions)",
    caption  = "Source : opendata.tpg.ch | Zone rouge = période COVID · Lignes vertes/oranges = ruptures structurelles"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid.minor = element_blank(),
    legend.position  = "top"
  )

ggsave("outputs/07_impact_covid_ruptures_completes.png",
       width = 14, height = 6, dpi = 150)
message("✓ Graphique ruptures complètes sauvegardé")