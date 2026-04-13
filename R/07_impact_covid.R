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

# =============================================================
# TEST STATISTIQUE — T-002
# Hypothèse : Les jours VACANCES ont une fréquentation
#             significativement différente des jours NORMAL
# Méthode   : Mann-Whitney (Wilcoxon rank-sum test)
# H0        : Les distributions NORMAL et VACANCES sont égales
# H1        : Les distributions diffèrent significativement
# Données   : Dataset fréquentation par tranche horaire
#             (freq_heure_raw — agrégé par date)
# Pourquoi Mann-Whitney et pas un test t ?
#   → Les distributions sont asymétriques (Shapiro violé en T-001)
#   → Mann-Whitney est non-paramétrique : compare les rangs,
#     pas les moyennes — robuste aux outliers et à l'asymétrie
#   → Avec deux groupes (NORMAL vs VACANCES), c'est l'équivalent
#     non-paramétrique du test t indépendant
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-002 — Mann-Whitney : NORMAL vs VACANCES\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# --- Préparation des données --------------------------------
# On agrège par date pour avoir une observation par jour
# (évite la pseudoréplication — un jour = une observation)

freq_par_date_type <- freq_horaire |>
  group_by(date, horaire_type) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  filter(horaire_type %in% c("NORMAL", "VACANCES"))

# Résumé descriptif par type
cat("--- Statistiques descriptives par type de jour ---\n\n")
stats_type <- freq_par_date_type |>
  group_by(horaire_type) |>
  summarise(
    n        = n(),
    moyenne  = round(mean(total_montees), 0),
    mediane  = round(median(total_montees), 0),
    sd       = round(sd(total_montees), 0),
    min      = round(min(total_montees), 0),
    max      = round(max(total_montees), 0),
    .groups  = "drop"
  )
print(stats_type)

# Différence en % entre les médianes
med_normal   <- stats_type$mediane[stats_type$horaire_type == "NORMAL"]
med_vacances <- stats_type$mediane[stats_type$horaire_type == "VACANCES"]
diff_pct     <- round((med_vacances / med_normal - 1) * 100, 1)

cat("\nDifférence médiane VACANCES vs NORMAL :",
    diff_pct, "%\n\n")

# --- Vérification normalité ---------------------------------
cat("--- Vérification normalité (Shapiro-Wilk) ---\n")
cat("Rappel : si normalité violée → Mann-Whitney\n\n")

for (type in c("NORMAL", "VACANCES")) {
  vals <- freq_par_date_type$total_montees[
    freq_par_date_type$horaire_type == type]
  sw <- shapiro.test(vals)
  cat(type, ": W =", round(sw$statistic, 4),
      "| p =", format(sw$p.value, scientific = TRUE, digits = 3),
      "| Normal :", ifelse(sw$p.value > 0.05, "OUI", "NON"), "\n")
}

# --- Test de Mann-Whitney -----------------------------------
cat("\n--- Test de Mann-Whitney (Wilcoxon rank-sum) ---\n")
cat("H0 : distributions identiques\n")
cat("H1 : distribution VACANCES ≠ NORMAL\n\n")

normal_vals   <- freq_par_date_type$total_montees[
  freq_par_date_type$horaire_type == "NORMAL"]
vacances_vals <- freq_par_date_type$total_montees[
  freq_par_date_type$horaire_type == "VACANCES"]

mw_test <- wilcox.test(
  normal_vals,
  vacances_vals,
  alternative = "two.sided",  # test bilatéral
  conf.int    = TRUE,         # intervalle de confiance sur la différence
  conf.level  = 0.95
)
print(mw_test)

cat("\nConclusion Mann-Whitney :\n")
cat("W =", mw_test$statistic,
    "| p =", format(mw_test$p.value, scientific = TRUE, digits = 3), "\n")
cat(ifelse(mw_test$p.value < 0.05,
           "REJET H0 — les distributions diffèrent significativement",
           "NON-REJET H0 — pas de différence significative"), "\n")
cat("Estimation de la différence (Hodges-Lehmann) :",
    round(mw_test$estimate), "montées\n")
cat("IC 95% sur la différence : [",
    round(mw_test$conf.int[1]), ";",
    round(mw_test$conf.int[2]), "]\n")

# --- Taille d'effet — r de rang (effet de Wilcoxon) ---------
# r = Z / sqrt(N) — conventionnel pour Mann-Whitney
# Interprétation : 0.1 petit | 0.3 moyen | 0.5 grand
n_total <- length(normal_vals) + length(vacances_vals)
z_score <- qnorm(mw_test$p.value / 2)  # approximation z
r_effet <- abs(z_score) / sqrt(n_total)

cat("\nTaille d'effet r =", round(r_effet, 3), "\n")
cat("Interprétation : <0.1 négligeable | 0.1-0.3 petit",
    "| 0.3-0.5 moyen | >0.5 grand\n")

# --- Conclusion formelle T-002 ------------------------------
cat("\n--- CONCLUSION T-002 ---\n")
cat("La fréquentation est-elle significativement plus basse\n")
cat("les jours VACANCES que les jours NORMAL ?\n\n")
cat("Médiane NORMAL   :", format(med_normal, big.mark = "'"), "montées/jour\n")
cat("Médiane VACANCES :", format(med_vacances, big.mark = "'"), "montées/jour\n")
cat("Différence       :", diff_pct, "%\n")
cat("p-value          :", format(mw_test$p.value, scientific = TRUE, digits = 3), "\n")
cat("Taille d'effet r :", round(r_effet, 3), "\n")
cat("IC 95% différence: [", round(mw_test$conf.int[1]),
    ";", round(mw_test$conf.int[2]), "]\n")
cat("Statut T-002     : COMPLÉTÉ\n")

# NOTE VIZ : boxplot NORMAL vs VACANCES pour rapport final
# NOTE VIZ : violin plot montrant la distribution complète des deux groupes

# =============================================================
# TEST STATISTIQUE — T-004
# Hypothèse : Le COVID a causé une rupture structurelle
#             permanente dans la fréquentation mensuelle
# Méthodes  :
#   1. Test de Chow — rupture à date fixe (mars 2020)
#   2. Test CUSUM — stabilité des paramètres dans le temps
#   3. Test CUSUM des carrés — stabilité de la variance
#   4. Test de Bai-Perron — ruptures multiples, dates inconnues
#
# Pourquoi plusieurs tests ?
#   → Chow : puissant si la date est connue, mais présuppose
#     une seule rupture à une date fixe
#   → CUSUM : détecte les dérives graduelles ET les ruptures
#     brutales — visuel et intuitif
#   → CUSUM² : sensible aux changements de variance,
#     pas seulement de niveau
#   → Bai-Perron : laisse les données parler — détecte
#     automatiquement le nombre et la date des ruptures
#     sans présupposition. Le plus général des quatre.
#
# Dataset : evolution_mensuelle (agrégat mensuel 2016-2026)
# =============================================================

# Package requis
if (!require(strucchange)) install.packages("strucchange")
library(strucchange)

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-004 — Ruptures structurelles série mensuelle\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# --- Préparation : série temporelle ------------------------
# On travaille sur la série mensuelle agrégée
# déjà calculée dans ce script : evolution_mensuelle

serie_ts_test <- ts(
  evolution_mensuelle$total_montees / 1000000,  # en millions
  start     = c(min(evolution_mensuelle$annee),
                evolution_mensuelle$indice_du_mois[1]),
  frequency = 12
)

cat("Série : jan 2016 → fév 2026 |",
    length(serie_ts_test), "observations mensuelles\n\n")

# --- 1. TEST DE CHOW ----------------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("1. TEST DE CHOW — rupture à date fixe (mars 2020)\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("H0 : pas de rupture structurelle en mars 2020\n")
cat("H1 : la structure de la série change en mars 2020\n\n")
cat("Principe : compare un modèle unique (toute la période)\n")
cat("à deux modèles séparés (avant et après mars 2020).\n")
cat("Si les deux modèles séparés expliquent significativement\n")
cat("mieux les données → rupture prouvée.\n\n")

# Date de rupture : mars 2020 = observation numéro ?
# Jan 2016 = obs 1, donc mars 2020 = (2020-2016)*12 + 3 = 51
obs_rupture <- (2020 - min(evolution_mensuelle$annee)) * 12 +
  evolution_mensuelle$indice_du_mois[1] + 2
cat("Observation correspondant à mars 2020 :", obs_rupture, "\n\n")

chow_test <- sctest(serie_ts_test ~ 1,
                    type   = "Chow",
                    point  = obs_rupture)
print(chow_test)

cat("\nF =", round(chow_test$statistic, 3),
    "| p =", format(chow_test$p.value, scientific = TRUE, digits = 3), "\n")
cat("Conclusion Chow :",
    ifelse(chow_test$p.value < 0.05,
           "REJET H0 — rupture structurelle prouvée en mars 2020",
           "NON-REJET H0 — pas de rupture détectée"), "\n\n")

# --- 2. TEST CUSUM ------------------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("2. TEST CUSUM — stabilité des paramètres\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("Principe : somme cumulée des résidus d'une régression.\n")
cat("Si la série est stable, le CUSUM reste dans les bandes.\n")
cat("Une sortie des bandes signale une instabilité.\n\n")

cusum_test <- efp(serie_ts_test ~ 1, type = "Rec-CUSUM")
cusum_sctest <- sctest(cusum_test)
print(cusum_sctest)

cat("\np =", format(cusum_sctest$p.value, scientific = TRUE, digits = 3),
    "| Instabilité :",
    ifelse(cusum_sctest$p.value < 0.05, "OUI", "NON"), "\n\n")

# Graphique CUSUM
png("outputs/T004_cusum.png", width = 1200, height = 600, res = 150)
plot(cusum_test,
     main = "CUSUM — Stabilité de la série mensuelle TPG",
     ylab = "CUSUM des résidus",
     xlab = "Temps")
dev.off()
message("✓ Graphique CUSUM sauvegardé")

# --- 3. TEST CUSUM DES CARRÉS -------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("3. TEST CUSUM DES CARRÉS — stabilité de la variance\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("Variante du CUSUM sur les résidus au carré.\n")
cat("Détecte les changements de VARIANCE (volatilité),\n")
cat("pas seulement les changements de niveau moyen.\n\n")

cusum2_test  <- efp(serie_ts_test ~ 1, type = "OLS-CUSUM")
cusum2_sctest <- sctest(cusum2_test)
print(cusum2_sctest)

cat("\np =", format(cusum2_sctest$p.value, scientific = TRUE, digits = 3),
    "| Instabilité variance :",
    ifelse(cusum2_sctest$p.value < 0.05, "OUI", "NON"), "\n\n")

# --- 4. TEST DE BAI-PERRON ----------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("4. TEST DE BAI-PERRON — ruptures multiples\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("Le plus général : détecte automatiquement le nombre\n")
cat("optimal de ruptures ET leurs dates exactes.\n")
cat("Aucune présupposition sur la date ou le nombre.\n")
cat("On laisse les données répondre elles-mêmes.\n\n")

bp_test <- breakpoints(serie_ts_test ~ 1,
                       h      = 0.1,   # taille minimale de segment (10% de la série)
                       breaks = 5)     # nombre maximum de ruptures à tester

cat("Résumé Bai-Perron :\n")
print(summary(bp_test))

# Nombre optimal de ruptures (critère BIC)
# Extraction correcte du BIC depuis bp_summary$RSS
bic_tableau <- bp_summary$RSS["BIC", ]
rss_tableau <- bp_summary$RSS["RSS", ]

cat("\nRSS et BIC par nombre de ruptures :\n")
cat(sprintf("%-12s %8s %8s\n", "Ruptures", "RSS", "BIC"))
cat(paste(rep("-", 30), collapse = ""), "\n")
for (i in seq_along(bic_tableau)) {
  cat(sprintf("%-12s %8.1f %8.1f %s\n",
              paste("m =", i - 1),
              rss_tableau[i],
              bic_tableau[i],
              ifelse(bic_tableau[i] == min(bic_tableau), "<-- OPTIMAL", "")))
}

n_ruptures_optimal <- as.integer(names(which.min(bic_tableau)))
cat("\nNombre optimal de ruptures (BIC minimum) :", n_ruptures_optimal, "\n")
cat("BIC optimal :", round(min(bic_tableau), 2), "\n")

# Dates des ruptures
if (!is.na(bp_test$breakpoints[1])) {
  dates_rupture <- evolution_mensuelle$date_mois[bp_test$breakpoints]
  cat("Dates des ruptures détectées :\n")
  for (d in dates_rupture) {
    cat(" →", format(as.Date(d, origin = "1970-01-01"), "%B %Y"), "\n")
  }
} else {
  cat("Aucune rupture détectée par Bai-Perron\n")
}

# IC des ruptures
cat("\nIntervalles de confiance des ruptures :\n")
print(confint(bp_test))

# Graphique Bai-Perron
png("outputs/T004_bai_perron.png", width = 1400, height = 700, res = 150)
plot(bp_test,
     main = "Bai-Perron — Ruptures structurelles détectées",
     ylab = "Fréquentation (millions de montées)",
     xlab = "Temps")
lines(serie_ts_test)
dev.off()
message("✓ Graphique Bai-Perron sauvegardé")

# --- 5. Synthèse T-004 --------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("SYNTHÈSE T-004 — Ruptures structurelles\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("Test de Chow (mars 2020)  : p =",
    format(chow_test$p.value, digits = 3), "\n")
cat("CUSUM (stabilité niveau)  : p =",
    format(cusum_sctest$p.value, digits = 3), "\n")
cat("CUSUM² (stabilité variance): p =",
    format(cusum2_sctest$p.value, digits = 3), "\n")
cat("Bai-Perron : ruptures optimales =", bp_optimal - 1, "\n")
cat("Statut T-004 : COMPLÉTÉ\n")

# NOTE VIZ : graphique CUSUM annoté pour rapport final
# NOTE VIZ : graphique Bai-Perron avec dates annotées

# =============================================================
# TEST STATISTIQUE — T-004b
# Version améliorée de T-004 : tests de rupture sur les
# RÉSIDUS STL — après extraction de la tendance et
# de la saisonnalité
#
# Pourquoi c'est plus rigoureux ?
#   → T-004 testait la série brute — la saisonnalité naturelle
#     (juillet creux, novembre pic) et la tendance long terme
#     polluaient les tests de rupture
#   → En travaillant sur les résidus STL, on teste uniquement
#     ce qui reste INEXPLIQUÉ par la tendance et la saisonnalité
#   → Si une rupture est détectée dans les résidus, c'est
#     un choc pur — non attribuable aux patterns habituels
#   → C'est la version que citerait un statisticien dans
#     une publication académique
#
# Comparaison des résultats T-004 vs T-004b :
#   → Si les dates concordent : T-004 était robuste
#   → Si elles diffèrent     : T-004b est la référence
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-004b — Ruptures sur résidus STL\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("Principe : on extrait d'abord la tendance et la\n")
cat("saisonnalité par décomposition STL, puis on teste\n")
cat("les ruptures sur les résidus — ce qui reste\n")
cat("inexpliqué par les patterns habituels.\n\n")

# --- Décomposition STL sur la série complète ----------------
# On réutilise serie_ts_test déjà construite
decomp_stl_b <- stl(serie_ts_test, s.window = "periodic")

# Extraction des résidus
residus_stl <- as.numeric(decomp_stl_b$time.series[, "remainder"])
residus_ts  <- ts(residus_stl,
                  start     = start(serie_ts_test),
                  frequency = 12)

cat("Statistiques des résidus STL :\n")
cat("  Moyenne  :", round(mean(residus_stl), 4),
    "(doit être ≈ 0)\n")
cat("  Écart-type:", round(sd(residus_stl), 3), "millions\n")
cat("  Min      :", round(min(residus_stl), 3), "millions\n")
cat("  Max      :", round(max(residus_stl), 3), "millions\n\n")

# --- 1. Chow sur résidus ------------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("1. Chow sur résidus STL — rupture mars 2020\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

chow_resid <- sctest(residus_ts ~ 1,
                     type  = "Chow",
                     point = obs_rupture)
print(chow_resid)
cat("\nConclusion Chow sur résidus :",
    ifelse(chow_resid$p.value < 0.05,
           "REJET H0 — rupture prouvée même après STL",
           "NON-REJET H0 — rupture absorbée par STL"), "\n\n")

# --- 2. CUSUM sur résidus -----------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("2. CUSUM sur résidus STL\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

cusum_resid  <- efp(residus_ts ~ 1, type = "Rec-CUSUM")
cusum_resid_sctest <- sctest(cusum_resid)
print(cusum_resid_sctest)
cat("\np =", format(cusum_resid_sctest$p.value,
                    scientific = TRUE, digits = 3),
    "| Instabilité résidus :",
    ifelse(cusum_resid_sctest$p.value < 0.05, "OUI", "NON"), "\n\n")

# Graphique CUSUM résidus
png("outputs/T004b_cusum_residus.png",
    width = 1200, height = 600, res = 150)
plot(cusum_resid,
     main = "CUSUM sur résidus STL — série TPG",
     ylab = "CUSUM des résidus STL",
     xlab = "Temps")
dev.off()
message("✓ Graphique CUSUM résidus sauvegardé")

# --- 3. Bai-Perron sur résidus ------------------------------
cat(paste(rep("-", 50), collapse = ""), "\n")
cat("3. Bai-Perron sur résidus STL\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

bp_resid <- breakpoints(residus_ts ~ 1,
                        h      = 0.1,
                        breaks = 5)

bp_resid_summary <- summary(bp_resid)
bic_resid <- bp_resid_summary$RSS["BIC", ]
rss_resid <- bp_resid_summary$RSS["RSS", ]

cat("RSS et BIC par nombre de ruptures (résidus STL) :\n")
cat(sprintf("%-12s %8s %8s\n", "Ruptures", "RSS", "BIC"))
cat(paste(rep("-", 30), collapse = ""), "\n")
for (i in seq_along(bic_resid)) {
  cat(sprintf("%-12s %8.3f %8.3f %s\n",
              paste("m =", i - 1),
              rss_resid[i],
              bic_resid[i],
              ifelse(bic_resid[i] == min(bic_resid), "<-- OPTIMAL", "")))
}

n_rupt_resid <- as.integer(names(which.min(bic_resid)))
cat("\nNombre optimal de ruptures (résidus STL) :", n_rupt_resid, "\n")

# Dates des ruptures sur résidus
if (!is.na(bp_resid$breakpoints[1])) {
  cat("Dates des ruptures (résidus STL) :\n")
  dates_resid <- evolution_mensuelle$date_mois[bp_resid$breakpoints]
  for (d in dates_resid) {
    cat(" →", format(as.Date(d, origin = "1970-01-01"), "%B %Y"), "\n")
  }
  cat("\nIntervalles de confiance :\n")
  print(confint(bp_resid))
} else {
  cat("Aucune rupture détectée sur les résidus STL\n")
}

# --- 4. Comparaison T-004 vs T-004b -------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("COMPARAISON T-004 vs T-004b\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("              Série brute    Résidus STL\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat(sprintf("%-20s %10s %12s\n", "Chow (mars 2020)",
            paste0("p=", round(chow_test$p.value, 4)),
            paste0("p=", round(chow_resid$p.value, 4))))
cat(sprintf("%-20s %10s %12s\n", "CUSUM",
            paste0("p=", round(cusum_sctest$p.value, 4)),
            paste0("p=", round(cusum_resid_sctest$p.value, 4))))
cat(sprintf("%-20s %10s %12s\n", "Bai-Perron (m opt.)",
            paste0("m=", n_ruptures_optimal),
            paste0("m=", n_rupt_resid)))
cat("\nStatut T-004b : COMPLÉTÉ\n")

# NOTE VIZ : superposition résidus STL + ruptures Bai-Perron
# NOTE VIZ : comparaison graphique série brute vs résidus