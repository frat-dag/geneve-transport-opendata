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

# =============================================================
# TEST STATISTIQUE — T-003
# Hypothèse : Le pic du soir (17h) est significativement
#             plus élevé que le pic du matin (8h)
# Méthode   : Test de Wilcoxon apparié (signed-rank test)
# H0        : Pas de différence entre pic matin et pic soir
# H1        : Le pic soir > pic matin (test unilatéral)
# Données   : Dataset fréquentation par tranche horaire
#             jours NORMAL uniquement
#
# Pourquoi Wilcoxon APPARIÉ et pas Mann-Whitney ?
#   → Les observations matin et soir viennent du MÊME jour
#   → Ce sont des mesures appariées : pour chaque date,
#     on compare la tranche 8h à la tranche 17h
#   → L'appariement élimine la variabilité inter-journalière
#     et augmente la puissance du test
#   → La normalité des différences n'est pas garantie
#     → Wilcoxon signed-rank (non-paramétrique apparié)
#
# Pourquoi test UNILATÉRAL ?
#   → Notre hypothèse est directionnelle : on prédit que
#     le soir EST PLUS GRAND que le matin, pas juste différent
#   → Un test unilatéral est plus puissant quand la direction
#     est clairement anticipée — mais il faut le justifier
#     AVANT de voir les données, pas après (biais de confirmation)
#   → Justification a priori : la littérature sur les transports
#     urbains montre universellement un pic soir > matin
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-003 — Wilcoxon apparié : pic matin vs pic soir\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# --- Préparation des données --------------------------------
# Pour chaque date, extraire les montées à 8h et à 17h
# On filtre sur jours NORMAL uniquement

freq_apparie <- freq_horaire |>
  filter(
    horaire_type == "NORMAL",
    heure %in% c(8, 17)
  ) |>
  group_by(date, heure) |>
  summarise(
    montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from  = heure,
    values_from = montees,
    names_prefix = "h"
  ) |>
  filter(!is.na(h8), !is.na(h17))  # garder uniquement les jours avec les deux mesures

cat("Nombre de jours avec les deux mesures :", nrow(freq_apparie), "\n\n")

# --- Statistiques descriptives ------------------------------
cat("--- Statistiques descriptives ---\n")
cat("Pic matin (8h)  — médiane :",
    round(median(freq_apparie$h8)), "montées\n")
cat("Pic soir  (17h) — médiane :",
    round(median(freq_apparie$h17)), "montées\n")
cat("Ratio soir/matin           :",
    round(median(freq_apparie$h17) / median(freq_apparie$h8), 2), "\n")

# Distribution des différences
diff_soir_matin <- freq_apparie$h17 - freq_apparie$h8
cat("\nDifférence soir-matin :\n")
cat("  Médiane :", round(median(diff_soir_matin)), "montées\n")
cat("  Moyenne :", round(mean(diff_soir_matin)), "montées\n")
cat("  Min     :", round(min(diff_soir_matin)), "\n")
cat("  Max     :", round(max(diff_soir_matin)), "\n")
cat("  % jours où soir > matin :",
    round(mean(diff_soir_matin > 0) * 100, 1), "%\n\n")

# --- Vérification normalité des différences -----------------
cat("--- Normalité des différences (Shapiro-Wilk) ---\n")
cat("On teste la normalité des DIFFÉRENCES (h17 - h8)\n")
cat("C'est ce qui compte pour le test apparié\n\n")
sw_diff <- shapiro.test(diff_soir_matin)
cat("W =", round(sw_diff$statistic, 4),
    "| p =", format(sw_diff$p.value, scientific = TRUE, digits = 3),
    "| Normal :", ifelse(sw_diff$p.value > 0.05, "OUI", "NON"), "\n")
cat("→ Si NON : Wilcoxon signed-rank (non-paramétrique)\n")
cat("→ Si OUI : test t apparié possible\n\n")

# --- Test de Wilcoxon apparié -------------------------------
cat("--- Test de Wilcoxon signed-rank (apparié) ---\n")
cat("H0 : médiane des différences = 0\n")
cat("H1 : médiane des différences > 0 (soir > matin)\n\n")

wilcox_apparie <- wilcox.test(
  freq_apparie$h17,
  freq_apparie$h8,
  paired      = TRUE,          # CRUCIAL : appariement par date
  alternative = "greater",     # test unilatéral : soir > matin
  conf.int    = TRUE,
  conf.level  = 0.95
)
print(wilcox_apparie)

cat("\nV =", wilcox_apparie$statistic,
    "| p =", format(wilcox_apparie$p.value, scientific = TRUE, digits = 3), "\n")
cat(ifelse(wilcox_apparie$p.value < 0.05,
           "REJET H0 — le pic soir est significativement > pic matin",
           "NON-REJET H0"), "\n")
cat("Estimation Hodges-Lehmann :",
    round(wilcox_apparie$estimate), "montées\n")
cat("IC 95% inférieur           :",
    round(wilcox_apparie$conf.int[1]), "montées\n\n")

# --- Taille d'effet -----------------------------------------
# r = Z / sqrt(N) pour Wilcoxon apparié
n_paires <- nrow(freq_apparie)
z_score  <- qnorm(wilcox_apparie$p.value)
r_effet  <- abs(z_score) / sqrt(n_paires)

cat("Taille d'effet r =", round(r_effet, 3), "\n")
cat("Interprétation : <0.1 négligeable | 0.1-0.3 petit",
    "| 0.3-0.5 moyen | >0.5 grand\n\n")

# --- Conclusion formelle T-003 ------------------------------
cat("--- CONCLUSION T-003 ---\n")
cat("Le pic du soir (17h) est-il significativement\n")
cat("plus élevé que le pic du matin (8h) ?\n\n")
cat("Médiane matin (8h)  :", round(median(freq_apparie$h8)), "montées\n")
cat("Médiane soir (17h)  :", round(median(freq_apparie$h17)), "montées\n")
cat("Différence médiane  :", round(median(diff_soir_matin)), "montées\n")
cat("% jours soir > matin:",
    round(mean(diff_soir_matin > 0) * 100, 1), "%\n")
cat("p-value             :",
    format(wilcox_apparie$p.value, scientific = TRUE, digits = 3), "\n")
cat("Taille d'effet r    :", round(r_effet, 3), "\n")
cat("Statut T-003        : COMPLÉTÉ\n")

# NOTE VIZ : boxplot apparié matin vs soir par jour de semaine
# NOTE VIZ : courbe densité des différences soir-matin