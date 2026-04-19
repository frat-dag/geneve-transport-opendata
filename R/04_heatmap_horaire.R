# ============================================================
# SCRIPT 04 — HEATMAP HORAIRE
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Analyser les patterns de fréquentation par heure
# et par jour de semaine. Produire la heatmap de référence.
# Ce qu'on sait déjà (script 00) :
#   - Dataset horaire : 61 826 lignes, jan 2019 → avr 2026
#   - Filtre donnees_definitives == TRUE obligatoire
#   - Filtre !is.na(horaire_tranche_stop_theo) — 2.3% NA
#   - Filtre horaire_type : inclure NORMAL, SAMEDI, DIMANCHE
#   - Tests à intégrer : T-003 (pic soir vs matin)
# ============================================================

# ── 1. NETTOYAGE ET PACKAGES ─────────────────────────────────

rm(list = ls())
gc()

# Définir le répertoire de travail — adapter selon votre environnement
# setwd("chemin/vers/tpg-opendata-analysis/R")

source("00_palette.R")

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(viridis)   # palette heatmap

# ── 2. CHARGEMENT ───────────────────────────────────────────

horaire <- readRDS("../data/raw/horaire.rds")

cat("Dimensions brutes :", nrow(horaire), "lignes x",
    ncol(horaire), "colonnes\n")
cat("Période           :", format(min(horaire$date)),
    "→", format(max(horaire$date)), "\n")
cat("Colonnes          :", paste(names(horaire), collapse = ", "), "\n")

# ── 3. FILTRE QUALITÉ ────────────────────────────────────────

n_avant <- nrow(horaire)

horaire <- horaire %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(horaire_tranche_stop_theo)) %>%
  filter(horaire_type %in% c("NORMAL", "SAMEDI", "DIMANCHE",
                             "VACANCES", "FERIE")) %>%
  mutate(
    heure = as.integer(horaire_tranche_stop_theo),
    jour  = case_when(
      horaire_type == "SAMEDI"   ~ "Samedi",
      horaire_type == "DIMANCHE" ~ "Dimanche",
      TRUE ~ substr(jour_semaine, 3, nchar(jour_semaine))
    )
  ) %>%
  filter(!is.na(heure))

n_apres <- nrow(horaire)

cat("Avant filtre :", n_avant, "\n")
cat("Après filtre :", n_apres, "\n")
cat("Exclus       :", n_avant - n_apres,
    "(", round((n_avant - n_apres) / n_avant * 100, 1), "%)\n")

cat("\nTypes horaire présents :\n")
print(table(horaire$horaire_type))

cat("\nJours présents :\n")
print(table(horaire$jour))

# ── 4. AGRÉGATION HEURE × JOUR ──────────────────────────────
# Objectif : médiane des montées par combinaison heure × jour
# Pourquoi la médiane et pas la moyenne ?
# La moyenne est sensible aux jours atypiques (grèves, événements)
# La médiane représente le "jour typique ordinaire" — plus robuste

# Ordre des jours pour l'affichage
ordre_jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi",
                 "Vendredi", "Samedi", "Dimanche")

heatmap_data <- horaire %>%
  filter(heure >= 5 & heure <= 23) %>%  # exclure nuit profonde
  group_by(heure, jour) %>%
  summarise(
    mediane_montees = median(nb_de_montees, na.rm = TRUE),
    n_obs           = n(),
    .groups         = "drop"
  ) %>%
  mutate(jour = factor(jour, levels = ordre_jours))

cat("Dimensions heatmap_data :", nrow(heatmap_data), "lignes\n")
cat("Heures couvertes        : 5h à 23h\n")
cat("Combinaisons jour×heure :", n_distinct(heatmap_data$jour),
    "jours ×", n_distinct(heatmap_data$heure), "heures\n")

# Aperçu des valeurs max
cat("\nTop 5 combinaisons heure×jour :\n")
heatmap_data %>%
  arrange(desc(mediane_montees)) %>%
  head(5) %>%
  print()

# ── 5. HEATMAP HEURE × JOUR ─────────────────────────────────
# NOTE VIZ : graphique de référence du projet — version finale Python

p_heatmap <- ggplot(heatmap_data,
                    aes(x = jour,
                        y = factor(heure, levels = rev(5:23)),
                        fill = mediane_montees)) +
  geom_tile(color = "white", linewidth = 0.3) +
  
  scale_fill_viridis_c(
    option  = "magma",
    name    = "Médiane\nmontées",
    labels  = label_number(suffix = "k", scale = 1e-3)
  ) +
  
  scale_y_discrete(
    labels = function(x) paste0(x, "h")
  ) +
  
  labs(
    title    = "Heatmap de fréquentation TPG — heure × jour",
    subtitle = "Médiane des montées par tranche horaire — jours NORMAL, SAMEDI, DIMANCHE",
    x        = NULL,
    y        = "Heure",
    caption  = "Source : TPG Open Data | jan. 2019 → avr. 2026"
  ) +
  theme_tpg() +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    legend.position  = "right"
  )

print(p_heatmap)

ggsave("../outputs/04_heatmap_horaire.png",
       plot = p_heatmap, width = 12, height = 8, dpi = 150)

message("Heatmap sauvegardée.")

# ── 6. TEST T-003 — PIC SOIR (17h) VS PIC MATIN (8h) ────────
# Hypothèse H1 : médiane montées 17h > médiane montées 8h
# Test : Wilcoxon signed-rank apparié (unilatéral)
# Pourquoi apparié ? Matin et soir du MÊME jour — mesures liées
# Pourquoi unilatéral ? Direction fixée a priori (littérature
# transports urbains) AVANT de voir les données — sinon p-hacking
# Pourquoi Wilcoxon et pas t apparié ? Normalité à vérifier d'abord

# On travaille sur jours NORMAL uniquement
# Raison : T-002 prouve que VACANCES ≠ NORMAL — biais de confusion
# si on mélange les deux dans un test sur les heures

pic_data <- horaire %>%
  filter(horaire_type == "NORMAL") %>%
  filter(heure %in% c(8, 17)) %>%
  dplyr::select(date, heure, nb_de_montees) %>%
  tidyr::pivot_wider(names_from  = heure,
                     values_from = nb_de_montees,
                     names_prefix = "h") %>%
  filter(!is.na(h8) & !is.na(h17))

cat("Nombre de jours appariés :", nrow(pic_data), "\n")
cat("Médiane h8  :", round(median(pic_data$h8)), "montées\n")
cat("Médiane h17 :", round(median(pic_data$h17)), "montées\n")
cat("Ratio h17/h8 :", round(median(pic_data$h17) /
                              median(pic_data$h8), 3), "\n")

# Vérification normalité des différences
diff_h <- pic_data$h17 - pic_data$h8
cat("\nTest de normalité des différences (Shapiro-Wilk) :\n")
shapiro_res <- shapiro.test(diff_h)
cat("W =", round(shapiro_res$statistic, 3),
    "p =", format(shapiro_res$p.value, scientific = TRUE), "\n")
cat("→", ifelse(shapiro_res$p.value < 0.05,
                "NON normale → Wilcoxon signed-rank justifié",
                "Normale → t apparié possible"), "\n")

# ── 7. WILCOXON SIGNED-RANK UNILATÉRAL ──────────────────────

wilcox_res <- wilcox.test(
  pic_data$h17,
  pic_data$h8,
  paired      = TRUE,
  alternative = "greater",  # H1 : soir > matin
  conf.int    = TRUE,
  conf.level  = 0.95
)

cat("=== RÉSULTATS T-003 ===\n\n")
cat("V (statistique)     :", wilcox_res$statistic, "\n")
cat("p-value             :", format(wilcox_res$p.value,
                                    scientific = TRUE), "\n")
cat("Hodges-Lehmann      :", round(wilcox_res$estimate), "montées\n")
cat("IC 95% borne inf.   :", round(wilcox_res$conf.int[1]), "\n")

# Taille d'effet r = Z / sqrt(N)
n_paires <- nrow(pic_data)
Z <- qnorm(wilcox_res$p.value, lower.tail = FALSE)
r_effet <- Z / sqrt(n_paires)

cat("Taille d'effet r    :", round(r_effet, 3), "\n")
cat("Magnitude           :", case_when(
  r_effet >= 0.5 ~ "Grand (≥ 0.5)",
  r_effet >= 0.3 ~ "Moyen (0.3-0.5)",
  r_effet >= 0.1 ~ "Petit (0.1-0.3)",
  TRUE           ~ "Négligeable (< 0.1)"
), "\n")

# % jours où soir > matin
pct_soir_sup <- mean(pic_data$h17 > pic_data$h8) * 100
cat("% jours soir > matin:", round(pct_soir_sup, 1), "%\n")

# ── 8. BLOC DÉCISION — T-003 ────────────────────────────────

# RÉSULTATS T-003 — Wilcoxon signed-rank apparié unilatéral
# H1 : montées 17h > montées 8h sur jours NORMAL
#
# CE QU'ON PEUT AFFIRMER :
# - Pic 17h significativement > pic 8h (p = 9.6×10⁻²²⁶)
# - Différence médiane : +13 807 montées
# - IC 95% borne inférieure : +13 653 (entièrement positif)
# - Taille d'effet r = 0.866 — très grand
# - 99.9% des jours respectent cette asymétrie — quasi-règle absolue
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que l'asymétrie est uniforme sur tous les jours de semaine
#   → le mercredi pourrait faire exception (pic midi → T-005)
# - Que 8h et 17h sont les vrais pics pour toutes les lignes
#   → analyse par ligne en Phase 3
#
# LIMITE :
# On compare 8h et 17h spécifiquement — pas les vrais pics absolus
# par jour. Le vrai pic peut être 7h ou 18h selon le jour.
#
# NOTE VIZ : asymétrie matin/soir → graphique profil journalier
# fort pour publication — "le soir transporte 22% de plus que le matin"

# ── 9. SAUVEGARDE ───────────────────────────────────────────
ggsave("../outputs/04_heatmap_horaire.png",
       plot = p_heatmap, width = 12, height = 8, dpi = 150)

message("Script 04 terminé.")