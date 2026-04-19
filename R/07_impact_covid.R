# ============================================================
# SCRIPT 07 — IMPACT COVID ET RUPTURES STRUCTURELLES
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Tester formellement l'impact du COVID sur la
# fréquentation TPG et analyser la récupération post-COVID.
# Tests à intégrer :
#   T-002 : NORMAL vs VACANCES (Mann-Whitney)
#   T-004 : Ruptures structurelles série brute (Chow+CUSUM+BP)
#   T-004b: Ruptures sur résidus STL
#   T-006 : Pré-COVID vs post-COVID (Mann-Whitney)
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
library(strucchange)  # Chow, CUSUM, Bai-Perron

# ── 2. CHARGEMENT ───────────────────────────────────────────

# Données mensuelles agrégées
mensuel <- readRDS("../data/raw/mensuel.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois), ligne = as.character(ligne))

mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(montees_totales = sum(nb_de_montees, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(date)

# Résidus STL produits en script 06
composantes <- readRDS("../data/processed/stl_composantes.rds")

# Données horaires pour T-002
horaire <- readRDS("../data/raw/horaire.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(horaire_tranche_stop_theo)) %>%
  mutate(heure = as.integer(horaire_tranche_stop_theo)) %>%
  filter(!is.na(heure))

cat("Mensuel global  :", nrow(mensuel_global), "mois\n")
cat("Composantes STL :", nrow(composantes), "mois\n")
cat("Horaire         :", nrow(horaire), "obs.\n")


# ── 3. TEST T-002 — NORMAL VS VACANCES ──────────────────────
# Hypothèse : fréquentation NORMAL > fréquentation VACANCES
# Test : Mann-Whitney (deux groupes indépendants, normalité violée)
# Agrégation par date d'abord — une observation par jour
# Raison : éviter pseudoréplication (tranches horaires non indépendantes)

jour_data <- horaire %>%
  filter(horaire_type %in% c("NORMAL", "VACANCES")) %>%
  group_by(date, horaire_type) %>%
  summarise(
    montees_jour = sum(nb_de_montees, na.rm = TRUE),
    .groups      = "drop"
  )

# Statistiques descriptives
cat("=== STATISTIQUES DESCRIPTIVES T-002 ===\n\n")
jour_data %>%
  group_by(horaire_type) %>%
  summarise(
    n        = n(),
    moyenne  = round(mean(montees_jour)),
    mediane  = round(median(montees_jour)),
    ecart_type = round(sd(montees_jour))
  ) %>%
  print()

# Différence des médianes
med_normal   <- median(jour_data$montees_jour[jour_data$horaire_type == "NORMAL"])
med_vacances <- median(jour_data$montees_jour[jour_data$horaire_type == "VACANCES"])
cat("\nDifférence médianes :", round((med_vacances - med_normal) / med_normal * 100, 1), "%\n")

# Vérification normalité
cat("\nShapiro-Wilk par groupe :\n")
for (g in c("NORMAL", "VACANCES")) {
  vals <- jour_data$montees_jour[jour_data$horaire_type == g]
  sw   <- shapiro.test(vals)
  cat(sprintf("  %-10s W = %.3f  p = %s\n", g,
              sw$statistic,
              format(sw$p.value, scientific = TRUE, digits = 3)))
}


# ── 4. MANN-WHITNEY T-002 ───────────────────────────────────

mw_res <- wilcox.test(
  jour_data$montees_jour[jour_data$horaire_type == "NORMAL"],
  jour_data$montees_jour[jour_data$horaire_type == "VACANCES"],
  alternative = "greater",  # H1 : NORMAL > VACANCES
  conf.int    = TRUE,
  conf.level  = 0.95
)

cat("=== RÉSULTATS T-002 ===\n\n")
cat("W               :", mw_res$statistic, "\n")
cat("p-value         :", format(mw_res$p.value, scientific = TRUE), "\n")
cat("Hodges-Lehmann  :", round(mw_res$estimate), "montées\n")
cat("IC 95% borne inf:", round(mw_res$conf.int[1]), "\n")

# Taille d'effet r
n1 <- sum(jour_data$horaire_type == "NORMAL")
n2 <- sum(jour_data$horaire_type == "VACANCES")
Z  <- qnorm(mw_res$p.value, lower.tail = FALSE)
r  <- Z / sqrt(n1 + n2)
cat("Taille effet r  :", round(r, 3), "\n")
cat("Magnitude       :", ifelse(r >= 0.5, "Grand (≥ 0.5)",
                                ifelse(r >= 0.3, "Moyen", "Petit")), "\n")

# Impact concret
diff_mediane <- med_normal - med_vacances
cat("\nImpact concret  :", round(diff_mediane), "montées/jour de moins\n")
cat("Soit ~", round(diff_mediane / 600), "bus remplis retirés quotidiennement\n")

# ── 5. TEST T-004 — RUPTURES STRUCTURELLES SÉRIE BRUTE ──────
# Quatre tests complémentaires — chacun répond à une question
# différente (voir DOCUMENTATION_TECHNIQUE.md pour le détail)

ts_mensuel <- ts(mensuel_global$montees_totales,
                 start = c(2016, 1), frequency = 12)

# Position COVID dans la série (mars 2020 = mois 39)
pos_covid <- which(mensuel_global$date == as.Date("2020-03-01"))
cat("Position COVID dans la série :", pos_covid, "sur",
    length(ts_mensuel), "mois\n\n")

# ── TEST DE CHOW ─────────────────────────────────────────────
cat("--- TEST DE CHOW (mars 2020) ---\n")
chow_res <- sctest(ts_mensuel ~ 1,
                   type  = "Chow",
                   point = pos_covid)
cat("F   =", round(chow_res$statistic, 3), "\n")
cat("p   =", format(chow_res$p.value, scientific = TRUE), "\n")
cat("→", ifelse(chow_res$p.value < 0.05,
                "Rupture structurelle prouvée à mars 2020",
                "Pas de rupture détectée"), "\n\n")

# ── CUSUM ────────────────────────────────────────────────────
cat("--- CUSUM (stabilité du niveau) ---\n")
cusum_res <- efp(ts_mensuel ~ 1, type = "OLS-CUSUM")
cusum_test <- sctest(cusum_res)
cat("S   =", round(cusum_test$statistic, 3), "\n")
cat("p   =", format(cusum_test$p.value, scientific = TRUE), "\n")
cat("→", ifelse(cusum_test$p.value < 0.05,
                "Instabilité du niveau prouvée",
                "Série stable"), "\n\n")

# ── CUSUM² ───────────────────────────────────────────────────
cat("--- CUSUM² (stabilité de la variance) ---\n")
cusum2_res  <- efp(ts_mensuel ~ 1, type = "OLS-CUSUM")
cusum2_test <- sctest(efp(ts_mensuel ~ 1, type = "RE"))
cat("S   =", round(cusum2_test$statistic, 3), "\n")
cat("p   =", format(cusum2_test$p.value, scientific = TRUE), "\n")
cat("→", ifelse(cusum2_test$p.value < 0.05,
                "Instabilité de la variance prouvée",
                "Variance stable"), "\n")

# ── CUSUM² CORRIGÉ ───────────────────────────────────────────
cat("--- CUSUM² (stabilité de la variance — type RE) ---\n")
cusum2_test <- sctest(efp(ts_mensuel ~ 1, type = "RE"))
cat("S   =", round(cusum2_test$statistic, 3), "\n")
cat("p   =", format(cusum2_test$p.value, scientific = TRUE), "\n")
cat("→", ifelse(cusum2_test$p.value < 0.05,
                "Instabilité de la variance prouvée",
                "Variance stable"), "\n\n")

# ── BAI-PERRON ───────────────────────────────────────────────
cat("--- BAI-PERRON (nombre et dates des ruptures) ---\n")
bp_test <- breakpoints(ts_mensuel ~ 1)

# Extraction BIC
bp_sum_obj  <- summary(bp_test)
bic_tableau <- bp_sum_obj$RSS["BIC", ]
n_opt       <- as.integer(names(which.min(bic_tableau)))

cat("BIC par nombre de ruptures :\n")
print(round(bic_tableau, 1))
cat("\nNombre optimal de ruptures (BIC) :", n_opt, "\n")

# Dates des ruptures optimales
if (n_opt > 0) {
  bp_opt        <- breakpoints(bp_test, breaks = n_opt)
  dates_ruptures <- mensuel_global$date[bp_opt$breakpoints]
  cat("Dates des ruptures :\n")
  for (i in seq_along(dates_ruptures)) {
    cat(" ", i, ":", format(dates_ruptures[i], "%B %Y"), "\n")
  }
}

# ── 6. TEST T-004b — RUPTURES SUR RÉSIDUS STL ───────────────
# On teste si les ruptures persistent après retrait de la
# tendance et de la saisonnalité
# Si oui → rupture permanente de niveau (mean shift)
# Si non → choc transitoire absorbé par la tendance

ts_residus <- ts(composantes$residus,
                 start = c(2016, 1), frequency = 12)

cat("--- RÉSIDUS STL : statistiques ---\n")
cat("Moyenne    :", round(mean(composantes$residus) / 1e6, 4), "M\n")
cat("Écart-type :", round(sd(composantes$residus) / 1e6, 3), "M\n\n")

# Chow sur résidus
cat("--- CHOW sur résidus STL (mars 2020) ---\n")
chow_res2 <- sctest(ts_residus ~ 1,
                    type  = "Chow",
                    point = pos_covid)
cat("F =", round(chow_res2$statistic, 3),
    "  p =", format(chow_res2$p.value, scientific = TRUE), "\n")
cat("→", ifelse(chow_res2$p.value < 0.05,
                "Rupture encore visible dans les résidus",
                "Rupture absente des résidus — choc transitoire"), "\n\n")

# CUSUM sur résidus
cat("--- CUSUM sur résidus STL ---\n")
cusum_res2  <- efp(ts_residus ~ 1, type = "OLS-CUSUM")
cusum_test2 <- sctest(cusum_res2)
cat("S =", round(cusum_test2$statistic, 3),
    "  p =", format(cusum_test2$p.value, scientific = TRUE), "\n")
cat("→", ifelse(cusum_test2$p.value < 0.05,
                "Instabilité encore présente",
                "Série stable après STL"), "\n\n")

# Bai-Perron sur résidus
cat("--- BAI-PERRON sur résidus STL ---\n")
bp_res      <- breakpoints(ts_residus ~ 1)
bp_res_sum  <- summary(bp_res)
bic_res     <- bp_res_sum$RSS["BIC", ]
n_opt_res   <- as.integer(names(which.min(bic_res)))

cat("BIC par nombre de ruptures :\n")
print(round(bic_res, 1))
cat("Nombre optimal :", n_opt_res, "\n")
cat("→", ifelse(n_opt_res == 0,
                "Aucune rupture dans les résidus — COVID = choc transitoire",
                paste(n_opt_res, "rupture(s) encore présente(s) dans les résidus")), "\n")


# Identifier les dates des 2 ruptures dans les résidus
bp_res_opt    <- breakpoints(bp_res, breaks = 2)
dates_res_rup <- mensuel_global$date[bp_res_opt$breakpoints]

cat("Dates des 2 ruptures dans les résidus :\n")
for (i in seq_along(dates_res_rup)) {
  cat(" ", i, ":", format(dates_res_rup[i], "%B %Y"), "\n")
}

# Vérifier le BIC de près — est-ce que m=0 est proche de m=2 ?
cat("\nDifférence BIC m=0 vs m=2 :",
    round(bic_res["0"] - bic_res["2"], 1), "points\n")
cat("(< 10 points = différence marginale)\n")

# ── 7. BLOC DÉCISION — T-004 ET T-004b ──────────────────────

# T-004 — SÉRIE BRUTE :
# Chow F=7.003, p=0.009 → rupture prouvée à mars 2020
# CUSUM S=1.990, p=7.26×10⁻⁴ → instabilité niveau prouvée
# RE    S=1.990, p=7.26×10⁻⁴ → instabilité variance prouvée
# Bai-Perron m=3 : fév 2020, août 2021, fév 2023

# T-004b — RÉSIDUS STL :
# Chow  F=1.96,  p=0.164 → rupture ABSENTE des résidus ✅
# CUSUM S=1.094, p=0.183 → série stable après STL ✅
# Bai-Perron m=2 : nov 2019, mai 2021
#   → Nuance : écart BIC m=0 vs m=2 = 15.3 points
#   → Ces "ruptures" correspondent aux limites du modèle STL
#     avec s.window periodic sur une période incluant COVID
#   → Interprétation : artefacts de modélisation, pas des
#     ruptures structurelles réelles
#
# CONCLUSION COMMUNE T-004 + T-004b :
# Le COVID est un choc transitoire absorbé par la tendance STL
# Pas de rupture permanente de niveau — récupération complète
# Cohérent avec T-006 (à venir) : pré vs post-COVID indistinguables
#
# LIMITE DOCUMENTÉE :
# s.window = "periodic" impose une saisonnalité rigide — un STL
# flexible pourrait mieux isoler les résidus en période COVID
# Sensibilité à tester dans script 09 si temps disponible

# ── 8. TEST T-006 — PRÉ-COVID VS POST-COVID ─────────────────
# Hypothèse : la fréquentation post-COVID est-elle revenue
# au niveau pré-COVID ?
# Test : Mann-Whitney bilatéral (pas de direction a priori)
# Périodes :
#   Pré-COVID  : jan 2016 → fév 2020 (50 mois)
#   Post-COVID : jan 2022 → fév 2026 (50 mois)
#   Exclu      : mar 2020 → déc 2021 (régime COVID distinct)

pre_covid  <- mensuel_global %>%
  filter(date >= as.Date("2016-01-01") &
           date <= as.Date("2020-02-01"))

post_covid <- mensuel_global %>%
  filter(date >= as.Date("2022-01-01") &
           date <= as.Date("2026-02-01"))

cat("=== STATISTIQUES T-006 ===\n\n")
cat("Pré-COVID  : n =", nrow(pre_covid),
    "| médiane =", round(median(pre_covid$montees_totales) / 1e6, 2), "M\n")
cat("Post-COVID : n =", nrow(post_covid),
    "| médiane =", round(median(post_covid$montees_totales) / 1e6, 2), "M\n")
cat("Différence médianes :",
    round((median(post_covid$montees_totales) -
             median(pre_covid$montees_totales)) /
            median(pre_covid$montees_totales) * 100, 2), "%\n\n")

# Normalité
cat("Shapiro-Wilk :\n")
sw_pre  <- shapiro.test(pre_covid$montees_totales)
sw_post <- shapiro.test(post_covid$montees_totales)
cat("  Pré-COVID  W =", round(sw_pre$statistic, 3),
    " p =", format(sw_pre$p.value, scientific = TRUE), "\n")
cat("  Post-COVID W =", round(sw_post$statistic, 3),
    " p =", format(sw_post$p.value, scientific = TRUE), "\n\n")

# Mann-Whitney bilatéral
mw_t006 <- wilcox.test(
  pre_covid$montees_totales,
  post_covid$montees_totales,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

cat("=== RÉSULTATS T-006 ===\n\n")
cat("W               :", mw_t006$statistic, "\n")
cat("p-value         :", format(mw_t006$p.value, scientific = TRUE), "\n")
cat("Hodges-Lehmann  :", round(mw_t006$estimate / 1e6, 3), "M\n")
cat("IC 95%          : [",
    round(mw_t006$conf.int[1] / 1e6, 3), ";",
    round(mw_t006$conf.int[2] / 1e6, 3), "] M\n")

# Taille d'effet
n_pre  <- nrow(pre_covid)
n_post <- nrow(post_covid)
Z_t006 <- qnorm(mw_t006$p.value / 2, lower.tail = FALSE)
r_t006 <- Z_t006 / sqrt(n_pre + n_post)
cat("Taille effet r  :", round(r_t006, 3), "\n")
cat("→", ifelse(mw_t006$p.value > 0.05,
                "NON-REJET H0 — récupération statistiquement complète",
                "Rejet H0 — différence significative pré vs post"), "\n")


# ── 9. BLOC DÉCISION FINAL — T-006 ──────────────────────────

# RÉSULTATS T-006 :
# p = 0.506 → NON-REJET H0
# r = 0.067 → effet négligeable
# IC 95% [-0.864 ; +0.434] M → contient zéro, direction non prouvée
#
# CE QU'ON PEUT AFFIRMER :
# - Distributions pré et post-COVID statistiquement indistinguables
# - Récupération complète — le niveau d'avant COVID est retrouvé
# - Cohérent avec T-004b : COVID = choc transitoire absorbé
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que pré et post-COVID sont identiques — absence de preuve ≠ preuve d'absence
# - Que la récupération est uniforme par type de ligne (SCOLAIRE à 72%)
#
# RÉCONCILIATION T-004, T-004b, T-006 :
# T-004  : ruptures dans la série brute ✅
# T-004b : ruptures portées par la tendance, pas par niveau résiduel ✅
# T-006  : une fois COVID exclu, pré et post indistinguables ✅
# → Histoire cohérente : choc transitoire, pas changement de régime

# ── 10. SAUVEGARDE ──────────────────────────────────────────
message("Script 07 terminé.")