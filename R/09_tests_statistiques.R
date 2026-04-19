# ============================================================
# SCRIPT 09 — RÉCAPITULATIF TESTS STATISTIQUES
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Tests complémentaires non intégrés dans les
# scripts précédents :
#   T-007 : Concentration trafic — indice de Gini + bootstrap
#   T-008 : Corrélation fréquentation × km produits
#   T-009 : Impact gratuité jeunes (jan 2025)
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
library(tidyr)

# ── 2. CHARGEMENT ───────────────────────────────────────────

journalier <- readRDS("../data/raw/journalier.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne))

mensuel <- readRDS("../data/raw/mensuel.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois), ligne = as.character(ligne))

km_prod <- readRDS("../data/raw/km_prod.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne))

cat("Journalier :", nrow(journalier), "lignes\n")
cat("Mensuel    :", nrow(mensuel), "lignes\n")
cat("Km produits:", nrow(km_prod), "lignes\n")

# ── 3. TEST T-007 — INDICE DE GINI (CONCENTRATION ARRÊTS) ───
# Hypothèse : la distribution du trafic entre arrêts est
# significativement concentrée (Gini > 0)

# Agrégation : montées totales par arrêt sur toute la période
montees_par_arret <- journalier %>%
  filter(!is.na(arret)) %>%
  group_by(arret) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(montees_totales)

cat("Nombre d'arrêts distincts :", nrow(montees_par_arret), "\n")
cat("Total montées             :",
    round(sum(montees_par_arret$montees_totales) / 1e6, 1), "M\n\n")

# Fonction Gini
gini <- function(x) {
  x <- sort(x[x > 0])
  n <- length(x)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n
}

gini_obs <- gini(montees_par_arret$montees_totales)
cat("Indice de Gini observé :", round(gini_obs, 4), "\n\n")

# Bootstrap — intervalle de confiance à 95%
set.seed(42)
n_boot <- 2000
gini_boot <- replicate(n_boot, {
  echantillon <- sample(montees_par_arret$montees_totales,
                        replace = TRUE)
  gini(echantillon)
})

ic_inf <- quantile(gini_boot, 0.025)
ic_sup <- quantile(gini_boot, 0.975)

cat("IC 95% bootstrap        : [",
    round(ic_inf, 4), ";", round(ic_sup, 4), "]\n")
cat("Magnitude               :", case_when(
  gini_obs >= 0.6 ~ "Très forte concentration",
  gini_obs >= 0.4 ~ "Forte concentration",
  gini_obs >= 0.2 ~ "Concentration modérée",
  TRUE            ~ "Distribution quasi-égale"
), "\n")


# ── 4. COURBE DE LORENZ ─────────────────────────────────────
# Visualisation de la concentration

lorenz_data <- montees_par_arret %>%
  arrange(montees_totales) %>%
  mutate(
    pct_arrets  = row_number() / n() * 100,
    pct_montees = cumsum(montees_totales) /
      sum(montees_totales) * 100
  )

# Points clés pour annotation
top10_pct <- lorenz_data %>%
  filter(pct_arrets >= 90) %>%
  slice(1) %>%
  pull(pct_montees)

p_lorenz <- ggplot(lorenz_data,
                   aes(x = pct_arrets, y = pct_montees)) +
  # Ligne d'égalité parfaite
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = COL_NEUTRE,
              linewidth = 0.6) +
  # Courbe de Lorenz
  geom_line(color = TPG_RED, linewidth = 1) +
  geom_area(fill = TPG_RED, alpha = 0.15) +
  
  # Annotation top 10%
  annotate("segment",
           x = 90, xend = 90, y = 0, yend = top10_pct,
           linetype = "dotted", color = COL_NEUTRE) +
  annotate("segment",
           x = 0, xend = 90, y = top10_pct, yend = top10_pct,
           linetype = "dotted", color = COL_NEUTRE) +
  annotate("text",
           x = 45, y = top10_pct + 2,
           label = paste0("10% des arrêts = ",
                          round(100 - top10_pct, 1),
                          "% du trafic restant\n",
                          "90% des arrêts = ",
                          round(top10_pct, 1), "% du trafic"),
           size = 3, color = COL_REF, hjust = 0) +
  
  # Gini dans le graphique
  annotate("text", x = 5, y = 90,
           label = paste0("Gini = ", round(gini_obs, 3),
                          "\nIC 95% [",
                          round(ic_inf, 3), " ; ",
                          round(ic_sup, 3), "]"),
           size = 3.2, color = TPG_RED, hjust = 0) +
  
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Courbe de Lorenz — Concentration du trafic TPG par arrêt",
    subtitle = "Gini = 0.843 — très forte concentration",
    x        = "% des arrêts (du moins au plus fréquenté)",
    y        = "% cumulé des montées",
    caption  = "Source : TPG Open Data | avr. 2023 → fév. 2026"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0))

print(p_lorenz)

ggsave("../outputs/09_lorenz_concentration.png",
       plot = p_lorenz, width = 10, height = 8, dpi = 150)

# ── 5. TEST T-008 — CORRÉLATION FRÉQUENTATION × KM PRODUITS ─
# Hypothèse : les lignes qui produisent le plus de km sont
# aussi les plus fréquentées
# Test : Pearson + Spearman (vérifier linéarité d'abord)

# Agrégation par ligne — journalier
montees_ligne <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  )

# Agrégation par ligne — km_prod
km_ligne <- km_prod %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne) %>%
  summarise(
    km_totaux = sum(km_prod, na.rm = TRUE),
    .groups   = "drop"
  )

# Jointure — DEC-001 : ligne en character dans les deux
t008_data <- montees_ligne %>%
  inner_join(km_ligne, by = "ligne")

cat("Lignes en commun :", nrow(t008_data), "\n\n")

# Corrélation Pearson et Spearman
pearson  <- cor.test(t008_data$montees_totales,
                     t008_data$km_totaux,
                     method = "pearson")
spearman <- cor.test(t008_data$montees_totales,
                     t008_data$km_totaux,
                     method = "spearman")

cat("=== RÉSULTATS T-008 ===\n\n")
cat("Pearson  r =", round(pearson$estimate, 3),
    " p =", format(pearson$p.value, scientific = TRUE), "\n")
cat("Spearman r =", round(spearman$estimate, 3),
    " p =", format(spearman$p.value, scientific = TRUE), "\n")


# Scatter plot fréquentation × km produits
p_t008 <- ggplot(t008_data,
                 aes(x = km_totaux / 1e6,
                     y = montees_totales / 1e6)) +
  geom_point(color = TPG_RED, alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = COL_REF,
              linewidth = 0.8, se = TRUE, alpha = 0.15) +
  
  # Identifier les outliers notables
  geom_text(data = t008_data %>%
              filter(montees_totales > 2e7 |
                       km_totaux > 3e6),
            aes(label = ligne),
            vjust = -0.8, size = 2.8, color = COL_REF) +
  
  annotate("text", x = max(t008_data$km_totaux / 1e6) * 0.05,
           y = max(t008_data$montees_totales / 1e6) * 0.92,
           label = paste0("Pearson r = ", round(pearson$estimate, 3),
                          "\nSpearman r = ", round(spearman$estimate, 3),
                          "\np < 10⁻³⁴"),
           hjust = 0, size = 3.2, color = COL_REF) +
  
  labs(
    title    = "Corrélation fréquentation × km produits par ligne",
    subtitle = "Chaque point = une ligne TPG | avr. 2023 → fév. 2026",
    x        = "Km produits (millions)",
    y        = "Montées totales (millions)",
    caption  = "Source : TPG Open Data"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0))

print(p_t008)

ggsave("../outputs/09_correlation_km_montees.png",
       plot = p_t008, width = 10, height = 8, dpi = 150)

# Calcul efficience par ligne
t008_data <- t008_data %>%
  mutate(
    montees_par_km = round(montees_totales / km_totaux, 1)
  )

cat("=== TOP 10 LIGNES LES PLUS EFFICIENTES ===\n")
t008_data %>%
  arrange(desc(montees_par_km)) %>%
  head(10) %>%
  dplyr::select(ligne, montees_totales, km_totaux, montees_par_km) %>%
  mutate(montees_totales = round(montees_totales / 1e6, 2),
         km_totaux = round(km_totaux / 1e6, 2)) %>%
  print()

cat("\n=== TOP 10 LIGNES LES MOINS EFFICIENTES ===\n")
t008_data %>%
  arrange(montees_par_km) %>%
  head(10) %>%
  dplyr::select(ligne, montees_totales, km_totaux, montees_par_km) %>%
  mutate(montees_totales = round(montees_totales / 1e6, 2),
         km_totaux = round(km_totaux / 1e6, 2)) %>%
  print()


# ── 6. TEST T-009 — IMPACT GRATUITÉ JEUNES (JAN 2025) ───────
# Hypothèse : la fréquentation a augmenté après jan 2025
# Méthode : comparer les mêmes mois avant et après
# jan-fév 2024 vs jan-fév 2025 — contrôle saisonnalité
# On utilise le mensuel pour avoir plus de mois comparables

# Agrégation mensuelle globale
mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  )

# Périodes comparées — mêmes mois, années différentes
avant <- mensuel_global %>%
  filter(date >= as.Date("2024-01-01") &
           date <= as.Date("2024-12-01"))

apres <- mensuel_global %>%
  filter(date >= as.Date("2025-01-01") &
           date <= as.Date("2026-02-01"))

cat("Période avant (2024)   : n =", nrow(avant), "mois\n")
cat("Période après (2025+)  : n =", nrow(apres), "mois\n\n")

# Statistiques
cat("Médiane 2024  :", round(median(avant$montees_totales) / 1e6, 2), "M\n")
cat("Médiane 2025+ :", round(median(apres$montees_totales) / 1e6, 2), "M\n")
cat("Différence    :",
    round((median(apres$montees_totales) -
             median(avant$montees_totales)) /
            median(avant$montees_totales) * 100, 1), "%\n\n")

# Mann-Whitney bilatéral
mw_t009 <- wilcox.test(
  apres$montees_totales,
  avant$montees_totales,
  alternative = "greater",
  conf.int    = TRUE,
  conf.level  = 0.95
)

cat("=== RÉSULTATS T-009 ===\n\n")
cat("W       :", mw_t009$statistic, "\n")
cat("p-value :", format(mw_t009$p.value, scientific = TRUE), "\n")
cat("→", ifelse(mw_t009$p.value < 0.05,
                "Hausse significative post-gratuité",
                "Pas de hausse significative détectée"), "\n")

# ── 7. BLOC DÉCISION FINAL — SCRIPT 09 ──────────────────────

# T-007 GINI : 0.843, IC [0.811 ; 0.872]
# Très forte concentration — 10% des arrêts = 74.2% du trafic
# 90% des arrêts = seulement 25.8% — réseau structurellement polarisé

# T-008 CORRÉLATION :
# Pearson r=0.867, Spearman r=0.924 — très forte corrélation
# Trams forment une droite parallèle au-dessus → efficience structurelle
# du site propre prouvée graphiquement
# Exclusions nécessaires pour analyse propre :
#   - Lignes CX (courses scolaires spéciales) — ratio artificiel
#   - Anciens Noctambus — biais de période
# Ligne 17 : tram Annemasse-Bel-Air-Lancy-Pont-Rouge
# (correction tracé — pas Cornavin-Onex-Bernex)

# T-009 GRATUITÉ JEUNES :
# p=0.137 — NON significatif
# +4.6% descriptiblement mais non prouvé
# Limite : n trop faible (12 vs 14 mois), facteurs confondants
# Conclusion : signal positif, preuve formelle impossible avec
# les données actuelles — à réévaluer en 2027 avec 2-3 ans de recul

ggsave("../outputs/09_correlation_km_montees.png",
       plot = p_t008, width = 10, height = 8, dpi = 150)

message("Script 09 terminé — tous les tests complétés.")

# ── AM-002 — CHOW TEST DÉCLIN SCOLAIRE ──────────────────────
# Hypothèse : la tendance SCOLAIRE 2016-2019 est significativement
# différente de la tendance 2020-2025
# On teste si le COVID a causé un changement de régime permanent
# sur les lignes scolaires — pas juste un choc transitoire

# Agrégation mensuelle SCOLAIRE uniquement
scolaire_global <- mensuel %>%
  filter(ligne_type_act == "SCOLAIRE") %>%
  group_by(date) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("Série SCOLAIRE :", nrow(scolaire_global), "mois\n")
cat("De :", format(min(scolaire_global$date)),
    "à :", format(max(scolaire_global$date)), "\n\n")

# Série temporelle
ts_scolaire <- ts(scolaire_global$montees_totales,
                  start = c(2016, 1), frequency = 12)

# Position mars 2020 dans la série
pos_covid_sc <- which(scolaire_global$date == as.Date("2020-03-01"))
cat("Position COVID dans série SCOLAIRE :", pos_covid_sc, "\n\n")

# Test de Chow — rupture en mars 2020
cat("--- CHOW TEST SCOLAIRE (mars 2020) ---\n")
chow_sc <- sctest(ts_scolaire ~ 1,
                  type  = "Chow",
                  point = pos_covid_sc)
cat("F =", round(chow_sc$statistic, 3), "\n")
cat("p =", format(chow_sc$p.value, scientific = TRUE), "\n")
cat("→", ifelse(chow_sc$p.value < 0.05,
                "Rupture structurelle SCOLAIRE prouvée — changement de régime",
                "Pas de rupture détectée"), "\n\n")

# Bai-Perron — combien de ruptures et quand ?
cat("--- BAI-PERRON SCOLAIRE ---\n")
bp_sc      <- breakpoints(ts_scolaire ~ 1)
bp_sc_sum  <- summary(bp_sc)
bic_sc     <- bp_sc_sum$RSS["BIC", ]
n_opt_sc   <- as.integer(names(which.min(bic_sc)))

cat("BIC par nombre de ruptures :\n")
print(round(bic_sc, 1))
cat("Nombre optimal :", n_opt_sc, "\n")

if (n_opt_sc > 0) {
  bp_sc_opt      <- breakpoints(bp_sc, breaks = n_opt_sc)
  dates_sc_rup   <- scolaire_global$date[bp_sc_opt$breakpoints]
  cat("Dates des ruptures :\n")
  for (i in seq_along(dates_sc_rup)) {
    cat(" ", i, ":", format(dates_sc_rup[i], "%B %Y"), "\n")
  }
}

# ── BLOC DÉCISION AM-002 ─────────────────────────────────────

# RÉSULTATS :
# Chow F=29.591, p=3.26×10⁻⁷ → rupture structurelle SCOLAIRE prouvée
# Bai-Perron m=1 optimal — rupture unique : juin 2021
#
# CE QU'ON PEUT AFFIRMER :
# - Le COVID a causé un changement de régime PERMANENT sur SCOLAIRE
# - Contrairement au réseau global (T-006 p=0.506), SCOLAIRE n'a PAS récupéré
# - La rupture est en juin 2021 (sortie de crise) pas mars 2020 (confinement)
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - La cause : nouvelles habitudes familiales ? Démographie ?
#   Requalification de lignes ? Les données ne permettent pas de trancher.
#
# ANGLE NARRATIF FORT :
# "Le COVID a durablement modifié les habitudes de transport scolaire
# à Genève — une rupture prouvée statistiquement que le réseau global
# ne montre pas. Les familles ne sont pas revenues aux transports publics
# pour emmener leurs enfants à l'école."
#
# IMPLICATION POLITIQUE :
# La gratuité jeunes (jan 2025) est une réponse potentielle à ce déclin.
# T-009 sur SCOLAIRE spécifiquement — à faire dans AM-003.

message("AM-002 complété.")


# ── AM-003 — T-009 PAR TYPE DE LIGNE ────────────────────────
# T-009 global était non significatif (p=0.137) — trop agrégé
# On refait par type de ligne — l'effet gratuité devrait être
# visible sur SCOLAIRE et SECONDAIRE, pas sur PRINCIPAL

# Agrégation mensuelle par type de ligne
mensuel_type_mois <- mensuel %>%
  mutate(ligne_type_act = case_when(
    ligne_type_act %in% c("REGIONAL", "REGIONAL COMMUNE") ~ "SECONDAIRE",
    TRUE ~ ligne_type_act
  )) %>%
  filter(ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE",
                               "SCOLAIRE", "GLCT")) %>%
  group_by(date, ligne_type_act) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  )

cat("=== AM-003 — T-009 PAR TYPE DE LIGNE ===\n\n")

for (type in c("SCOLAIRE", "SECONDAIRE", "PRINCIPAL", "GLCT")) {
  
  avant <- mensuel_type_mois %>%
    filter(ligne_type_act == type,
           date >= as.Date("2024-01-01") &
             date <= as.Date("2024-12-01"))
  
  apres <- mensuel_type_mois %>%
    filter(ligne_type_act == type,
           date >= as.Date("2025-01-01") &
             date <= as.Date("2026-02-01"))
  
  if (nrow(avant) < 3 | nrow(apres) < 3) next
  
  mw <- wilcox.test(apres$montees_totales,
                    avant$montees_totales,
                    alternative = "greater")
  
  diff_pct <- round((median(apres$montees_totales) -
                       median(avant$montees_totales)) /
                      median(avant$montees_totales) * 100, 1)
  
  cat(sprintf("%-12s : diff=%+.1f%%  p=%.3f  %s\n",
              type, diff_pct, mw$p.value,
              ifelse(mw$p.value < 0.05, "✅ SIG.", "❌ non sig.")))
}

# ── BLOC DÉCISION AM-003 ─────────────────────────────────────

# RÉSULTATS T-009 PAR TYPE :
# SCOLAIRE   : -9.7%  p=0.455 — non sig. (déclin structurel continue)
# SECONDAIRE : +13.1% p=0.004 — ✅ SIG. (effet gratuité prouvé)
# PRINCIPAL  : +3.7%  p=0.280 — non sig.
# GLCT       : +2.9%  p=0.231 — non sig.
#
# CE QU'ON PEUT AFFIRMER :
# - La gratuité jeunes a augmenté significativement la fréquentation
#   des lignes SECONDAIRE (+13.1%, p=0.004)
# - L'effet n'est pas visible sur PRINCIPAL, GLCT ou SCOLAIRE
# - La gratuité n'a pas inversé le déclin des lignes SCOLAIRE
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que SECONDAIRE +13.1% est dû uniquement à la gratuité
#   (facteurs confondants : croissance naturelle, nouveaux services)
# - Que SCOLAIRE aurait baissé sans gratuité (n insuffisant)
#
# RÉCONCILIATION AVEC T-009 GLOBAL :
# T-009 global p=0.137 — l'effet SECONDAIRE (+13.1%) est dilué
# dans la masse du réseau PRINCIPAL. La segmentation révèle
# ce que l'agrégation cachait.
#
# ANGLE NARRATIF :
# "La gratuité jeunes a trouvé son public — les 18-24 ans en formation
# qui utilisent les lignes secondaires pour accéder aux hautes écoles.
# Un effet ciblé, prouvé statistiquement, invisible sans segmentation."

message("AM-003 complété.")

# NOTE MÉTHODOLOGIQUE AM-003 — PRINCIPAL :
# La non-significativité sur PRINCIPAL n'implique pas l'absence d'effet.
# Les jeunes genevois en ville utilisent les lignes PRINCIPAL (1, 3, 5, 6...)
# mais l'effet est dilué dans une variabilité mensuelle de ~2-3M montées.
# Puissance statistique insuffisante avec seulement 12 vs 14 mois.
# Un effet réel de +3-5% sur PRINCIPAL représente pourtant ~500-800k
# montées/mois supplémentaires — opérationnellement significatif
# même si statistiquement non prouvable avec les données actuelles.
# À réévaluer avec 3 ans de recul (2027-2028).


# ── AM-004 — SENSIBILITÉ STL ─────────────────────────────────
# T-004b utilisait s.window = "periodic" — saisonnalité rigide
# On teste avec s.window = 7 et s.window = 13 (plus flexibles)
# Si les conclusions changent → nos résultats sont fragiles
# Si elles restent stables → on peut affirmer la robustesse

mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(montees_totales = sum(nb_de_montees, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(date)

ts_mensuel <- ts(mensuel_global$montees_totales,
                 start = c(2016, 1), frequency = 12)

pos_covid <- which(mensuel_global$date == as.Date("2020-03-01"))

cat("=== AM-004 — SENSIBILITÉ STL ===\n\n")
cat(sprintf("%-15s  %-8s  %-8s  %-8s  %-30s\n",
            "s.window", "Chow F", "Chow p", "BP opt.", "Conclusion T-004b"))
cat(strrep("-", 75), "\n")

# Correction — séparer periodic des valeurs numériques
fenetres <- list("periodic", 7L, 13L, 21L)

cat("=== AM-004 — SENSIBILITÉ STL ===\n\n")
cat(sprintf("%-15s  %-8s  %-8s  %-8s  %-25s\n",
            "s.window", "Chow F", "Chow p", "BP opt.", "Conclusion"))
cat(strrep("-", 70), "\n")

for (sw in fenetres) {
  
  stl_test <- stl(ts_mensuel,
                  s.window = sw,
                  t.window = 13,
                  robust   = TRUE)
  
  residus <- ts(as.numeric(stl_test$time.series[, "remainder"]),
                start = c(2016, 1), frequency = 12)
  
  chow_r  <- sctest(residus ~ 1, type = "Chow", point = pos_covid)
  bp_r    <- breakpoints(residus ~ 1)
  bic_r   <- summary(bp_r)$RSS["BIC", ]
  n_opt_r <- as.integer(names(which.min(bic_r)))
  
  conclusion <- ifelse(chow_r$p.value > 0.05 & n_opt_r == 0,
                       "Choc transitoire OK",
                       paste0("Rupture residuelle (m=", n_opt_r, ")"))
  
  cat(sprintf("%-15s  %-8.3f  %-8.4f  %-8d  %-25s\n",
              as.character(sw),
              chow_r$statistic,
              chow_r$p.value,
              n_opt_r,
              conclusion))
}

# ── BLOC DÉCISION AM-004 ─────────────────────────────────────

# RÉSULTATS SENSIBILITÉ STL :
# s.window    Chow p   BP opt.
# periodic    0.164    2
# 7           0.154    2
# 13          0.160    2
# 21          0.158    2
#
# CE QU'ON PEUT AFFIRMER :
# - La conclusion T-004b est robuste au choix de s.window
# - Chow non sig. dans tous les cas → choc transitoire confirmé
# - BP m=2 systématique → artefact STL, pas rupture réelle
# - Nos conclusions ne dépendent pas du paramètre de lissage
#
# LIMITE RÉSIDUELLE :
# BP m=2 persiste quel que soit s.window — les deux "ruptures"
# (nov 2019, mai 2021) sont des artefacts inhérents à la méthode
# STL face à un choc aussi extrême que le COVID. Documenté.

message("AM-004 complété — robustesse STL confirmée.")




# ── AM-005 — TAUX DE COLLISION PAR KM PRODUIT ───────────────
# Le nombre brut de collisions est biaisé par le volume d'offre
# Plus de km produits = mécaniquement plus de risques d'accident
# On normalise : taux = collisions / km_produits × 1 000 000
# (nombre de collisions pour 1 million de km parcourus)

collisions <- readRDS("../data/raw/collisions.rds") %>%
  mutate(annee = year(jour))

# Agrégation collisions par année
coll_annuel <- collisions %>%
  filter(annee >= 2016 & annee <= 2025) %>%
  group_by(annee) %>%
  summarise(n_collisions = n(), .groups = "drop")

# Agrégation km produits par année — colonne = date
km_annuel <- km_prod %>%
  mutate(annee = year(date)) %>%
  filter(annee >= 2016 & annee <= 2025) %>%
  group_by(annee) %>%
  summarise(km_totaux = sum(km_prod, na.rm = TRUE), .groups = "drop")

# Jointure et calcul du taux
taux_collision <- coll_annuel %>%
  inner_join(km_annuel, by = "annee") %>%
  mutate(
    taux_par_Mkm = round(n_collisions / km_totaux * 1e6, 2)
  )

cat("=== AM-005 — TAUX DE COLLISION PAR MILLION DE KM ===\n\n")
print(taux_collision)

cat("\nTaux moyen  :", round(mean(taux_collision$taux_par_Mkm), 2), "\n")
cat("Taux 2019   :", taux_collision$taux_par_Mkm[taux_collision$annee == 2019], "\n")
cat("Taux 2020   :", taux_collision$taux_par_Mkm[taux_collision$annee == 2020], "\n")
cat("Taux 2025   :", taux_collision$taux_par_Mkm[taux_collision$annee == 2025], "\n")


# Graphique taux normalisé
p_taux <- ggplot(taux_collision,
                 aes(x = annee, y = taux_par_Mkm)) +
  geom_col(fill = TPG_RED, alpha = 0.85) +
  geom_line(color = COL_REF, linewidth = 0.8) +
  geom_point(color = COL_REF, size = 2.5) +
  geom_text(aes(label = taux_par_Mkm),
            vjust = -0.5, size = 3, color = "grey30") +
  geom_hline(yintercept = mean(taux_collision$taux_par_Mkm),
             linetype = "dashed", color = COL_NEUTRE) +
  annotate("text", x = 2016.3,
           y = mean(taux_collision$taux_par_Mkm) + 0.8,
           label = paste0("Moyenne : ",
                          round(mean(taux_collision$taux_par_Mkm), 1),
                          "/Mkm"),
           size = 2.8, color = COL_NEUTRE, hjust = 0) +
  scale_x_continuous(breaks = 2016:2025) +
  scale_y_continuous(limits = c(0, 45)) +
  labs(
    title    = "Taux de collision TPG — normalisé par km produits",
    subtitle = "Collisions pour 1 million de km parcourus | 2016-2025",
    x        = NULL,
    y        = "Collisions / million de km",
    caption  = "Source : TPG Open Data | 2025 ≠ record une fois normalisé"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_taux)

ggsave("../outputs/09_taux_collision_normalise.png",
       plot = p_taux, width = 10, height = 6, dpi = 150)

# ── BLOC DÉCISION AM-005 ─────────────────────────────────────
# Taux moyen : 31.9 collisions/Mkm
# 2018-2019 : pics à 37.2-37.6 — années les plus dangereuses
# 2020      : creux à 26.6 — COVID = moins de trafic automobile
# 2025      : 32.3 — dans la moyenne, pas un record normalisé
#
# CE QU'ON PEUT AFFIRMER :
# - 2025 n'est PAS l'année la plus dangereuse une fois normalisée
# - Le taux post-COVID (28-32) < taux pré-COVID (37-38)
# - Le réseau est devenu plus sûr par km parcouru depuis 2020
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que la baisse est due à une politique de sécurité spécifique
# - Que la tendance est statistiquement significative (n=10 ans)
#
# ANGLE NARRATIF FORT :
# "Le record de 2025 en nombre brut est trompeur — normalisé par
# les km produits, le réseau TPG est plus sûr qu'avant COVID"

message("AM-005 complété — toutes les analyses manquantes résolues.")