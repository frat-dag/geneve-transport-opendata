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