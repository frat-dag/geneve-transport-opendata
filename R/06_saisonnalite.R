# ============================================================
# SCRIPT 06 — SAISONNALITÉ
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Décomposer la série temporelle mensuelle en
# tendance + saisonnalité + résidus (STL).
# Identifier les effets vacances, été, Noël sur 10 ans.
# Les résidus STL serviront de base pour T-004b (script 07).
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
library(stats)     # stl() inclus dans base R

# ── 2. CHARGEMENT ET PRÉPARATION ────────────────────────────

mensuel <- readRDS("../data/raw/mensuel.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(ligne)) %>%
  mutate(
    date  = ym(mois),
    ligne = as.character(ligne)
  )

# Agrégation mensuelle globale — même que script 03
mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("Série mensuelle :", nrow(mensuel_global), "mois\n")
cat("De :", format(min(mensuel_global$date)),
    "à :", format(max(mensuel_global$date)), "\n")


# ── 3. DÉCOMPOSITION STL ────────────────────────────────────
# s.window = "periodic" : saisonnalité identique chaque année
# t.window = 13 : fenêtre de lissage de la tendance (13 mois)
# robust = TRUE : résistant aux outliers (ex: COVID)

ts_mensuel <- ts(mensuel_global$montees_totales,
                 start     = c(2016, 1),
                 frequency = 12)

stl_res <- stl(ts_mensuel,
               s.window = "periodic",
               t.window = 13,
               robust   = TRUE)

# Extraire les composantes
composantes <- data.frame(
  date        = mensuel_global$date,
  observee    = mensuel_global$montees_totales,
  tendance    = as.numeric(stl_res$time.series[, "trend"]),
  saisonnalite = as.numeric(stl_res$time.series[, "seasonal"]),
  residus     = as.numeric(stl_res$time.series[, "remainder"])
)

# Statistiques des résidus
cat("=== STATISTIQUES DES RÉSIDUS STL ===\n\n")
cat("Moyenne   :", round(mean(composantes$residus) / 1e6, 4), "M\n")
cat("Écart-type:", round(sd(composantes$residus) / 1e6, 3), "M\n")
cat("Min       :", round(min(composantes$residus) / 1e6, 3), "M\n")
cat("Max       :", round(max(composantes$residus) / 1e6, 3), "M\n")

# Quel mois a le résidu le plus négatif ?
cat("\nPlancher résiduel :\n")
composantes %>%
  filter(residus == min(residus)) %>%
  mutate(mois = format(date, "%B %Y")) %>%
  dplyr::select(mois, residus) %>%
  mutate(residus = round(residus / 1e6, 3)) %>%
  print()


# ── 4. GRAPHIQUE — DÉCOMPOSITION STL ────────────────────────
# 4 panneaux : observée, tendance, saisonnalité, résidus
# NOTE VIZ : graphique technique — version finale Python

# Dates clés pour annotations
ruptures <- data.frame(
  date    = as.Date(c("2019-12-01", "2020-03-01", "2025-01-01")),
  label   = c("Léman Express", "COVID", "Gratuité jeunes"),
  couleur = c(COL_LEMAN, COL_COVID, COL_GRATUITE)
)

# Restructurer en format long pour facet
composantes_long <- composantes %>%
  tidyr::pivot_longer(
    cols      = c(observee, tendance, saisonnalite, residus),
    names_to  = "composante",
    values_to = "valeur"
  ) %>%
  mutate(
    composante = factor(composante,
                        levels = c("observee", "tendance",
                                   "saisonnalite", "residus"),
                        labels = c("Série observée",
                                   "Tendance",
                                   "Saisonnalité",
                                   "Résidus"))
  )

p_stl <- ggplot(composantes_long,
                aes(x = date, y = valeur / 1e6)) +
  geom_line(color = TPG_RED, linewidth = 0.7) +
  
  # Ligne zéro pour saisonnalité et résidus
  geom_hline(data = composantes_long %>%
               filter(composante %in% c("Saisonnalité", "Résidus")),
             aes(yintercept = 0),
             linetype = "dashed", color = COL_NEUTRE,
             linewidth = 0.4) +
  
  # Ruptures verticales
  geom_vline(xintercept = as.Date("2019-12-01"),
             linetype = "dashed", color = COL_LEMAN,
             linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = COL_COVID,
             linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dashed", color = COL_GRATUITE,
             linewidth = 0.4) +
  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  
  facet_wrap(~ composante, ncol = 1, scales = "free_y") +
  
  labs(
    title    = "Décomposition STL — Fréquentation TPG 2016-2026",
    subtitle = "s.window = periodic | t.window = 13 | robust = TRUE\nLignes : Léman Express (bleu), COVID (rouge), Gratuité jeunes (vert)",
    x        = NULL,
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data"
  ) +
  theme_tpg() +
  theme(
    strip.text       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_stl)

ggsave("../outputs/06_stl_decomposition.png",
       plot = p_stl, width = 12, height = 10, dpi = 150)

message("Graphique STL sauvegardé.")


# ── 5. ANALYSE DE LA SAISONNALITÉ ───────────────────────────
# Objectif : identifier les mois forts et faibles
# La saisonnalité STL est identique chaque année (s.window periodic)
# On extrait le profil saisonnier moyen sur 12 mois

saisonnalite_mois <- composantes %>%
  mutate(mois_num = month(date)) %>%
  group_by(mois_num) %>%
  summarise(
    saisonnalite_med = median(saisonnalite),
    .groups = "drop"
  ) %>%
  mutate(
    mois_label = factor(mois_num,
                        labels = c("Jan", "Fév", "Mar", "Avr",
                                   "Mai", "Jun", "Jul", "Aoû",
                                   "Sep", "Oct", "Nov", "Déc")),
    direction = ifelse(saisonnalite_med >= 0, "Surplus", "Déficit")
  )

cat("=== PROFIL SAISONNIER MENSUEL ===\n\n")
saisonnalite_mois %>%
  dplyr::select(mois_label, saisonnalite_med) %>%
  mutate(saisonnalite_med = round(saisonnalite_med / 1e3)) %>%
  print(n = 12)

# Graphique saisonnalité
p_saison <- ggplot(saisonnalite_mois,
                   aes(x = mois_label,
                       y = saisonnalite_med / 1e3,
                       fill = direction)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = 0, color = COL_REF, linewidth = 0.5) +
  scale_fill_manual(
    values = c("Surplus" = TPG_RED, "Déficit" = "#4A6FA5"),
    guide  = "none"
  ) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  labs(
    title    = "Profil saisonnier mensuel — Fréquentation TPG",
    subtitle = "Composante saisonnière STL — écart vs tendance long terme",
    x        = NULL,
    y        = "Milliers de montées (écart vs tendance)",
    caption  = "Source : TPG Open Data | Rouge = surplus, Bleu = déficit"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_saison)


# ── 6. BLOC DÉCISION — SAISONNALITÉ ─────────────────────────

# PROFIL SAISONNIER CONFIRMÉ :
# Mois forts   : Mar +1.55M, Nov +1.72M, Mai +1.24M, Oct +1.10M
# Mois faibles : Jul -2.60M, Aoû -2.31M, Fév -0.78M, Avr -0.72M
#
# CE QU'ON PEUT AFFIRMER :
# - La saisonnalité est stable sur 10 ans (s.window periodic justifié)
# - L'été (jul-aoû) = creux structurel dominant (-2.3 à -2.6M)
# - Les vacances scolaires (fév, avr) créent des creux secondaires
# - La saisonnalité n'a PAS changé après COVID — les habitudes
#   saisonnières des genevois sont restées stables
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que la saisonnalité est identique pour tous les types de lignes
#   → SCOLAIRE aura une saisonnalité très différente de PRINCIPAL
# - Que s.window = "periodic" est le meilleur paramètre — un s.window
#   flexible pourrait révéler des changements de saisonnalité post-COVID
#
# IMPLICATION OPÉRATIONNELLE (Q-002) :
# L'offre devrait être modulée selon ce profil — à croiser avec
# km_prod en Phase 3 pour mesurer l'écart offre/demande

# CORRECTION BLOC DÉCISION :
# Les horaires VACANCES TPG existent déjà — la modulation de l'offre
# est déjà en place. Ce profil saisonnier confirme que le système
# actuel est aligné avec les patterns de demande.
# Question ouverte (SYN-005) : l'amplitude de la réduction d'offre
# est-elle proportionnelle à la baisse de demande ?
# T-002 : demande -28% en VACANCES
# À comparer avec la baisse de km_prod en VACANCES — Phase 3
#
#
#
# RÉSIDUS STL → prêts pour T-004b en script 07
# saveRDS(composantes, "../data/processed/stl_composantes.rds")

saveRDS(composantes, "../data/processed/stl_composantes.rds")

ggsave("../outputs/06_saison_mensuelle.png",
       plot = p_saison, width = 10, height = 6, dpi = 150)

message("Script 06 terminé. Composantes STL sauvegardées.")