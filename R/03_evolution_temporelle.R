# ============================================================
# SCRIPT 03 — ÉVOLUTION TEMPORELLE DE LA FRÉQUENTATION
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Analyser l'évolution de la fréquentation sur
# 10 ans (jan 2016 → mar 2026) via le dataset mensuel.
# Ce qu'on sait déjà (script 00) :
#   - Dataset mensuel : 493 756 lignes, jan 2016 → mar 2026
#   - Colonne mois : character ("2016-01") → à convertir en Date
#   - Filtre donnees_definitives == TRUE obligatoire
#   - 5 659 NA sur ligne (1.1%) → quantifier avant d'exclure
#   - Trois ruptures à annoter : Léman Express (déc 2019),
#     COVID (mar 2020), gratuité jeunes (jan 2025)
# ============================================================

# ── 1. NETTOYAGE ET PACKAGES ─────────────────────────────────

rm(list = ls())
gc()

setwd("D:/Frat/Documents/IA/Claude/Projet TPG/tpg-opendata-analysis/R")

library(dplyr)
library(ggplot2)
library(lubridate)   # conversion dates
library(scales)      # formatage axes

# ── 2. CHARGEMENT ───────────────────────────────────────────

mensuel <- readRDS("../data/raw/mensuel.rds")

cat("Dimensions :", nrow(mensuel), "lignes x", ncol(mensuel), "colonnes\n")
cat("Période brute :", mensuel$mois[1], "→", mensuel$mois[nrow(mensuel)], "\n")
cat("Type colonne mois :", class(mensuel$mois), "\n")


# ── 3. FILTRE QUALITÉ ET CONVERSION DATE ────────────────────

n_avant <- nrow(mensuel)

mensuel <- mensuel %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(
    date    = ym(mois),          # "2016-01" → Date 2016-01-01
    ligne   = as.character(ligne) # DEC-001 : ligne en character
  )

n_apres <- nrow(mensuel)

cat("Lignes avant filtre :", n_avant, "\n")
cat("Lignes après filtre :", n_apres, "\n")
cat("Provisoires exclues :", n_avant - n_apres,
    "(", round((n_avant - n_apres) / n_avant * 100, 1), "%)\n")
cat("Période réelle      :", format(min(mensuel$date)), "→",
    format(max(mensuel$date)), "\n")
cat("Type colonne date   :", class(mensuel$date), "\n")

# ── 4. QUANTIFICATION DES NA SUR LIGNE ──────────────────────
# TODO-003 : avant d'exclure, mesurer ce qu'on perd
# Règle : ne jamais exclure sans avoir d'abord quantifié

na_ligne <- mensuel %>% filter(is.na(ligne))
ok_ligne <- mensuel %>% filter(!is.na(ligne))

cat("--- NA sur ligne ---\n")
cat("Enregistrements NA  :", nrow(na_ligne), "\n")
cat("Montées associées   :", round(sum(na_ligne$nb_de_montees, na.rm = TRUE)), "\n")
cat("Total montées réseau:", round(sum(mensuel$nb_de_montees, na.rm = TRUE)), "\n")
cat("Part des montées NA :", round(
  sum(na_ligne$nb_de_montees, na.rm = TRUE) /
    sum(mensuel$nb_de_montees, na.rm = TRUE) * 100, 3), "%\n")

# DÉCISION — NA sur ligne dans mensuel
# 5 659 enregistrements NA = 0.002% des montées totales
# Exclusion justifiée et sans impact sur les totaux
# On filtre définitivement ici — tous les calculs suivants
# travaillent sur des données sans NA sur ligne

mensuel <- mensuel %>% filter(!is.na(ligne))
rm(na_ligne, ok_ligne)  # plus besoin
gc()

cat("Dataset final :", nrow(mensuel), "lignes\n")

# ── 5. AGRÉGATION MENSUELLE GLOBALE ─────────────────────────
# Objectif : calculer les montées totales par mois
# sur l'ensemble du réseau — c'est la série temporelle principale

mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

cat("Nombre de mois :", nrow(mensuel_global), "\n")
cat("Premier mois   :", format(min(mensuel_global$date)), "\n")
cat("Dernier mois   :", format(max(mensuel_global$date)), "\n")
cat("Min montées    :", round(min(mensuel_global$montees_totales) / 1e6, 2), "M\n")
cat("Max montées    :", round(max(mensuel_global$montees_totales) / 1e6, 2), "M\n")

# ── 6. IDENTIFICATION DES POINTS EXTRÊMES ───────────────────
# Ces chiffres seront les ancres narratives du graphique

# Le plancher
plancher <- mensuel_global %>%
  filter(montees_totales == min(montees_totales))

# Le pic
pic <- mensuel_global %>%
  filter(montees_totales == max(montees_totales))

# Les 3 derniers mois disponibles
derniers <- mensuel_global %>% tail(3)

cat("--- PLANCHER ---\n")
cat("Mois    :", format(plancher$date, "%B %Y"), "\n")
cat("Montées :", round(plancher$montees_totales / 1e6, 2), "M\n")

cat("\n--- PIC ---\n")
cat("Mois    :", format(pic$date, "%B %Y"), "\n")
cat("Montées :", round(pic$montees_totales / 1e6, 2), "M\n")

cat("\n--- 3 DERNIERS MOIS ---\n")
print(derniers %>% mutate(
  mois    = format(date, "%B %Y"),
  montees = round(montees_totales / 1e6, 2)
) %>% dplyr::select(mois, montees))

# ── 7. GRAPHIQUE — ÉVOLUTION TEMPORELLE ─────────────────────
# NOTE VIZ : version finale en Python/Power BI

# Définition des dates clés pour les annotations
dates_cles <- data.frame(
  date  = as.Date(c("2019-12-01", "2020-03-01", "2025-01-01")),
  label = c("Léman Express\ndéc. 2019",
            "COVID\nmar. 2020",
            "Gratuité jeunes\njan. 2025"),
  couleur = c("#2196F3", "#E30613", "#4CAF50")
)

p_evolution <- ggplot(mensuel_global,
                      aes(x = date, y = montees_totales / 1e6)) +
  geom_line(color = "#E30613", linewidth = 0.8) +
  geom_point(data = filter(mensuel_global,
                           montees_totales == min(montees_totales) |
                             montees_totales == max(montees_totales)),
             color = "#E30613", size = 3) +
  
  geom_vline(data = dates_cles,
             aes(xintercept = date),
             linetype = "dashed", color = dates_cles$couleur,
             linewidth = 0.6) +
  
  # Léman Express — plus bas, aligné à gauche
  annotate("text", x = as.Date("2019-12-01"), y = 13,
           label = "Léman Express\ndéc. 2019",
           hjust = 1.05, size = 2.8, color = "#2196F3") +
  
  # COVID — position actuelle ok
  annotate("text", x = as.Date("2020-03-01"), y = 19,
           label = "COVID\nmar. 2020",
           hjust = -0.05, size = 2.8, color = "#E30613") +
  
  # Gratuité jeunes — plus bas
  annotate("text", x = as.Date("2025-01-01"), y = 14,
           label = "Gratuité jeunes\njan. 2025",
           hjust = -0.05, size = 2.8, color = "#4CAF50") +
  
  # Plancher — plus à droite
  annotate("text", x = as.Date("2020-08-01"), y = 5.5,
           label = "3.48M\n(plancher)", size = 2.8,
           color = "grey40", hjust = 0) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "M"),
                     limits = c(0, 22)) +
  labs(
    title    = "Évolution de la fréquentation TPG — 2016 à 2026",
    subtitle = "Montées mensuelles totales — données définitives",
    x        = NULL,
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

print(p_evolution)

ggsave("../outputs/03_evolution_temporelle.png",
       plot = p_evolution, width = 12, height = 6, dpi = 150)

message("Graphique sauvegardé.")

# ── 8. AGRÉGATION PAR TYPE DE LIGNE ─────────────────────────
# Objectif : voir si tous les types de lignes ont récupéré
# de la même façon après COVID
# On exclut NOCTAMBUS REGIONAL — système en extinction
# depuis déc 2023 (OBS-021), non comparable sur 10 ans

mensuel_type <- mensuel %>%
  filter(ligne_type_act != "NOCTAMBUS REGIONAL") %>%
  group_by(date, ligne_type_act) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(date)

# Combien de types et combien de mois par type ?
cat("Types de ligne retenus :\n")
print(table(mensuel_type$ligne_type_act))

# Quelles périodes couvrent REGIONAL et REGIONAL COMMUNE ?
cat("--- REGIONAL ---\n")
mensuel_type %>%
  filter(ligne_type_act == "REGIONAL") %>%
  summarise(debut = min(date), fin = max(date)) %>%
  print()

cat("\n--- REGIONAL COMMUNE ---\n")
mensuel_type %>%
  filter(ligne_type_act == "REGIONAL COMMUNE") %>%
  summarise(debut = min(date), fin = max(date)) %>%
  print()

# Quel volume représentent-ils ?
cat("\n--- VOLUME PAR TYPE ---\n")
mensuel_type %>%
  group_by(ligne_type_act) %>%
  summarise(
    montees_totales = sum(montees_totales),
    pct = round(montees_totales / sum(mensuel$nb_de_montees,
                                      na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(montees_totales)) %>%
  print()


# ── 9. REGROUPEMENT ET GRAPHIQUE PAR TYPE DE LIGNE ──────────
# REGIONAL + REGIONAL COMMUNE → SECONDAIRE (requalification
# suite à la réorganisation Léman Express déc 2019)
# Documenté dans le journal analytique (OBS à ajouter)

mensuel_type <- mensuel_type %>%
  mutate(ligne_type_act = case_when(
    ligne_type_act %in% c("REGIONAL", "REGIONAL COMMUNE") ~ "SECONDAIRE",
    TRUE ~ ligne_type_act
  )) %>%
  group_by(date, ligne_type_act) %>%
  summarise(
    montees_totales = sum(montees_totales, na.rm = TRUE),
    .groups = "drop"
  )

# Vérification
cat("Types après regroupement :\n")
print(table(mensuel_type$ligne_type_act))

# Couleurs par type
couleurs_type <- c(
  "PRINCIPAL"  = "#E30613",
  "SECONDAIRE" = "#4A6FA5",
  "GLCT"       = "#FF9800",
  "SCOLAIRE"   = "#4CAF50"
)

p_type <- ggplot(mensuel_type,
                 aes(x = date,
                     y = montees_totales / 1e6,
                     color = ligne_type_act)) +
  geom_line(linewidth = 0.8) +
  
  # Rupture COVID
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = "grey50", linewidth = 0.5) +
  annotate("text", x = as.Date("2020-03-01"), y = 19,
           label = "COVID", hjust = -0.1,
           size = 2.8, color = "grey40") +
  
  # Rupture Léman Express
  geom_vline(xintercept = as.Date("2019-12-01"),
             linetype = "dashed", color = "#2196F3", linewidth = 0.5) +
  annotate("text", x = as.Date("2019-12-01"), y = 17,
           label = "Léman Express", hjust = 1.05,
           size = 2.8, color = "#2196F3") +
  
  # Rupture gratuité
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dashed", color = "#4CAF50", linewidth = 0.5) +
  annotate("text", x = as.Date("2025-01-01"), y = 17,
           label = "Gratuité jeunes", hjust = -0.05,
           size = 2.8, color = "#4CAF50") +
  
  scale_color_manual(values = couleurs_type, name = "Type de ligne") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  labs(
    title    = "Évolution par type de ligne — 2016 à 2026",
    subtitle = "Montées mensuelles — REGIONAL et REGIONAL COMMUNE regroupés dans SECONDAIRE (réorg. Léman Express déc. 2019)",
    x        = NULL,
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "bottom"
  )

print(p_type)


# ── 10. GRAPHIQUE FACET PAR TYPE DE LIGNE ───────────────────
# facet_wrap avec scales = "free_y" — chaque type a sa propre
# échelle. On compare les dynamiques, pas les volumes absolus.
# NOTE : mentionner dans le titre que les échelles diffèrent

p_type_facet <- ggplot(mensuel_type,
                       aes(x = date, y = montees_totales / 1e6)) +
  geom_line(aes(color = ligne_type_act), linewidth = 0.7) +
  
  # Ruptures — sur tous les panneaux
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2019-12-01"),
             linetype = "dashed", color = "#2196F3", linewidth = 0.4) +
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dashed", color = "#4CAF50", linewidth = 0.4) +
  
  scale_color_manual(values = couleurs_type, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "M")) +
  
  facet_wrap(~ ligne_type_act, scales = "free_y", ncol = 2) +
  
  labs(
    title    = "Évolution par type de ligne — 2016 à 2026",
    subtitle = "Échelles Y indépendantes — comparer les dynamiques, pas les volumes\nLignes verticales : Léman Express (bleu), COVID (gris), Gratuité jeunes (vert)",
    x        = NULL,
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data | REGIONAL et REGIONAL COMMUNE regroupés dans SECONDAIRE"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    strip.text       = element_text(face = "bold", size = 11)
  )

print(p_type_facet)

ggsave("../outputs/03_evolution_par_type.png",
       plot = p_type_facet, width = 12, height = 8, dpi = 150)

message("Graphique sauvegardé.")


# ── 11. AGRÉGATION ANNUELLE — RÉCUPÉRATION POST-COVID ────────
# Objectif : quantifier la récupération par type de ligne
# Référence : 2019 = année pré-COVID de référence
# On exclut 2026 — année incomplète (jan-fév seulement)

mensuel_annuel <- mensuel %>%
  mutate(annee = year(date)) %>%
  filter(annee < 2026) %>%
  mutate(ligne_type_act = case_when(
    ligne_type_act %in% c("REGIONAL", "REGIONAL COMMUNE") ~ "SECONDAIRE",
    TRUE ~ ligne_type_act
  )) %>%
  filter(ligne_type_act != "NOCTAMBUS REGIONAL") %>%
  group_by(annee, ligne_type_act) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  )

# Référence 2019 par type
ref_2019 <- mensuel_annuel %>%
  filter(annee == 2019) %>%
  dplyr::select(ligne_type_act, ref_2019 = montees_totales)

# Taux de récupération par type et par année
recuperation <- mensuel_annuel %>%
  left_join(ref_2019, by = "ligne_type_act") %>%
  mutate(pct_vs_2019 = round(montees_totales / ref_2019 * 100, 1))

# Afficher 2019, 2020, 2022, 2023, 2024, 2025
cat("=== RÉCUPÉRATION PAR TYPE vs 2019 ===\n\n")
recuperation %>%
  filter(annee %in% c(2019, 2020, 2022, 2023, 2024, 2025)) %>%
  dplyr::select(annee, ligne_type_act, pct_vs_2019) %>%
  tidyr::pivot_wider(names_from = annee,
                     values_from = pct_vs_2019) %>%
  print()


# ── 12. BLOC DÉCISION — RÉCUPÉRATION PAR TYPE ───────────────

# DÉCISION 1 — GLCT 119% : biais de périmètre confirmé (SYN-003)
# Ne jamais citer ce chiffre sans mentionner l'appel d'offres 2023

# DÉCISION 2 — PRINCIPAL 101% : récupération complète en 2025
# Cohérent avec le chiffre global documenté (100.7%)

# DÉCISION 3 — SECONDAIRE 112% : croissance réelle post-2025
# Hypothèse : effet gratuité jeunes → à tester T-009

# DÉCISION 4 — SCOLAIRE : effondrement structurel post-COVID
# 59.7% en 2022 < 78.2% en 2020 → la baisse s'accélère après COVID
# Pas une non-récupération — un déclin structurel sur 10 ans
# Légère remontée 2024-2025 → lien possible gratuité jeunes
# À tester : Chow test sur série SCOLAIRE seule (2016-2019 vs 2020-2025)

# NOTE VIZ : tableau récupération → graphique barres grouped
# fort pour publication LinkedIn et présentation élus


# ── 13. GRAPHIQUE — TAUX DE RÉCUPÉRATION PAR TYPE ───────────
# On visualise les années clés vs 2019
# NOTE VIZ : graphique fort pour publication

recuperation_viz <- recuperation %>%
  filter(annee %in% c(2020, 2022, 2023, 2024, 2025)) %>%
  mutate(
    annee = as.factor(annee),
    ligne_type_act = factor(ligne_type_act,
                            levels = c("PRINCIPAL", "SECONDAIRE",
                                       "GLCT", "SCOLAIRE"))
  )

p_recuperation <- ggplot(recuperation_viz,
                         aes(x = ligne_type_act,
                             y = pct_vs_2019,
                             fill = annee)) +
  geom_col(position = "dodge", alpha = 0.85) +
  
  # Ligne de référence 2019 = 100%
  geom_hline(yintercept = 100, linetype = "dashed",
             color = "grey30", linewidth = 0.6) +
  annotate("text", x = 0.4, y = 101.5,
           label = "Niveau 2019", hjust = 0,
           size = 2.8, color = "grey30") +
  
  scale_fill_manual(
    values = c("2020" = "#E30613",
               "2022" = "#FF9800",
               "2023" = "#FFC107",
               "2024" = "#8BC34A",
               "2025" = "#2E7D32"),
    name = "Année"
  ) +
  scale_y_continuous(labels = label_number(suffix = "%"),
                     limits = c(0, 130)) +
  labs(
    title    = "Récupération post-COVID par type de ligne",
    subtitle = "Montées annuelles en % du niveau 2019 (référence = 100%)",
    x        = NULL,
    y        = "% vs 2019",
    caption  = "Source : TPG Open Data | GLCT : biais de périmètre (appel d'offres 2023)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position  = "bottom"
  )

print(p_recuperation)


# Charger la palette officielle
source("00_palette.R")

# ── 13b. GRAPHIQUE RÉCUPÉRATION — PALETTE OFFICIELLE ────────

recuperation_viz <- recuperation %>%
  filter(annee %in% c(2020, 2022, 2023, 2024, 2025)) %>%
  mutate(
    annee = as.factor(annee),
    ligne_type_act = factor(ligne_type_act,
                            levels = c("PRINCIPAL", "SECONDAIRE",
                                       "GLCT", "SCOLAIRE"))
  )

p_recuperation <- ggplot(recuperation_viz,
                         aes(x = ligne_type_act,
                             y = pct_vs_2019,
                             fill = annee)) +
  geom_col(position = "dodge", alpha = 0.9) +
  
  geom_hline(yintercept = 100, linetype = "dashed",
             color = COL_REF, linewidth = 0.6) +
  annotate("text", x = 0.4, y = 101.5,
           label = "Niveau 2019", hjust = 0,
           size = 2.8, color = COL_NEUTRE) +
  
  scale_fill_manual(
    values = c(
      "2020" = "#F1948A",
      "2022" = "#C0392B",
      "2023" = "#E30613",
      "2024" = "#A93226",
      "2025" = "#7B241C"
    ),
    name = "Année"
  ) +
  scale_y_continuous(labels = label_number(suffix = "%"),
                     limits = c(0, 130)) +
  labs(
    title    = "Récupération post-COVID par type de ligne",
    subtitle = "Montées annuelles en % du niveau 2019 (référence = 100%)",
    x        = NULL,
    y        = "% vs 2019",
    caption  = "Source : TPG Open Data | GLCT : biais de périmètre (appel d'offres 2023)"
  ) +
  theme_tpg()

print(p_recuperation)

# Sauvegarder le graphique
ggsave("../outputs/03_recuperation_par_type.png",
       plot = p_recuperation, width = 12, height = 7, dpi = 150)

message("Graphique sauvegardé.")