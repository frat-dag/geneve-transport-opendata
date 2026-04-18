# ============================================================
# SCRIPT 02 — FRÉQUENTATION JOURNALIÈRE
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Explorer la fréquentation journalière par arrêt
# et par ligne. Identifier les arrêts et lignes dominants.
# Ce qu'on sait déjà (script 00) :
#   - Période : avr 2023 → avr 2026 (1.75M lignes)
#   - Filtre donnees_definitives == TRUE obligatoire
#   - ligne en numeric → convertir en character
#   - 2 326 NA sur ligne (0.1%) → exclure analyses par ligne
#   - nb_de_montees = décimaux (correction qualité TPG)
# ============================================================

# ── 1. PACKAGES ─────────────────────────────────────────────

library(dplyr)
library(ggplot2)
library(scales)    # formatage des axes (millions, milliers)

# ── 2. CHARGEMENT ───────────────────────────────────────────

journalier <- readRDS("../data/raw/journalier.rds")

cat("Dimensions brutes :", nrow(journalier), "lignes x",
    ncol(journalier), "colonnes\n")

# ── 3. FILTRE QUALITÉ ────────────────────────────────────────
# Règle DEC-002 : donnees_definitives == TRUE systématique
# On documente combien on perd avant de filtrer

n_avant <- nrow(journalier)

journalier <- journalier %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne))  # DEC-001 : ligne en character

n_apres <- nrow(journalier)

cat("Après filtre qualité :", n_apres, "lignes\n")
cat("Lignes provisoires exclues :", n_avant - n_apres,
    "(", round((n_avant - n_apres) / n_avant * 100, 1), "%)\n")

# ── 4. VÉRIFICATION PÉRIODE APRÈS FILTRE ────────────────────
# On vérifie que le filtre n'a pas tronqué une période entière

cat("Période couverte après filtre :\n")
cat("  Du :", format(min(journalier$date)), "\n")
cat("  Au :", format(max(journalier$date)), "\n")

# Combien de dates distinctes ?
cat("  Dates distinctes :", n_distinct(journalier$date), "\n")

# Combien d'arrêts et de lignes distincts ?
cat("  Arrêts distincts :", n_distinct(journalier$arret), "\n")
cat("  Lignes distinctes :", n_distinct(journalier$ligne), "\n")
cat("  Types de ligne :", n_distinct(journalier$ligne_type_act), "\n")
print(sort(table(journalier$ligne_type_act), decreasing = TRUE))

# ── 5. FRÉQUENTATION TOTALE PAR TYPE DE LIGNE ───────────────
# Objectif : comprendre la structure du réseau avant d'aller
# au niveau arrêt ou ligne individuelle
# On agrège les montées totales par type — pas par date

freq_par_type <- journalier %>%
  group_by(ligne_type_act) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    n_lignes        = n_distinct(ligne),
    n_arrets        = n_distinct(arret)
  ) %>%
  mutate(
    pct_montees = round(montees_totales / sum(montees_totales) * 100, 1),
    montees_par_ligne = round(montees_totales / n_lignes)
  ) %>%
  arrange(desc(montees_totales))

cat("=== FRÉQUENTATION PAR TYPE DE LIGNE ===\n\n")
print(freq_par_type)

# ── 6. TOP 10 ARRÊTS — MONTÉES TOTALES ──────────────────────
# On agrège sur toute la période disponible
# Filtre : exclure les NA sur arret (sécurité)

top_arrets <- journalier %>%
  filter(!is.na(arret)) %>%
  group_by(arret) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    n_lignes        = n_distinct(ligne)
  ) %>%
  arrange(desc(montees_totales)) %>%
  slice_head(n = 10) %>%
  mutate(
    rang            = row_number(),
    pct_du_total    = round(montees_totales / sum(journalier$nb_de_montees,
                                                  na.rm = TRUE) * 100, 1)
  )

cat("=== TOP 10 ARRÊTS ===\n\n")
print(top_arrets)

# TOP 10 LIGNES
top_lignes <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne, ligne_type_act) %>%
  summarise(
    montees_totales = sum(nb_de_montees, na.rm = TRUE),
    n_arrets        = n_distinct(arret),
    .groups         = "drop"
  ) %>%
  arrange(desc(montees_totales)) %>%
  slice_head(n = 10) %>%
  mutate(
    rang         = row_number(),
    pct_du_total = round(montees_totales / sum(journalier$nb_de_montees,
                                               na.rm = TRUE) * 100, 1)
  )

cat("\n=== TOP 10 LIGNES ===\n\n")
print(top_lignes)

# ── 7. BLOC DÉCISION — SCRIPT 02 ────────────────────────────

# DÉCISION 1 — Structure du réseau : concentration massive sur PRINCIPAL
# 23 lignes PRINCIPAL = 85% des montées
# 56 lignes SECONDAIRE = 9.8% des montées
# Le réseau est structurellement asymétrique — à documenter dans toute
# communication publique sur l'efficacité des lignes

# DÉCISION 2 — NOCTAMBUS REGIONAL = système en extinction
# 0.0% des montées sur notre période (avr 2023 → fév 2026)
# Explication : absorbé dans les lignes diurnes depuis déc 2023 (OBS-021)
# Ne pas interpréter comme une désaffection du service nocturne
# Les montées nocturnes sont désormais comptabilisées en PRINCIPAL/SECONDAIRE

# DÉCISION 3 — Top 10 arrêts = 29.1% du trafic total
# Top 10 lignes = 61% du trafic total
# Ces chiffres posent la question de la concentration (T-007 — Gini)
# À ne pas publier sans la courbe de Lorenz complète

# DÉCISION 4 — Cornavin (31 lignes), Bel-Air (29), Rive (28)
# Ces arrêts sont des nœuds de correspondance, pas seulement des arrêts chargés
# Leur fréquentation reflète les transferts entre lignes autant que les montées nettes
# Limite : nb_de_montees ne distingue pas "montée directe" vs "montée après correspondance"

# ── 8. VISUALISATION — BARPLOT TOP 10 ARRÊTS ────────────────
# NOTE VIZ : ce graphique mérite une version finale en Python/Power BI

p_arrets <- ggplot(top_arrets,
                   aes(x = reorder(arret, montees_totales),
                       y = montees_totales / 1e6)) +
  geom_col(fill = "#E30613", alpha = 0.85) +
  geom_text(aes(label = paste0(pct_du_total, "%")),
            hjust = -0.1, size = 3.5, color = "grey30") +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(suffix = "M"),
    limits = c(0, 32)
  ) +
  labs(
    title    = "Top 10 arrêts TPG par fréquentation",
    subtitle = "Montées totales — avr 2023 à fév 2026 (données définitives)",
    x        = NULL,
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data | % = part du trafic total réseau"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_arrets)

p_lignes <- ggplot(top_lignes,
                   aes(x = reorder(ligne, montees_totales),
                       y = montees_totales / 1e6)) +
  geom_col(fill = "#E30613", alpha = 0.85) +
  geom_text(aes(label = paste0(pct_du_total, "%")),
            hjust = -0.1, size = 3.5, color = "grey30") +
  coord_flip() +
  scale_y_continuous(
    labels = label_number(suffix = "M"),
    limits = c(0, 46)
  ) +
  labs(
    title    = "Top 10 lignes TPG par fréquentation",
    subtitle = "Montées totales — avr 2023 à fév 2026 (données définitives)",
    x        = "Ligne",
    y        = "Millions de montées",
    caption  = "Source : TPG Open Data | % = part du trafic total réseau\nToutes les lignes sont de type PRINCIPAL"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

print(p_lignes)

ggsave("../outputs/02_top10_lignes.png",
       plot = p_lignes, width = 10, height = 6, dpi = 150)