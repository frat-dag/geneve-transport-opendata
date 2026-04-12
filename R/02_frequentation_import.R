# =============================================================
# PROJET TPG OPEN DATA - Phase 1, Analyses 1.2 / 1.3 / 1.4
# Fichier  : 02_frequentation_import.R
# Objectif : Télécharger la fréquentation journalière par
#            arrêt et par ligne, produire les tops 10 et
#            analyser la distribution
# Source   : https://opendata.tpg.ch
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
library(httr2)
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)

# --- 2. Paramètres de l'API ----------------------------------

url_freq <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/montees-par-arret-par-ligne/exports/csv"

# --- 3. Téléchargement ---------------------------------------

message("Téléchargement de la fréquentation journalière...")

reponse <- request(url_freq) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

message("✓ Données reçues.")

# --- 4. Chargement dans R ------------------------------------

freq_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

# --- 5. Exploration initiale ---------------------------------

message("Dimensions : ", nrow(freq_raw), " lignes x ", ncol(freq_raw), " colonnes")
message("Colonnes disponibles :")
print(colnames(freq_raw))
print(head(freq_raw, 5))

# --- 6. Nettoyage de base ------------------------------------

# janitor::clean_names() : déjà propres ici, mais bonne pratique systématique
freq <- freq_raw |>
  clean_names() |>
  # On garde uniquement les données définitives pour les analyses
  filter(donnees_definitives == TRUE)

message("Lignes avec données définitives : ", nrow(freq))

# --- 7. Analyse 1.2 — Top 10 arrêts les plus fréquentés -----

# On agrège toutes les dates et lignes pour avoir le total par arrêt
top10_arrets <- freq |>
  group_by(arret) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_montees)) |>
  slice_head(n = 10)

print(top10_arrets)

# Visualisation
ggplot(top10_arrets,
       aes(x = reorder(arret, total_montees),
           y = total_montees / 1000)) +
  geom_col(fill = "#E30613") +
  coord_flip() +
  labs(
    title    = "Top 10 des arrêts TPG les plus fréquentés",
    subtitle = "Total des montées sur la période disponible",
    x        = NULL,
    y        = "Montées (en milliers)",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/02_top10_arrets.png", width = 10, height = 6, dpi = 150)
message("✓ Graphique sauvegardé : outputs/02_top10_arrets.png")

# --- 8. Analyse 1.3 — Top 10 lignes les plus fréquentées ----

top10_lignes <- freq |>
  group_by(ligne) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_montees)) |>
  slice_head(n = 10)

print(top10_lignes)

# Visualisation
ggplot(top10_lignes,
       aes(x = reorder(ligne, total_montees),
           y = total_montees / 1000)) +
  geom_col(fill = "#E30613") +
  coord_flip() +
  labs(
    title    = "Top 10 des lignes TPG les plus fréquentées",
    subtitle = "Total des montées sur la période disponible",
    x        = "Ligne",
    y        = "Montées (en milliers)",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/02_top10_lignes.png", width = 10, height = 6, dpi = 150)
message("✓ Graphique sauvegardé : outputs/02_top10_lignes.png")

# --- 9. Analyse 1.4 — Distribution de la fréquentation ------

# Fréquentation totale par arrêt (toutes lignes et dates confondues)
freq_par_arret <- freq |>
  group_by(arret) |>
  summarise(
    total_montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(total_montees))

# Quelques statistiques descriptives
cat("\n=== Distribution de la fréquentation par arrêt ===\n")
cat("Nombre d'arrêts analysés :", nrow(freq_par_arret), "\n")
cat("Moyenne montées/arrêt    :", round(mean(freq_par_arret$total_montees)), "\n")
cat("Médiane montées/arrêt    :", round(median(freq_par_arret$total_montees)), "\n")
cat("Arrêt max                :", freq_par_arret$arret[1], "-", round(freq_par_arret$total_montees[1]), "\n")
cat("Arrêt min                :", freq_par_arret$arret[nrow(freq_par_arret)], "-", round(freq_par_arret$total_montees[nrow(freq_par_arret)]), "\n")

# Histogramme de la distribution
ggplot(freq_par_arret, aes(x = total_montees / 1000)) +
  geom_histogram(fill = "#E30613", color = "white", bins = 50) +
  scale_x_log10() +   # Échelle logarithmique — indispensable car distribution très asymétrique
  labs(
    title    = "Distribution de la fréquentation des arrêts TPG",
    subtitle = "Échelle logarithmique — 3 ans de données (fév. 2023 – fév. 2026)",
    x        = "Total montées (en milliers, échelle log)",
    y        = "Nombre d'arrêts",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/02_distribution_frequentation.png", width = 10, height = 6, dpi = 150)
message("✓ Graphique sauvegardé : outputs/02_distribution_frequentation.png")

# Version grand public — top 30 arrêts en barplot horizontal
top30_arrets <- freq_par_arret |> slice_head(n = 30)

ggplot(top30_arrets,
       aes(x = reorder(arret, total_montees),
           y = total_montees / 1000000)) +
  geom_col(fill = "#E30613") +
  geom_text(
    aes(label = paste0(round(total_montees / 1000000, 1), "M")),
    hjust    = -0.1,
    size     = 3,
    color    = "grey30"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0, 70),
    labels = function(x) paste0(x, "M")
  ) +
  labs(
    title    = "Les 30 arrêts TPG les plus fréquentés",
    subtitle = "Total des montées sur 3 ans (fév. 2023 – fév. 2026)",
    x        = NULL,
    y        = "Total montées (en millions)",
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey50"),
    panel.grid.major.y = element_blank()  # Supprime grilles horizontales — plus épuré
  )

ggsave("outputs/02_top30_arrets_grand_public.png",
       width = 10, height = 10, dpi = 150)
message("✓ Graphique grand public sauvegardé")

# Part du trafic concentrée sur le top 10%
seuil_top10 <- quantile(freq_par_arret$total_montees, 0.90)
part_top10  <- freq_par_arret |>
  filter(total_montees >= seuil_top10) |>
  summarise(part = sum(total_montees) / sum(freq_par_arret$total_montees) * 100)

cat("\nLes 10% d'arrêts les plus fréquentés concentrent",
    round(part_top10$part, 1), "% du trafic total\n")