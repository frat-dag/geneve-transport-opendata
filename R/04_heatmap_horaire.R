# ============================================================
# SCRIPT 04 - HEATMAP HORAIRE ET TEST T-003
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-09, T-06, S-10, S-13
# ------------------------------------------------------------
# OBJECTIF : patterns de fréquentation heure x jour de semaine.
# T-003 : test Wilcoxon apparié pic soir (17h) vs pic matin (8h).
# SOURCE : dataset horaire (réseau global, par tranche horaire).
# NOTE horaire_tranche_stop_theo : character "00"-"23" + "-" (NA).
# NOTE S-13 : T-003 teste le réseau global. L'assertion "partout,
# tout le temps" n'est pas testée par ligne, limite documentée.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(viridis)
library(tidyr)

# ── 1. CHARGEMENT ───────────────────────────────────────────
# lire() applique donnees_definitives et date <= DATE_COUPURE.

horaire_raw <- readRDS(file.path(DIR_RAW, "horaire.rds"))
horaire     <- lire("horaire")

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Lignes brutes              :", nrow(horaire_raw), "\n")
cat("Lignes après lire()        :", nrow(horaire),
    "(définitives + coupure)\n")
cat("Période :", format(min(horaire$date)), "à",
    format(max(horaire$date)), "\n")
cat("Colonnes :", paste(names(horaire), collapse = ", "), "\n\n")
rm(horaire_raw)

# ── 2. FILTRE QUALITE ───────────────────────────────────────

n_avant <- nrow(horaire)

horaire <- horaire %>%
  # "-" encode les valeurs manquantes de horaire_tranche_stop_theo
  filter(horaire_tranche_stop_theo != "-") %>%
  filter(horaire_type %in% c("NORMAL", "SAMEDI", "DIMANCHE",
                              "VACANCES", "FERIE")) %>%
  mutate(
    heure = as.integer(horaire_tranche_stop_theo),
    # Pour SAMEDI/DIMANCHE, horaire_type prime sur le calendrier
    jour  = case_when(
      horaire_type == "SAMEDI"   ~ "Samedi",
      horaire_type == "DIMANCHE" ~ "Dimanche",
      TRUE ~ substr(jour_semaine, 3, nchar(jour_semaine))
    )
  ) %>%
  filter(!is.na(heure))

cat("Avant filtres supplémentaires :", n_avant, "\n")
cat("Après                         :", nrow(horaire),
    "| exclus :", n_avant - nrow(horaire),
    "(", round(100 * (n_avant - nrow(horaire)) / n_avant, 1), "% )\n")
cat("\nTypes horaire présents :\n")
print(table(horaire$horaire_type))
cat("\nJours présents :\n")
print(table(horaire$jour))

PERIODE_TXT <- paste(format(min(horaire$date), "%d.%m.%Y"),
                     "au", format(max(horaire$date), "%d.%m.%Y"))

# ── 3. AGRÉGATION HEURE x JOUR ──────────────────────────────
# Médiane : robuste aux jours atypiques (grèves, événements).
# Heures 5h-23h : la nuit profonde (0h-4h) est analysée séparément
# dans le script 14 (Noctambus).

ordre_jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi",
                 "Vendredi", "Samedi", "Dimanche")

heatmap_data <- horaire %>%
  filter(heure >= 5 & heure <= 23) %>%
  group_by(heure, jour) %>%
  summarise(
    mediane_montees = median(nb_de_montees, na.rm = TRUE),
    n_obs           = n(),
    .groups         = "drop"
  ) %>%
  mutate(jour = factor(jour, levels = ordre_jours))

cat("\nCombinaisons heure x jour :", nrow(heatmap_data),
    "(", n_distinct(heatmap_data$heure), "heures x",
    n_distinct(heatmap_data$jour), "jours )\n")

cat("\nTop 5 combinaisons :\n")
print(as.data.frame(heatmap_data %>%
  arrange(desc(mediane_montees)) %>% head(5)))

# ── 4. FIGURE 1 : HEATMAP ───────────────────────────────────

p_heatmap <- ggplot(heatmap_data,
                    aes(x = jour,
                        y = factor(heure, levels = rev(5:23)),
                        fill = mediane_montees)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(
    option = "magma",
    name   = "Médiane\nmontées",
    labels = label_number(suffix = "k", scale = 1e-3)
  ) +
  scale_y_discrete(labels = function(x) paste0(x, "h")) +
  labs(
    title    = "Heatmap de fréquentation, heure x jour",
    subtitle = paste0("Médiane des montées par tranche horaire, ",
                     PERIODE_TXT, ", données définitives"),
    x        = NULL, y = "Heure",
    caption  = SOURCE_TPG
  ) +
  theme_projet() +
  theme(
    axis.text.x      = element_text(angle = 0, hjust = 0.5),
    panel.grid.major = element_blank(),
    legend.position  = "right"
  )

print(p_heatmap)
ggsave(file.path(DIR_FIG, "04_heatmap_horaire.png"),
       p_heatmap, width = 12, height = 8, dpi = 150)
message("Figure 1 enregistrée.")

# ── 5. T-003 : PIC SOIR (17h) vs PIC MATIN (8h) ────────────
# Hypothèse H1 pré-spécifiée (avant de voir les données) :
# montées 17h > montées 8h sur jours NORMAL.
# Direction fixée a priori (littérature transports urbains) ->
# test unilatéral défendable.
#
# NOTE S-10 / AUTOCORRÉLATION : les jours consécutifs ne sont pas
# indépendants (un mardi chargé suit souvent un lundi chargé).
# Le Wilcoxon signed-rank suppose l'indépendance des paires.
# La p-value extrême produite ici n'a donc pas de sens littéral.
# On publie l'effet (Hodges-Lehmann, IC, r) pas la p-value.
#
# NOTE S-13 : ce test porte sur le réseau agrégé. L'assertion
# "partout, tout le temps" resterait à tester par ligne.

pic_data <- horaire %>%
  filter(horaire_type == "NORMAL",
         heure %in% c(8L, 17L)) %>%
  select(date, heure, nb_de_montees) %>%
  pivot_wider(names_from   = heure,
              values_from  = nb_de_montees,
              names_prefix = "h") %>%
  filter(!is.na(h8) & !is.na(h17))

n_paires <- nrow(pic_data)
cat("\n=== T-003 : PIC SOIR vs PIC MATIN (jours NORMAL) ===\n")
cat("Paires (jours) :", n_paires, "\n")
cat("Médiane  8h :", round(median(pic_data$h8)),  "montées\n")
cat("Médiane 17h :", round(median(pic_data$h17)), "montées\n")
cat("Ratio 17h/8h :", round(median(pic_data$h17) /
                              median(pic_data$h8), 3), "\n")

# Normalite des differences (triviale a rejeter avec n >> 5000)
diff_h      <- pic_data$h17 - pic_data$h8
shapiro_res <- shapiro.test(sample(diff_h, min(5000, n_paires)))
cat("\nShapiro-Wilk (échantillon 5000 max) : W =",
    round(shapiro_res$statistic, 3),
    "| p =", format(shapiro_res$p.value, scientific = TRUE), "\n")
cat("NOTE : avec n >>", n_paires,
    "Shapiro rejette quasi-mécaniquement. Wilcoxon retenu.\n")

# Test
wilcox_res <- wilcox.test(
  pic_data$h17, pic_data$h8,
  paired      = TRUE,
  alternative = "greater",
  conf.int    = TRUE,
  conf.level  = 0.95
)

# Taille d'effet r = Z / sqrt(N)
Z_score <- qnorm(wilcox_res$p.value, lower.tail = FALSE)
r_effet <- round(Z_score / sqrt(n_paires), 3)

magnitude <- case_when(
  r_effet >= 0.5 ~ "Grand (>= 0.5)",
  r_effet >= 0.3 ~ "Moyen (0.3-0.5)",
  r_effet >= 0.1 ~ "Petit (0.1-0.3)",
  TRUE           ~ "Négligeable (< 0.1)"
)

pct_soir_sup <- round(100 * mean(pic_data$h17 > pic_data$h8), 1)

cat("\n--- Résultats ---\n")
cat("V (statistique)       :", wilcox_res$statistic, "\n")
cat("p-value               :", format(wilcox_res$p.value,
                                       scientific = TRUE), "\n")
cat("  -> A NE PAS PUBLIER (autocorrélation non corrigée)\n")
cat("Hodges-Lehmann (diff) :", round(wilcox_res$estimate),
    "montées\n")
cat("IC 95% borne inf.     :", round(wilcox_res$conf.int[1]), "\n")
cat("Taille d'effet r      :", r_effet, "|", magnitude, "\n")
cat("% jours soir > matin  :", pct_soir_sup, "%\n")

# ── 6. ENREGISTREMENT T-003 ─────────────────────────────────
# On publie effet et IC. La p-value est produite ici à titre
# informatif mais EXCLUE du registre (autocorrélation).

enregistrer(
  test_id     = "T-003",
  script      = "04_heatmap_horaire.R",
  methode     = "Wilcoxon signed-rank apparié unilatéral (h17 > h8, NORMAL)",
  n           = n_paires,
  statistique = wilcox_res$statistic,
  p_value     = NA,   # non publiée : autocorrélation non corrigée
  effet_nom   = "Hodges-Lehmann",
  effet       = round(wilcox_res$estimate),
  ic_inf      = round(wilcox_res$conf.int[1]),
  ic_sup      = NA,
  note        = paste0("r = ", r_effet, " (", magnitude,
                       "). p-value exclue (autocorrélation).",
                       " % jours soir > matin : ", pct_soir_sup, "%.",
                       " Réseau global uniquement (S-13).")
)

message("Script 04 terminé. Figures dans figures/, résultats dans resultats/.")
