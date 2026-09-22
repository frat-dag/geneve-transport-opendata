# ============================================================
# SCRIPT 03 - ÉVOLUTION TEMPORELLE DE LA FRÉQUENTATION
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-04, R-05, R-07, R-09, T-04, T-06
# ------------------------------------------------------------
# OBJECTIF : Analyser l'évolution de la fréquentation sur
# 10 ans (jan 2016 -> juin 2026) via le dataset mensuel.
# SOURCES :
#   - mensuel (jan 2016 -> DATE_COUPURE)
#   - journalier non utilisé ici (mensuel = seule série longue)
# NOTE (F-08) : lire("mensuel") applique désormais donnees_definitives ET
# DATE_COUPURE (comparaison du premier jour du mois converti). Le filtre
# manuel ci-dessous est donc redondant mais conservé par sécurité et pour
# la clarté du calcul de date.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(strucchange)

# ── 1. CHARGEMENT ET NETTOYAGE ──────────────────────────────

mensuel_raw <- lire("mensuel")   # filtre donnees_definitives

mensuel <- mensuel_raw %>%
  mutate(
    date  = ym(mois),
    ligne = as.character(ligne)
  ) %>%
  filter(date <= DATE_COUPURE)   # coupure manuelle (mois = character)

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Lignes avant coupure :", nrow(mensuel_raw),
    "| après :", nrow(mensuel), "\n")
cat("Période :", format(min(mensuel$date)), "à",
    format(max(mensuel$date)), "\n")
cat("Mois distincts :", n_distinct(mensuel$date), "\n\n")

# ── 2. NA SUR LIGNE ─────────────────────────────────────────

na_ligne <- mensuel %>% filter(is.na(ligne))
cat("NA sur ligne :", nrow(na_ligne),
    "| part des montées :",
    round(100 * sum(na_ligne$nb_de_montees, na.rm = TRUE) /
            sum(mensuel$nb_de_montees, na.rm = TRUE), 4), "%\n")

mensuel <- filter(mensuel, !is.na(ligne))
cat("Lignes après exclusion NA :", nrow(mensuel), "\n\n")

# ── 3. TYPES REGIONAL/REGIONAL COMMUNE ──────────────────────
# Ces deux types disparaissent en janvier 2020 (réorganisation
# Léman Express déc. 2019). On les regroupe dans SECONDAIRE
# pour pouvoir comparer la série sur 10 ans.

for (t in c("REGIONAL", "REGIONAL COMMUNE")) {
  x <- filter(mensuel, ligne_type_act == t)
  if (nrow(x) > 0)
    cat(t, ":", n_distinct(x$date), "mois de",
        format(min(x$date)), "à", format(max(x$date)), "\n")
}

mensuel <- mensuel %>%
  mutate(ligne_type_act = ifelse(
    ligne_type_act %in% c("REGIONAL", "REGIONAL COMMUNE"),
    "SECONDAIRE", ligne_type_act
  ))

# ── 4. SÉRIE GLOBALE MENSUELLE ───────────────────────────────

mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

plancher   <- mensuel_global %>% slice_min(montees, n = 1)
pic        <- mensuel_global %>% slice_max(montees, n = 1)
y_max      <- max(mensuel_global$montees) / 1e6

cat("\nPlancher :", format(plancher$date, "%B %Y"),
    round(plancher$montees / 1e6, 2), "M\n")
cat("Pic      :", format(pic$date, "%B %Y"),
    round(pic$montees / 1e6, 2), "M\n")
cat("3 derniers mois :\n")
print(tail(mensuel_global, 3) %>%
        mutate(mois = format(date, "%B %Y"),
               montees_M = round(montees / 1e6, 2)) %>%
        select(mois, montees_M))

PERIODE_TXT <- paste(format(min(mensuel_global$date), "%d.%m.%Y"),
                     "au", format(max(mensuel_global$date), "%d.%m.%Y"))

# ── 5. FIGURE 1 : ÉVOLUTION TEMPORELLE ──────────────────────

p_evolution <- ggplot(mensuel_global,
                      aes(x = date, y = montees / 1e6)) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.8) +
  geom_point(
    data = bind_rows(plancher, pic),
    color = ROUGE_PRINCIPAL, size = 3
  ) +
  geom_vline(xintercept = D_LEMAN_EXPRESS,
             linetype = "dashed", color = COL_LEMAN,  linewidth = 0.6) +
  geom_vline(xintercept = D_COVID,
             linetype = "dashed", color = COL_COVID,  linewidth = 0.6) +
  geom_vline(xintercept = D_ETAPE_DEC2024,
             linetype = "dashed", color = COL_NEUTRE, linewidth = 0.5) +
  geom_vline(xintercept = D_GRATUITE,
             linetype = "dashed", color = COL_GRATUITE, linewidth = 0.6) +
  annotate("text", x = D_LEMAN_EXPRESS, y = y_max * 0.62,
           label = "Léman Express\ndéc. 2019",
           hjust = 1.05, size = 2.8, color = COL_LEMAN) +
  annotate("text", x = D_COVID, y = y_max * 0.90,
           label = "COVID\nmar. 2020",
           hjust = -0.05, size = 2.8, color = COL_COVID) +
  annotate("text", x = D_ETAPE_DEC2024, y = y_max * 0.50,
           label = "Renf. offre\ndéc. 2024",
           hjust = 1.05, size = 2.5, color = COL_NEUTRE) +
  annotate("text", x = D_GRATUITE, y = y_max * 0.62,
           label = "Gratuité jeunes\njan. 2025",
           hjust = -0.05, size = 2.8, color = COL_GRATUITE) +
  annotate("text",
           x = plancher$date + 90, y = plancher$montees / 1e6 + 0.8,
           label = paste0(round(plancher$montees / 1e6, 2), "M (plancher)"),
           size = 2.8, color = "grey40", hjust = 0) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = " M"),
                     limits = c(0, y_max * 1.12)) +
  labs(
    title    = "Évolution de la fréquentation, 2016-2026",
    subtitle = paste0("Montées mensuelles totales, ", PERIODE_TXT,
                     ", données définitives"),
    x = NULL, y = "Millions de montées",
    caption  = SOURCE_TPG
  ) +
  theme_projet()

print(p_evolution)
ggsave(file.path(DIR_FIG, "03_evolution_temporelle.png"),
       p_evolution, width = 12, height = 6, dpi = 150)
message("Figure 1 enregistrée.")

# ── 6. SERIE PAR TYPE DE LIGNE ───────────────────────────────

mensuel_type <- mensuel %>%
  filter(ligne_type_act != "NOCTAMBUS REGIONAL") %>%
  group_by(date, ligne_type_act) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

cat("\nTypes retenus :\n")
print(table(mensuel_type$ligne_type_act))

p_type_facet <- ggplot(mensuel_type,
                       aes(x = date, y = montees / 1e6,
                           color = ligne_type_act)) +
  geom_line(linewidth = 0.7) +
  geom_vline(xintercept = D_COVID,
             linetype = "dashed", color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = D_LEMAN_EXPRESS,
             linetype = "dashed", color = COL_LEMAN, linewidth = 0.4) +
  geom_vline(xintercept = D_GRATUITE,
             linetype = "dashed", color = COL_GRATUITE, linewidth = 0.4) +
  scale_color_manual(values = PALETTE_TYPES, guide = "none") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  facet_wrap(~ ligne_type_act, scales = "free_y", ncol = 2) +
  labs(
    title    = "Évolution par type de ligne, 2016-2026",
    subtitle = paste0("Échelles Y indépendantes : comparer les dynamiques, ",
                     "pas les volumes\n",
                     "Lignes verticales : Léman Express (bleu), ",
                     "COVID (gris), Gratuité jeunes (vert)"),
    x = NULL, y = "Millions de montées",
    caption  = paste(SOURCE_TPG,
                     "REGIONAL et REGIONAL COMMUNE regroupés dans SECONDAIRE",
                     sep = "\n")
  ) +
  theme_projet() +
  theme(strip.text = element_text(face = "bold", size = 11))

print(p_type_facet)
ggsave(file.path(DIR_FIG, "03_evolution_par_type.png"),
       p_type_facet, width = 12, height = 8, dpi = 150)
message("Figure 2 enregistrée.")

# ── 7. RÉCUPÉRATION POST-COVID ───────────────────────────────
# Référence 2019. Exclure 2026 : année partielle
# (DATE_COUPURE = juin 2026, soit seulement 6 mois).
# Règle générale : exclure l'année courante si < 12 mois.

ANNEE_COUPURE <- year(DATE_COUPURE)
N_MOIS_COUPURE <- month(DATE_COUPURE)
ANNEE_MAX_ANNUEL <- if (N_MOIS_COUPURE < 12) ANNEE_COUPURE - 1 else ANNEE_COUPURE

mensuel_annuel <- mensuel %>%
  mutate(annee = year(date)) %>%
  filter(annee <= ANNEE_MAX_ANNUEL,
         ligne_type_act != "NOCTAMBUS REGIONAL") %>%
  group_by(annee, ligne_type_act) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

ref_2019 <- mensuel_annuel %>%
  filter(annee == 2019) %>%
  select(ligne_type_act, ref = montees)

recuperation <- mensuel_annuel %>%
  left_join(ref_2019, by = "ligne_type_act") %>%
  mutate(pct_vs_2019 = round(100 * montees / ref, 1))

cat("\n=== RÉCUPÉRATION PAR TYPE vs 2019 ===\n")
recuperation %>%
  filter(annee %in% c(2019, 2020, 2022, 2023, 2024, 2025)) %>%
  select(annee, ligne_type_act, pct_vs_2019) %>%
  tidyr::pivot_wider(names_from = annee, values_from = pct_vs_2019) %>%
  print()

recup_viz <- recuperation %>%
  filter(annee %in% c(2020, 2022, 2023, 2024, 2025)) %>%
  mutate(
    annee = as.factor(annee),
    ligne_type_act = factor(ligne_type_act,
                            levels = c("PRINCIPAL", "SECONDAIRE",
                                       "GLCT", "SCOLAIRE"))
  )

p_recuperation <- ggplot(recup_viz,
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
    values = PALETTE_ANNEES[c("2020", "2022", "2023", "2024", "2025")],
    name = "Année"
  ) +
  scale_y_continuous(labels = label_number(suffix = " %"),
                     limits = c(0, 130)) +
  labs(
    title    = "Récupération post-COVID par type de ligne",
    subtitle = "Montées annuelles en % du niveau 2019 (référence = 100 %)",
    x = NULL, y = "% vs 2019",
    caption  = paste(SOURCE_TPG,
                     "GLCT : biais de périmètre (appel d'offres 2023)",
                     sep = "\n")
  ) +
  theme_projet() +
  theme(panel.grid.major.x = element_blank())

print(p_recuperation)
ggsave(file.path(DIR_FIG, "03_recuperation_par_type.png"),
       p_recuperation, width = 12, height = 7, dpi = 150)
message("Figure 3 enregistrée.")

# ── 8. TEST DE RUPTURE - LÉMAN EXPRESS ──────────────────────
# Série pré-COVID uniquement (jan 2016 à fév 2020 : 50 mois).
# CONTINUE (vérifiée : 0 mois manquant) : ts() est valide ici.
# Ce test est absent du reste du projet, il teste si le
# Léman Express est visible à l'échelle globale du réseau.

serie_pre_covid <- mensuel_global %>%
  filter(date >= as.Date("2016-01-01"),
         date <  D_COVID) %>%
  arrange(date)

cat("\nSérie pré-COVID :", nrow(serie_pre_covid), "mois (",
    format(min(serie_pre_covid$date)), "à",
    format(max(serie_pre_covid$date)), ")\n")

ts_pre <- ts(serie_pre_covid$montees, start = c(2016, 1), frequency = 12)
bp_pre <- breakpoints(ts_pre ~ 1)
cat("\n--- BAI-PERRON (série pré-COVID, continue) ---\n")
print(summary(bp_pre))

# NOTE S-06 : la série globale hors COVID serait discontinue
# (gap mars 2020 - déc 2021). ts() lui attribuerait de fausses
# dates aux observations post-COVID. Ce test étendu est documenté
# dans CORRECTIONS.md (S-06) et sera refait avec une approche
# robuste aux séries discontinues.

# ── 9. RESULTATS ────────────────────────────────────────────

write.csv(mensuel_global,
          file.path(DIR_RES, paste0("03_global_mensuel_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)
write.csv(recuperation,
          file.path(DIR_RES, paste0("03_recuperation_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)

message("Script 03 terminé. Figures dans figures/, résultats dans resultats/.")
