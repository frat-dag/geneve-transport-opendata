# ============================================================
# SCRIPT 06 - SAISONNALITÉ (DECOMPOSITION STL)
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-09, T-06, S-02, S-06
# ------------------------------------------------------------
# OBJECTIF : décomposer la série mensuelle en tendance,
# saisonnalité et résidus. Produire les séries de travail du
# script 07.
#
# POINT CRITIQUE (S-02) : ce script produit DEUX séries pour le
# script 07, et elles ne servent pas à la même chose.
#   - residus       : observée - tendance - saisonnalité.
#                     La tendance STL (t.window = 13) suit les
#                     changements de niveau et les absorbe.
#                     Tester une rupture de niveau sur les résidus
#                     revient à chercher ce qu'on vient de retirer :
#                     c'est circulaire. NE PAS UTILISER pour cela.
#   - desaisonnalisee : observée - saisonnalité.
#                     La tendance et les changements de niveau y
#                     sont conservés. C'est la série à utiliser
#                     pour T-004b et tout test de rupture.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)

# ── 1. CHARGEMENT ───────────────────────────────────────────

mensuel <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)

mensuel_global <- mensuel %>%
  group_by(date) %>%
  summarise(montees_totales = sum(nb_de_montees, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(date)

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Série mensuelle :", nrow(mensuel_global), "mois, de",
    format(min(mensuel_global$date)), "à",
    format(max(mensuel_global$date)), "\n")

# ── 2. VÉRIFICATION DE CONTINUITÉ (S-06) ────────────────────
# ts() numérote les observations et leur attribue des dates par
# comptage. Si un mois manque, toutes les dates suivantes sont
# décalées, et les dates de rupture trouvées ensuite sont fausses.
# On vérifie donc AVANT de construire la série temporelle.

mois_attendus  <- seq(min(mensuel_global$date),
                      max(mensuel_global$date), by = "month")
mois_manquants <- mois_attendus[!mois_attendus %in% mensuel_global$date]

cat("Mois attendus :", length(mois_attendus),
    "| présents :", nrow(mensuel_global),
    "| manquants :", length(mois_manquants), "\n")

if (length(mois_manquants) > 0) {
  print(mois_manquants)
  stop("Série discontinue : ts() attribuerait de fausses dates. ",
       "Compléter la série ou utiliser une méthode robuste aux trous.")
}

annee_debut <- year(min(mensuel_global$date))
mois_debut  <- month(min(mensuel_global$date))

ts_mensuel <- ts(mensuel_global$montees_totales,
                 start = c(annee_debut, mois_debut), frequency = 12)

PERIODE_TXT <- paste(format(min(mensuel_global$date), "%m.%Y"),
                     "au", format(max(mensuel_global$date), "%m.%Y"))

# ── 3. DÉCOMPOSITION STL ────────────────────────────────────
# s.window = "periodic" : saisonnalité constante d'une année sur
#   l'autre. Hypothèse vérifiée en section 5.
# t.window = 13 : tendance souple.
# robust = TRUE : limite l'influence du choc COVID.

T_WINDOW <- 13

stl_res <- stl(ts_mensuel, s.window = "periodic",
               t.window = T_WINDOW, robust = TRUE)

composantes <- data.frame(
  date         = mensuel_global$date,
  observee     = mensuel_global$montees_totales,
  tendance     = as.numeric(stl_res$time.series[, "trend"]),
  saisonnalite = as.numeric(stl_res$time.series[, "seasonal"]),
  residus      = as.numeric(stl_res$time.series[, "remainder"])
) %>%
  # Série de travail pour les tests de rupture (voir en-tête)
  mutate(desaisonnalisee = observee - saisonnalite)

cat("\n=== RÉSIDUS STL ===\n")
cat("Moyenne    :", round(mean(composantes$residus) / 1e6, 4), "M\n")
cat("Écart-type :", round(sd(composantes$residus)   / 1e6, 3), "M\n")
cat("Min        :", round(min(composantes$residus)  / 1e6, 3), "M\n")
cat("Max        :", round(max(composantes$residus)  / 1e6, 3), "M\n")

cat("\nMois du résidu le plus négatif :\n")
print(as.data.frame(composantes %>%
  slice_min(residus, n = 1) %>%
  transmute(mois = format(date, "%B %Y"),
            residu_M = round(residus / 1e6, 3))))

# ── 4. DÉMONSTRATION DE S-02 ────────────────────────────────
# On montre, chiffres à l'appui, pourquoi les résidus ne peuvent
# pas servir à détecter un changement de niveau : autour d'une
# date d'intérêt, la tendance se déplace et les résidus restent
# plats, alors que la série désaisonnalisée bouge.

fenetre <- composantes %>%
  filter(date >= D_GRATUITE %m-% months(4),
         date <= D_GRATUITE %m+% months(4))

cat("\n=== S-02 : POURQUOI PAS LES RÉSIDUS ===\n")
cat("Fenêtre autour du", format(D_GRATUITE), "(millions de montées)\n")
print(as.data.frame(fenetre %>%
  transmute(mois = format(date, "%Y-%m"),
            observee = round(observee / 1e6, 2),
            tendance = round(tendance / 1e6, 2),
            residus  = round(residus  / 1e6, 2),
            desaisonnalisee = round(desaisonnalisee / 1e6, 2))))

depl_tendance <- fenetre$tendance[nrow(fenetre)] - fenetre$tendance[1]
depl_desais   <- fenetre$desaisonnalisee[nrow(fenetre)] - fenetre$desaisonnalisee[1]
moy_res_avant <- mean(fenetre$residus[fenetre$date <  D_GRATUITE])
moy_res_apres <- mean(fenetre$residus[fenetre$date >= D_GRATUITE])

cat("\nSur cette fenêtre, déplacement du début à la fin :\n")
cat("  tendance        :", round(depl_tendance / 1e6, 3), "M\n")
cat("  desaisonnalisee :", round(depl_desais   / 1e6, 3), "M\n")
cat("  residus, moyenne avant / après :",
    round(moy_res_avant / 1e6, 3), "/",
    round(moy_res_apres / 1e6, 3), "M\n")
cat("La tendance se déplace, les résidus oscillent autour de zéro",
    "sans décalage.\nLe changement de niveau est DANS la tendance,",
    "donc absent des résidus.\n")
cat("Les tests de rupture du script 07 utilisent 'desaisonnalisee'.\n")

# ── 5. STABILITÉ DE LA SAISONNALITÉ ─────────────────────────
# s.window = "periodic" impose une saisonnalité identique chaque
# année. On vérifie cette hypothèse avec une décomposition à
# saisonnalité flexible, en comparant les profils avant et après
# la période COVID.

S_WINDOW_FLEX <- 11

stl_flex <- stl(ts_mensuel, s.window = S_WINDOW_FLEX,
                t.window = T_WINDOW, robust = TRUE)

saison_flex <- data.frame(
  date   = mensuel_global$date,
  saison = as.numeric(stl_flex$time.series[, "seasonal"])
) %>%
  mutate(
    mois_num = month(date),
    periode  = case_when(
      date <  D_COVID                     ~ "Avant 2020",
      date >= as.Date("2022-01-01")       ~ "Depuis 2022",
      TRUE                                ~ "COVID (exclu)"
    )
  ) %>%
  filter(periode != "COVID (exclu)")

comparaison_saison <- saison_flex %>%
  group_by(mois_num, periode) %>%
  summarise(saison_med = median(saison), .groups = "drop") %>%
  pivot_wider(names_from = periode, values_from = saison_med) %>%
  mutate(ecart_M = round((`Depuis 2022` - `Avant 2020`) / 1e6, 3),
         across(c(`Avant 2020`, `Depuis 2022`), ~ round(. / 1e6, 2)))

cat("\n=== STABILITÉ DE LA SAISONNALITÉ (s.window flexible) ===\n")
cat("Profil saisonnier avant 2020 contre depuis 2022, en millions\n")
print(as.data.frame(comparaison_saison))

correl_saison <- cor(comparaison_saison$`Avant 2020`,
                     comparaison_saison$`Depuis 2022`)
ecart_max <- max(abs(comparaison_saison$ecart_M))
cat("\nCorrélation des deux profils :", round(correl_saison, 3), "\n")
cat("Écart mensuel maximal        :", ecart_max, "M\n")
cat(ifelse(correl_saison > 0.9,
           "Profils très proches : s.window = 'periodic' est défendable.\n",
           "Profils divergents : revoir l'hypothèse de saisonnalité constante.\n"))

# ── 6. FIGURE 1 : DÉCOMPOSITION STL ─────────────────────────

composantes_long <- composantes %>%
  select(date, observee, tendance, saisonnalite, residus) %>%
  pivot_longer(cols = -date, names_to = "composante",
               values_to = "valeur") %>%
  mutate(composante = factor(
    composante,
    levels = c("observee", "tendance", "saisonnalite", "residus"),
    labels = c("Série observée", "Tendance", "Saisonnalité", "Résidus")))

p_stl <- ggplot(composantes_long, aes(x = date, y = valeur / 1e6)) +
  geom_hline(data = composantes_long %>%
               filter(composante %in% c("Saisonnalité", "Résidus")),
             aes(yintercept = 0), linetype = "dashed",
             color = COL_NEUTRE, linewidth = 0.4) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.7) +
  geom_vline(xintercept = D_LEMAN_EXPRESS, linetype = "dashed",
             color = COL_LEMAN, linewidth = 0.4) +
  geom_vline(xintercept = D_COVID, linetype = "dashed",
             color = COL_COVID, linewidth = 0.4) +
  geom_vline(xintercept = D_GRATUITE, linetype = "dashed",
             color = COL_GRATUITE, linewidth = 0.4) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  facet_wrap(~ composante, ncol = 1, scales = "free_y") +
  labs(
    title    = "Décomposition STL de la fréquentation",
    subtitle = paste0("Série mensuelle ", PERIODE_TXT,
                      ". s.window = periodic, t.window = ", T_WINDOW,
                      ", robust.\nLignes verticales : Léman Express, COVID, gratuité jeunes."),
    x = NULL, y = "Millions de montées",
    caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

print(p_stl)
ggsave(file.path(DIR_FIG, "06_stl_decomposition.png"),
       p_stl, width = 12, height = 10, dpi = 150)
message("Figure 1 enregistrée.")

# ── 7. FIGURE 2 : PROFIL SAISONNIER MENSUEL ─────────────────

noms_mois <- c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun",
               "Jul", "Aoû", "Sep", "Oct", "Nov", "Déc")

saisonnalite_mois <- composantes %>%
  mutate(mois_num = month(date)) %>%
  group_by(mois_num) %>%
  summarise(saisonnalite_med = median(saisonnalite), .groups = "drop") %>%
  mutate(mois_label = factor(mois_num, levels = 1:12, labels = noms_mois),
         direction  = ifelse(saisonnalite_med >= 0, "Surplus", "Déficit"))

cat("\n=== PROFIL SAISONNIER MENSUEL (millions) ===\n")
print(as.data.frame(saisonnalite_mois %>%
  transmute(mois = mois_label,
            ecart_M = round(saisonnalite_med / 1e6, 2))))

mois_fort   <- saisonnalite_mois %>% slice_max(saisonnalite_med, n = 1)
mois_faible <- saisonnalite_mois %>% slice_min(saisonnalite_med, n = 1)
cat("\nMois le plus fort  :", as.character(mois_fort$mois_label),
    round(mois_fort$saisonnalite_med / 1e6, 2), "M\n")
cat("Mois le plus faible:", as.character(mois_faible$mois_label),
    round(mois_faible$saisonnalite_med / 1e6, 2), "M\n")
cat("Amplitude          :",
    round((mois_fort$saisonnalite_med - mois_faible$saisonnalite_med) / 1e6, 2),
    "M\n")

p_saison <- ggplot(saisonnalite_mois,
                   aes(x = mois_label, y = saisonnalite_med / 1e6,
                       fill = direction)) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 0, color = COL_REF, linewidth = 0.5) +
  scale_fill_manual(values = c("Surplus" = ROUGE_PRINCIPAL,
                               "Déficit" = COL_NEUTRE), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  labs(
    title    = "Profil saisonnier de la fréquentation",
    subtitle = paste("Composante saisonnière STL, écart à la tendance.",
                     PERIODE_TXT),
    x = NULL, y = "Millions de montées (écart à la tendance)",
    caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

print(p_saison)
ggsave(file.path(DIR_FIG, "06_saison_mensuelle.png"),
       p_saison, width = 10, height = 6, dpi = 150)
message("Figure 2 enregistrée.")

# ── 8. SAUVEGARDE POUR LE SCRIPT 07 ─────────────────────────

saveRDS(composantes, file.path(DIR_PROC, "stl_composantes.rds"))
write.csv(comparaison_saison,
          file.path(DIR_RES, paste0("06_stabilite_saisonnalite_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)
write.csv(saisonnalite_mois %>% select(mois_num, mois_label, saisonnalite_med),
          file.path(DIR_RES, paste0("06_profil_saisonnier_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)

message("Script 06 terminé. Composantes STL dans ", DIR_PROC, ".")
message("RAPPEL : le script 07 doit utiliser la colonne 'desaisonnalisee', pas 'residus'.")
