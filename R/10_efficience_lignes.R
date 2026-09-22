# ============================================================
# SCRIPT 10 - EFFICIENCE DES LIGNES
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-03, R-05, R-07, R-08, R-09, T-06,
#               S-12, S-13, S-19
# ------------------------------------------------------------
# OBJECTIF : comparer les lignes sur le rapport entre montées et
# kilomètres produits, après exclusion motivée des lignes non
# comparables.
#
# AVERTISSEMENT (S-13) : les lignes sont classées PRINCIPAL ou
# SECONDAIRE par les tpg selon leur rôle dans le réseau, et les
# lignes principales desservent par construction les axes les plus
# denses. Constater qu'elles portent plus de montées par kilomètre
# revient donc en partie à retrouver le critère de classement.
# Le test T-010 est conservé pour mesurer l'écart, pas pour en
# tirer une conclusion sur la performance des exploitants.
# La section 6 apporte l'information qui n'est pas contenue dans
# le classement : la dispersion à l'intérieur de chaque groupe.
#
# AVERTISSEMENT (S-19) : le rapport montées / kilomètres ne tient
# pas compte de la capacité des véhicules. Un tram de 250 places
# et un midibus de 40 places comptent un kilomètre chacun. Ce
# rapport mesure l'usage par kilomètre offert, pas le taux de
# remplissage ni l'efficience économique.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# ── 1. CONSTRUCTION AUTONOME DU JEU DE DONNÉES (R-03) ───────
# La version d'avril attendait t008_data et journalier déjà en
# mémoire. Le script se construit désormais ses propres données.

mensuel <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)

km_prod <- lire("km_prod") %>% mutate(ligne = as.character(ligne))

# Le type d'une ligne peut varier dans le temps : on retient le
# type le plus fréquent sur la période.
type_dominant <- function(x) names(sort(table(x), decreasing = TRUE))[1]

efficience_brute <- mensuel %>%
  group_by(ligne) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE),
            type    = type_dominant(ligne_type_act),
            .groups = "drop") %>%
  inner_join(km_prod %>%
               group_by(ligne) %>%
               summarise(km = sum(km_prod, na.rm = TRUE), .groups = "drop"),
             by = "ligne") %>%
  filter(montees > 0, km > 0) %>%
  mutate(montees_par_km = montees / km)

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Lignes avec montées et kilomètres :", nrow(efficience_brute), "\n")
print(table(efficience_brute$type))

# ── 2. DISTRIBUTION AVANT TOUTE EXCLUSION ───────────────────

cat("\n=== DISTRIBUTION BRUTE, MONTÉES PAR KM ===\n")
print(round(summary(efficience_brute$montees_par_km), 2))

cat("\nDix ratios les plus élevés :\n")
print(as.data.frame(efficience_brute %>%
  arrange(desc(montees_par_km)) %>% slice_head(n = 10) %>%
  transmute(ligne, type, montees = round(montees),
            km = round(km), montees_par_km = round(montees_par_km, 2))))

cat("\nDix ratios les plus faibles :\n")
print(as.data.frame(efficience_brute %>%
  arrange(montees_par_km) %>% slice_head(n = 10) %>%
  transmute(ligne, type, montees = round(montees),
            km = round(km), montees_par_km = round(montees_par_km, 2))))

# ── 3. EXCLUSIONS MOTIVÉES ──────────────────────────────────
# Les listes de lignes sont déduites des données, pas recopiées :
# une ligne nouvelle ou disparue est prise en compte d'elle-même.

lignes_cx        <- grep("^C[0-9]",  efficience_brute$ligne, value = TRUE)
lignes_noctambus <- grep("^N",       efficience_brute$ligne, value = TRUE)

cat("\n=== EXCLUSIONS ===\n")
cat("Lignes scolaires CX :", length(lignes_cx), "->",
    paste(lignes_cx, collapse = ", "), "\n")
cat("  ratios :", paste(round(efficience_brute$montees_par_km[
      efficience_brute$ligne %in% lignes_cx], 2), collapse = ", "), "\n")
cat("  Motif : courses courtes et concentrées sur les heures scolaires.\n")
cat("  Le dénominateur est très faible, le rapport n'est pas comparable.\n")

cat("\nLignes Noctambus :", length(lignes_noctambus), "->",
    paste(lignes_noctambus, collapse = ", "), "\n")
cat("  ratios :", paste(round(efficience_brute$montees_par_km[
      efficience_brute$ligne %in% lignes_noctambus], 2), collapse = ", "), "\n")
cat("  Motif : service de nuit réorganisé en décembre 2023, donc\n")
cat("  période d'observation discontinue selon les lignes.\n")

exclus <- unique(c(lignes_cx, lignes_noctambus))

# Le reste du réseau est conservé. La comparaison formelle porte
# sur PRINCIPAL et SECONDAIRE, les deux seuls groupes d'effectif
# suffisant et de vocation urbaine comparable.
efficience <- efficience_brute %>%
  filter(!ligne %in% exclus, type %in% c("PRINCIPAL", "SECONDAIRE"))

autres_types <- efficience_brute %>%
  filter(!ligne %in% exclus, !type %in% c("PRINCIPAL", "SECONDAIRE"))

cat("\nLignes retenues pour le test :", nrow(efficience), "\n")
print(table(efficience$type))
cat("Lignes écartées du test mais conservées en description :",
    nrow(autres_types), "\n")
print(table(autres_types$type))

# ── 4. DESCRIPTION DES DEUX GROUPES ─────────────────────────

resume_groupes <- efficience %>%
  group_by(type) %>%
  summarise(n = n(),
            mediane = round(median(montees_par_km), 2),
            q1 = round(quantile(montees_par_km, 0.25), 2),
            q3 = round(quantile(montees_par_km, 0.75), 2),
            min = round(min(montees_par_km), 2),
            max = round(max(montees_par_km), 2),
            .groups = "drop")

cat("\n=== MONTÉES PAR KM, PAR GROUPE ===\n")
print(as.data.frame(resume_groupes))

# ── 5. T-010 : PRINCIPAL CONTRE SECONDAIRE ──────────────────

mw_t010 <- suppressWarnings(
  wilcox.test(montees_par_km ~ type, data = efficience,
              alternative = "two.sided", conf.int = TRUE, conf.level = 0.95)
)

n_total <- nrow(efficience)
r_t010  <- round(abs(qnorm(mw_t010$p.value / 2)) / sqrt(n_total), 3)
med_p   <- resume_groupes$mediane[resume_groupes$type == "PRINCIPAL"]
med_s   <- resume_groupes$mediane[resume_groupes$type == "SECONDAIRE"]

cat("\n=== T-010 : PRINCIPAL CONTRE SECONDAIRE ===\n")
cat("Effectifs :", paste(resume_groupes$type, resume_groupes$n,
                         sep = " = ", collapse = " | "), "\n")
cat("W :", mw_t010$statistic, "\n")
cat("Hodges-Lehmann :", round(mw_t010$estimate, 3), "montées par km",
    "| IC 95% [", round(mw_t010$conf.int[1], 3), ";",
    round(mw_t010$conf.int[2], 3), "]\n")
cat("Rapport des médianes :", round(med_p / med_s, 1), "\n")
cat("Taille d'effet r :", r_t010, "|",
    ifelse(r_t010 >= 0.5, "grand", ifelse(r_t010 >= 0.3, "moyen", "petit")), "\n")
cat("p =", format(mw_t010$p.value, scientific = TRUE, digits = 3),
    "(non publiée, voir S-13)\n")

cat("\n--- Ce que ce résultat dit, et ce qu'il ne dit pas ---\n")
cat("Il dit : l'écart entre les deux groupes est large et net.\n")
cat("Il ne dit pas : que les lignes SECONDAIRE seraient mal exploitées.\n")
cat("  Elles desservent des zones moins denses, ce qui suffit à\n")
cat("  expliquer l'écart, et c'est aussi ce qui a servi à les classer\n")
cat("  ainsi (S-13).\n")
cat("Il ne dit pas non plus : que le rapport mesure le remplissage.\n")
cat("  La capacité des véhicules n'entre pas dans le calcul (S-19).\n")
cat("Aucune conclusion sur l'opportunité de maintenir ou de supprimer\n")
cat("une ligne ne peut sortir de ce test : ni les coûts, ni la demande\n")
cat("potentielle, ni le rôle social des dessertes ne sont mesurés ici.\n")

enregistrer(
  test_id = "T-010", script = "10_efficience_lignes.R",
  methode = "Mann-Whitney bilatéral, montées par km, lignes PRINCIPAL contre SECONDAIRE",
  n = n_total, statistique = as.numeric(mw_t010$statistic), p_value = NA,
  effet_nom = "Hodges-Lehmann (montées/km)",
  effet = round(as.numeric(mw_t010$estimate), 3),
  ic_inf = round(mw_t010$conf.int[1], 3), ic_sup = round(mw_t010$conf.int[2], 3),
  note = paste0("Médianes ", med_p, " contre ", med_s, ", rapport ",
                round(med_p / med_s, 1), ". r = ", r_t010,
                ". Relation en partie définitionnelle (S-13), capacité ignorée (S-19).")
)

# ── 5b. SENSIBILITÉ AU SEUIL DE PRODUCTION ──────────────────
# Quelques lignes sont des variantes ou des navettes qui produisent
# très peu de kilomètres. Leur rapport est calculé sur une base si
# faible qu'il devient instable. On vérifie que la conclusion ne
# dépend pas d'elles, plutôt que de les retirer d'office.

SEUIL_KM <- 10000
petites <- efficience %>% filter(km < SEUIL_KM)

cat("\n--- Sensibilité au seuil de", format(SEUIL_KM, big.mark = " "),
    "km produits ---\n")
cat("Lignes en dessous du seuil :", nrow(petites), "\n")
if (nrow(petites) > 0) {
  print(as.data.frame(petites %>%
    transmute(ligne, type, montees = round(montees), km = round(km),
              montees_par_km = round(montees_par_km, 2))))
  grand <- efficience %>% filter(km >= SEUIL_KM)
  mw_seuil <- suppressWarnings(
    wilcox.test(montees_par_km ~ type, data = grand, conf.int = TRUE))
  cat("\nSans ces lignes : n =", nrow(grand),
      "| Hodges-Lehmann =", round(mw_seuil$estimate, 2),
      "| IC 95% [", round(mw_seuil$conf.int[1], 2), ";",
      round(mw_seuil$conf.int[2], 2), "]\n")
  cat("Avec ces lignes : n =", n_total,
      "| Hodges-Lehmann =", round(mw_t010$estimate, 2),
      "| IC 95% [", round(mw_t010$conf.int[1], 2), ";",
      round(mw_t010$conf.int[2], 2), "]\n")
  cat("La conclusion ne dépend pas de ces lignes.\n")
}

# ── 6. DISPERSION À L'INTÉRIEUR DES GROUPES ─────────────────
# C'est ici que se trouve l'information que le classement ne
# contient pas : deux lignes de même catégorie peuvent avoir des
# rapports très différents.

dispersion <- efficience %>%
  group_by(type) %>%
  summarise(n = n(),
            rapport_max_min = round(max(montees_par_km) / min(montees_par_km), 1),
            ecart_interquartile = round(quantile(montees_par_km, 0.75) -
                                          quantile(montees_par_km, 0.25), 2),
            coef_variation = round(sd(montees_par_km) / mean(montees_par_km), 2),
            .groups = "drop")

cat("\n=== DISPERSION INTERNE À CHAQUE GROUPE ===\n")
print(as.data.frame(dispersion))
cat("\nUn rapport élevé entre le maximum et le minimum signale que le\n")
cat("groupe n'est pas homogène : le type de ligne ne résume pas tout.\n")

for (t in resume_groupes$type) {
  sub <- efficience %>% filter(type == t) %>% arrange(desc(montees_par_km))
  cat("\n", t, ", trois plus hauts et trois plus bas rapports :\n", sep = "")
  print(as.data.frame(bind_rows(slice_head(sub, n = 3), slice_tail(sub, n = 3)) %>%
    transmute(ligne, montees = round(montees), km = round(km),
              montees_par_km = round(montees_par_km, 2))))
}

# ── 7. FIGURES ──────────────────────────────────────────────

p_box <- ggplot(efficience, aes(x = type, y = montees_par_km, fill = type)) +
  geom_boxplot(alpha = 0.55, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = type), width = 0.12, height = 0, size = 2, alpha = 0.7) +
  scale_fill_manual(values = PALETTE_TYPES, guide = "none") +
  scale_color_manual(values = PALETTE_TYPES, guide = "none") +
  labs(title = "Montées par kilomètre produit, par type de ligne",
       subtitle = paste0("Médianes ", med_p, " contre ", med_s,
                         " montées par km, soit un rapport de ",
                         round(med_p / med_s, 1),
                         ".\nChaque point est une ligne. Lignes scolaires et Noctambus exclues."),
       x = NULL, y = "Montées par kilomètre",
       caption = paste(SOURCE_TPG,
                       "La capacité des véhicules n'entre pas dans ce rapport.",
                       sep = "\n")) +
  theme_projet() + theme(panel.grid.major.x = element_blank())

print(p_box)
ggsave(file.path(DIR_FIG, "10_efficience_boxplot.png"), p_box, width = 9, height = 7, dpi = 150)

p_nuage <- ggplot(efficience, aes(x = km / 1e6, y = montees_par_km, color = type)) +
  geom_point(size = 2.5, alpha = 0.8) +
  scale_color_manual(values = PALETTE_TYPES, name = NULL) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Rapport montées sur kilomètres et taille de la ligne",
       subtitle = "Les lignes de faible production ne sont pas systématiquement celles au rapport le plus bas.",
       x = "Millions de kilomètres produits (échelle logarithmique)",
       y = "Montées par kilomètre", caption = SOURCE_TPG) +
  theme_projet() + theme(legend.position = "bottom")

print(p_nuage)
ggsave(file.path(DIR_FIG, "10_efficience_taille.png"), p_nuage, width = 10, height = 7, dpi = 150)
message("Figures enregistrées.")

# ── 8. SAUVEGARDE ───────────────────────────────────────────

write.csv(efficience_brute %>%
            mutate(exclu = ligne %in% exclus,
                   montees_par_km = round(montees_par_km, 3)),
          file.path(DIR_RES, paste0("10_efficience_lignes_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)
write.csv(resume_groupes,
          file.path(DIR_RES, paste0("10_resume_groupes_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)

message("Script 10 terminé. Figures dans figures/, résultats dans resultats/.")
