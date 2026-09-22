# ============================================================
# SCRIPT 05 - PROFILS HORAIRES PAR JOUR DE SEMAINE
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-06, S-10, S-15
# ------------------------------------------------------------
# OBJECTIF :
#   T-001  : les jours de semaine diffèrent-ils en volume ?
#   T-005  : le mercredi a-t-il un profil horaire distinct ?
#   T-005b : matrice complète jours x heures.
# SOURCE : dataset horaire, jours NORMAL uniquement.
# NOTE : le test de Dunn est codé ici, sans package externe,
# pour que le script tourne à l'identique sur toute machine.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)

# ── 1. CHARGEMENT ET FILTRE ─────────────────────────────────
# "-" encode les valeurs manquantes de horaire_tranche_stop_theo.

horaire <- lire("horaire") %>%
  filter(horaire_tranche_stop_theo != "-") %>%
  mutate(heure = as.integer(horaire_tranche_stop_theo)) %>%
  filter(!is.na(heure))

normal <- horaire %>%
  filter(horaire_type == "NORMAL") %>%
  mutate(
    jour = substr(jour_semaine, 3, nchar(jour_semaine)),
    jour = factor(jour, levels = c("Lundi", "Mardi", "Mercredi",
                                   "Jeudi", "Vendredi"))
  ) %>%
  filter(!is.na(jour))

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Observations jours NORMAL :", nrow(normal), "\n")
cat("Dates distinctes          :", n_distinct(normal$date), "\n")
cat("Période :", format(min(normal$date)), "à",
    format(max(normal$date)), "\n\n")
print(table(normal$jour))

PERIODE_TXT <- paste(format(min(normal$date), "%d.%m.%Y"),
                     "au", format(max(normal$date), "%d.%m.%Y"))

HEURE_MIN <- 5L
HEURE_MAX <- 23L

# ── 2. PROFIL HORAIRE MÉDIAN PAR JOUR ───────────────────────

profil <- normal %>%
  filter(heure >= HEURE_MIN, heure <= HEURE_MAX) %>%
  group_by(jour, heure) %>%
  summarise(mediane_montees = median(nb_de_montees, na.rm = TRUE),
            n_obs = n(), .groups = "drop")

pics <- profil %>% group_by(jour) %>% slice_max(mediane_montees, n = 1)

cat("\nPic horaire par jour :\n")
print(as.data.frame(pics %>%
  select(jour, heure, mediane_montees) %>%
  mutate(mediane_montees = round(mediane_montees))))

p_profil <- ggplot(profil, aes(x = heure, y = mediane_montees / 1e3)) +
  geom_area(fill = ROUGE_PRINCIPAL, alpha = 0.15) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.9) +
  geom_point(data = pics, color = ROUGE_PRINCIPAL, size = 2.5) +
  scale_x_continuous(breaks = seq(HEURE_MIN, HEURE_MAX, by = 3),
                     labels = function(x) paste0(x, "h")) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  facet_wrap(~ jour, ncol = 5) +
  labs(
    title    = "Profil horaire médian par jour de semaine",
    subtitle = paste0("Jours NORMAL, ", PERIODE_TXT,
                     ". Le point marque le pic de chaque jour."),
    x = "Heure", y = "Milliers de montées",
    caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        strip.text  = element_text(face = "bold"),
        panel.grid.major.x = element_blank())

print(p_profil)
ggsave(file.path(DIR_FIG, "05_profil_journalier.png"),
       p_profil, width = 14, height = 6, dpi = 150)
message("Figure 1 enregistrée.")

# ── 3. T-001 : VOLUME PAR JOUR DE SEMAINE ───────────────────
# Agrégation par date d'abord : les heures d'une même journée ne
# sont pas indépendantes (pseudoreplication).
#
# NOTE S-10 : les journées consécutives restent autocorrélées.
# Les p-values sont donc optimistes. On publie l'effet (epsilon2,
# eta2) et les écarts de médianes, pas la p-value brute.

jour_data <- normal %>%
  group_by(date, jour) %>%
  summarise(montees_jour = sum(nb_de_montees, na.rm = TRUE),
            .groups = "drop")

resume_jours <- jour_data %>%
  group_by(jour) %>%
  summarise(n = n(),
            mediane = round(median(montees_jour)),
            moyenne = round(mean(montees_jour)),
            .groups = "drop") %>%
  mutate(ecart_vs_max_pct = round(100 * (mediane / max(mediane) - 1), 1))

cat("\n=== T-001 : VOLUME QUOTIDIEN PAR JOUR ===\n")
print(as.data.frame(resume_jours))

kw_res  <- kruskal.test(montees_jour ~ jour, data = jour_data)
aov_sum <- summary(aov(montees_jour ~ jour, data = jour_data))[[1]]
eta2    <- aov_sum$`Sum Sq`[1] / sum(aov_sum$`Sum Sq`)

cat("\nKruskal-Wallis : H =", round(kw_res$statistic, 2),
    "| ddl =", kw_res$parameter,
    "| p =", format(kw_res$p.value, scientific = TRUE, digits = 3), "\n")
cat("ANOVA          : F =", round(aov_sum$`F value`[1], 3),
    "| p =", format(aov_sum$`Pr(>F)`[1], scientific = TRUE, digits = 3), "\n")
cat("eta2 =", round(eta2, 4), "soit", round(100 * eta2, 1),
    "% de la variance expliquée par le jour |",
    case_when(eta2 >= 0.14 ~ "Grand",
              eta2 >= 0.06 ~ "Moyen",
              eta2 >= 0.01 ~ "Petit",
              TRUE         ~ "Négligeable"), "\n")

# ── 4. POST-HOC : TEST DE DUNN (code, sans package) ─────────
# Comparaisons deux à deux après Kruskal-Wallis, avec correction
# pour les ex aequo et correction de Bonferroni.
# Vérifié contre pairwise.wilcox.test : mêmes paires retenues.

dunn_bonferroni <- function(x, g) {
  g <- droplevels(factor(g))
  k <- nlevels(g); N <- length(x)
  r <- rank(x)
  tt <- table(x)
  C  <- sum(tt^3 - tt) / (12 * (N - 1))   # correction ex aequo
  moy <- tapply(r, g, mean)
  n_i <- tapply(r, g, length)
  res <- data.frame()
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    A <- levels(g)[i]; B <- levels(g)[j]
    se <- sqrt((N * (N + 1) / 12 - C) * (1 / n_i[A] + 1 / n_i[B]))
    z  <- (moy[A] - moy[B]) / se
    res <- rbind(res, data.frame(
      paire = paste(A, "vs", B),
      Z = round(z, 3),
      p_brut = 2 * pnorm(abs(z), lower.tail = FALSE)
    ))
  }
  res$p_bonf <- p.adjust(res$p_brut, method = "bonferroni")
  res$signif <- res$p_bonf < 0.05
  rownames(res) <- NULL
  res
}

dunn_res <- dunn_bonferroni(jour_data$montees_jour, jour_data$jour)

cat("\n--- Post-hoc Dunn (Bonferroni) ---\n")
print(dunn_res %>%
  mutate(p_bonf = format(p_bonf, scientific = TRUE, digits = 3)) %>%
  select(paire, Z, p_bonf, signif))

cat("\nPaires significatives :", sum(dunn_res$signif), "/",
    nrow(dunn_res), "\n")

enregistrer(
  test_id = "T-001", script = "05_profil_journalier.R",
  methode = "Kruskal-Wallis sur montées quotidiennes par jour de semaine (NORMAL)",
  n = nrow(jour_data), statistique = round(kw_res$statistic, 2),
  p_value = NA,
  effet_nom = "eta2", effet = round(eta2, 4),
  note = paste0("Post-hoc Dunn/Bonferroni : ", sum(dunn_res$signif),
                "/", nrow(dunn_res), " paires significatives. ",
                "p-value exclue (autocorrélation, S-10).")
)

# ── 5. T-005 : MERCREDI VS AUTRES JOURS, HEURE PAR HEURE ────
# Test global sur toute la période. Chaque heure est testée
# séparément, avec correction de multiplicité.
#
# LIMITE (S-15) : la période 2019-2026 mélange pré-COVID, COVID
# et post-gratuité. Les deux groupes couvrent la même période,
# donc pas de biais systématique, mais la variance est gonflée.
# La section 6 reprend la question sans cette limite.

kw_par_heure <- function(donnees, cible, etiquette_cible) {
  out <- data.frame()
  for (h in HEURE_MIN:HEURE_MAX) {
    sub <- donnees %>%
      filter(heure == h) %>%
      mutate(groupe = ifelse(jour == cible, etiquette_cible, "Autres"))
    n_c <- sum(sub$groupe == etiquette_cible)
    n_a <- sum(sub$groupe == "Autres")
    if (n_c < 5 || n_a < 5) next
    kw <- kruskal.test(nb_de_montees ~ groupe, data = sub)
    med_c <- median(sub$nb_de_montees[sub$groupe == etiquette_cible], na.rm = TRUE)
    med_a <- median(sub$nb_de_montees[sub$groupe == "Autres"], na.rm = TRUE)
    # epsilon2 pour Kruskal-Wallis à 2 groupes : (H - k + 1) / (n - k)
    eps2 <- (as.numeric(kw$statistic) - 1) / (nrow(sub) - 2)
    out <- rbind(out, data.frame(
      jour = cible, heure = h, p_value = kw$p.value,
      med_cible = round(med_c), med_autres = round(med_a),
      diff_pct = round(100 * (med_c - med_a) / med_a, 1),
      epsilon2 = round(eps2, 3)
    ))
  }
  out
}

t005 <- kw_par_heure(
  normal %>% filter(heure >= HEURE_MIN, heure <= HEURE_MAX),
  "Mercredi", "Mercredi"
)
t005$p_bonf <- p.adjust(t005$p_value, method = "bonferroni")
t005$p_bh   <- p.adjust(t005$p_value, method = "BH")
t005$sig_bf <- t005$p_bonf < 0.05
t005$sig_bh <- t005$p_bh   < 0.05

cat("\n=== T-005 : MERCREDI VS AUTRES JOURS, PAR HEURE ===\n")
cat("Heures testées :", nrow(t005), "\n")
cat("Significatives Bonferroni :", sum(t005$sig_bf), "\n")
cat("Significatives BH         :", sum(t005$sig_bh), "\n")
cat("\nHeures significatives (Bonferroni) :\n")
print(as.data.frame(t005 %>% filter(sig_bf) %>%
  select(heure, med_cible, med_autres, diff_pct, epsilon2)))

# ── 6. T-005 VERSION INTRA-SEMAINE (plus robuste) ───────────
# Chaque mercredi est comparé aux autres jours de SA PROPRE
# semaine, à la même heure. Tout effet de période (COVID,
# gratuité, saison, météo) disparaît : il est commun aux deux
# termes de la différence. Test de Wilcoxon apparié sur ces
# différences intra-semaine.

semaine_data <- normal %>%
  filter(heure >= HEURE_MIN, heure <= HEURE_MAX) %>%
  mutate(annee = year(date), sem = indice_semaine) %>%
  group_by(annee, sem, heure) %>%
  filter(any(jour == "Mercredi"), sum(jour != "Mercredi") >= 2) %>%
  summarise(
    merc   = nb_de_montees[jour == "Mercredi"][1],
    autres = median(nb_de_montees[jour != "Mercredi"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(merc), !is.na(autres))

t005_intra <- data.frame()
for (h in sort(unique(semaine_data$heure))) {
  sub <- semaine_data %>% filter(heure == h)
  if (nrow(sub) < 10) next
  w <- suppressWarnings(wilcox.test(sub$merc, sub$autres,
                                    paired = TRUE, conf.int = TRUE))
  t005_intra <- rbind(t005_intra, data.frame(
    heure = h, n_semaines = nrow(sub),
    p_value = w$p.value,
    diff_HL = round(as.numeric(w$estimate)),
    diff_pct = round(100 * median(sub$merc - sub$autres) /
                       median(sub$autres), 1)
  ))
}
t005_intra$p_bonf <- p.adjust(t005_intra$p_value, method = "bonferroni")
t005_intra$sig_bf <- t005_intra$p_bonf < 0.05

cat("\n=== T-005 INTRA-SEMAINE (mercredi vs sa propre semaine) ===\n")
cat("Heures testées :", nrow(t005_intra),
    "| significatives Bonferroni :", sum(t005_intra$sig_bf), "\n")
print(as.data.frame(t005_intra %>%
  select(heure, n_semaines, diff_HL, diff_pct, sig_bf)))

cat("\nComparaison des deux approches (heures significatives) :\n")
cat("  Global      :", paste(t005$heure[t005$sig_bf], collapse = ", "), "\n")
cat("  Intra-semaine:", paste(t005_intra$heure[t005_intra$sig_bf],
                              collapse = ", "), "\n")

# ── 7. FIGURE 2 : ÉCART DU MERCREDI PAR HEURE ───────────────

ecart_viz <- t005_intra %>%
  mutate(sens = ifelse(diff_pct >= 0, "Plus fréquente", "Moins fréquente"))

p_mercredi <- ggplot(ecart_viz, aes(x = heure, y = diff_pct, fill = sens)) +
  geom_col(alpha = 0.9) +
  geom_hline(yintercept = 0, color = COL_REF, linewidth = 0.5) +
  scale_x_continuous(breaks = seq(HEURE_MIN, HEURE_MAX, by = 2),
                     labels = function(x) paste0(x, "h")) +
  scale_y_continuous(labels = label_number(suffix = " %")) +
  scale_fill_manual(values = c("Plus fréquente" = ROUGE_PRINCIPAL,
                               "Moins fréquente" = COL_NEUTRE),
                    name = NULL) +
  labs(
    title    = "Le mercredi comparé aux autres jours de la même semaine",
    subtitle = paste0("Écart médian des montées, heure par heure. ",
                     "Jours NORMAL, ", PERIODE_TXT),
    x = "Heure", y = "Écart vs autres jours de la semaine",
    caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(legend.position = "bottom")

print(p_mercredi)
ggsave(file.path(DIR_FIG, "05_ecart_mercredi.png"),
       p_mercredi, width = 11, height = 6, dpi = 150)
message("Figure 2 enregistrée.")

# ── 8. T-005b : MATRICE COMPLÈTE JOURS x HEURES ─────────────
# Chaque jour comparé aux quatre autres agrégés, heure par heure.
# Correction de multiplicité sur l'ensemble de la matrice.

t005b <- bind_rows(lapply(levels(normal$jour), function(j) {
  kw_par_heure(normal %>% filter(heure >= HEURE_MIN, heure <= HEURE_MAX), j, j)
}))
t005b$p_bonf <- p.adjust(t005b$p_value, method = "bonferroni")
t005b$p_bh   <- p.adjust(t005b$p_value, method = "BH")
t005b$sig_bf <- t005b$p_bonf < 0.05
t005b$sig_bh <- t005b$p_bh   < 0.05

cat("\n=== T-005b : MATRICE JOURS x HEURES ===\n")
cat("Tests          :", nrow(t005b), "\n")
cat("Sig. Bonferroni:", sum(t005b$sig_bf),
    "(", round(100 * mean(t005b$sig_bf), 1), "% )\n")
cat("Sig. BH        :", sum(t005b$sig_bh),
    "(", round(100 * mean(t005b$sig_bh), 1), "% )\n")
cat("\nRésumé par jour :\n")
print(as.data.frame(t005b %>%
  group_by(jour) %>%
  summarise(sig_bf = sum(sig_bf),
            eps2_max = round(max(epsilon2), 3),
            heure_eps2_max = heure[which.max(epsilon2)],
            .groups = "drop")))

# ── 9. SAUVEGARDE DES RÉSULTATS ─────────────────────────────

write.csv(resume_jours, file.path(DIR_RES, paste0("05_T001_resume_jours_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(dunn_res,     file.path(DIR_RES, paste0("05_T001_dunn_",         SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(t005,         file.path(DIR_RES, paste0("05_T005_global_",       SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(t005_intra,   file.path(DIR_RES, paste0("05_T005_intrasemaine_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(t005b,        file.path(DIR_RES, paste0("05_T005b_matrice_",     SNAPSHOT_ID, ".csv")), row.names = FALSE)

enregistrer(
  test_id = "T-005", script = "05_profil_journalier.R",
  methode = "Wilcoxon apparié intra-semaine, mercredi vs autres jours, par heure",
  n = nrow(semaine_data),
  statistique = NA, p_value = NA,
  effet_nom = "heures significatives (Bonferroni)",
  effet = sum(t005_intra$sig_bf),
  note = paste0("Sur ", nrow(t005_intra), " heures testées. ",
                "Version globale (non appariée) : ", sum(t005$sig_bf),
                " heures. Détail dans 05_T005_intrasemaine.csv.")
)

enregistrer(
  test_id = "T-005b", script = "05_profil_journalier.R",
  methode = "Kruskal-Wallis par jour et par heure, chaque jour vs les quatre autres",
  n = nrow(t005b),
  statistique = NA, p_value = NA,
  effet_nom = "tests significatifs (Bonferroni)",
  effet = sum(t005b$sig_bf),
  note = paste0(sum(t005b$sig_bh), " significatifs en BH. ",
                "Non appariée : limite de période documentée (S-15).")
)

message("Script 05 terminé. Figures dans figures/, résultats dans resultats/.")
