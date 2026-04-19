# ============================================================
# SCRIPT 05 — PROFIL JOURNALIER
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : avril 2026
# ------------------------------------------------------------
# OBJECTIF : Analyser les profils horaires par jour de semaine.
# Tester si les jours ont des fréquentations différentes (T-001)
# et si le mercredi a un profil horaire distinct (T-005/T-005b).
# Ce qu'on sait déjà :
#   - Même dataset que script 04 (horaire.rds)
#   - Filtre jours NORMAL uniquement (T-002 prouve VACANCES ≠ NORMAL)
#   - Médiane sur jours NORMAL = mesure de référence (pas la moyenne)
#   - Biais des fériés documenté — toujours filtrer NORMAL
# ============================================================

# ── 1. NETTOYAGE ET PACKAGES ─────────────────────────────────

rm(list = ls())
gc()

setwd("D:/Frat/Documents/IA/Claude/Projet TPG/tpg-opendata-analysis/R")

source("00_palette.R")

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)

# ── 2. CHARGEMENT ET FILTRE ─────────────────────────────────

horaire <- readRDS("../data/raw/horaire.rds") %>%
  filter(donnees_definitives == TRUE) %>%
  filter(!is.na(horaire_tranche_stop_theo)) %>%
  mutate(heure = as.integer(horaire_tranche_stop_theo)) %>%
  filter(!is.na(heure))

# Jours NORMAL uniquement pour les analyses intra-semaine
normal <- horaire %>%
  filter(horaire_type == "NORMAL") %>%
  mutate(
    jour = substr(jour_semaine, 3, nchar(jour_semaine)),
    jour = factor(jour, levels = c("Lundi", "Mardi", "Mercredi",
                                   "Jeudi", "Vendredi"))
  )

cat("Jours NORMAL disponibles :", nrow(normal), "observations\n")
cat("Jours distincts          :", n_distinct(normal$jour), "\n")
print(table(normal$jour))


# ── 3. PROFIL HORAIRE PAR JOUR ───────────────────────────────
# Médiane des montées par heure et par jour
# On garde toutes les heures ici — le profil complet

profil <- normal %>%
  filter(heure >= 5 & heure <= 23) %>%
  group_by(jour, heure) %>%
  summarise(
    mediane_montees = median(nb_de_montees, na.rm = TRUE),
    n_obs           = n(),
    .groups         = "drop"
  )

cat("Combinaisons jour×heure :", nrow(profil), "\n")

# Pic par jour — quelle heure est la plus chargée ?
cat("\nPic horaire par jour :\n")
profil %>%
  group_by(jour) %>%
  slice_max(mediane_montees, n = 1) %>%
  dplyr::select(jour, heure, mediane_montees) %>%
  mutate(mediane_montees = round(mediane_montees)) %>%
  print()

# ── 4. GRAPHIQUE — PROFILS HORAIRES PAR JOUR ────────────────
# facet_wrap — une courbe par jour, même échelle Y
# NOTE VIZ : graphique fort pour publication

p_profil <- ggplot(profil,
                   aes(x = heure, y = mediane_montees / 1e3)) +
  geom_line(color = TPG_RED, linewidth = 0.9) +
  geom_area(fill = TPG_RED, alpha = 0.15) +
  
  # Marquer le pic de chaque jour
  geom_point(data = profil %>%
               group_by(jour) %>%
               slice_max(mediane_montees, n = 1),
             color = TPG_RED, size = 2.5) +
  
  scale_x_continuous(breaks = seq(5, 23, by = 2),
                     labels = function(x) paste0(x, "h")) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  
  facet_wrap(~ jour, ncol = 5) +
  
  labs(
    title    = "Profil horaire médian par jour de semaine",
    subtitle = "Jours NORMAL uniquement — médiane des montées par tranche horaire",
    x        = "Heure",
    y        = "Milliers de montées",
    caption  = "Source : TPG Open Data | jan. 2019 → avr. 2026"
  ) +
  theme_tpg() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
    strip.text       = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(p_profil)

ggsave("../outputs/05_profil_journalier.png",
       plot = p_profil, width = 14, height = 6, dpi = 150)

message("Graphique sauvegardé.")

# ── 5. TEST T-001 — DIFFÉRENCES ENTRE JOURS DE SEMAINE ──────
# Question : les jours de semaine ont-ils des fréquentations
# significativement différentes ?
# On agrège d'abord par date — une observation par jour
# Raison : éviter la pseudoréplication (les heures d'un même
# jour ne sont pas indépendantes entre elles)

jour_data <- normal %>%
  group_by(date, jour) %>%
  summarise(
    montees_jour = sum(nb_de_montees, na.rm = TRUE),
    .groups      = "drop"
  )

cat("Observations par jour :\n")
jour_data %>%
  group_by(jour) %>%
  summarise(
    n       = n(),
    mediane = round(median(montees_jour)),
    moyenne = round(mean(montees_jour))
  ) %>%
  print()

# Vérification normalité par jour (Shapiro-Wilk)
cat("\nTest de normalité par jour :\n")
for (j in levels(jour_data$jour)) {
  vals <- jour_data %>% filter(jour == j) %>% pull(montees_jour)
  sw   <- shapiro.test(vals)
  cat(sprintf("  %-10s W = %.3f  p = %s\n",
              j, sw$statistic,
              format(sw$p.value, scientific = TRUE, digits = 3)))
}

# ── 6. KRUSKAL-WALLIS + ANOVA — T-001 ───────────────────────
# Kruskal-Wallis : test de référence (normalité violée)
# ANOVA : citée en complément (robuste via TCL avec n > 260)

kw_res <- kruskal.test(montees_jour ~ jour, data = jour_data)

cat("=== KRUSKAL-WALLIS ===\n")
cat("χ² =", round(kw_res$statistic, 2), "\n")
cat("ddl =", kw_res$parameter, "\n")
cat("p   =", format(kw_res$p.value, scientific = TRUE), "\n")
cat("→", ifelse(kw_res$p.value < 0.05,
                "Rejet H0 — au moins un jour diffère",
                "Non-rejet H0"), "\n\n")

# ANOVA en complément
aov_res <- aov(montees_jour ~ jour, data = jour_data)
aov_sum <- summary(aov_res)[[1]]

cat("=== ANOVA ===\n")
cat("F   =", round(aov_sum$`F value`[1], 3), "\n")
cat("p   =", format(aov_sum$`Pr(>F)`[1], scientific = TRUE), "\n\n")

# Taille d'effet η²
ss_between <- aov_sum$`Sum Sq`[1]
ss_total   <- sum(aov_sum$`Sum Sq`)
eta2       <- ss_between / ss_total

cat("Taille d'effet η² =", round(eta2, 4), "\n")
cat("Soit", round(eta2 * 100, 1),
    "% de la variance expliquée par le jour\n")
cat("Magnitude :", case_when(
  eta2 >= 0.14 ~ "Grand (≥ 0.14)",
  eta2 >= 0.06 ~ "Moyen (0.06-0.14)",
  eta2 >= 0.01 ~ "Petit (0.01-0.06)",
  TRUE         ~ "Négligeable (< 0.01)"
), "\n")

if (!require(dunn.test)) install.packages("dunn.test")

# ── 7. POST-HOC — QUELS JOURS DIFFÈRENT ? ───────────────────
# Dunn test (non-paramétrique) — cohérent avec KW
# Correction Bonferroni — conservatrice mais rigoureuse
# pour des conclusions fermes

library(dunn.test)

cat("=== DUNN TEST (post-hoc KW) — correction Bonferroni ===\n\n")
dunn_res <- dunn.test(
  jour_data$montees_jour,
  jour_data$jour,
  method = "bonferroni",
  altp   = TRUE   # p-values bilatérales
)

# ── 8. BLOC DÉCISION — T-001 ────────────────────────────────

# RÉSULTATS T-001
# KW χ² = 58.28, p = 6.65×10⁻¹² → Rejet H0
# η² = 1.1% → effet petit mais réel
#
# POST-HOC DUNN (Bonferroni) :
# Lundi diffère significativement de TOUS les autres jours
# Jeudi > Mercredi (p = 0.020) — seule paire significative hors Lundi
# Aucune autre paire significative
#
# CE QU'ON PEUT AFFIRMER :
# - Lundi est significativement moins fréquenté que tous les autres jours
# - Jeudi est significativement plus fréquenté que Mercredi
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - Que Jeudi est "le jour le plus chargé" — pas de différence
#   significative avec Mardi ou Vendredi
# - Que le vendredi est différent des autres jours de milieu de semaine
#
# ANGLE NARRATIF :
# "Le lundi post-COVID est structurellement plus creux — le télétravail
# du lundi est une réalité mesurable dans les données TPG"
# → hypothèse non prouvée causalement mais plausible et intéressante
#
# AMÉLIORATION vs ancienne documentation :
# Dunn non-paramétrique utilisé ici (vs Tukey paramétrique avant)
# Dunn est plus rigoureux quand la normalité est violée

# On passe maintenant à T-005 — profil mercredi vs autres jours


# ── 9. TEST T-005 — PROFIL HORAIRE MERCREDI VS AUTRES ────────
# Question : le mercredi a-t-il un profil horaire significativement
# différent des autres jours de semaine ?
# Approche : KW par heure + corrections multiplicité
# Pourquoi heure par heure ? On ne teste pas le volume global
# (T-001 l'a déjà fait) mais la FORME du profil — à quelle heure
# le mercredi diffère-t-il des autres jours ?

# Filtre minimum 5 observations par groupe par heure
profil_test <- normal %>%
  filter(heure >= 5 & heure <= 23) %>%
  mutate(
    est_mercredi = ifelse(jour == "Mercredi", "Mercredi", "Autres")
  ) %>%
  group_by(heure) %>%
  filter(
    sum(est_mercredi == "Mercredi") >= 5 &
      sum(est_mercredi == "Autres")   >= 5
  ) %>%
  ungroup()

# KW par heure
heures_test <- sort(unique(profil_test$heure))
resultats_t005 <- data.frame()

for (h in heures_test) {
  sub <- profil_test %>% filter(heure == h)
  kw  <- kruskal.test(nb_de_montees ~ est_mercredi, data = sub)
  
  # Médiane par groupe
  med_merc  <- median(sub$nb_de_montees[sub$est_mercredi == "Mercredi"],
                      na.rm = TRUE)
  med_autres <- median(sub$nb_de_montees[sub$est_mercredi == "Autres"],
                       na.rm = TRUE)
  
  # Taille d'effet η²
  n_total <- nrow(sub)
  eta2_h  <- (kw$statistic - 1) / (n_total - 1)
  
  resultats_t005 <- rbind(resultats_t005, data.frame(
    heure       = h,
    p_value     = kw$p.value,
    med_merc    = round(med_merc),
    med_autres  = round(med_autres),
    diff_pct    = round((med_merc - med_autres) / med_autres * 100, 1),
    eta2        = round(eta2_h, 3)
  ))
}

# Corrections multiplicité
resultats_t005$p_bonf <- p.adjust(resultats_t005$p_value,
                                  method = "bonferroni")
resultats_t005$p_bh   <- p.adjust(resultats_t005$p_value,
                                  method = "BH")
resultats_t005$sig_bf <- resultats_t005$p_bonf < 0.05
resultats_t005$sig_bh <- resultats_t005$p_bh   < 0.05

cat("=== T-005 — MERCREDI VS AUTRES JOURS PAR HEURE ===\n\n")
cat("Heures sig. Bonferroni :",
    sum(resultats_t005$sig_bf), "/", nrow(resultats_t005), "\n")
cat("Heures sig. BH         :",
    sum(resultats_t005$sig_bh), "/", nrow(resultats_t005), "\n\n")

cat("Détail heures significatives (Bonferroni) :\n")
resultats_t005 %>%
  filter(sig_bf) %>%
  dplyr::select(heure, diff_pct, eta2, p_bonf) %>%
  mutate(p_bonf = format(p_bonf, scientific = TRUE, digits = 3)) %>%
  print()

# ── 10. BLOC DÉCISION — T-005 ───────────────────────────────

# RÉSULTATS T-005 — KW par heure, corrections Bonferroni et BH
# 8/19 heures sig. Bonferroni | 12/19 heures sig. BH
#
# PATTERN STRUCTUREL :
# 7h-8h  : Mercredi < autres (-3 à -4%)  — petit effet
# 11h-15h: Mercredi > autres (+8 à +18%) — effet moyen à fort
#           pic à 14h : diff +17.8%, η² = 0.152
# 16h    : Mercredi < autres (-11.9%)    — retournement fort
# 17h    : Mercredi < autres (-3.7%)     — petit effet
#
# CE QU'ON PEUT AFFIRMER :
# - Le mercredi a un profil horaire significativement distinct
# - La bosse 11h-15h est robuste (Bonferroni)
# - Le creux à 16h est robuste (η² = 0.117)
#
# CE QU'ON NE PEUT PAS AFFIRMER :
# - La cause — organisation scolaire, télétravail, loisirs ?
# - La stabilité sur toute la période 2019-2026
#
# ANGLE NARRATIF :
# "Le mercredi genevois a une identité horaire propre — les transports
# publics absorbent un pic de mi-journée inexistant les autres jours"

# ── 11. SAUVEGARDE FINALE ───────────────────────────────────
ggsave("../outputs/05_profil_journalier.png",
       plot = p_profil, width = 14, height = 6, dpi = 150)

message("Script 05 terminé.")