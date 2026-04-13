# =============================================================
# PROJET TPG OPEN DATA - Phase 2, Analyse 2.1
# Fichier  : 04_heatmap_horaire.R
# Objectif : Visualiser la fréquentation par heure et par
#            jour de la semaine — "quand Genève prend le bus"
# Dataset  : frequentation-journaliere-par-tranche-horaire
# Date     : avril 2026
# =============================================================

# --- 1. Chargement des packages ------------------------------
# Nouveaux par rapport aux scripts précédents :
install.packages("viridis")
library(viridis)     # Palettes de couleurs perceptuellement uniformes
# — idéales pour les heatmaps (daltonisme-friendly)

# Déjà utilisés :
library(httr2)
library(readr)
library(dplyr)
library(ggplot2)

# --- 2. Téléchargement ---------------------------------------

url_horaire <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets/frequentation-journaliere-par-tranche-horaire/exports/csv"

message("Téléchargement fréquentation par tranche horaire...")

reponse <- request(url_horaire) |>
  req_url_query(
    lang      = "fr",
    delimiter = ";",
    timezone  = "Europe/Zurich"
  ) |>
  req_perform()

freq_horaire_raw <- resp_body_string(reponse) |>
  read_delim(
    delim          = ";",
    locale         = locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )

message("✓ Données reçues : ", nrow(freq_horaire_raw), " lignes x ",
        ncol(freq_horaire_raw), " colonnes")
message("Colonnes :")
print(colnames(freq_horaire_raw))
print(head(freq_horaire_raw, 3))

# --- 3. Nettoyage et préparation -----------------------------

freq_horaire <- freq_horaire_raw |>
  filter(donnees_definitives == TRUE) |>
  # Extraire l'heure comme nombre entier
  mutate(
    heure = as.integer(horaire_tranche_stop_theo),
    # Simplifier le nom du jour (supprimer le numéro devant)
    jour  = sub("^[0-9]-", "", jour_semaine),
    # Ordonner les jours correctement (lundi → dimanche)
    jour  = factor(jour, levels = c(
      "Lundi", "Mardi", "Mercredi", "Jeudi",
      "Vendredi", "Samedi", "Dimanche"
    ))
  ) |>
  filter(!is.na(heure), !is.na(jour))

# --- 4. Agrégation heure × jour ------------------------------

# Moyenne des montées par tranche horaire et par jour de la semaine
heatmap_data <- freq_horaire |>
  group_by(jour, heure) |>
  summarise(
    moy_montees = mean(nb_de_montees, na.rm = TRUE),
    .groups     = "drop"
  )

# Aperçu
print(heatmap_data)

# --- 5. Heatmap heure × jour ---------------------------------

ggplot(heatmap_data,
       aes(x = heure, y = jour, fill = moy_montees)) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_fill_viridis(
    name   = "Montées\nmoyennes",
    option = "C",      # Palette "inferno" — du sombre au jaune vif
    labels = scales::comma
  ) +
  scale_x_continuous(
    breaks = seq(0, 23, by = 2),
    labels = function(x) paste0(x, "h")
  ) +
  labs(
    title    = "Fréquentation TPG par heure et jour de la semaine",
    subtitle = "Moyenne des montées par tranche horaire — réseau complet",
    x        = "Heure de la journée",
    y        = NULL,
    caption  = "Source : opendata.tpg.ch"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(color = "grey50"),
    panel.grid       = element_blank(),
    legend.position  = "right"
  )

ggsave("outputs/04_heatmap_horaire.png",
       width = 12, height = 5, dpi = 150)
message("✓ Heatmap sauvegardée : outputs/04_heatmap_horaire.png")

# =============================================================
# TEST STATISTIQUE — T-003
# Hypothèse : Le pic du soir (17h) est significativement
#             plus élevé que le pic du matin (8h)
# Méthode   : Test de Wilcoxon apparié (signed-rank test)
# H0        : Pas de différence entre pic matin et pic soir
# H1        : Le pic soir > pic matin (test unilatéral)
# Données   : Dataset fréquentation par tranche horaire
#             jours NORMAL uniquement
#
# Pourquoi Wilcoxon APPARIÉ et pas Mann-Whitney ?
#   → Les observations matin et soir viennent du MÊME jour
#   → Ce sont des mesures appariées : pour chaque date,
#     on compare la tranche 8h à la tranche 17h
#   → L'appariement élimine la variabilité inter-journalière
#     et augmente la puissance du test
#   → La normalité des différences n'est pas garantie
#     → Wilcoxon signed-rank (non-paramétrique apparié)
#
# Pourquoi test UNILATÉRAL ?
#   → Notre hypothèse est directionnelle : on prédit que
#     le soir EST PLUS GRAND que le matin, pas juste différent
#   → Un test unilatéral est plus puissant quand la direction
#     est clairement anticipée — mais il faut le justifier
#     AVANT de voir les données, pas après (biais de confirmation)
#   → Justification a priori : la littérature sur les transports
#     urbains montre universellement un pic soir > matin
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-003 — Wilcoxon apparié : pic matin vs pic soir\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# --- Préparation des données --------------------------------
# Pour chaque date, extraire les montées à 8h et à 17h
# On filtre sur jours NORMAL uniquement

freq_apparie <- freq_horaire |>
  filter(
    horaire_type == "NORMAL",
    heure %in% c(8, 17)
  ) |>
  group_by(date, heure) |>
  summarise(
    montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from  = heure,
    values_from = montees,
    names_prefix = "h"
  ) |>
  filter(!is.na(h8), !is.na(h17))  # garder uniquement les jours avec les deux mesures

cat("Nombre de jours avec les deux mesures :", nrow(freq_apparie), "\n\n")

# --- Statistiques descriptives ------------------------------
cat("--- Statistiques descriptives ---\n")
cat("Pic matin (8h)  — médiane :",
    round(median(freq_apparie$h8)), "montées\n")
cat("Pic soir  (17h) — médiane :",
    round(median(freq_apparie$h17)), "montées\n")
cat("Ratio soir/matin           :",
    round(median(freq_apparie$h17) / median(freq_apparie$h8), 2), "\n")

# Distribution des différences
diff_soir_matin <- freq_apparie$h17 - freq_apparie$h8
cat("\nDifférence soir-matin :\n")
cat("  Médiane :", round(median(diff_soir_matin)), "montées\n")
cat("  Moyenne :", round(mean(diff_soir_matin)), "montées\n")
cat("  Min     :", round(min(diff_soir_matin)), "\n")
cat("  Max     :", round(max(diff_soir_matin)), "\n")
cat("  % jours où soir > matin :",
    round(mean(diff_soir_matin > 0) * 100, 1), "%\n\n")

# --- Vérification normalité des différences -----------------
cat("--- Normalité des différences (Shapiro-Wilk) ---\n")
cat("On teste la normalité des DIFFÉRENCES (h17 - h8)\n")
cat("C'est ce qui compte pour le test apparié\n\n")
sw_diff <- shapiro.test(diff_soir_matin)
cat("W =", round(sw_diff$statistic, 4),
    "| p =", format(sw_diff$p.value, scientific = TRUE, digits = 3),
    "| Normal :", ifelse(sw_diff$p.value > 0.05, "OUI", "NON"), "\n")
cat("→ Si NON : Wilcoxon signed-rank (non-paramétrique)\n")
cat("→ Si OUI : test t apparié possible\n\n")

# --- Test de Wilcoxon apparié -------------------------------
cat("--- Test de Wilcoxon signed-rank (apparié) ---\n")
cat("H0 : médiane des différences = 0\n")
cat("H1 : médiane des différences > 0 (soir > matin)\n\n")

wilcox_apparie <- wilcox.test(
  freq_apparie$h17,
  freq_apparie$h8,
  paired      = TRUE,          # CRUCIAL : appariement par date
  alternative = "greater",     # test unilatéral : soir > matin
  conf.int    = TRUE,
  conf.level  = 0.95
)
print(wilcox_apparie)

cat("\nV =", wilcox_apparie$statistic,
    "| p =", format(wilcox_apparie$p.value, scientific = TRUE, digits = 3), "\n")
cat(ifelse(wilcox_apparie$p.value < 0.05,
           "REJET H0 — le pic soir est significativement > pic matin",
           "NON-REJET H0"), "\n")
cat("Estimation Hodges-Lehmann :",
    round(wilcox_apparie$estimate), "montées\n")
cat("IC 95% inférieur           :",
    round(wilcox_apparie$conf.int[1]), "montées\n\n")

# --- Taille d'effet -----------------------------------------
# r = Z / sqrt(N) pour Wilcoxon apparié
n_paires <- nrow(freq_apparie)
z_score  <- qnorm(wilcox_apparie$p.value)
r_effet  <- abs(z_score) / sqrt(n_paires)

cat("Taille d'effet r =", round(r_effet, 3), "\n")
cat("Interprétation : <0.1 négligeable | 0.1-0.3 petit",
    "| 0.3-0.5 moyen | >0.5 grand\n\n")

# --- Conclusion formelle T-003 ------------------------------
cat("--- CONCLUSION T-003 ---\n")
cat("Le pic du soir (17h) est-il significativement\n")
cat("plus élevé que le pic du matin (8h) ?\n\n")
cat("Médiane matin (8h)  :", round(median(freq_apparie$h8)), "montées\n")
cat("Médiane soir (17h)  :", round(median(freq_apparie$h17)), "montées\n")
cat("Différence médiane  :", round(median(diff_soir_matin)), "montées\n")
cat("% jours soir > matin:",
    round(mean(diff_soir_matin > 0) * 100, 1), "%\n")
cat("p-value             :",
    format(wilcox_apparie$p.value, scientific = TRUE, digits = 3), "\n")
cat("Taille d'effet r    :", round(r_effet, 3), "\n")
cat("Statut T-003        : COMPLÉTÉ\n")

# NOTE VIZ : boxplot apparié matin vs soir par jour de semaine
# NOTE VIZ : courbe densité des différences soir-matin

# =============================================================
# TEST STATISTIQUE — T-005
# Hypothèse : Le mercredi a un profil horaire significativement
#             différent des autres jours de semaine
# Approche  : Double approche
#   1. Heures ciblées (8h, 12h, 17h) — Kruskal-Wallis
#      + comparaisons Mercredi vs autres jours (Dunn)
#   2. Toutes les heures — Kruskal-Wallis par tranche horaire
#      avec correction de Bonferroni (inflation alpha)
#
# Pourquoi Kruskal-Wallis et pas ANOVA ?
#   → Normalité violée dans T-001 pour des données similaires
#   → On applique le même principe de rigueur ici
#   → Kruskal-Wallis est notre test de référence par défaut
#     dès que la normalité n'est pas vérifiée
#
# Pourquoi la correction de Bonferroni ?
#   → Approche 2 : on fait 24 tests (une par heure)
#   → Sans correction : risque alpha global = 1-(0.95)^24 = 71%
#   → Avec Bonferroni : seuil ajusté = 0.05/24 = 0.0021
#   → Benjamini-Hochberg (BH) aussi calculé — moins conservateur
#     que Bonferroni, meilleur contrôle du taux de faux positifs
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-005 — Profil horaire du mercredi\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

# --- Préparation --------------------------------------------
# Données : jours NORMAL uniquement, agrégées par date et heure
freq_profil <- freq_horaire |>
  filter(horaire_type == "NORMAL") |>
  group_by(date, jour, heure) |>
  summarise(
    montees = sum(nb_de_montees, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    est_mercredi = ifelse(jour == "Mercredi", "Mercredi", "Autres jours")
  )

cat("Observations disponibles :\n")
print(freq_profil |>
        group_by(est_mercredi) |>
        summarise(n_dates = n_distinct(date),
                  n_obs   = n(),
                  .groups = "drop"))

# ============================================================
# APPROCHE 1 — Heures ciblées : 8h, 12h, 17h
# ============================================================
cat("\n", paste(rep("-", 50), collapse = ""), "\n")
cat("APPROCHE 1 — Heures ciblées : 8h, 12h, 17h\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")
cat("Hypothèse spécifique : le mercredi se distingue\n")
cat("particulièrement à 12h (fin des cours à 11h30)\n\n")

heures_cibles <- c(8, 12, 17)

for (h in heures_cibles) {
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat("Heure :", h, "h\n\n")
  
  donnees_h <- freq_profil |>
    filter(heure == h)
  
  # Statistiques descriptives
  stats_h <- donnees_h |>
    group_by(est_mercredi) |>
    summarise(
      n       = n(),
      mediane = round(median(montees)),
      moyenne = round(mean(montees)),
      .groups = "drop"
    )
  print(stats_h)
  
  diff_pct <- round(
    (stats_h$mediane[stats_h$est_mercredi == "Mercredi"] /
       stats_h$mediane[stats_h$est_mercredi == "Autres jours"] - 1) * 100, 1)
  cat("Écart médiane Mercredi vs Autres :", diff_pct, "%\n\n")
  
  # Kruskal-Wallis
  kw_h <- kruskal.test(montees ~ est_mercredi, data = donnees_h)
  cat("Kruskal-Wallis : χ²=", round(kw_h$statistic, 3),
      "| p =", format(kw_h$p.value, scientific = TRUE, digits = 3), "\n")
  cat("Conclusion :",
      ifelse(kw_h$p.value < 0.05,
             "SIGNIFICATIF — Mercredi diffère à cette heure",
             "Non significatif"), "\n\n")
  
  # Taille d'effet eta² (approximation depuis KW)
  n_tot <- nrow(donnees_h)
  eta2_kw <- (kw_h$statistic - 1) / (n_tot - 1)
  cat("Taille d'effet η² ≈", round(eta2_kw, 4), "\n\n")
}

# ============================================================
# APPROCHE 2 — Toutes les heures avec correction Bonferroni
# ============================================================
cat("\n", paste(rep("-", 50), collapse = ""), "\n")
cat("APPROCHE 2 — Toutes les heures (0h à 23h)\n")
cat("Correction Bonferroni et Benjamini-Hochberg\n")
cat(paste(rep("-", 50), collapse = ""), "\n\n")

cat("Seuil Bonferroni : α/24 =",
    round(0.05 / 24, 5), "(très conservateur)\n")
cat("Seuil BH         : contrôle FDR à 5%",
    "(moins conservateur)\n\n")

# Test KW pour chaque heure
# Précaution : certaines tranches horaires (nuit profonde)
# peuvent n'avoir qu'un seul groupe — on les exclut
resultats_heures <- freq_profil |>
  group_by(heure) |>
  filter(
    n_distinct(est_mercredi) == 2,  # les deux groupes doivent exister
    sum(est_mercredi == "Mercredi") >= 5,   # au moins 5 obs mercredi
    sum(est_mercredi == "Autres jours") >= 5 # au moins 5 obs autres
  ) |>
  summarise(
    kw_stat    = kruskal.test(montees ~ est_mercredi)$statistic,
    kw_pvalue  = kruskal.test(montees ~ est_mercredi)$p.value,
    med_mercr  = median(montees[est_mercredi == "Mercredi"]),
    med_autres = median(montees[est_mercredi == "Autres jours"]),
    n_mercr    = sum(est_mercredi == "Mercredi"),
    n_autres   = sum(est_mercredi == "Autres jours"),
    .groups    = "drop"
  ) |>
  mutate(
    diff_pct       = round((med_mercr / med_autres - 1) * 100, 1),
    p_bonferroni   = p.adjust(kw_pvalue, method = "bonferroni"),
    p_bh           = p.adjust(kw_pvalue, method = "BH"),
    sig_bonferroni = ifelse(p_bonferroni < 0.05, "✓", ""),
    sig_bh         = ifelse(p_bh < 0.05, "✓", "")
  ) |>
  arrange(heure)

cat("Heures testées :", nrow(resultats_heures),
    "(heures avec données insuffisantes exclues)\n\n")

cat("Résultats par heure :\n")
cat(sprintf("%-6s %-10s %-10s %-10s %-8s %-8s %-8s\n",
            "Heure", "p brute", "p Bonferr.", "p BH",
            "Diff%", "Sig.BF", "Sig.BH"))
cat(paste(rep("-", 65), collapse = ""), "\n")

for (i in 1:nrow(resultats_heures)) {
  r <- resultats_heures[i, ]
  cat(sprintf("%-6s %-10s %-10s %-10s %-8s %-8s %-8s\n",
              paste0(r$heure, "h"),
              format(r$kw_pvalue, digits = 3, scientific = TRUE),
              format(r$p_bonferroni, digits = 3, scientific = TRUE),
              format(r$p_bh, digits = 3, scientific = TRUE),
              paste0(r$diff_pct, "%"),
              r$sig_bonferroni,
              r$sig_bh))
}

# Résumé
n_sig_bf <- sum(resultats_heures$sig_bonferroni == "✓")
n_sig_bh <- sum(resultats_heures$sig_bh == "✓")
heures_sig_bf <- resultats_heures$heure[resultats_heures$sig_bonferroni == "✓"]
heures_sig_bh <- resultats_heures$heure[resultats_heures$sig_bh == "✓"]

cat("\n--- Résumé ---\n")
cat("Heures significatives après Bonferroni :", n_sig_bf, "\n")
if (n_sig_bf > 0) cat(" →", paste(heures_sig_bf, collapse = ", "), "h\n")
cat("Heures significatives après BH         :", n_sig_bh, "\n")
if (n_sig_bh > 0) cat(" →", paste(heures_sig_bh, collapse = ", "), "h\n")

# --- Conclusion formelle T-005 ------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CONCLUSION T-005\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("Le mercredi a-t-il un profil horaire\n")
cat("significativement différent des autres jours ?\n\n")
cat("Approche 1 (heures ciblées) :\n")
cat("  8h  : voir résultats ci-dessus\n")
cat("  12h : voir résultats ci-dessus\n")
cat("  17h : voir résultats ci-dessus\n")
cat("Approche 2 (Bonferroni) :", n_sig_bf, "heures significatives\n")
cat("Approche 2 (BH)         :", n_sig_bh, "heures significatives\n")
cat("Statut T-005 : COMPLÉTÉ\n")

# NOTE VIZ : heatmap des p-values par heure (mercredi vs autres)
# NOTE VIZ : courbe profil mercredi vs moyenne autres jours avec IC



# =============================================================
# TEST STATISTIQUE — T-005b
# Extension de T-005 : cartographie complète
# tous les jours × toutes les heures
#
# Objectif : pour chaque jour de semaine et chaque heure,
# tester si ce jour diffère significativement des autres jours
# à cette heure précise.
#
# Résultat : une matrice 5 jours × 23 heures de p-values
# et de tailles d'effet — cartographie complète des
# profils horaires différentiels
#
# Méthode : même approche que T-005
#   → KW pour chaque combinaison jour × heure
#   → Correction BH (moins conservatrice que Bonferroni
#     quand le nombre de tests est très élevé : 5×23 = 115)
#   → Bonferroni aussi calculé pour référence
#
# Pourquoi BH privilégiée ici ?
#   → Avec 115 tests simultanés, Bonferroni devient
#     excessivement conservateur : seuil = 0.05/115 = 0.00043
#   → Le risque de manquer de vrais effets (faux négatifs)
#     devient trop élevé
#   → BH offre un meilleur équilibre puissance/rigueur
#     pour les analyses exploratoires à grand nombre de tests
# =============================================================

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("TEST T-005b — Matrice complète jours × heures\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")

cat("Construction de la matrice 5 jours × heures\n")
cat("de p-values et tailles d'effet...\n\n")

jours_semaine <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi")

# --- Construction de la matrice complète --------------------
# Pour chaque jour et chaque heure :
# on compare ce jour vs tous les autres jours NORMAL

resultats_matrice <- data.frame()

for (j in jours_semaine) {
  
  donnees_jour <- freq_profil |>
    mutate(est_ce_jour = ifelse(jour == j, j, "Autres"))
  
  resultats_jour <- donnees_jour |>
    group_by(heure) |>
    filter(
      n_distinct(est_ce_jour) == 2,
      sum(est_ce_jour == j) >= 5,
      sum(est_ce_jour == "Autres") >= 5
    ) |>
    summarise(
      kw_stat    = kruskal.test(montees ~ est_ce_jour)$statistic,
      kw_pvalue  = kruskal.test(montees ~ est_ce_jour)$p.value,
      med_jour   = median(montees[est_ce_jour == j]),
      med_autres = median(montees[est_ce_jour == "Autres"]),
      n_jour     = sum(est_ce_jour == j),
      n_autres   = sum(est_ce_jour == "Autres"),
      .groups    = "drop"
    ) |>
    mutate(
      jour       = j,
      diff_pct   = round((med_jour / med_autres - 1) * 100, 1),
      # Taille d'effet eta² depuis KW
      eta2       = round((kw_stat - 1) / (n_jour + n_autres - 1), 4)
    )
  
  resultats_matrice <- bind_rows(resultats_matrice, resultats_jour)
}

# Correction multiplicité sur l'ensemble des tests
n_tests_total <- nrow(resultats_matrice)
cat("Nombre total de tests effectués :", n_tests_total, "\n")
cat("Seuil Bonferroni : 0.05 /", n_tests_total, "=",
    round(0.05 / n_tests_total, 6), "\n\n")

resultats_matrice <- resultats_matrice |>
  mutate(
    p_bonferroni   = p.adjust(kw_pvalue, method = "bonferroni"),
    p_bh           = p.adjust(kw_pvalue, method = "BH"),
    sig_bf         = p_bonferroni < 0.05,
    sig_bh         = p_bh < 0.05,
    # Catégorie de l'effet pour la visualisation
    direction      = case_when(
      diff_pct > 5  ~ "Plus chargé",
      diff_pct < -5 ~ "Moins chargé",
      TRUE          ~ "Neutre"
    )
  )

# --- Résumé par jour ----------------------------------------
cat("=== Résumé par jour (tests significatifs après BH) ===\n\n")

for (j in jours_semaine) {
  res_j <- resultats_matrice |> filter(jour == j)
  n_sig_bh <- sum(res_j$sig_bh, na.rm = TRUE)
  n_sig_bf <- sum(res_j$sig_bf, na.rm = TRUE)
  heures_sig <- res_j$heure[res_j$sig_bh & !is.na(res_j$sig_bh)]
  plus_charge <- res_j$heure[res_j$sig_bh &
                               res_j$diff_pct > 5 & !is.na(res_j$sig_bh)]
  moins_charge <- res_j$heure[res_j$sig_bh &
                                res_j$diff_pct < -5 & !is.na(res_j$sig_bh)]
  
  cat(j, ":\n")
  cat("  Heures sig. BF :", n_sig_bf, "| BH :", n_sig_bh, "\n")
  if (length(plus_charge) > 0)
    cat("  Plus chargé    :", paste(plus_charge, collapse = ", "), "h\n")
  if (length(moins_charge) > 0)
    cat("  Moins chargé   :", paste(moins_charge, collapse = ", "), "h\n")
  
  # Heure avec la plus grande différence
  max_eta <- res_j |> slice_max(eta2, n = 1, with_ties = FALSE)
  cat("  Effet max (η²) :", max_eta$heure, "h |",
      "η² =", max_eta$eta2, "| diff =",
      max_eta$diff_pct, "%\n\n")
}

# --- Tableau détaillé des effets forts ----------------------
cat("=== Effets forts (η² > 0.05) après BH ===\n\n")
effets_forts <- resultats_matrice |>
  filter(sig_bh == TRUE, eta2 > 0.05) |>
  arrange(desc(eta2)) |>
  select(jour, heure, diff_pct, eta2, p_bh)

if (nrow(effets_forts) > 0) {
  cat(sprintf("%-12s %-6s %-8s %-8s %-12s\n",
              "Jour", "Heure", "Diff%", "η²", "p BH"))
  cat(paste(rep("-", 50), collapse = ""), "\n")
  for (i in 1:nrow(effets_forts)) {
    r <- effets_forts[i, ]
    cat(sprintf("%-12s %-6s %-8s %-8s %-12s\n",
                r$jour,
                paste0(r$heure, "h"),
                paste0(r$diff_pct, "%"),
                r$eta2,
                format(r$p_bh, scientific = TRUE, digits = 3)))
  }
} else {
  cat("Aucun effet fort détecté\n")
}

# --- Conclusion T-005b --------------------------------------
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("CONCLUSION T-005b\n")
cat(paste(rep("=", 60), collapse = ""), "\n\n")
cat("Cartographie complète des profils horaires différentiels\n")
cat("par jour de semaine sur jours NORMAL (2019-2026)\n\n")

total_sig_bh <- sum(resultats_matrice$sig_bh, na.rm = TRUE)
total_sig_bf <- sum(resultats_matrice$sig_bf, na.rm = TRUE)
cat("Tests totaux effectués :", n_tests_total, "\n")
cat("Significatifs après BH :", total_sig_bh,
    "(", round(total_sig_bh / n_tests_total * 100, 1), "% )\n")
cat("Significatifs après BF :", total_sig_bf,
    "(", round(total_sig_bf / n_tests_total * 100, 1), "% )\n")
cat("\nStatut T-005b : COMPLÉTÉ\n")

# Sauvegarde des résultats pour visualisation future
saveRDS(resultats_matrice,
        "outputs/T005b_matrice_jours_heures.rds")
message("✓ Matrice résultats sauvegardée")

# NOTE VIZ : heatmap matrice jours × heures colorée par
#            direction (rouge = plus chargé / bleu = moins chargé)
#            et intensité = taille d'effet η²
# NOTE VIZ : graphique en radar (spider chart) par jour
#            montrant le profil différentiel heure par heure