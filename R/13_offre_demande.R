# ── SCRIPT 13 — OFFRE VS DEMANDE : KM PRODUITS × FRÉQUENTATION ──────────────
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : mai 2026
# Données: km_prod.rds, journalier.rds, mensuel.rds
# ─────────────────────────────────────────────────────────────────────────────
#
# APPROCHE : inductive — exploration visuelle avant hypothèse.
# Synergies traitées : SYN-005, SYN-006, SYN-010
#
# EXCLUSIONS (cohérentes script 10 — DEC-015) :
#   Lignes CX        : ratio artificiel, courses courtes dédiées
#   Noctambus        : service en extinction, données partielles
#   GLCT             : biais de périmètre post-appel d'offres 2023
#
# TESTS FORMELS :
#   T-013a — MW bilatéral : ratio montées/km NORMAL vs VACANCES (SYN-005)
#   T-013b — MW bilatéral : ratio montées/km TRAM vs BUS (SYN-010)
#   T-013c — Spearman     : tendance temporelle du ratio réseau (SYN-006)
# ─────────────────────────────────────────────────────────────────────────────

# =============================================================================
# 1. SETUP
# =============================================================================

rm(list = ls())
gc()

source("00_palette.R")

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(here)
library(forecast)

# =============================================================================
# 2. CHARGEMENT
# =============================================================================

journalier <- readRDS(here::here("data/raw/journalier.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne),
         date  = as.Date(date))

km_prod <- readRDS(here::here("data/raw/km_prod.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne),
         date  = as.Date(date))

mensuel <- readRDS(here::here("data/raw/mensuel.rds")) %>%
  filter(donnees_definitives == TRUE, !is.na(ligne)) %>%
  mutate(ligne = as.character(ligne),
         date  = ym(mois))

cat("=== CHARGEMENT ===\n")
cat("journalier :", nrow(journalier), "lignes |",
    format(min(journalier$date)), "->", format(max(journalier$date)), "\n")
cat("km_prod    :", nrow(km_prod), "lignes |",
    format(min(km_prod$date)), "->", format(max(km_prod$date)), "\n")
cat("mensuel    :", nrow(mensuel), "lignes |",
    format(min(mensuel$date)), "->", format(max(mensuel$date)), "\n\n")

# Identifier la colonne km dans km_prod (km_prod ou km_produits selon version API)
col_km <- if ("km_produits" %in% names(km_prod)) "km_produits" else "km_prod"
cat("Colonne km identifiee :", col_km, "\n\n")
km_prod <- km_prod %>% rename(km_col = !!sym(col_km))

# =============================================================================
# 3. EXPLORATION PRÉLIMINAIRE
# =============================================================================
# Regarder AVANT de décider. Règle inductive.

cat("--- Types de lignes km_prod ---\n")
km_prod %>% count(ligne_type_act, sort = TRUE) %>% print()

cat("\n--- Lignes communes journalier x km_prod ---\n")
lignes_communes <- intersect(unique(journalier$ligne), unique(km_prod$ligne))
cat("Lignes communes :", length(lignes_communes), "\n")

lignes_km_seul <- setdiff(unique(km_prod$ligne), unique(journalier$ligne))
cat("Uniquement km_prod (historiques pre-2023) :", length(lignes_km_seul), "\n\n")

# =============================================================================
# 4. EXCLUSIONS — COHÉRENTES AVEC SCRIPT 10
# =============================================================================

lignes_cx    <- c("C1","C3","C4","C5","C6","C7","C8","C9")
lignes_nocta <- c("NC","ND","NE","NJ","NK","NM","NP","NS","NT","NV","NO",
                  "NB1","NB2","NB3","NB4","NB5","NB6")
lignes_glct  <- km_prod %>%
  filter(ligne_type_act == "GLCT") %>%
  distinct(ligne) %>%
  pull(ligne)

exclus <- c(lignes_cx, lignes_nocta, lignes_glct)

cat("=== EXCLUSIONS ===\n")
cat("CX        :", length(lignes_cx), "lignes\n")
cat("Noctambus :", length(lignes_nocta), "lignes\n")
cat("GLCT      :", length(lignes_glct), "lignes —",
    paste(lignes_glct, collapse = ", "), "\n")
cat("Total exclus :", length(exclus), "\n\n")

# =============================================================================
# 5. SYN-005 — RIGIDITÉ DE L'OFFRE : L'OFFRE SUIT-ELLE LA DEMANDE ?
# =============================================================================
# T-002 a prouvé que les montees baissent de -28.2% en VACANCES.
# Question inductive : l'offre (km produits) baisse-t-elle dans les
# memes proportions ? Si l'ecart est grand -> offre structurellement rigide.
#
# H0 : ratio montees/km identique entre NORMAL et VACANCES
# H1 : ratio different (bilateral — direction prouvee par IC)
# Test : Mann-Whitney bilateral (non-parametrique, confirme par Shapiro)
# Agregation : un ratio par jour reseau -> evite la pseudoreplication

# --- 5.1 Agrégation journalière ---

# Montees agregees par date x ligne x horaire_type (somme sur arrets)
montees_jour_ligne <- journalier %>%
  filter(!ligne %in% exclus,
         ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE")) %>%
  group_by(date, ligne, ligne_type_act, horaire_type) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

# km produits agreges par date x ligne
km_jour_ligne <- km_prod %>%
  filter(!ligne %in% exclus,
         ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE")) %>%
  group_by(date, ligne, ligne_type_act) %>%
  summarise(km = sum(km_col, na.rm = TRUE), .groups = "drop")

# Jointure sur date x ligne — horaire_type herite du cote journalier
offre_demande <- montees_jour_ligne %>%
  inner_join(km_jour_ligne, by = c("date", "ligne", "ligne_type_act")) %>%
  filter(km > 0)

cat("=== DATASET OFFRE x DEMANDE ===\n")
cat("Observations (ligne x jour) :", nrow(offre_demande), "\n")
cat("Lignes distinctes           :", n_distinct(offre_demande$ligne), "\n")
cat("Periode :", format(min(offre_demande$date)), "->",
    format(max(offre_demande$date)), "\n\n")

# Un ratio par jour (reseau entier) -> evite pseudoreplication
reseau_jour <- offre_demande %>%
  group_by(date, horaire_type) %>%
  summarise(
    montees_tot = sum(montees),
    km_tot      = sum(km),
    ratio       = sum(montees) / sum(km),
    .groups     = "drop"
  ) %>%
  filter(horaire_type %in% c("NORMAL", "VACANCES"))

# --- 5.2 Exploration : amplitude comparée ---

cat("--- Statistiques par periode (reseau, par jour) ---\n")
reseau_jour %>%
  group_by(horaire_type) %>%
  summarise(
    n           = n(),
    med_montees = round(median(montees_tot)),
    med_km      = round(median(km_tot)),
    med_ratio   = round(median(ratio), 4),
    .groups     = "drop"
  ) %>%
  print()

med_norm_mont <- median(reseau_jour$montees_tot[reseau_jour$horaire_type == "NORMAL"])
med_norm_km   <- median(reseau_jour$km_tot[reseau_jour$horaire_type == "NORMAL"])
med_vac_mont  <- median(reseau_jour$montees_tot[reseau_jour$horaire_type == "VACANCES"])
med_vac_km    <- median(reseau_jour$km_tot[reseau_jour$horaire_type == "VACANCES"])

baisse_mont_pct <- round((med_vac_mont / med_norm_mont - 1) * 100, 1)
baisse_km_pct   <- round((med_vac_km   / med_norm_km   - 1) * 100, 1)
ecart_pp        <- round(baisse_mont_pct - baisse_km_pct, 1)

cat("\n--- AMPLITUDE COMPAREE ---\n")
cat("Baisse montees VACANCES vs NORMAL :", baisse_mont_pct, "%\n")
cat("Baisse km_prod VACANCES vs NORMAL :", baisse_km_pct, "%\n")
cat("Ecart                             :", ecart_pp, "pp\n")
if (abs(ecart_pp) > 5) {
  cat("-> L'offre et la demande ne bougent PAS dans les memes proportions\n")
  cat("   L'offre est structurellement plus rigide que la demande\n")
} else {
  cat("-> L'offre et la demande bougent dans des proportions similaires\n")
}

# --- 5.3 Test de normalité ---

cat("\n--- Normalite du ratio journalier (Shapiro-Wilk, echantillon <= 5000) ---\n")
for (grp in c("NORMAL", "VACANCES")) {
  vals <- reseau_jour$ratio[reseau_jour$horaire_type == grp]
  s    <- if (length(vals) > 5000) sample(vals, 5000) else vals
  sw   <- shapiro.test(s)
  cat(sprintf("  %-10s n=%-4d W=%.3f p=%.4f %s\n",
              grp, length(vals), sw$statistic, sw$p.value,
              ifelse(sw$p.value < 0.05, "-> NON normale", "-> normale")))
}

# --- 5.4 Test T-013a : Mann-Whitney NORMAL vs VACANCES ---

cat("\n=== T-013a — MANN-WHITNEY : ratio montees/km NORMAL vs VACANCES ===\n")

mw_t013a <- wilcox.test(
  reseau_jour$ratio[reseau_jour$horaire_type == "NORMAL"],
  reseau_jour$ratio[reseau_jour$horaire_type == "VACANCES"],
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

n_t013a <- nrow(reseau_jour)
z_t013a <- qnorm(mw_t013a$p.value / 2)
r_t013a <- abs(z_t013a) / sqrt(n_t013a)

cat("NORMAL   mediane :",
    round(median(reseau_jour$ratio[reseau_jour$horaire_type == "NORMAL"]), 4),
    "montees/km\n")
cat("VACANCES mediane :",
    round(median(reseau_jour$ratio[reseau_jour$horaire_type == "VACANCES"]), 4),
    "montees/km\n")
cat("W (statistique)  :", mw_t013a$statistic, "\n")
cat("p-value          :", format(mw_t013a$p.value, scientific = TRUE), "\n")
cat("Hodges-Lehmann   :", round(mw_t013a$estimate, 4), "montees/km\n")
cat("IC 95%           : [", round(mw_t013a$conf.int[1], 4),
    ";", round(mw_t013a$conf.int[2], 4), "]\n")
cat("Taille effet r   :", round(r_t013a, 3), "\n")
cat("->", ifelse(mw_t013a$p.value < 0.05,
               "REJET H0 — ratio significativement different",
               "NON-REJET H0"), "\n\n")

cat("--- Conclusions formelles T-013a ---\n")
cat("Ce qu'on peut affirmer :\n")
if (mw_t013a$p.value < 0.05) {
  cat("  - Le ratio montees/km est significativement plus bas en VACANCES\n")
  cat("    (p =", format(mw_t013a$p.value, scientific = TRUE), ")\n")
  cat("  - Montees :", baisse_mont_pct, "% | km_prod :", baisse_km_pct, "%\n")
  cat("  - Ecart :", ecart_pp, "pp -> l'offre est structurellement\n")
  cat("    plus rigide que la demande en periode de vacances\n")
} else {
  cat("  - Pas de difference significative du ratio entre NORMAL et VACANCES\n")
  cat("  - Malgre l'ecart descriptif de", ecart_pp, "pp, la variabilite\n")
  cat("    intra-groupe absorbe la difference\n")
}
cat("Ce qu'on NE peut PAS affirmer :\n")
cat("  - Que cette rigidite est un dysfonctionnement : les contraintes\n")
cat("    contractuelles et operationnelles (rotations conducteurs, depots)\n")
cat("    limitent la flexibilite a court terme — non mesurable ici\n")
cat("  - Que l'offre pourrait etre reduite sans impact sur le service\n")
cat("    de base garanti aux usagers presents en vacances\n")

# [TPG] L'ecart montees/km VACANCES vs NORMAL (baisse_km_pct vs baisse_mont_pct)
# quantifie le surdimensionnement de l'offre en vacances. A croiser avec les
# donnees de cout marginal d'exploitation par ligne (non publiques) pour
# estimer le cout de cette rigidite. Question pertinente pour la planification
# du contrat de prestations 2025-2029.

# =============================================================================
# 6. SYN-006 — PROXY VITESSE COMMERCIALE : ÉVOLUTION TEMPORELLE 2016-2026
# =============================================================================
# Hypothese inductive apres exploration :
# Si le ratio montees/km augmente au fil du temps -> le reseau devient plus efficient.
# Si il baisse -> la croissance de l'offre depasse la croissance de la demande.
# On teste la tendance : Spearman ratio x date (monotone, sans hypothese de linearite).

# --- 6.1 Ratio par ligne sur la période commune (jours NORMAL uniquement) ---

ratio_par_ligne <- offre_demande %>%
  filter(horaire_type == "NORMAL") %>%
  group_by(ligne, ligne_type_act) %>%
  summarise(
    montees_tot = sum(montees),
    km_tot      = sum(km),
    ratio       = sum(montees) / sum(km),
    n_jours     = n_distinct(date),
    .groups     = "drop"
  ) %>%
  filter(km_tot > 0, montees_tot > 0) %>%
  arrange(desc(ratio))

cat("\n=== 6. SYN-006 — RATIO PAR LIGNE (NORMAL, avr 2023 ->) ===\n")
cat("--- TOP 10 ---\n")
ratio_par_ligne %>%
  head(10) %>%
  dplyr::select(ligne, ligne_type_act, montees_tot, km_tot, ratio, n_jours) %>%
  mutate(montees_tot = round(montees_tot / 1e6, 2),
         km_tot      = round(km_tot / 1e6, 2)) %>%
  print()

cat("\n--- BOTTOM 10 ---\n")
ratio_par_ligne %>%
  tail(10) %>%
  dplyr::select(ligne, ligne_type_act, montees_tot, km_tot, ratio, n_jours) %>%
  mutate(montees_tot = round(montees_tot / 1e6, 2),
         km_tot      = round(km_tot / 1e6, 2)) %>%
  print()

# --- 6.2 Évolution mensuelle 2016-2026 (mensuel.rds × km_prod agrégés) ---

mensuel_agg <- mensuel %>%
  filter(!ligne %in% exclus,
         ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE"),
         !is.na(nb_de_montees)) %>%
  group_by(date) %>%
  summarise(montees_tot = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

km_agg_mois <- km_prod %>%
  filter(!ligne %in% exclus,
         ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE")) %>%
  mutate(mois_date = floor_date(date, "month")) %>%
  group_by(mois_date) %>%
  summarise(km_tot = sum(km_col, na.rm = TRUE), .groups = "drop") %>%
  rename(date = mois_date)

ratio_mensuel <- mensuel_agg %>%
  inner_join(km_agg_mois, by = "date") %>%
  filter(km_tot > 0, montees_tot > 0) %>%
  mutate(ratio = montees_tot / km_tot,
         annee = year(date),
         t     = as.numeric(date)) %>%
  arrange(date)

cat("\n--- Evolution annuelle du ratio reseau (mediane) ---\n")
ratio_mensuel %>%
  group_by(annee) %>%
  summarise(n = n(), med_ratio = round(median(ratio), 4), .groups = "drop") %>%
  print()

# --- 6.3 Test T-013c : tendance monotone Spearman ---

cat("\n=== T-013c — SPEARMAN : tendance temporelle du ratio 2016-2026 ===\n")
cat("Hypothese H0 : pas de tendance monotone du ratio dans le temps\n")
cat("Hypothese H1 : tendance monotone (hausse ou baisse) — bilateral\n\n")

# Exclure COVID (2020-2021) pour tester la tendance structurelle
ratio_hors_covid <- ratio_mensuel %>%
  filter(!(annee %in% c(2020, 2021)))

# --- Ljung-Box : autocorrelation de la serie hors COVID (prerequis Spearman) ---
# H0 : residus independants -> Spearman valide
# H1 : autocorrelation (p < 0.05) -> p-value Spearman biaisee -> retrograder en LOESS

lb_t013c      <- Box.test(ratio_hors_covid$ratio, lag = 12, type = "Ljung-Box")
ljung_autocorr <- lb_t013c$p.value < 0.05

cat("--- Ljung-Box (lag=12) : autocorrelation serie ratio hors COVID ---\n")
cat("X-squared :", round(lb_t013c$statistic, 3),
    "| df =", lb_t013c$parameter, "\n")
cat("p-value   :", format(lb_t013c$p.value, digits = 4, scientific = FALSE), "\n")
cat("->", ifelse(ljung_autocorr,
               "AUTOCORRELATION CONFIRMEE (p < 0.05) — Spearman retrogradee en descriptif LOESS",
               "Pas d'autocorrelation significative — Spearman valide"), "\n\n")

sp_t013c <- cor.test(ratio_hors_covid$t, ratio_hors_covid$ratio,
                     method = "spearman")

cat("Serie hors COVID (2020-2021) : n =", nrow(ratio_hors_covid), "mois\n")
if (!ljung_autocorr) {
  cat("Spearman rho :", round(sp_t013c$estimate, 3), "\n")
  cat("p-value      :", format(sp_t013c$p.value, scientific = TRUE), "\n")
  cat("->", ifelse(sp_t013c$p.value < 0.05,
                 ifelse(sp_t013c$estimate > 0,
                        "REJET H0 — ratio augmente : reseau plus efficient au fil du temps",
                        "REJET H0 — ratio baisse : croissance offre > croissance demande"),
                 "NON-REJET H0 — pas de tendance monotone claire"), "\n\n")
} else {
  cat("LIMITE : autocorrelation confirmee — p-value Spearman non fiable.\n")
  cat("Analyse retrogradee : tendance LOESS uniquement (visuelle, non testee).\n")
  cat("Spearman rho (informatif, NON conclusif) :",
      round(sp_t013c$estimate, 3), "\n\n")
}

# Note : on teste aussi sur serie complete pour reference
sp_complet <- cor.test(ratio_mensuel$t, ratio_mensuel$ratio,
                       method = "spearman")
cat("Reference serie complete (avec COVID) : rho =",
    round(sp_complet$estimate, 3),
    "p =", format(sp_complet$p.value, scientific = TRUE), "\n\n")

cat("--- Conclusions formelles T-013c ---\n")
cat("Ce qu'on peut affirmer :\n")
if (!ljung_autocorr && sp_t013c$p.value < 0.05) {
  if (sp_t013c$estimate > 0) {
    cat("  - Tendance haussiere du ratio montees/km (rho =",
        round(sp_t013c$estimate, 3), ") — reseau plus efficient\n")
    cat("  - La demande croit plus vite que l'offre sur la periode\n")
  } else {
    cat("  - Tendance baissiere du ratio montees/km (rho =",
        round(sp_t013c$estimate, 3), ")\n")
    cat("  - L'offre (km_prod) croit plus vite que la demande\n")
    cat("  - Peut refleter l'expansion du reseau (contrat 2025-2029)\n")
  }
} else if (ljung_autocorr) {
  cat("  - La tendance LOESS indique une evolution descriptive du ratio 2016-2026\n")
  cat("    (direction visible graphiquement — non testee formellement)\n")
  cat("  - Autocorrelation detectee (Ljung-Box p < 0.05) : Spearman retrogradee ;\n")
  cat("    rho =", round(sp_t013c$estimate, 3), "fourni a titre informatif seulement\n")
} else {
  cat("  - Pas de tendance monotone significative (rho =",
      round(sp_t013c$estimate, 3),
      "p =", format(sp_t013c$p.value, scientific = TRUE), ")\n")
  cat("  - Le ratio reste relativement stable hors choc COVID\n")
}
cat("Ce qu'on NE peut PAS affirmer :\n")
cat("  - Que la tendance est lineaire (Spearman teste la monotonie seule)\n")
cat("  - Que les variations ponctuelles refletent des decisions\n")
cat("    operationnelles (facteurs saisonniers non isoles ici)\n")
cat("  - Que la tendance reflete une evolution pure de l'efficience :\n")
cat("    le perimetre n'est pas strictement stable 2016-2026 (entrees et\n")
cat("    sorties de lignes meme apres exclusions CX/Noctambus/GLCT) —\n")
cat("    biais de composition non eliminable sur serie longue\n")

# =============================================================================
# 7. SYN-010 — TRAMS VS BUS : L'EFFICIENCE DU SITE PROPRE EST-ELLE PROUVÉE ?
# =============================================================================
# T-008 a montre graphiquement que les trams forment une droite parallele
# au-dessus de la regression principale. On le teste formellement.
#
# Lignes tram TPG (site propre integral) : 12, 14, 15, 17, 18
# Toutes les autres lignes = bus (PRINCIPAL ou SECONDAIRE dans ratio_par_ligne)
#
# H0 : ratio montees/km identique entre trams et bus
# H1 : different (bilateral — sens confirme par IC Hodges-Lehmann)
# Test : Mann-Whitney (non-parametrique — n_tram faible)
# Limite : n_tram = 5 -> puissance statistique reduite

lignes_tram <- c("12", "14", "15", "17", "18")

# Verification croisee : couverture temporelle et volume pour les 5 trams
cat("--- Verification croisee trams : n_jours et km_tot ---\n")
ratio_par_ligne %>%
  filter(ligne %in% lignes_tram) %>%
  mutate(km_M      = round(km_tot / 1e6, 3),
         montees_M = round(montees_tot / 1e6, 3)) %>%
  dplyr::select(ligne, ligne_type_act, n_jours, km_M, montees_M, ratio) %>%
  arrange(ligne) %>%
  print()
cat("\n")

trams_dispo <- intersect(lignes_tram, ratio_par_ligne$ligne)
cat("=== 7. SYN-010 — TRAMS VS BUS ===\n")
cat("Trams attendus  :", paste(lignes_tram, collapse = ", "), "\n")
cat("Trams dans data :", paste(trams_dispo, collapse = ", "), "\n\n")

ratio_mode <- ratio_par_ligne %>%
  mutate(mode = ifelse(ligne %in% lignes_tram, "Tram", "Bus"))

cat("--- Distribution par mode ---\n")
ratio_mode %>%
  group_by(mode) %>%
  summarise(
    n         = n(),
    med_ratio = round(median(ratio), 3),
    moy_ratio = round(mean(ratio), 3),
    min_ratio = round(min(ratio), 3),
    max_ratio = round(max(ratio), 3),
    .groups   = "drop"
  ) %>%
  print()

cat("\n--- Normalite par mode (Shapiro-Wilk) ---\n")
cat("Note : n_tram =", sum(ratio_mode$mode == "Tram"),
    "— test de faible puissance pour ce groupe\n")
for (md in c("Tram", "Bus")) {
  vals <- ratio_mode$ratio[ratio_mode$mode == md]
  if (length(vals) >= 3) {
    sw <- shapiro.test(vals)
    cat(sprintf("  %-5s n=%-3d W=%.3f p=%.4f %s\n",
                md, length(vals), sw$statistic, sw$p.value,
                ifelse(sw$p.value < 0.05, "-> NON normale", "-> normale")))
  }
}

cat("\n=== T-013b — MANN-WHITNEY : ratio montees/km TRAM vs BUS ===\n")
cat("Limite : n_tram =", sum(ratio_mode$mode == "Tram"),
    "-> puissance statistique reduite. Resultat a interpreter avec prudence.\n\n")

tram_r <- ratio_mode$ratio[ratio_mode$mode == "Tram"]
bus_r  <- ratio_mode$ratio[ratio_mode$mode == "Bus"]

mw_t013b <- wilcox.test(
  tram_r, bus_r,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

n_t013b <- length(tram_r) + length(bus_r)
z_t013b <- qnorm(mw_t013b$p.value / 2)
r_t013b <- abs(z_t013b) / sqrt(n_t013b)

cat("Tram mediane  :", round(median(tram_r), 3), "montees/km\n")
cat("Bus  mediane  :", round(median(bus_r), 3), "montees/km\n")
cat("Hodges-Lehmann:", round(mw_t013b$estimate, 3), "montees/km\n")
cat("IC 95%        : [", round(mw_t013b$conf.int[1], 3),
    ";", round(mw_t013b$conf.int[2], 3), "]\n")
cat("p-value       :", format(mw_t013b$p.value, scientific = TRUE), "\n")
cat("Taille effet r:", round(r_t013b, 3), "\n")
cat("->", ifelse(mw_t013b$p.value < 0.05,
               "REJET H0 — trams significativement plus efficients",
               "NON-REJET H0 — difference non prouvee formellement"), "\n\n")

cat("--- Conclusions formelles T-013b ---\n")
cat("Ce qu'on peut affirmer :\n")
if (mw_t013b$p.value < 0.05) {
  cat("  - Les trams generent plus de montees/km que les bus\n")
  cat("    (p =", format(mw_t013b$p.value, scientific = TRUE), ")\n")
  cat("  - Difference estimee : +", round(mw_t013b$estimate, 2), "montees/km\n")
  cat("  - IC 95% [", round(mw_t013b$conf.int[1], 2),
      ";", round(mw_t013b$conf.int[2], 2), "] — direction prouvee\n")
  cat("  - Coherent avec T-008 : les trams au-dessus de la droite de regression\n")
  cat("  - L'efficience structurelle du site propre est formellement etablie\n")
} else {
  cat("  - Difference descriptive (trams", round(median(tram_r), 3),
      "vs bus", round(median(bus_r), 3), ") non prouvee formellement\n")
  cat("  - Cause probable : n_tram = 5 -> puissance insuffisante\n")
  cat("  - Le signal visuel de T-008 reste la reference disponible\n")
}
cat("Ce qu'on NE peut PAS affirmer :\n")
cat("  - Que l'extension du reseau tram garantirait ce ratio :\n")
cat("    les trams desservent les axes les plus denses (correlation vs causalite)\n")
cat("  - Que les bus sont sous-performants : ils operent dans des\n")
cat("    contextes de densite structurellement differents\n")

# [TPG] Le ratio trams > bus chiffre l'argument d'investissement
# ferroviaire. A croiser avec les couts d'investissement km de voie
# (non publics) pour un ROI complet sur le contrat 2025-2029.

# =============================================================================
# 8. VISUALISATIONS
# NOTE VIZ : candidats Python / Power BI final
# =============================================================================

# --- VIZ 1 : Scatter km_prod x montees colore NORMAL vs VACANCES ---
# NOTE VIZ : bicolore — gap offre/demande visible en un coup d'oeil

p_scatter <- reseau_jour %>%
  ggplot(aes(x = km_tot / 1000, y = montees_tot / 1000,
             color = horaire_type, alpha = horaire_type)) +
  geom_point(size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
  scale_color_manual(
    values = c("NORMAL" = TPG_RED, "VACANCES" = "#4A6FA5"),
    labels = c("NORMAL" = "Jours NORMAL", "VACANCES" = "Jours VACANCES")
  ) +
  scale_alpha_manual(
    values = c("NORMAL" = 0.3, "VACANCES" = 0.65),
    guide  = "none"
  ) +
  scale_x_continuous(labels = label_number(suffix = "k km")) +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  annotate("text",
           x = quantile(reseau_jour$km_tot / 1000, 0.02),
           y = max(reseau_jour$montees_tot / 1000) * 0.95,
           label = paste0(
             "Montees vacances : ", baisse_mont_pct, "%\n",
             "km_prod vacances : ", baisse_km_pct, "%\n",
             "Ecart : ", ecart_pp, " pp"
           ),
           hjust = 0, size = 3, color = COL_REF) +
  labs(
    title    = "Offre vs Demande — km produits x montees par jour",
    subtitle = paste0(
      "Reseau PRINCIPAL + SECONDAIRE | avr. 2023 -> fev. 2026 | ",
      "T-013a MW p=", format(mw_t013a$p.value, digits = 2, scientific = TRUE),
      " r=", round(r_t013a, 3)
    ),
    x       = "km produits / jour (reseau)",
    y       = "Montees / jour (reseau)",
    color   = "Periode",
    caption = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0))

ggsave(here::here("figures/13_scatter_offre_demande.png"),
       p_scatter, width = 10, height = 6, dpi = 150)
cat("\nGraphique sauvegarde : figures/13_scatter_offre_demande.png\n")

# --- VIZ 2 : Evolution temporelle ratio montees/km 2016-2026 ---
# NOTE VIZ : serie longue avec ruptures — narrative COVID + efficience

y_max_r <- max(ratio_mensuel$ratio, na.rm = TRUE)

p_ratio_evo <- ggplot(ratio_mensuel, aes(x = date, y = ratio)) +
  geom_line(color = TPG_RED, linewidth = 0.7, alpha = 0.8) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE,
              color = COL_REF, linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dotted", color = COL_COVID, linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2019-12-01"),
             linetype = "dotted", color = COL_NEUTRE, linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2023-12-01"),
             linetype = "dotted", color = COL_NEUTRE, linewidth = 0.6) +
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dotted", color = COL_GRATUITE, linewidth = 0.9) +
  annotate("text", x = as.Date("2020-04-01"), y = y_max_r * 0.97,
           label = "COVID", hjust = 0, size = 2.8, color = COL_COVID) +
  annotate("text", x = as.Date("2019-10-01"), y = y_max_r * 0.84,
           label = "Leman\nExpress", hjust = 1, size = 2.5, color = COL_NEUTRE) +
  annotate("text", x = as.Date("2023-10-01"), y = y_max_r * 0.84,
           label = "Abs.\nNocta.", hjust = 1, size = 2.5, color = COL_NEUTRE) +
  annotate("text", x = as.Date("2025-02-01"), y = y_max_r * 0.97,
           label = "Gratuite\njeunes", hjust = 0, size = 2.8, color = COL_GRATUITE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Evolution du ratio montees/km produit — reseau TPG 2016-2026",
    subtitle = if (!ljung_autocorr) {
      paste0("PRINCIPAL + SECONDAIRE | Tendance Spearman rho=",
             round(sp_t013c$estimate, 3),
             " (hors COVID) | Pointille = tendance LOESS")
    } else {
      paste0("PRINCIPAL + SECONDAIRE | Tendance LOESS (Spearman retrogradee — autocorrelation) | ",
             "rho=", round(sp_t013c$estimate, 3), " informatif")
    },
    x       = NULL,
    y       = "Montees / km produit",
    caption = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave(here::here("figures/13_evolution_ratio_mensuel.png"),
       p_ratio_evo, width = 12, height = 6, dpi = 150)
cat("Graphique sauvegarde : figures/13_evolution_ratio_mensuel.png\n")

# --- VIZ 3 : Boxplot ratio montees/km Tram vs Bus ---
# NOTE VIZ : argument visuel fort pour investissement ferroviaire

p_tram_bus <- ggplot(ratio_mode,
                     aes(x = mode, y = ratio, fill = mode)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2.5,
               outlier.fill = "white", alpha = 0.85) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 2.5, color = "grey30") +
  geom_text(data = ratio_mode %>% filter(mode == "Tram"),
            aes(label = ligne),
            vjust = -0.9, size = 3.2, color = COL_REF) +
  scale_fill_manual(values = c("Tram" = TPG_RED, "Bus" = "#4A6FA5")) +
  labs(
    title    = "Efficience par mode — Trams vs Bus",
    subtitle = paste0(
      "Ratio montees/km (jours NORMAL) | ",
      "MW p=", format(mw_t013b$p.value, digits = 2, scientific = TRUE),
      " | r=", round(r_t013b, 3),
      " | Trams : ", paste(trams_dispo, collapse = ", ")
    ),
    x       = "Mode",
    y       = "Montees / km produit",
    caption = "Source : opendata.tpg.ch | avr. 2023 -> fev. 2026 | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(legend.position = "none")

ggsave(here::here("figures/13_boxplot_tram_bus.png"),
       p_tram_bus, width = 8, height = 6, dpi = 150)
cat("Graphique sauvegarde : figures/13_boxplot_tram_bus.png\n")

# =============================================================================
# 9. BILAN SCRIPT 13
# =============================================================================

cat("\n=== BILAN SCRIPT 13 ===\n\n")

cat("SYN-005 — Rigidite de l'offre en vacances :\n")
cat("  Baisse montees :", baisse_mont_pct, "% | Baisse km :", baisse_km_pct, "%\n")
cat("  Ecart           :", ecart_pp, "pp\n")
cat("  T-013a MW p =", format(mw_t013a$p.value, scientific = TRUE),
    "| HL =", round(mw_t013a$estimate, 4),
    "| r =", round(r_t013a, 3), "\n\n")

cat("SYN-006 — Tendance temporelle ratio 2016-2026 :\n")
cat("  Mediane 2016 :",
    round(median(ratio_mensuel$ratio[ratio_mensuel$annee == 2016]), 4), "\n")
cat("  Mediane 2025 :",
    round(median(ratio_mensuel$ratio[ratio_mensuel$annee == 2025], na.rm = TRUE), 4), "\n")
cat("  T-013c Spearman rho =", round(sp_t013c$estimate, 3),
    "p =", format(sp_t013c$p.value, scientific = TRUE),
    if (ljung_autocorr) "(informatif — autocorrelation confirmee)\n\n"
    else "(hors COVID)\n\n")

cat("SYN-010 — Trams vs Bus :\n")
cat("  Trams mediane :", round(median(tram_r), 3),
    "| Bus mediane :", round(median(bus_r), 3), "montees/km\n")
cat("  T-013b MW p =", format(mw_t013b$p.value, scientific = TRUE),
    "| r =", round(r_t013b, 3),
    "| Limite : n_tram =", length(tram_r), "\n\n")

cat("3 graphiques sauvegardes dans figures/\n")
cat("[TPG] Observations sensibles marquees # [TPG] dans le script\n")

message("Script 13 termine.")
