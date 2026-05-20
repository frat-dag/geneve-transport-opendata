# ── SCRIPT 14 — NOCTAMBUS REGIONAL 2016-2026 + LIGNE 10 AÉROPORT ───────────
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : mai 2026
# Données: mensuel.rds, journalier.rds, horaire.rds, km_prod.rds
# ─────────────────────────────────────────────────────────────────────────────
#
# APPROCHE : inductive — exploration visuelle avant hypothèse.
# Synergies : SYN-007 (Noctambus 2016-2026), SYN-008 / IDEE-005 (Ligne 10).
#
# TESTS FORMELS :
#   T-014a — Bai-Perron + Chow : ruptures série Noctambus (BP détecte, Chow confirme)
#   T-014b — MW      : fréquentation nocturne réseau avant/après déc 2023
#   T-014c — rang    : position ligne 10 dans distribution PRINCIPAL (purement descriptif)
#   T-014d — MW      : ratio VACANCES/NORMAL ligne 10 vs autres PRINCIPAL
#   T-014e — MW      : déclin pré-COVID Noctambus (avant/après 1re rupture Bai-Perron)
#
# LIMITE DOCUMENTÉE :
#   Pas de données horaires par ligne disponibles (horaire.rds = réseau global,
#   mn.rds = CCG uniquement). Le profil horaire ligne 10 individuel
#   ne peut pas être isolé — documenté comme limite et non contourné.
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
library(strucchange)

# =============================================================================
# 2. CHARGEMENT
# =============================================================================

mensuel <- readRDS(here::here("data/raw/mensuel.rds")) %>%
  filter(donnees_definitives == TRUE, !is.na(ligne)) %>%
  mutate(ligne = as.character(ligne),
         date  = ym(mois))

journalier <- readRDS(here::here("data/raw/journalier.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne),
         date  = as.Date(date))

horaire <- readRDS(here::here("data/raw/horaire.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(date = as.Date(date),
         heure = as.integer(horaire_tranche_stop_theo))

km_prod <- readRDS(here::here("data/raw/km_prod.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne),
         date  = as.Date(date))

# Identifier la colonne km
col_km <- if ("km_produits" %in% names(km_prod)) "km_produits" else "km_prod"
km_prod <- km_prod %>% rename(km_col = !!sym(col_km))

cat("=== CHARGEMENT ===\n")
cat("mensuel    :", nrow(mensuel), "lignes |",
    format(min(mensuel$date)), "->", format(max(mensuel$date)), "\n")
cat("journalier :", nrow(journalier), "lignes |",
    format(min(journalier$date)), "->", format(max(journalier$date)), "\n")
cat("horaire    :", nrow(horaire), "lignes |",
    format(min(horaire$date)), "->", format(max(horaire$date)), "\n")
cat("km_prod    :", nrow(km_prod), "lignes\n\n")

# Vérifier la valeur exacte du type Noctambus dans mensuel
cat("--- Types de ligne dans mensuel (vérification NOCTAMBUS) ---\n")
mensuel %>% count(ligne_type_act, sort = TRUE) %>% print()
cat("\n")

# =============================================================================
# 3. SYN-007 — NOCTAMBUS REGIONAL 2016-2026 : TRAJECTOIRE COMPLÈTE
# =============================================================================
# Exploration préliminaire : combien de mois, quelles lignes, quel volume ?
# Avant de formuler une hypothèse sur la rupture.

# --- 3.1 Série mensuelle Noctambus ---

nocta_mensuel <- mensuel %>%
  filter(ligne_type_act == "NOCTAMBUS REGIONAL") %>%
  group_by(date) %>%
  summarise(
    montees_tot = sum(nb_de_montees, na.rm = TRUE),
    n_lignes    = n_distinct(ligne),
    .groups     = "drop"
  ) %>%
  arrange(date)

cat("=== 3. NOCTAMBUS REGIONAL — SÉRIE MENSUELLE ===\n")
cat("Nombre de mois :", nrow(nocta_mensuel), "\n")
cat("Période        :", format(min(nocta_mensuel$date)), "->",
    format(max(nocta_mensuel$date)), "\n")
cat("Lignes Noctambus présentes :", "\n")
mensuel %>%
  filter(ligne_type_act == "NOCTAMBUS REGIONAL") %>%
  distinct(ligne) %>%
  arrange(ligne) %>%
  pull(ligne) %>%
  paste(collapse = ", ") %>%
  cat(., "\n\n")

# Référence 2019 (pré-COVID)
ref_2019 <- nocta_mensuel %>%
  filter(year(date) == 2019) %>%
  summarise(moy = mean(montees_tot)) %>%
  pull(moy)

cat("--- Récupération vs 2019 ---\n")
nocta_mensuel %>%
  mutate(annee = year(date)) %>%
  filter(annee %in% c(2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)) %>%
  group_by(annee) %>%
  summarise(
    moy_mensuelle  = round(mean(montees_tot)),
    pct_vs_2019   = round(mean(montees_tot) / ref_2019 * 100, 1),
    .groups        = "drop"
  ) %>%
  print()

# Observation : quel niveau atteint en 2023 juste avant l'absorption ?
mois_finaux <- nocta_mensuel %>%
  filter(year(date) == 2023) %>%
  summarise(moy_2023 = mean(montees_tot)) %>%
  pull(moy_2023)
cat("\nMoyenne mensuelle 2023 (derniere annee) :",
    round(mois_finaux), "montees\n")
cat("Soit", round(mois_finaux / ref_2019 * 100, 1), "% du niveau 2019\n\n")

# --- 3.2 Exploration visuelle : y a-t-il un signe de déclin avant l'absorption ? ---
# On regarde la serie 2021-2023 — si le Noctambus était déjà en déclin
# avant dec 2023, le Chow test devrait le détecter.

nocta_post_covid <- nocta_mensuel %>%
  filter(date >= as.Date("2021-06-01"))

cat("--- Série post-COVID (jun 2021 -> déc 2023) ---\n")
cat("n =", nrow(nocta_post_covid), "mois\n")
cat("Tendance descriptive :\n")
nocta_post_covid %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarise(moy = round(mean(montees_tot)), .groups = "drop") %>%
  print()

# --- 3.3 Test T-014a : Bai-Perron puis Chow — ruptures série Noctambus ---
# Méthode : Bai-Perron détecte sans a priori, Chow confirme chaque date détectée.
# Cohérent avec script 07 (T-004b). Le point sept 2023 était ad hoc — supprimé.

cat("\n=== T-014a — BAI-PERRON + CHOW : ruptures série Noctambus ===\n")

ts_nocta <- ts(nocta_mensuel$montees_tot,
               start     = c(year(min(nocta_mensuel$date)),
                              month(min(nocta_mensuel$date))),
               frequency = 12)

# --- Étape 1 : Bai-Perron — détection automatique sans a priori ---
cat("--- Étape 1 : Bai-Perron ---\n")
bp_nocta     <- breakpoints(ts_nocta ~ 1)
bp_nocta_sum <- summary(bp_nocta)
bic_nocta    <- bp_nocta_sum$RSS["BIC", ]
n_opt_nocta  <- as.integer(names(which.min(bic_nocta)))

cat("BIC par nombre de ruptures :\n")
print(round(bic_nocta, 1))
cat("Nombre optimal :", n_opt_nocta, "\n")

if (n_opt_nocta > 0) {
  bp_nocta_opt  <- breakpoints(bp_nocta, breaks = n_opt_nocta)
  dates_nocta_r <- nocta_mensuel$date[bp_nocta_opt$breakpoints]
  cat("Dates détectées :\n")
  for (i in seq_along(dates_nocta_r)) {
    cat(" ", i, ":", format(dates_nocta_r[i], "%B %Y"), "\n")
  }
} else {
  dates_nocta_r <- as.Date(character(0))
  cat("Aucune rupture détectée.\n")
}

# --- Étape 2 : Chow sur chaque date Bai-Perron + COVID pour référence ---
cat("\n--- Étape 2 : Chow — confirmation des dates détectées + COVID (ref) ---\n")

dates_chow <- c(
  if (length(dates_nocta_r) > 0)
    setNames(as.list(dates_nocta_r),
             paste0("BP", seq_along(dates_nocta_r),
                    " ", format(dates_nocta_r, "%b %Y")))
  else list(),
  list("COVID mars 2020 (ref)" = as.Date("2020-03-01"))
)

for (lbl in names(dates_chow)) {
  dt  <- dates_chow[[lbl]]
  pos <- which(nocta_mensuel$date == dt)
  if (length(pos) > 0 && pos > 2 && pos < length(ts_nocta) - 2) {
    chow_n <- sctest(ts_nocta ~ 1, type = "Chow", point = pos)
    cat(sprintf("  %-35s pos=%-3d F=%-7.3f p=%s %s\n",
                lbl, pos, chow_n$statistic,
                format(chow_n$p.value, scientific = TRUE),
                ifelse(chow_n$p.value < 0.05, "-> CONFIRME", "-> ns")))
  } else {
    cat(sprintf("  %-35s : hors plage testable\n", lbl))
  }
}

cat("\n--- Conclusions formelles T-014a ---\n")
cat("Ce qu'on peut affirmer :\n")
cat("  - Ruptures detaillees selon resultats ci-dessus\n")
cat("  - COVID (mars 2020) = choc brutal : plancher avr 2020 attendu\n")
cat("  - Rupture post-COVID : le Noctambus a-t-il recupere ou\n")
cat("    est-il reste en dessous de 2019 (contrairement au reseau global) ?\n")
cat("Ce qu'on NE peut PAS affirmer :\n")
cat("  - Que le declin du Noctambus est cause par un changement\n")
cat("    de comportement plutot que par une decision administrative\n")
cat("  - Que les voyageurs Noctambus ont migre vers d'autres modes\n")
cat("    (Uber, voiture personnelle) — aucune donnee modale disponible\n")

# --- 3.4 Test T-014e : déclin pré-COVID Noctambus (avant/après 1re rupture BP) ---
# Hypothèse inductive : Bai-Perron a détecté une rupture avant le COVID.
# On quantifie le déclin entre les deux périodes pré-COVID.
# Mise en regard : Uber à Genève depuis 2014 (OBS-023) — hypothèse interprétative,
# non testable causalement.

cat("\n=== T-014e — DÉCLIN PRÉ-COVID NOCTAMBUS ===\n")

bp_pre_covid <- dates_nocta_r[!is.na(dates_nocta_r) &
                               dates_nocta_r < as.Date("2020-01-01")]

if (length(bp_pre_covid) > 0) {
  date_bp_precovid <- bp_pre_covid[1]
  cat("Rupture Bai-Perron pré-COVID retenue :",
      format(date_bp_precovid, "%B %Y"), "\n")
  cat("Périodes : jan 2016 ->",
      format(date_bp_precovid - months(1), "%b %Y"),
      " | ", format(date_bp_precovid, "%b %Y"),
      "-> déc 2019\n\n")

  nocta_t014e <- nocta_mensuel %>%
    filter(date < as.Date("2020-01-01")) %>%
    mutate(periode_e = case_when(
      date <  date_bp_precovid ~ "PERIODE_1",
      date >= date_bp_precovid ~ "PERIODE_2",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(periode_e))

  cat("--- Volumes par période (hors COVID) ---\n")
  nocta_t014e %>%
    group_by(periode_e) %>%
    summarise(
      n             = n(),
      debut         = format(min(date), "%b %Y"),
      fin           = format(max(date), "%b %Y"),
      moy_mensuelle = round(mean(montees_tot)),
      pct_vs_2019   = round(mean(montees_tot) / ref_2019 * 100, 1),
      .groups       = "drop"
    ) %>%
    print()

  p1_vals <- nocta_t014e$montees_tot[nocta_t014e$periode_e == "PERIODE_1"]
  p2_vals <- nocta_t014e$montees_tot[nocta_t014e$periode_e == "PERIODE_2"]

  if (length(p1_vals) >= 3 && length(p2_vals) >= 3) {
    mw_t014e <- wilcox.test(p2_vals, p1_vals,
                             alternative = "two.sided",
                             conf.int    = TRUE,
                             conf.level  = 0.95)

    n_t014e <- length(p1_vals) + length(p2_vals)
    z_t014e <- qnorm(mw_t014e$p.value / 2)
    r_t014e <- abs(z_t014e) / sqrt(n_t014e)
    diff_e_pct <- round((mean(p2_vals) / mean(p1_vals) - 1) * 100, 1)

    cat("\nT-014e — Mann-Whitney PERIODE_2 vs PERIODE_1 :\n")
    cat("W             :", mw_t014e$statistic, "\n")
    cat("p-value       :", format(mw_t014e$p.value, scientific = TRUE), "\n")
    cat("HL estimee    :", round(mw_t014e$estimate, 0), "montees\n")
    cat("IC 95%        : [", round(mw_t014e$conf.int[1], 0),
        ";", round(mw_t014e$conf.int[2], 0), "]\n")
    cat("r             :", round(r_t014e, 3), "\n")
    cat("Diff. moyenne :", diff_e_pct, "%\n")
    cat("->", ifelse(mw_t014e$p.value < 0.05,
                   "REJET H0 — declin significatif avant COVID",
                   "NON-REJET H0"), "\n\n")

    cat("--- Conclusions formelles T-014e ---\n")
    cat("Ce qu'on peut affirmer :\n")
    if (mw_t014e$p.value < 0.05 && diff_e_pct < 0) {
      cat("  - Le Noctambus etait deja en declin AVANT le COVID\n")
      cat("  - Baisse de", abs(diff_e_pct), "% en moyenne mensuelle\n")
      cat("    entre PERIODE_1 et PERIODE_2 (", format(date_bp_precovid, "%b %Y"),
          ")\n")
    } else if (mw_t014e$p.value >= 0.05) {
      cat("  - Pas de declin significatif detecte entre PERIODE_1 et PERIODE_2\n")
      cat("  - La rupture Bai-Perron ne se traduit pas en baisse MW formelle\n")
    } else {
      cat("  - Hausse detectee entre les deux periodes (inattendu)\n")
    }
    cat("Ce qu'on NE peut PAS affirmer :\n")
    cat("  - Que ce declin est cause par la concurrence VTC (Uber a Geneve\n")
    cat("    depuis 2014, OBS-023) : correlation temporelle, pas causalite\n")
    cat("  - Que l'hypothese VTC est la seule plausible : changements sociaux\n")
    cat("    post-2015 (offre de transports alternatifs, evolution des usages\n")
    cat("    nocturnes) sont des facteurs confondants non mesurables ici\n")
  } else {
    cat("Donnees insuffisantes pour T-014e (n < 3 dans un groupe)\n")
    mw_t014e <- list(p.value = NA, estimate = NA, conf.int = c(NA, NA))
    r_t014e  <- NA
  }
} else {
  cat("Bai-Perron n'a detecte aucune rupture pre-COVID — T-014e non applicable\n")
  mw_t014e <- list(p.value = NA, estimate = NA, conf.int = c(NA, NA))
  r_t014e  <- NA
}

# --- 3.5 Post-absorption : la fréquentation nocturne a-t-elle migré ? ---
# Hypothèse : si le Noctambus transportait des voyageurs réels,
# ces voyageurs se retrouvent quelque part après déc 2023.
# Proxy : fréquentation heures 0h-5h dans horaire.rds avant/après déc 2023.

cat("\n=== T-014b — FRÉQUENTATION NOCTURNE AVANT/APRÈS DÉC 2023 ===\n")
cat("Proxy : heures 0h, 1h, 2h, 3h, 4h, 5h dans horaire.rds\n")
cat("Avant : jan 2022 -> nov 2023 | Après : jan 2024 -> déc 2025\n\n")

nuit <- horaire %>%
  filter(!is.na(heure), heure %in% 0:5,
         horaire_type %in% c("SAMEDI", "DIMANCHE")) %>%
  mutate(periode = case_when(
    date >= as.Date("2022-01-01") & date < as.Date("2023-12-01") ~ "AVANT",
    date >= as.Date("2024-01-01") & date <= as.Date("2025-12-31") ~ "APRES",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(periode))

cat("--- Statistiques nocturnes (0h-5h, weekend) ---\n")
nuit %>%
  group_by(periode) %>%
  summarise(
    n           = n(),
    med_montees = round(median(nb_de_montees, na.rm = TRUE)),
    moy_montees = round(mean(nb_de_montees, na.rm = TRUE)),
    .groups     = "drop"
  ) %>%
  print()

if (nrow(nuit) > 0 && length(unique(nuit$periode)) == 2) {
  avant_nuit  <- nuit$nb_de_montees[nuit$periode == "AVANT"]
  apres_nuit  <- nuit$nb_de_montees[nuit$periode == "APRES"]

  cat("\n--- Normalite (Shapiro-Wilk, n <= 5000) ---\n")
  for (p in c("AVANT", "APRES")) {
    v <- nuit$nb_de_montees[nuit$periode == p]
    s <- if (length(v) > 5000) sample(v, 5000) else v
    sw <- shapiro.test(s)
    cat(sprintf("  %-6s n=%-4d W=%.3f p=%.4f %s\n",
                p, length(v), sw$statistic, sw$p.value,
                ifelse(sw$p.value < 0.05, "-> NON normale", "-> normale")))
  }

  mw_t014b <- wilcox.test(apres_nuit, avant_nuit,
                           alternative = "two.sided",
                           conf.int    = TRUE,
                           conf.level  = 0.95)

  n_t014b <- length(avant_nuit) + length(apres_nuit)
  z_t014b <- qnorm(mw_t014b$p.value / 2)
  r_t014b <- abs(z_t014b) / sqrt(n_t014b)

  diff_pct <- round((median(apres_nuit, na.rm = TRUE) /
                       median(avant_nuit, na.rm = TRUE) - 1) * 100, 1)

  cat("\nW            :", mw_t014b$statistic, "\n")
  cat("p-value      :", format(mw_t014b$p.value, scientific = TRUE), "\n")
  cat("HL estimee   :", round(mw_t014b$estimate, 1), "montees\n")
  cat("IC 95%       : [", round(mw_t014b$conf.int[1], 1),
      ";", round(mw_t014b$conf.int[2], 1), "]\n")
  cat("Diff. mediane:", diff_pct, "%\n")
  cat("r            :", round(r_t014b, 3), "\n")
  cat("->", ifelse(mw_t014b$p.value < 0.05,
                 "REJET H0 — frequentation nocturne differente apres absorption",
                 "NON-REJET H0 — pas de migration nocturne detectable"), "\n\n")

  cat("--- Conclusions formelles T-014b ---\n")
  if (mw_t014b$p.value < 0.05 && diff_pct > 0) {
    cat("Ce qu'on peut affirmer :\n")
    cat("  - La frequentation nocturne (0h-5h, weekend) est statistiquement\n")
    cat("    plus elevee apres dec 2023 (p =",
        format(mw_t014b$p.value, scientific = TRUE), ")\n")
    cat("  - MAIS effet pratiquement marginal : r =", round(r_t014b, 3),
        "(negligeable)\n")
    cat("    +", round(mw_t014b$estimate, 0), "montees sur une mediane de",
        round(median(avant_nuit, na.rm = TRUE), 0), "montees (periode AVANT)\n")
  } else if (mw_t014b$p.value < 0.05 && diff_pct < 0) {
    cat("Ce qu'on peut affirmer :\n")
    cat("  - La frequentation nocturne a baisse apres absorption (p =",
        format(mw_t014b$p.value, scientific = TRUE), ")\n")
    cat("  - r =", round(r_t014b, 3), "(negligeable) — impact pratique marginal\n")
  } else {
    cat("Ce qu'on peut affirmer :\n")
    cat("  - Pas de changement significatif de la frequentation nocturne\n")
    cat("  - La disparition du Noctambus n'a pas modifie le profil nocturne global\n")
  }
  cat("Ce qu'on NE peut PAS affirmer :\n")
  cat("  - Que les memes individus sont concernes (pas de donnees individuelles)\n")
  cat("  - BIAIS DE SIMULTANEITE (limite principale) : les heures 0h-5h\n")
  cat("    post-absorption incluent les bus reguliers prolonges crees\n")
  cat("    precisement pour remplacer le Noctambus. L'effet (migration\n")
  cat("    eventuelle) et sa cause (offre de remplacement) sont mesures\n")
  cat("    simultanement — les deux ne sont pas separables ici.\n")
  cat("  - Que la hausse observee serait due aux anciens usagers Noctambus\n")
  cat("    plutot qu'au nouveau service de remplacement lui-meme\n")
} else {
  cat("Donnees insuffisantes pour le test T-014b\n")
  r_t014b <- NA
  mw_t014b <- list(p.value = NA, estimate = NA, conf.int = c(NA, NA))
}

# [TPG] La fréquentation nocturne post-absorption est un indicateur clé
# de la réussite de l'intégration du Noctambus dans le réseau régulier.
# Si elle baisse, les voyageurs ont trouvé une alternative non-TPG.
# Si elle est stable ou hausse, l'intégration est un succès.

# =============================================================================
# 4. SYN-008 / IDEE-005 — LIGNE 10 AÉROPORT : PROFIL SPÉCIFIQUE
# =============================================================================
# Exploration : la ligne 10 est la seule ligne PRINCIPAL à desservir
# l'aéroport de Genève. Elle est 10e au classement du réseau alors que
# l'aéroport est le 2e pôle d'emploi genevois.
# Hypothèse : concurrence CFF (Cornavin → Aéroport ≈ 5 min) explique
# le rang modeste. Mais les profils des usagers (employés vs passagers)
# méritent une exploration.
#
# LIMITE DOCUMENTÉE : pas de données horaires par ligne disponibles.
# L'hypothèse "pic précoce 5h-6h employés aéroport" ne peut pas être
# testée formellement sans données horaires par ligne.

cat("\n=== 4. LIGNE 10 AÉROPORT ===\n")

# --- 4.1 Exploration de base ---

ligne10 <- journalier %>%
  filter(ligne == "10")

cat("Ligne 10 — observations journalier :", nrow(ligne10), "\n")
cat("Période  :", format(min(ligne10$date)), "->",
    format(max(ligne10$date)), "\n")
cat("Arrêts distincts :", n_distinct(ligne10$arret), "\n\n")

# Type de la ligne 10
cat("Type ligne 10 :", unique(ligne10$ligne_type_act), "\n\n")

# Montées totales ligne 10 vs réseau
total_L10    <- sum(ligne10$nb_de_montees, na.rm = TRUE)
total_reseau <- sum(journalier$nb_de_montees, na.rm = TRUE)
cat("Montées totales L10    :",
    format(round(total_L10 / 1e6, 2)), "M\n")
cat("% réseau               :",
    round(total_L10 / total_reseau * 100, 2), "%\n\n")

# Rang dans le réseau (classement par montées totales, toutes lignes)
rang_L10 <- journalier %>%
  group_by(ligne, ligne_type_act) %>%
  summarise(mont = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mont)) %>%
  mutate(rang = row_number()) %>%
  filter(ligne == "10")

cat("Rang ligne 10 :", rang_L10$rang, "sur",
    n_distinct(journalier$ligne), "lignes\n\n")

# --- 4.2 Évolution mensuelle ligne 10 (mensuel 2016-2026) ---

ligne10_mensuel <- mensuel %>%
  filter(ligne == "10") %>%
  group_by(date) %>%
  summarise(montees_tot = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

cat("--- Évolution mensuelle ligne 10 ---\n")
cat("Disponible de :", format(min(ligne10_mensuel$date)),
    "à :", format(max(ligne10_mensuel$date)), "\n")

ref_L10_2019 <- ligne10_mensuel %>%
  filter(year(date) == 2019) %>%
  summarise(moy = mean(montees_tot)) %>%
  pull(moy)

ligne10_mensuel %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarise(
    moy_mensuelle = round(mean(montees_tot)),
    pct_vs_2019   = round(mean(montees_tot) / ref_L10_2019 * 100, 1),
    .groups       = "drop"
  ) %>%
  print()

# --- 4.3 Profil journalier ligne 10 (jours de semaine vs weekend) ---

cat("\n--- Profil par horaire_type (journalier) ---\n")
ligne10 %>%
  group_by(horaire_type) %>%
  summarise(
    n           = n_distinct(date),
    med_montees = round(median(nb_de_montees, na.rm = TRUE)),
    moy_montees = round(mean(nb_de_montees, na.rm = TRUE)),
    .groups     = "drop"
  ) %>%
  arrange(desc(moy_montees)) %>%
  print()

# Comparaison : la ligne 10 baisse-t-elle autant que le réseau en vacances ?
cat("\n--- L10 : différence NORMAL vs VACANCES ---\n")
L10_agg_ht <- ligne10 %>%
  filter(horaire_type %in% c("NORMAL", "VACANCES")) %>%
  group_by(date, horaire_type) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

L10_norm_med <- median(L10_agg_ht$montees[L10_agg_ht$horaire_type == "NORMAL"],
                        na.rm = TRUE)
L10_vac_med  <- median(L10_agg_ht$montees[L10_agg_ht$horaire_type == "VACANCES"],
                         na.rm = TRUE)
L10_diff     <- round((L10_vac_med / L10_norm_med - 1) * 100, 1)

cat("NORMAL   mediane :", round(L10_norm_med), "montees/jour\n")
cat("VACANCES mediane :", round(L10_vac_med), "montees/jour\n")
cat("Difference       :", L10_diff, "%\n")
cat("(Rappel reseau global T-013a : -28.2%) \n")
cat("-> Si L10 baisse MOINS que reseau : usage employes (stable vacances)\n")
cat("-> Si L10 baisse PLUS  que reseau : usage voyageurs (pic estival TPG)\n\n")

# --- 4.4 T-014c : position de la ligne 10 dans la distribution PRINCIPAL ---
# LIMITE MÉTHODOLOGIQUE : n_L10 = 1 — un seul ratio par définition.
# Mann-Whitney avec n=1 = test de rang exact non interprétable comme test
# d'hypothèse classique. Résultat purement descriptif (rang dans la distribution).

cat("=== T-014c — DESCRIPTIF : position ligne 10 dans distribution PRINCIPAL ===\n")
cat("LIMITE : n_L10 = 1 — rang informatif uniquement, pas de test formel.\n\n")

# Ratio global par ligne (jours NORMAL — cohérent avec script 13)
exclus14 <- c("C1","C3","C4","C5","C6","C7","C8","C9",
              "NC","ND","NE","NJ","NK","NM","NP","NS","NT","NV","NO",
              "NB1","NB2","NB3","NB4","NB5","NB6")

# km ligne 10
col_km2 <- if ("km_produits" %in% names(km_prod)) "km_produits" else "km_col"

ratio_principal <- journalier %>%
  filter(!ligne %in% exclus14,
         ligne_type_act == "PRINCIPAL",
         horaire_type == "NORMAL") %>%
  group_by(ligne) %>%
  summarise(montees_tot = sum(nb_de_montees, na.rm = TRUE),
            .groups     = "drop") %>%
  inner_join(
    km_prod %>%
      filter(!ligne %in% exclus14, ligne_type_act == "PRINCIPAL") %>%
      group_by(ligne) %>%
      summarise(km_tot = sum(km_col, na.rm = TRUE), .groups = "drop"),
    by = "ligne"
  ) %>%
  filter(km_tot > 0, montees_tot > 0) %>%
  mutate(ratio = montees_tot / km_tot,
         is_L10 = ligne == "10")

ratio_L10     <- ratio_principal$ratio[ratio_principal$is_L10]
ratio_autres  <- ratio_principal$ratio[!ratio_principal$is_L10]

cat("Lignes PRINCIPAL dans le calcul :", nrow(ratio_principal), "\n")
cat("Ratio ligne 10     :", round(ratio_L10, 3), "montees/km\n")
cat("Mediane autres PRINCIPAL :", round(median(ratio_autres), 3), "montees/km\n")
cat("Rang ligne 10 dans PRINCIPAL :",
    sum(ratio_principal$ratio >= ratio_L10), "sur",
    nrow(ratio_principal), "(rang croissant)\n\n")

cat("--- Distribution PRINCIPAL (hors L10) ---\n")
cat("Min :", round(min(ratio_autres), 3),
    "| Q1 :", round(quantile(ratio_autres, 0.25), 3),
    "| Med :", round(median(ratio_autres), 3),
    "| Q3 :", round(quantile(ratio_autres, 0.75), 3),
    "| Max :", round(max(ratio_autres), 3), "\n\n")

mw_t014c <- wilcox.test(
  ratio_L10, ratio_autres,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

cat("--- Position dans la distribution PRINCIPAL ---\n")
cat("Ratio ligne 10        :", round(ratio_L10, 3), "montees/km\n")
cat("Mediane PRINCIPAL     :", round(median(ratio_autres), 3), "montees/km\n")
cat("Rang (descendant)     :", sum(ratio_principal$ratio >= ratio_L10),
    "sur", nrow(ratio_principal), "\n")
cat("(Test de rang MW calculé — informatif, NON conclusif (n_L10=1) :\n")
cat(" W =", mw_t014c$statistic,
    "| p =", format(mw_t014c$p.value, scientific = TRUE), ")\n\n")

cat("--- Analyse descriptive T-014c ---\n")
cat("Ce qu'on peut observer :\n")
cat("  - La ligne 10 occupe le rang", sum(ratio_principal$ratio >= ratio_L10),
    "sur", nrow(ratio_principal), "lignes PRINCIPAL\n")
cat("  - Son ratio (", round(ratio_L10, 3), "montees/km) est",
    ifelse(ratio_L10 < median(ratio_autres), "sous", "au-dessus de"),
    "la mediane (", round(median(ratio_autres), 3), "montees/km)\n", sep = "")
cat("  - C'est un constat de position relative, pas un ecart statistiquement prouve\n")
cat("Ce qu'on NE peut PAS affirmer :\n")
cat("  - Que la position de la ligne 10 est significativement differente\n")
cat("    du reste du groupe PRINCIPAL (n=1 : test de rang non interpretable)\n")
cat("  - Que la performance est due a la competition CFF — cette\n")
cat("    hypothese est interpretative et non testable avec ces donnees\n")
cat("  - Que le profil horaire est atypique — LIMITE DOCUMENTEE :\n")
cat("    pas de donnees horaires par ligne dans les datasets disponibles\n")
cat("  - Que les voyageurs sont des passagers aeroportuaires plutot\n")
cat("    que des employes de l'aeroport — pas de donnees OD\n")

# [TPG] La ligne 10 est le seul cas du réseau avec un concurrent externe fixe
# et connu (CFF Cornavin-Aéroport ≈ 5 min, ~5 CHF). Le rang modeste (#10)
# de la ligne peut s'expliquer par : (1) préférence passagers pour le train
# (plus rapide, plus fréquent), (2) zone de chalandise de la ligne 10 = quartiers
# résidentiels (Sécheron, Varembé, Pregny-Chambésy) plutôt que le seul aéroport.
# Une analyse OD (Origine-Destination) permettrait de distinguer les deux.

# --- 4.5 Test T-014d : interaction VACANCES — ligne 10 vs réseau PRINCIPAL ---
# H0 : ratio VACANCES/NORMAL L10 = ratio VACANCES/NORMAL autres PRINCIPAL
# H1 : différent (bilatéral)
# Signal interprétable : si L10 perd MOINS en vacances -> usage employes dominant
#                        si L10 perd PLUS en vacances  -> usage voyageurs dominant
# Note : n_L10=1 — même limite que T-014c, résultat purement descriptif.

cat("\n=== T-014d — INTERACTION VACANCES : ligne 10 vs PRINCIPAL ===\n")
cat("H0 : ratio VACANCES/NORMAL L10 = ratio VACANCES/NORMAL autres PRINCIPAL\n")
cat("H1 : different (bilateral) | LIMITE : n_L10=1 — descriptif uniquement\n\n")

ratio_vn_ligne <- journalier %>%
  filter(ligne_type_act == "PRINCIPAL",
         !ligne %in% exclus14,
         horaire_type %in% c("NORMAL", "VACANCES")) %>%
  group_by(ligne, date, horaire_type) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  group_by(ligne, horaire_type) %>%
  summarise(med = median(montees, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = horaire_type, values_from = med,
              names_prefix = "med_") %>%
  filter(!is.na(med_NORMAL), !is.na(med_VACANCES), med_NORMAL > 0) %>%
  mutate(ratio_vn = med_VACANCES / med_NORMAL,
         is_L10   = ligne == "10")

ratio_vn_L10_d    <- ratio_vn_ligne$ratio_vn[ratio_vn_ligne$is_L10]
ratio_vn_autres_d <- ratio_vn_ligne$ratio_vn[!ratio_vn_ligne$is_L10]

cat("Lignes dans le calcul :", nrow(ratio_vn_ligne), "\n")
cat("Distribution ratio VACANCES/NORMAL PRINCIPAL (hors L10) :\n")
cat("  Min :", round(min(ratio_vn_autres_d), 3),
    "| Q1 :", round(quantile(ratio_vn_autres_d, 0.25), 3),
    "| Med :", round(median(ratio_vn_autres_d), 3),
    "| Q3 :", round(quantile(ratio_vn_autres_d, 0.75), 3),
    "| Max :", round(max(ratio_vn_autres_d), 3), "\n")
cat("  Ligne 10 :", round(ratio_vn_L10_d, 3),
    ifelse(ratio_vn_L10_d > median(ratio_vn_autres_d),
           "(plus stable que mediane PRINCIPAL en vacances)",
           "(moins stable que mediane PRINCIPAL en vacances)"), "\n\n")

if (length(ratio_vn_L10_d) == 1 && length(ratio_vn_autres_d) >= 3) {
  mw_t014d <- wilcox.test(ratio_vn_L10_d, ratio_vn_autres_d,
                           alternative = "two.sided",
                           conf.int    = TRUE,
                           conf.level  = 0.95)

  n_t014d <- length(ratio_vn_L10_d) + length(ratio_vn_autres_d)
  z_t014d <- qnorm(mw_t014d$p.value / 2)
  r_t014d <- abs(z_t014d) / sqrt(n_t014d)

  cat("Test de rang MW (informatif, n_L10=1, NON conclusif) :\n")
  cat("  W =", mw_t014d$statistic,
      "| p =", format(mw_t014d$p.value, scientific = TRUE),
      "| r =", round(r_t014d, 3), "\n\n")

  cat("--- Analyse descriptive T-014d ---\n")
  cat("Ce qu'on peut observer :\n")
  if (ratio_vn_L10_d > median(ratio_vn_autres_d)) {
    cat("  - La ligne 10 perd proportionnellement MOINS de voyageurs\n")
    cat("    en vacances que la mediane des lignes PRINCIPAL\n")
    cat("  - Signal compatible avec un usage employes dominant (stable en vacances)\n")
    cat("  - Ratio L10 :", round(ratio_vn_L10_d, 3),
        "vs mediane PRINCIPAL :", round(median(ratio_vn_autres_d), 3), "\n")
  } else {
    cat("  - La ligne 10 perd proportionnellement PLUS de voyageurs\n")
    cat("    en vacances que la mediane des lignes PRINCIPAL\n")
    cat("  - Signal compatible avec un usage voyageurs/touristes dominant\n")
    cat("  - Ratio L10 :", round(ratio_vn_L10_d, 3),
        "vs mediane PRINCIPAL :", round(median(ratio_vn_autres_d), 3), "\n")
  }
  cat("Ce qu'on NE peut PAS affirmer :\n")
  cat("  - Que cette difference est statistiquement prouvee (n=1)\n")
  cat("  - Que l'usage est compose uniquement d'employes OU de voyageurs :\n")
  cat("    les deux coexistent sur la ligne — pas de donnees OD disponibles\n")
} else {
  cat("Donnees insuffisantes pour T-014d\n")
  mw_t014d <- list(p.value = NA, estimate = NA, conf.int = c(NA, NA))
  r_t014d  <- NA
}

# [TPG] T-014d : si ratio_vn_L10 > mediane PRINCIPAL, la ligne 10 fonctionne
# davantage en mode "navette employes" que "navette voyageurs". Implication :
# elle ne doit pas être dimensionnée sur des pics touristiques estivaux mais
# sur des horaires de travail stables toute l'année.

# =============================================================================
# 5. VISUALISATIONS
# NOTE VIZ : candidats Python / Power BI final
# =============================================================================

# --- VIZ 1 : Série temporelle Noctambus 2016-2023 avec annotations ---
# NOTE VIZ : serie avec ruptures — narrative COVID + absorption

p_nocta <- ggplot(nocta_mensuel, aes(x = date, y = montees_tot / 1000)) +
  geom_line(color = TPG_RED, linewidth = 0.8) +
  geom_area(fill = TPG_RED, alpha = 0.12) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dotted", color = COL_COVID, linewidth = 0.9) +
  geom_vline(xintercept = as.Date("2021-06-01"),
             linetype = "dotted", color = COL_NEUTRE, linewidth = 0.7) +
  geom_vline(xintercept = as.Date("2023-12-01"),
             linetype = "dashed", color = COL_REF, linewidth = 1) +
  annotate("text", x = as.Date("2020-04-01"),
           y = max(nocta_mensuel$montees_tot / 1000) * 0.95,
           label = "COVID", hjust = 0, size = 2.8, color = COL_COVID) +
  annotate("text", x = as.Date("2021-08-01"),
           y = max(nocta_mensuel$montees_tot / 1000) * 0.80,
           label = "Sortie crise", hjust = 0, size = 2.5, color = COL_NEUTRE) +
  annotate("text", x = as.Date("2023-10-01"),
           y = max(nocta_mensuel$montees_tot / 1000) * 0.95,
           label = "Absorption\ndec 2023", hjust = 1, size = 2.8, color = COL_REF) +
  annotate("text",
           x = as.Date("2017-01-01"),
           y = max(nocta_mensuel$montees_tot / 1000) * 0.60,
           label = paste0("Recup. 2023 vs 2019 : ",
                          round(mois_finaux / ref_2019 * 100, 1), "%"),
           hjust = 0, size = 3, color = COL_NEUTRE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = "k")) +
  labs(
    title    = "Noctambus REGIONAL — trajectoire 2016-2023",
    subtitle = "Montees mensuelles | Absorption dans le reseau regulier en dec 2023",
    x        = NULL,
    y        = "Montees (milliers)",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave(here::here("figures/14_noctambus_serie_temporelle.png"),
       p_nocta, width = 12, height = 6, dpi = 150)
cat("\nGraphique sauvegarde : figures/14_noctambus_serie_temporelle.png\n")

# --- VIZ 2 : Évolution mensuelle ligne 10 vs réseau PRINCIPAL ---
# NOTE VIZ : comparaison recupération COVID L10 vs reseau

# Réseau PRINCIPAL mensuel (agrégé)
principal_mensuel <- mensuel %>%
  filter(ligne_type_act == "PRINCIPAL") %>%
  group_by(date) %>%
  summarise(montees_tot = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

# Normaliser sur base 100 = moyenne 2019
base_L10      <- mean(ligne10_mensuel$montees_tot[year(ligne10_mensuel$date) == 2019],
                       na.rm = TRUE)
base_principal <- mean(principal_mensuel$montees_tot[year(principal_mensuel$date) == 2019],
                        na.rm = TRUE)

evol_compare <- bind_rows(
  ligne10_mensuel   %>% mutate(serie = "Ligne 10",     idx = montees_tot / base_L10 * 100),
  principal_mensuel %>% mutate(serie = "PRINCIPAL",    idx = montees_tot / base_principal * 100)
)

p_L10_evo <- ggplot(evol_compare,
                    aes(x = date, y = idx, color = serie, linewidth = serie)) +
  geom_line(alpha = 0.85) +
  geom_hline(yintercept = 100, linetype = "dashed",
             color = COL_NEUTRE, linewidth = 0.5) +
  geom_vline(xintercept = as.Date("2020-03-01"),
             linetype = "dotted", color = COL_COVID, linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-01-01"),
             linetype = "dotted", color = COL_GRATUITE, linewidth = 0.8) +
  annotate("text", x = as.Date("2020-05-01"), y = 115,
           label = "COVID", hjust = 0, size = 2.8, color = COL_COVID) +
  annotate("text", x = as.Date("2025-02-01"), y = 115,
           label = "Gratuite\njeunes", hjust = 0, size = 2.8, color = COL_GRATUITE) +
  scale_color_manual(values = c("Ligne 10" = "#4A6FA5", "PRINCIPAL" = TPG_RED)) +
  scale_linewidth_manual(values = c("Ligne 10" = 1.2, "PRINCIPAL" = 0.7),
                         guide = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  annotate("text", x = as.Date("2016-03-01"), y = 63,
           label = "Base 100 = moyenne 2019", hjust = 0, size = 2.5,
           color = COL_NEUTRE) +
  labs(
    title    = "Ligne 10 vs PRINCIPAL — trajectoire 2016-2026 (base 100 = 2019)",
    subtitle = "Comparaison recupération COVID et impact gratuité jeunes",
    x        = NULL,
    y        = "Indice (100 = moy. 2019)",
    color    = NULL,
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave(here::here("figures/14_ligne10_vs_principal.png"),
       p_L10_evo, width = 12, height = 6, dpi = 150)
cat("Graphique sauvegarde : figures/14_ligne10_vs_principal.png\n")

# --- VIZ 3 : Boxplot ratio PRINCIPAL + point ligne 10 ---
# NOTE VIZ : identifier la position relative de la ligne 10 dans PRINCIPAL

p_ratio_L10 <- ggplot(ratio_principal %>% filter(!is_L10),
                       aes(x = "PRINCIPAL\n(hors L10)", y = ratio)) +
  geom_boxplot(fill = "#4A6FA5", alpha = 0.7, outlier.shape = 21,
               outlier.fill = "white") +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2, color = "grey30") +
  geom_point(data = ratio_principal %>% filter(is_L10),
             aes(x = "PRINCIPAL\n(hors L10)", y = ratio),
             color = TPG_RED, size = 5, shape = 18) +
  geom_text(data = ratio_principal %>% filter(is_L10),
            aes(x = "PRINCIPAL\n(hors L10)", y = ratio,
                label = paste0("Ligne 10\n(", round(ratio, 2), " mont./km)")),
            hjust = -0.15, size = 3, color = TPG_RED) +
  labs(
    title    = "Position de la ligne 10 dans le cluster PRINCIPAL",
    subtitle = paste0(
      "Ratio montees/km (jours NORMAL) | Rang descriptif : ",
      sum(ratio_principal$ratio >= ratio_L10), "/", nrow(ratio_principal),
      " | n_L10=1 — analyse descriptive uniquement"
    ),
    x        = NULL,
    y        = "Montees / km produit",
    caption  = "Source : opendata.tpg.ch | avr. 2023 -> fev. 2026 | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0))

ggsave(here::here("figures/14_ratio_ligne10_principal.png"),
       p_ratio_L10, width = 8, height = 6, dpi = 150)
cat("Graphique sauvegarde : figures/14_ratio_ligne10_principal.png\n")

# =============================================================================
# 6. BILAN SCRIPT 14
# =============================================================================

cat("\n=== BILAN SCRIPT 14 ===\n\n")

cat("SYN-007 — Noctambus REGIONAL :\n")
cat("  Serie mensuelle", format(min(nocta_mensuel$date)), "->",
    format(max(nocta_mensuel$date)), "\n")
cat("  Recuperation 2023 vs 2019 :", round(mois_finaux / ref_2019 * 100, 1), "%\n")
cat("  T-014a Bai-Perron + Chow : resultats ci-dessus\n")
cat("  T-014b MW nocturne : ")
if (!is.na(mw_t014b$p.value)) {
  cat("p =", format(mw_t014b$p.value, scientific = TRUE),
      "| r =", round(r_t014b, 3), "(marginal)\n")
} else {
  cat("donnees insuffisantes\n")
}
cat("  T-014e declin pre-COVID : ")
if (!is.na(mw_t014e$p.value)) {
  cat("p =", format(mw_t014e$p.value, scientific = TRUE), "\n\n")
} else {
  cat("non applicable (pas de rupture BP pre-COVID)\n\n")
}

cat("SYN-008 / IDEE-005 — Ligne 10 aeroport :\n")
cat("  Rang :", rang_L10$rang, "sur", n_distinct(journalier$ligne), "lignes\n")
cat("  % reseau :", round(total_L10 / total_reseau * 100, 2), "%\n")
cat("  Diff NORMAL/VACANCES :", L10_diff, "% (reseau global : -28.2%)\n")
cat("  T-014c descriptif (n_L10=1) : rang",
    sum(ratio_principal$ratio >= ratio_L10), "/", nrow(ratio_principal),
    "| ratio :", round(ratio_L10, 3), "montees/km\n")
cat("  T-014d interaction vacances : ratio_vn L10 =",
    if (!is.na(ratio_vn_L10_d)) round(ratio_vn_L10_d, 3) else "NA",
    "vs mediane PRINCIPAL =",
    round(median(ratio_vn_autres_d), 3), "\n")
cat("  LIMITE : profil horaire par ligne non disponible — documente\n\n")

cat("3 graphiques sauvegardes dans figures/\n")
cat("[TPG] Observations sensibles marquees # [TPG] dans le script\n")

message("Script 14 termine.")
