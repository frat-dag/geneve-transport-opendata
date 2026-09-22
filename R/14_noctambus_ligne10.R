# ============================================================
# SCRIPT 14 - NOCTAMBUS RÉGIONAL, RÉSEAU DE NUIT ET LIGNE 10
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-03, T-06,
#               S-06, S-08, S-09, S-10, S-25, S-26, S-34, S-36, F-07
# ------------------------------------------------------------
# QUESTIONS :
#   T-014a : la fréquentation du Noctambus régional baissait-elle
#            déjà avant le COVID ?
#   T-014b : le réseau de nuit du 10 décembre 2023 a-t-il augmenté
#            la fréquentation nocturne ?
#   T-014c / T-014d : la ligne 10 (aéroport) se distingue-t-elle
#            des autres lignes principales ? (descriptif)
#
# PÉRIMÈTRE (S-25, F-07) : la catégorie NOCTAMBUS REGIONAL des
# données tpg ne contient que les lignes régionales (NA et NC à NV).
# La ligne NA était perdue à la lecture des CSV avant F-07 : son nom
# était pris pour une valeur manquante. Les lignes urbaines du
# Noctambus n'y figurent pas. Selon Wikipedia (article « Noctambus
# (Genève) », consulté le 20.09.2026), le réseau complet a transporté
# 767 000 personnes en 2019 ; la part couverte par la série analysée
# est calculée plus bas. Les conclusions portent sur le Noctambus
# régional, pas sur le Noctambus dans son ensemble.
#
# CONTEXTE VÉRIFIÉ (tpg.ch, réseau 2024) : le Noctambus circulait
# les nuits du vendredi au samedi et du samedi au dimanche. Il est
# remplacé le 10.12.2023 par le prolongement nocturne des lignes de
# jour, les mêmes nuits et celle du 31 décembre. Les tpg annoncent
# pour 2024 110 000 voyageurs de plus entre 1h et 4h, soit +21,6 %
# (communiqué 2025 relayé par Radio Lac, 24.03.2025).
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(strucchange)

# ── 1. CHARGEMENT ───────────────────────────────────────────

mensuel <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)

horaire <- lire("horaire") %>%
  filter(horaire_tranche_stop_theo != "-") %>%
  mutate(heure = as.integer(horaire_tranche_stop_theo)) %>%
  filter(!is.na(heure))

journalier <- lire("journalier")

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")

# ── 2. SERIE NOCTAMBUS REGIONAL ─────────────────────────────

nocta <- mensuel %>%
  filter(ligne_type_act == "NOCTAMBUS REGIONAL") %>%
  group_by(date) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE),
            n_lignes = n_distinct(ligne), .groups = "drop") %>%
  arrange(date)

lignes_nocta <- sort(unique(mensuel$ligne[mensuel$ligne_type_act == "NOCTAMBUS REGIONAL"]))

cat("\n=== NOCTAMBUS RÉGIONAL ===\n")
cat("Lignes (", length(lignes_nocta), ") :", paste(lignes_nocta, collapse = ", "), "\n")
cat("Mois :", nrow(nocta), "de", format(min(nocta$date)), "à",
    format(max(nocta$date)), "\n")

# S-06 : continuite. Les mois absents sont des mois sans service.
attendus  <- seq(min(nocta$date), max(nocta$date), by = "month")
manquants <- attendus[!attendus %in% nocta$date]
cat("Mois sans données :", length(manquants), "\n")
if (length(manquants) > 0) print(format(manquants, "%Y-%m"))
cat("La version d'avril construisait ts(start = c(2016, 1)) sur cette\n")
cat("série : après mars 2020, chaque date était décalée du nombre de\n")
cat("mois manquants. Les dates de rupture obtenues étaient fausses.\n")

annuel_nocta <- nocta %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarise(mois_de_service = n(),
            montees_milliers = round(sum(montees) / 1e3, 1), .groups = "drop")
ref19 <- annuel_nocta$montees_milliers[annuel_nocta$annee == 2019]
annuel_nocta <- annuel_nocta %>%
  mutate(pct_vs_2019 = round(100 * (montees_milliers / ref19 - 1), 1))

cat("\nMontées annuelles (milliers) :\n")
print(as.data.frame(annuel_nocta))
cat("2020 et 2021 sont incomplètes (service suspendu). En 2023, seul\n")
cat("décembre est partiel (arrêt le 10.12.2023).\n")

# Ordre de grandeur du périmètre (S-25). Voyageurs et montées ne sont
# pas la même unité : le rapport indique une échelle, pas une part exacte.
VOYAGEURS_NOCTAMBUS_2019 <- 767000   # Wikipedia, consulté le 20.09.2026
cat("Montées 2019 de la série rapportées aux voyageurs 2019 du réseau\n")
cat("Noctambus complet :", round(100 * ref19 * 1e3 / VOYAGEURS_NOCTAMBUS_2019, 1),
    "% (ordre de grandeur).\n")

# ── 3. T-014a : TENDANCE AVANT LE COVID ─────────────────────
# On ne travaille que sur le segment continu qui précède le COVID,
# où ts() est valide. La série est désaisonnalisée avant le test.
#
# S-08 : la version d'avril trouvait des dates avec Bai-Perron puis
# les « confirmait » par un test de Chow à ces mêmes dates. C'est
# utiliser deux fois les mêmes données : le Chow est alors
# significatif presque par construction. Le test correct d'une
# rupture à date inconnue est le supF d'Andrews, dont la loi tient
# compte de la recherche de la date.

pre <- nocta %>% filter(date < D_COVID)
att_pre <- seq(min(pre$date), max(pre$date), by = "month")
stopifnot(all(att_pre %in% pre$date))

ts_pre  <- ts(pre$montees, start = c(year(min(pre$date)), month(min(pre$date))),
              frequency = 12)
stl_pre <- stl(ts_pre, s.window = "periodic", robust = TRUE)
des_pre <- ts(as.numeric(ts_pre) - as.numeric(stl_pre$time.series[, "seasonal"]),
              start = start(ts_pre), frequency = 12)

fs    <- Fstats(des_pre ~ 1, from = 0.15)
supf  <- sctest(fs, type = "supF")
date_supf <- pre$date[fs$breakpoint]

sp_pre <- suppressWarnings(cor.test(seq_along(des_pre), as.numeric(des_pre),
                                    method = "spearman"))
r1_pre <- as.numeric(acf(as.numeric(des_pre), plot = FALSE, lag.max = 1)$acf[2])
neff_pre <- round(length(des_pre) * (1 - r1_pre) / (1 + r1_pre), 1)

var_16_19 <- annuel_nocta$pct_vs_2019[annuel_nocta$annee == 2016]

cat("\n=== T-014a : AVANT LE COVID (", format(min(pre$date), "%m.%Y"), "à",
    format(max(pre$date), "%m.%Y"), ",", nrow(pre), "mois continus ) ===\n")
cat("Montées 2019 par rapport à 2016 :",
    round(100 * (ref19 / annuel_nocta$montees_milliers[annuel_nocta$annee == 2016] - 1), 1),
    "%\n")
cat("Tendance (Spearman, série désaisonnalisée) : rho =",
    round(as.numeric(sp_pre$estimate), 3), "\n")
cat("Autocorrélation au premier retard :", round(r1_pre, 3),
    "| observations effectives :", neff_pre, "\n")
cat("Test supF (rupture à date inconnue) : F max =",
    round(as.numeric(supf$statistic), 2),
    "| p =", format(supf$p.value, digits = 3),
    "| date du F max :", format(date_supf, "%m.%Y"), "\n")
cat("Le supF suppose des erreurs indépendantes : avec l'autocorrélation\n")
cat("observée, sa p-value est optimiste. Lecture principale : la baisse\n")
cat("progressive des totaux annuels et le signe de rho.\n")

enregistrer(
  test_id = "T-014a", script = "14_noctambus_ligne10.R",
  methode = "Noctambus régional avant COVID : supF d'Andrews et Spearman sur série désaisonnalisée",
  n = nrow(pre), statistique = round(as.numeric(supf$statistic), 2), p_value = NA,
  effet_nom = "Spearman rho (tendance)", effet = round(as.numeric(sp_pre$estimate), 3),
  note = paste0("2019 contre 2016 : ",
                round(100 * (ref19 / annuel_nocta$montees_milliers[annuel_nocta$annee == 2016] - 1), 1),
                " %. F max en ", format(date_supf, "%Y-%m"), ", p supF = ",
                format(supf$p.value, digits = 3), ". n effectif ", neff_pre,
                ". ", length(lignes_nocta), " lignes régionales, ligne NA comprise (S-25, F-07).",
                " Chow aux dates Bai-Perron retiré (S-08).")
)

# Reprise après COVID, comparée à celle du réseau entier
reseau_annuel <- mensuel %>%
  mutate(annee = year(date)) %>%
  filter(annee %in% c(2019, 2022)) %>%
  group_by(annee) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
reprise_reseau <- round(100 * (reseau_annuel$t[reseau_annuel$annee == 2022] /
                                 reseau_annuel$t[reseau_annuel$annee == 2019] - 1), 1)
reprise_nocta <- annuel_nocta$pct_vs_2019[annuel_nocta$annee == 2022]

cat("\nDernière année complète de service, 2022, par rapport à 2019 :\n")
cat("  Noctambus régional :", reprise_nocta, "%\n")
cat("  Réseau entier      :", reprise_reseau, "%\n")
cat("Le Noctambus régional n'avait pas retrouvé son niveau d'avant le\n")
cat("COVID quand il a été remplacé. Les données ne disent pas pourquoi.\n")

# ── 4. T-014b : FRÉQUENTATION ENTRE 1H ET 4H ────────────────
# S-09 : la version d'avril mélangeait les heures 0 à 5, dont 0h et
# 5h qui relèvent de la fin et du début du service de jour, et
# raisonnait en heure-jour. On retient les tranches 1h, 2h et 3h,
# soit la plage « entre 1h et 4h » retenue par les tpg.
#
# S-36 : quel jour porte une heure après minuit ? On le vérifie dans
# les données plutôt que de le supposer. Les montées de 1h à 4h sont
# rattachées au jour de service (la nuit du vendredi au samedi porte
# la date du vendredi) : le vendredi et le samedi en portent la quasi
# totalité, le dimanche presque rien. Les nuits de fin de semaine
# sont donc les jours de service vendredi et samedi.
#
# S-36 : depuis décembre 2025, des montées apparaissent aussi entre
# 1h et 4h les autres nuits. Les compter dans un total divisé par le
# nombre de week-ends gonflait la série en 2026. Elles sont isolées
# et affichées à part.
#
# Comparaison appariée mois par mois, janvier à novembre 2023
# (ancien réseau) contre janvier à novembre 2024 (nouveau réseau).
# Décembre est exclu : la bascule a lieu le 10.12.2023, et la nuit
# du 31 décembre est un cas particulier.
# Contrôle : même calcul sur les heures de jour, 6h à 23h.

JOURS_WEEKEND <- c(5, 6)   # indice_jour_semaine : vendredi et samedi

repartition_nuit <- horaire %>%
  filter(heure %in% 1:3) %>%
  mutate(annee = year(date)) %>%
  group_by(annee, jour_semaine) %>%
  summarise(m = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  group_by(annee) %>%
  mutate(pct = round(100 * m / sum(m), 1)) %>%
  select(-m) %>%
  pivot_wider(names_from = jour_semaine, values_from = pct)

cat("\n=== S-36 : MONTÉES DE 1H À 4H PAR JOUR DE SERVICE (% de l'année) ===\n")
print(as.data.frame(repartition_nuit))
cat("Le pic isolé d'un autre jour une année donnée correspond à la nuit\n")
cat("du 31 décembre.\n")

samedis_du_mois <- function(d) {
  jours <- seq(floor_date(d, "month"), ceiling_date(d, "month") - 1, by = "day")
  sum(wday(jours, week_start = 1) == 6)
}

par_mois <- horaire %>%
  mutate(mois = floor_date(date, "month"),
         plage = case_when(heure %in% 1:3 & indice_jour_semaine %in% JOURS_WEEKEND ~ "nuit",
                           heure %in% 1:3                                          ~ "nuit_semaine",
                           heure >= 6                                              ~ "jour",
                           TRUE                                                    ~ NA_character_)) %>%
  filter(!is.na(plage)) %>%
  group_by(mois, plage) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = plage, values_from = montees, values_fill = 0) %>%
  mutate(weekends = sapply(mois, samedis_du_mois),
         nuit_par_weekend = nuit / weekends,
         annee = year(mois), mn = month(mois))

comparer <- function(a_ref, a_test, col) {
  x <- par_mois %>% filter(annee == a_ref, mn <= 11)
  y <- par_mois %>% filter(annee == a_test, mn <= 11)
  cm <- inner_join(x, y, by = "mn", suffix = c("_r", "_t"))
  v  <- 100 * (cm[[paste0(col, "_t")]] / cm[[paste0(col, "_r")]] - 1)
  w  <- suppressWarnings(wilcox.test(cm[[paste0(col, "_t")]],
                                     cm[[paste0(col, "_r")]], paired = TRUE))
  list(n = nrow(cm), var_mediane = round(median(v), 1),
       mois_en_hausse = sum(v > 0), p = w$p.value)
}

nuit_2324 <- comparer(2023, 2024, "nuit_par_weekend")
jour_2324 <- comparer(2023, 2024, "jour")
nuit_2223 <- comparer(2022, 2023, "nuit_par_weekend")
jour_2223 <- comparer(2022, 2023, "jour")

cat("\n=== T-014b : MONTÉES ENTRE 1H ET 4H, AVANT ET APRÈS LE 10.12.2023 ===\n")
cat("Nuits du vendredi et du samedi. Janvier à novembre, apparié mois par mois,\n")
cat("variation médiane :\n")
cat(sprintf("  2023 vers 2024 | nuit par week-end : %+.1f %% (%d mois sur %d en hausse) | jour : %+.1f %%\n",
            nuit_2324$var_mediane, nuit_2324$mois_en_hausse, nuit_2324$n, jour_2324$var_mediane))
cat(sprintf("  2022 vers 2023 | nuit par week-end : %+.1f %% | jour : %+.1f %%  (année sans changement de réseau)\n",
            nuit_2223$var_mediane, jour_2223$var_mediane))
effet_net    <- round(nuit_2324$var_mediane - jour_2324$var_mediane, 1)
ecart_temoin <- round(nuit_2223$var_mediane - jour_2223$var_mediane, 1)
effet_dd     <- round(effet_net - ecart_temoin, 1)
cat("Écart nuit moins jour, année du nouveau réseau :", effet_net, "points\n")
cat("Écart nuit moins jour, année témoin            :", ecart_temoin, "points\n")
cat("Différence des deux écarts                     :", effet_dd, "points\n")
cat("Lecture : en année sans changement de réseau, la nuit évolue comme\n")
cat("le jour à quelques points près. En 2024, elle s'en détache nettement.\n")
cat("C'est l'ordre de grandeur attribuable au nouveau réseau de nuit, sous\n")
cat("réserve que rien d'autre n'ait changé la nuit cette année-là.\n")

# S-34 : DISTRIBUTION PLACEBO SUR LES HEURES.
# Les données horaires commencent en 2019 : trop peu de paires
# d'années hors COVID pour un placebo dans le temps. On applique donc
# exactement le même calcul (vendredis et samedis, par week-end,
# écart au jour, différence avec l'année témoin) à chaque heure de
# 6h à 23h, qu'aucun changement de réseau de nuit ne concerne.
# Les heures 0h, 4h et 5h sont exclues : le prolongement nocturne
# des lignes de jour peut les toucher.
# Réserve : les heures de jour ont bien plus de montées que la nuit,
# donc moins de bruit. Le placebo sous-estime un peu la variabilité
# naturelle de la nuit.

HEURES_PLACEBO <- 6:23

difference_heure <- function(heures) {
  d <- horaire %>%
    filter(heure %in% heures, indice_jour_semaine %in% JOURS_WEEKEND) %>%
    group_by(mois = floor_date(date, "month")) %>%
    summarise(x = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
    mutate(x = x / sapply(mois, samedis_du_mois)) %>%
    inner_join(par_mois %>% select(mois, jour), by = "mois") %>%
    mutate(annee = year(mois), mn = month(mois))
  v <- function(a_ref, a_test, col) {
    cm <- inner_join(d %>% filter(annee == a_ref, mn <= 11),
                     d %>% filter(annee == a_test, mn <= 11),
                     by = "mn", suffix = c("_r", "_t"))
    median(100 * (cm[[paste0(col, "_t")]] / cm[[paste0(col, "_r")]] - 1))
  }
  (v(2023, 2024, "x") - v(2023, 2024, "jour")) - (v(2022, 2023, "x") - v(2022, 2023, "jour"))
}

placebo_heures <- data.frame(heure = HEURES_PLACEBO,
                             difference = round(sapply(HEURES_PLACEBO, difference_heure), 1))
diff_traitee <- round(difference_heure(1:3), 1)
rang_nuit <- sum(placebo_heures$difference >= diff_traitee) + 1
p_placebo <- round(rang_nuit / (nrow(placebo_heures) + 1), 3)
max_abs_placebo <- max(abs(placebo_heures$difference))

cat("\n=== S-34 : T-014b, DISTRIBUTION PLACEBO SUR LES HEURES ===\n")
cat("Différence des écarts au jour (2024 contre 2023, moins 2023 contre 2022),\n")
cat("vendredis et samedis, en points, heure par heure :\n")
print(as.data.frame(t(setNames(placebo_heures$difference, paste0(placebo_heures$heure, "h")))),
      row.names = FALSE)
cat("\nHeures placebo :", nrow(placebo_heures), "| plage : de",
    min(placebo_heures$difference), "à", max(placebo_heures$difference),
    "points | écart-type :", round(sd(placebo_heures$difference), 1), "\n")
cat("Plage 1h à 4h :", diff_traitee, "points | rang", rang_nuit, "sur",
    nrow(placebo_heures) + 1, "| p placebo =", p_placebo, "\n")
cat("La plage de nuit vaut", round(diff_traitee / max_abs_placebo, 1),
    "fois la plus grande valeur placebo en valeur absolue.\n")
cat("Avec ", nrow(placebo_heures), " heures placebo, p ne peut pas descendre sous ",
    round(1 / (nrow(placebo_heures) + 1), 3), ".\n", sep = "")

cat("\nAutres nuits (dimanche à jeudi), montées de 1h à 4h par mois :\n")
print(as.data.frame(par_mois %>%
  filter(mois >= as.Date("2025-06-01")) %>%
  transmute(mois = format(mois, "%m.%Y"), nuits_de_semaine = round(nuit_semaine))),
  row.names = FALSE)
cat("Ces montées restent faibles jusqu'en novembre 2025 puis augmentent\n")
cat("nettement : l'offre de nuit en semaine a changé. Elles sont exclues\n")
cat("de T-014b et de la figure, qui portent sur les nuits de fin de semaine.\n")

# Réplication de l'annonce des tpg sur l'année civile (toutes les nuits,
# comme l'annonce)
civil <- horaire %>%
  filter(heure %in% 1:3, year(date) %in% c(2023, 2024)) %>%
  group_by(annee = year(date)) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
gain_abs <- civil$t[civil$annee == 2024] - civil$t[civil$annee == 2023]
gain_pct <- round(100 * (civil$t[civil$annee == 2024] / civil$t[civil$annee == 2023] - 1), 1)

cat("\nRéplication de l'annonce des tpg (années civiles, 1h à 4h) :\n")
cat("  Gain absolu 2024 contre 2023 :", format(round(gain_abs), big.mark = " "),
    "montées (annonce : 110 000)\n")
cat("  Variation relative :", gain_pct, "% (annonce : +21,6 %)\n")
cat("Le gain absolu est retrouvé. Le pourcentage diffère : les tpg le\n")
cat("rapportent à une base plus petite, que ces données ne permettent\n")
cat("pas de reconstituer. Question transmise aux tpg (Q-06).\n")

# Pourquoi le melange 0h-5h d'avril sous-estimait l'effet
melange <- horaire %>%
  filter(heure %in% 0:5, year(date) %in% c(2023, 2024)) %>%
  group_by(annee = year(date)) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
cat("\nAvec les heures 0 à 5 mélangées (méthode d'avril) :",
    round(100 * (melange$t[2] / melange$t[1] - 1), 1),
    "%. Les heures 0h et 5h, qui relèvent du service de jour, diluent l'effet.\n")

cat("\nAu-delà de 2024, d'autres changements d'offre interviennent\n")
cat("(lignes nocturnes ajoutées au 15.12.2024, nuits de semaine visibles\n")
cat("depuis décembre 2025). La comparaison est donc limitée à 2023 contre 2024.\n")

enregistrer(
  test_id = "T-014b", script = "14_noctambus_ligne10.R",
  methode = "Montées 1h-4h des nuits du vendredi et du samedi, par week-end, janvier-novembre 2023 contre 2024, apparié par mois, contrôlé par le jour",
  n = nuit_2324$n, statistique = NA, p_value = NA,
  effet_nom = "variation médiane nuit par week-end (%)", effet = nuit_2324$var_mediane,
  note = paste0("Jour : ", jour_2324$var_mediane, " %. Écart nuit moins jour : ",
                effet_net, " points (année témoin : ", ecart_temoin,
                ", différence : ", effet_dd, "). Placebo sur ", nrow(placebo_heures),
                " heures de 6h à 23h (S-34) : de ", min(placebo_heures$difference), " à ",
                max(placebo_heures$difference), " points, nuit rang ", rang_nuit,
                ", p placebo ", p_placebo, ". Nuits de semaine exclues (S-36). ",
                "Année civile, toutes nuits : +", format(round(gain_abs), big.mark = " "),
                " montées (annonce tpg : 110 000), ", gain_pct,
                " % (annonce : 21,6 %). Heures 0-5 mélangées : sous-estimation (S-09).")
)

# ── 5. LIGNE 10 (DESCRIPTIF) ────────────────────────────────
# Avec une seule ligne d'un côté, aucun test n'a de sens : on situe
# la ligne 10 dans la distribution des lignes principales.
# La version d'avril avançait une explication (concurrence du train
# vers l'aéroport). Elle n'est pas testable avec ces données et
# n'est pas reprise comme conclusion.

principal <- mensuel %>% filter(ligne_type_act == "PRINCIPAL")

reprise <- principal %>%
  mutate(annee = year(date)) %>%
  filter(annee %in% c(2019, 2025)) %>%
  group_by(ligne, annee) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = annee, values_from = t, names_prefix = "a") %>%
  filter(!is.na(a2019), !is.na(a2025), a2019 > 1e5) %>%
  mutate(pct = round(100 * (a2025 / a2019 - 1), 1)) %>%
  arrange(pct)

rang_10 <- which(reprise$ligne == "10")
cat("\n=== T-014c : LIGNE 10, 2025 CONTRE 2019 ===\n")
cat("Lignes principales comparables :", nrow(reprise), "\n")
if (length(rang_10) == 1) {
  cat("Ligne 10 :", reprise$pct[rang_10], "% | rang", rang_10, "sur",
      nrow(reprise), "(1 = plus faible reprise)\n")
  cat("Médiane des lignes principales :", median(reprise$pct), "%\n")
}

vac <- journalier %>%
  filter(ligne_type_act == "PRINCIPAL", horaire_type %in% c("NORMAL", "VACANCES")) %>%
  group_by(ligne, date, horaire_type) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  group_by(ligne, horaire_type) %>%
  summarise(med = median(t), .groups = "drop") %>%
  pivot_wider(names_from = horaire_type, values_from = med) %>%
  filter(!is.na(NORMAL), !is.na(VACANCES), NORMAL > 1000) %>%
  mutate(ratio = round(VACANCES / NORMAL, 3)) %>%
  arrange(desc(ratio))

rang_v <- which(vac$ligne == "10")
cat("\n=== T-014d : LIGNE 10, RAPPORT VACANCES / JOURS NORMAUX ===\n")
if (length(rang_v) == 1) {
  cat("Ligne 10 :", vac$ratio[rang_v], "| rang", rang_v, "sur", nrow(vac),
      "(1 = fréquentation la mieux maintenue en vacances)\n")
  cat("Médiane des lignes principales :", median(vac$ratio), "\n")
}
cat("Lecture descriptive : une ligne qui dessert l'aéroport perd moins\n")
cat("en vacances si ses usagers ne sont pas liés au calendrier scolaire.\n")
cat("Les données ne permettent pas de vérifier qui sont ces usagers.\n")

enregistrer(
  test_id = "T-014c", script = "14_noctambus_ligne10.R",
  methode = "Descriptif : rang de la ligne 10 parmi les lignes principales, 2025 contre 2019",
  n = nrow(reprise), statistique = NA, p_value = NA,
  effet_nom = "variation 2025 contre 2019 (%)",
  effet = if (length(rang_10) == 1) reprise$pct[rang_10] else NA,
  note = paste0("Rang ", rang_10, " sur ", nrow(reprise), ". Médiane ", median(reprise$pct), " %.")
)
enregistrer(
  test_id = "T-014d", script = "14_noctambus_ligne10.R",
  methode = "Descriptif : rapport vacances / jours normaux de la ligne 10 parmi les lignes principales",
  n = nrow(vac), statistique = NA, p_value = NA,
  effet_nom = "rapport vacances / normal",
  effet = if (length(rang_v) == 1) vac$ratio[rang_v] else NA,
  note = paste0("Rang ", rang_v, " sur ", nrow(vac), ". Médiane ", median(vac$ratio), ".")
)

# ── 6. FIGURES ──────────────────────────────────────────────
# La série Noctambus est tracée avec ses trous : aucune
# interpolation sur les mois sans service.

nocta_plot <- data.frame(date = attendus) %>% left_join(nocta, by = "date")

p_nocta <- ggplot(nocta_plot, aes(x = date, y = montees / 1e3)) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.8, na.rm = FALSE) +
  geom_vline(xintercept = D_COVID, linetype = "dashed", color = COL_COVID, linewidth = 0.4) +
  geom_vline(xintercept = D_RESEAU_NUIT, linetype = "dashed", color = COL_NEUTRE, linewidth = 0.4) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Noctambus régional, montées mensuelles",
       subtitle = paste0(length(lignes_nocta), " lignes régionales (",
                         paste(lignes_nocta, collapse = ", "),
                         "). Les interruptions sont des mois sans service.\n",
                         "Traits : COVID et remplacement par le réseau de nuit le 10.12.2023."),
       x = NULL, y = "Milliers de montées", caption = SOURCE_TPG) +
  theme_projet()

print(p_nocta)
ggsave(file.path(DIR_FIG, "14_noctambus_regional.png"), p_nocta, width = 12, height = 6, dpi = 150)

p_nuit <- ggplot(par_mois %>% filter(mois >= as.Date("2022-01-01")),
                 aes(x = mois, y = nuit_par_weekend / 1e3)) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.8) +
  geom_point(color = ROUGE_PRINCIPAL, size = 1.5) +
  geom_vline(xintercept = D_RESEAU_NUIT, linetype = "dashed", color = COL_NEUTRE, linewidth = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m.%Y") +
  labs(title = "Montées entre 1h et 4h, nuits du vendredi et du samedi",
       subtitle = paste0("Réseau entier, moyenne par week-end. Trait : réseau de nuit du 10.12.2023.\n",
                         "Janvier à novembre, 2024 contre 2023 : ",
                         sprintf("%+.1f %%", nuit_2324$var_mediane), " la nuit, ",
                         sprintf("%+.1f %%", jour_2324$var_mediane), " le jour."),
       x = NULL, y = "Milliers de montées par week-end", caption = SOURCE_TPG) +
  theme_projet() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_nuit)
ggsave(file.path(DIR_FIG, "14_nuit_1h_4h.png"), p_nuit, width = 12, height = 6, dpi = 150)
message("Figures enregistrées.")

# ── 7. SAUVEGARDE ───────────────────────────────────────────

write.csv(annuel_nocta,     file.path(DIR_RES, paste0("14_noctambus_annuel_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(par_mois,         file.path(DIR_RES, paste0("14_nuit_par_mois_",      SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(repartition_nuit, file.path(DIR_RES, paste0("14_nuit_par_jour_",      SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(reprise,          file.path(DIR_RES, paste0("14_principal_reprise_",  SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(vac,              file.path(DIR_RES, paste0("14_principal_vacances_", SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 14 terminé. Figures dans figures/, résultats dans resultats/.")
