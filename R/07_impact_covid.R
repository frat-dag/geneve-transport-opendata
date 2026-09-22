# ============================================================
# SCRIPT 07 - IMPACT COVID ET RUPTURES STRUCTURELLES
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-06,
#               S-01, S-02, S-03, S-04, S-05, S-06, S-10, S-32
# ------------------------------------------------------------
# TESTS :
#   T-002  : jours NORMAL contre jours VACANCES
#   T-004  : ruptures structurelles, série observée
#   T-004b : ruptures sur série désaisonnalisée (voir S-02)
#   T-006  : niveau pré-COVID contre niveau du dernier segment
#            (régression, erreur type HAC, voir S-32)
#
# AVERTISSEMENT TRANSVERSAL (S-10) : la série mensuelle est
# fortement autocorrélée. Les tests employés ici (Mann-Whitney,
# Chow, CUSUM) supposent des observations indépendantes. Leurs
# p-values sont donc trop optimistes. Le script calcule la taille
# d'échantillon effective et publie les tailles d'effet et les
# intervalles de confiance plutôt que les p-values seules.
# T-006 utilise une erreur type de Newey-West (HAC), qui tient
# compte de l'autocorrélation.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(strucchange)
library(sandwich)

# ── 1. CHARGEMENT ───────────────────────────────────────────

composantes <- readRDS(file.path(DIR_PROC, "stl_composantes.rds"))

horaire <- lire("horaire") %>%
  filter(horaire_tranche_stop_theo != "-") %>%
  mutate(heure = as.integer(horaire_tranche_stop_theo)) %>%
  filter(!is.na(heure))

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Composantes STL :", nrow(composantes), "mois, de",
    format(min(composantes$date)), "à", format(max(composantes$date)), "\n")
cat("Horaire         :", nrow(horaire), "observations\n")

# ── 2. AUTOCORRÉLATION ET TAILLE EFFECTIVE (S-10) ───────────
# Avec une autocorrélation r au premier retard, le nombre
# d'observations réellement indépendantes vaut approximativement
# n * (1 - r) / (1 + r). Ce chiffre conditionne la lecture de
# toutes les p-values du script.

acf_serie <- acf(composantes$desaisonnalisee, plot = FALSE, lag.max = 3)
r1 <- as.numeric(acf_serie$acf[2])
n_obs <- nrow(composantes)
n_eff <- round(n_obs * (1 - r1) / (1 + r1), 1)

cat("\n=== AUTOCORRÉLATION DE LA SÉRIE (S-10) ===\n")
cat("ACF aux retards 1 à 3 :", round(as.numeric(acf_serie$acf[2:4]), 3), "\n")
cat("Observations          :", n_obs, "\n")
cat("Observations effectives (approx.) :", n_eff, "\n")
cat("Les p-values ci-dessous sont calculées comme si les", n_obs,
    "mois\nétaient indépendants. Ils ne le sont pas. À lire comme",
    "des indications,\npas comme des niveaux de preuve.\n")

# ── 3. T-002 : JOURS NORMAL CONTRE JOURS VACANCES ───────────
# Agrégation par date d'abord : les tranches horaires d'une même
# journée ne sont pas indépendantes (pseudoreplication).

jour_data <- horaire %>%
  filter(horaire_type %in% c("NORMAL", "VACANCES")) %>%
  group_by(date, horaire_type) %>%
  summarise(montees_jour = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

resume_t002 <- jour_data %>%
  group_by(horaire_type) %>%
  summarise(n = n(),
            mediane = round(median(montees_jour)),
            moyenne = round(mean(montees_jour)),
            ecart_type = round(sd(montees_jour)), .groups = "drop")

cat("\n=== T-002 : NORMAL CONTRE VACANCES ===\n")
print(as.data.frame(resume_t002))

med_normal   <- median(jour_data$montees_jour[jour_data$horaire_type == "NORMAL"])
med_vacances <- median(jour_data$montees_jour[jour_data$horaire_type == "VACANCES"])
baisse_pct   <- round(100 * (med_vacances - med_normal) / med_normal, 1)

mw_t002 <- wilcox.test(
  jour_data$montees_jour[jour_data$horaire_type == "NORMAL"],
  jour_data$montees_jour[jour_data$horaire_type == "VACANCES"],
  alternative = "greater", conf.int = TRUE, conf.level = 0.95
)

n_t002 <- nrow(jour_data)
r_t002 <- round(qnorm(mw_t002$p.value, lower.tail = FALSE) / sqrt(n_t002), 3)

cat("\nBaisse médiane en vacances :", baisse_pct, "%\n")
cat("Hodges-Lehmann :", round(mw_t002$estimate), "montées par jour",
    "| IC 95% borne inf. :", round(mw_t002$conf.int[1]), "\n")
cat("Taille d'effet r :", r_t002, "\n")
cat("p-value :", format(mw_t002$p.value, scientific = TRUE, digits = 3),
    "(autocorrélation non corrigée)\n")

# NOTE S-04 : la version d'avril convertissait cette différence en
# "bus remplis retirés" via une division par 600. Ce chiffre n'a
# aucune base : il ignore la répartition dans la journée, le taux
# d'occupation réel et la capacité par type de véhicule. Supprimé.

enregistrer(
  test_id = "T-002", script = "07_impact_covid.R",
  methode = "Mann-Whitney unilatéral, montées quotidiennes NORMAL contre VACANCES",
  n = n_t002, statistique = as.numeric(mw_t002$statistic), p_value = NA,
  effet_nom = "Hodges-Lehmann (montées/jour)",
  effet = round(mw_t002$estimate),
  ic_inf = round(mw_t002$conf.int[1]), ic_sup = NA,
  note = paste0("Baisse médiane ", baisse_pct, " %. r = ", r_t002,
                ". p-value exclue (S-10).")
)

# ── 4. T-004 : RUPTURES SUR LA SÉRIE OBSERVÉE ───────────────
# Vérification de continuité avant ts() (S-06).

mois_attendus <- seq(min(composantes$date), max(composantes$date), by = "month")
if (any(!mois_attendus %in% composantes$date))
  stop("Série discontinue : ts() attribuerait de fausses dates.")

ts_obs <- ts(composantes$observee,
             start = c(year(min(composantes$date)),
                       month(min(composantes$date))), frequency = 12)
pos_covid <- which(composantes$date == D_COVID)

cat("\n=== T-004 : RUPTURES, SÉRIE OBSERVÉE ===\n")
cat("Position de mars 2020 dans la série :", pos_covid, "sur", n_obs, "\n")

# Test de Chow à une date PRÉ-SPÉCIFIÉE.
# Cette date vient du calendrier (début de la pandémie), pas des
# données. Le test est donc légitime. À ne pas confondre avec un
# Chow appliqué à une date trouvée par Bai-Perron, qui utiliserait
# deux fois les mêmes données (voir S-08, script 14).
chow_t004 <- sctest(ts_obs ~ 1, type = "Chow", point = pos_covid)
cat("\nChow à mars 2020 (date pré-spécifiée, calendaire) :\n")
cat("  F =", round(chow_t004$statistic, 3),
    "| p =", format(chow_t004$p.value, scientific = TRUE, digits = 3), "\n")

# NOTE S-05 : ce F se rapporte à MARS 2020 et à rien d'autre.
# Le README d'avril l'attribuait à juin 2021, date issue de
# Bai-Perron. Les deux résultats sont distincts et ne doivent pas
# être mélangés.

cusum_t004 <- sctest(efp(ts_obs ~ 1, type = "OLS-CUSUM"))
cat("\nOLS-CUSUM (stabilité du niveau) :\n")
cat("  S =", round(cusum_t004$statistic, 3),
    "| p =", format(cusum_t004$p.value, scientific = TRUE, digits = 3), "\n")

# ── 5. S-01 : LE PRÉTENDU CUSUM CARRÉ ───────────────────────
# La version d'avril présentait efp(type = "RE") comme un
# "CUSUM carré" testant la stabilité de la VARIANCE. C'est faux :
# le test RE suit les estimations récursives des COEFFICIENTS,
# donc ici la moyenne. Sur un modèle à simple constante, il
# renvoie d'ailleurs la même valeur que le CUSUM. On le vérifie
# plutôt que de l'affirmer.

re_t004 <- sctest(efp(ts_obs ~ 1, type = "RE"))
cat("\n=== S-01 : VÉRIFICATION DU PRÉTENDU CUSUM CARRÉ ===\n")
cat("efp type OLS-CUSUM : S =", round(cusum_t004$statistic, 3), "\n")
cat("efp type RE        : S =", round(re_t004$statistic, 3), "\n")
cat("Statistiques identiques :",
    isTRUE(all.equal(as.numeric(cusum_t004$statistic),
                     as.numeric(re_t004$statistic))), "\n")
cat("Le test RE ne teste pas la variance. L'affirmation",
    "d'avril est retirée.\n")

# Vrai test de stabilité de la variance : Fligner-Killeen entre
# la période d'avant et la période actuelle, sur la série
# désaisonnalisée. Robuste à la non-normalité.
DEBUT_POST <- as.Date("2022-01-01")

var_data <- composantes %>%
  mutate(periode = case_when(
    date <  D_COVID    ~ "Avant 2020",
    date >= DEBUT_POST ~ "Depuis 2022",
    TRUE               ~ NA_character_)) %>%
  filter(!is.na(periode))

fk <- fligner.test(desaisonnalisee ~ factor(periode), data = var_data)
sd_pre  <- sd(var_data$desaisonnalisee[var_data$periode == "Avant 2020"])
sd_post <- sd(var_data$desaisonnalisee[var_data$periode == "Depuis 2022"])

cat("\nVrai test de variance (Fligner-Killeen, série désaisonnalisée) :\n")
cat("  Écart-type avant 2020 :", round(sd_pre  / 1e6, 3), "M\n")
cat("  Écart-type depuis 2022:", round(sd_post / 1e6, 3), "M",
    "| rapport :", round(sd_post / sd_pre, 2), "\n")
cat("  chi2 =", round(fk$statistic, 3),
    "| p =", format(fk$p.value, digits = 3), "\n")
if (fk$p.value < 0.05) {
  cat("  Conclusion : variance instable entre les deux périodes.\n")
} else {
  cat("  Conclusion : pas de preuve de changement de variance, malgré des\n")
  cat("  écarts-types assez différents. Avec environ", n_eff, "observations\n")
  cat("  effectives (S-10), ce test a peu de puissance : l'absence de preuve\n")
  cat("  n'est pas une preuve d'absence.\n")
}

# ── 6. BAI-PERRON SUR LA SÉRIE OBSERVÉE ─────────────────────
# Le nombre de ruptures est choisi par le BIC, sans imposition.

bp_obs   <- breakpoints(ts_obs ~ 1)
bic_obs  <- summary(bp_obs)$RSS["BIC", ]
m_opt_obs <- as.integer(names(which.min(bic_obs)))

cat("\nBai-Perron, série observée :\n")
cat("  BIC :", paste(names(bic_obs), round(bic_obs, 1),
                     sep = " = ", collapse = " | "), "\n")
cat("  Nombre de ruptures retenu par le BIC :", m_opt_obs, "\n")
if (m_opt_obs > 0) {
  dates_obs <- composantes$date[breakpoints(bp_obs, breaks = m_opt_obs)$breakpoints]
  cat("  Dates :", paste(format(dates_obs, "%B %Y"), collapse = " | "), "\n")
}

# ── 7. T-004b : RUPTURES SUR LA SÉRIE DÉSAISONNALISÉE ───────
# S-02 : la version d'avril testait les RÉSIDUS STL. Or les
# résidus valent observée moins tendance moins saisonnalité, et
# la tendance STL suit les changements de niveau. Chercher une
# rupture de niveau dans les résidus revient à chercher ce qu'on
# vient de retirer. On teste ici la série désaisonnalisée, qui
# conserve la tendance et les changements de niveau.
# Le script compare explicitement les deux pour que l'écart soit
# visible et documenté.

ts_des <- ts(composantes$desaisonnalisee,
             start = c(year(min(composantes$date)),
                       month(min(composantes$date))), frequency = 12)
ts_rem <- ts(composantes$residus,
             start = c(year(min(composantes$date)),
                       month(min(composantes$date))), frequency = 12)

analyse_bp <- function(serie, etiquette) {
  bp  <- breakpoints(serie ~ 1)
  bic <- summary(bp)$RSS["BIC", ]
  m   <- as.integer(names(which.min(bic)))
  dates <- if (m > 0) composantes$date[breakpoints(bp, breaks = m)$breakpoints] else as.Date(character(0))
  cat("\n", etiquette, "\n", sep = "")
  cat("  BIC :", paste(names(bic), round(bic, 1), sep = " = ", collapse = " | "), "\n")
  cat("  Ruptures retenues par le BIC :", m, "\n")
  if (m > 0) cat("  Dates :", paste(format(dates, "%B %Y"), collapse = " | "), "\n")
  cat("  Gain de BIC entre m = 0 et l'optimum :",
      round(bic["0"] - min(bic), 1), "points\n")
  list(m = m, dates = dates, bic = bic)
}

cat("\n=== T-004b : COMPARAISON DES DEUX SÉRIES (S-02) ===\n")
bp_rem <- analyse_bp(ts_rem, "Résidus STL (méthode d'avril, à ne pas utiliser)")
bp_des <- analyse_bp(ts_des, "Série désaisonnalisée (méthode retenue)")

cat("\nLecture : sur la série désaisonnalisée le gain de BIC est",
    "de", round(bp_des$bic["0"] - min(bp_des$bic), 1), "points,",
    "\ncontre", round(bp_rem$bic["0"] - min(bp_rem$bic), 1),
    "points sur les résidus. Les ruptures détectées sur les résidus",
    "\nsont faibles et ne correspondent pas aux événements connus.\n")
cat("Les dates affichées sont le DERNIER mois de chaque segment",
    "(convention de\nbreakpoints()). Le segment suivant commence le mois",
    "d'après.\n")

# NOTE : le script d'avril annonçait un seuil ("moins de 10 points
# de BIC = différence marginale"), obtenait un écart supérieur à ce
# seuil, et concluait quand même au caractère marginal. Le seuil
# est ici appliqué tel qu'annoncé, sans exception.

enregistrer(
  test_id = "T-004b", script = "07_impact_covid.R",
  methode = "Bai-Perron sur série désaisonnalisée, nombre de ruptures choisi par BIC",
  n = n_obs, statistique = NA, p_value = NA,
  effet_nom = "nombre de ruptures", effet = bp_des$m,
  note = paste0("Dates (dernier mois de chaque segment) : ",
                paste(format(bp_des$dates, "%Y-%m"), collapse = ", "),
                ". Gain BIC ", round(bp_des$bic["0"] - min(bp_des$bic), 1),
                " points. Méthode d'avril (résidus STL) circulaire, voir S-02.")
)

# ── 8. T-006 : NIVEAU PRÉ-COVID CONTRE NIVEAU ACTUEL (S-32) ─
# S-03 : un non-rejet n'est pas une preuve d'équivalence, et le
# groupe "depuis 2022" agrégeait une remontée et un plateau. On
# commence donc par la trajectoire annuelle, avant tout test.
#
# S-32 : la version du 19.09 comparait les deux périodes par
# Mann-Whitney et TOST, comme si les mois étaient indépendants
# (n effectif environ 10.7). Elle est remplacée par :
#   - une période "après" égale au DERNIER segment Bai-Perron de
#     la série désaisonnalisée (section 7), donc sans la remontée ;
#   - une régression du log de la série désaisonnalisée sur une
#     indicatrice de période : le coefficient donne l'écart en % ;
#   - une erreur type de Newey-West (HAC), retard fixé à 12 mois
#     AVANT de voir le résultat, qui tient compte de l'autocorrélation ;
#   - deux périmètres : réseau complet (lecture principale : niveau
#     atteint avec le réseau d'aujourd'hui) et quais desservis tous
#     les mois des deux périodes (robustesse : géographie constante).
#     Le périmètre par lignes est calculé et écarté (voir plus bas).

annuel <- composantes %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarise(n_mois = n(),
            mediane_M = round(median(observee) / 1e6, 2), .groups = "drop")

ref_2019 <- annuel$mediane_M[annuel$annee == 2019]
annuel <- annuel %>% mutate(pct_vs_2019 = round(100 * (mediane_M / ref_2019 - 1), 1))

cat("\n=== T-006 : TRAJECTOIRE ANNUELLE (S-03) ===\n")
print(as.data.frame(annuel))
cat("\nLes années depuis 2022 vont de ",
    min(annuel$pct_vs_2019[annuel$annee >= 2022]), " % à ",
    max(annuel$pct_vs_2019[annuel$annee >= 2022]),
    " % du niveau 2019.\nLes résumer par un seul groupe masquerait cette progression.\n", sep = "")

# 8a. Périodes
if (bp_des$m == 0) stop("Aucune rupture retenue : le dernier segment n'est pas défini.")
DEBUT_SEGMENT <- max(bp_des$dates) %m+% months(1)
if (DEBUT_SEGMENT <= D_COVID)
  stop("Le dernier segment commence avant mars 2020 : T-006 n'a pas de sens.")
MOIS_AVANT <- seq(min(composantes$date), D_COVID %m-% months(1), by = "month")
MOIS_APRES <- seq(DEBUT_SEGMENT, max(composantes$date), by = "month")
LAG_HAC <- 12

cat("\n=== T-006 : NIVEAUX COMPARÉS (S-32) ===\n")
cat("Avant  :", format(min(MOIS_AVANT), "%m.%Y"), "à", format(max(MOIS_AVANT), "%m.%Y"),
    "(", length(MOIS_AVANT), "mois )\n")
cat("Après  :", format(min(MOIS_APRES), "%m.%Y"), "à", format(max(MOIS_APRES), "%m.%Y"),
    "(", length(MOIS_APRES), "mois, dernier segment Bai-Perron )\n")
cat("Exclus : pandémie et remontée,", n_obs - length(MOIS_AVANT) - length(MOIS_APRES), "mois\n")
cat("Erreur type de Newey-West, retard", LAG_HAC, "mois, fixé à l'avance.\n")

# 8b. Estimation, identique pour chaque périmètre
desaisonnaliser <- function(serie_mensuelle) {
  # mêmes réglages que le script 06
  s <- stl(ts(serie_mensuelle$y,
              start = c(year(min(serie_mensuelle$date)), month(min(serie_mensuelle$date))),
              frequency = 12),
           s.window = "periodic", t.window = 13, robust = TRUE)
  serie_mensuelle$desaisonnalisee <- serie_mensuelle$y - as.numeric(s$time.series[, "seasonal"])
  serie_mensuelle
}

ecart_hac <- function(serie, etiquette) {
  d <- serie %>%
    filter(date %in% c(MOIS_AVANT, MOIS_APRES)) %>%
    mutate(apres = as.numeric(date %in% MOIS_APRES))
  f <- lm(log(desaisonnalisee) ~ apres, data = d)
  b <- coef(f)[["apres"]]
  se_hac  <- sqrt(NeweyWest(f, lag = LAG_HAC, prewhite = FALSE)["apres", "apres"])
  se_naif <- summary(f)$coefficients["apres", "Std. Error"]
  z <- qnorm(0.975)
  en_pct <- function(x) round(100 * (exp(x) - 1), 2)
  res <- data.frame(perimetre = etiquette, n = nrow(d),
                    ecart_pct = en_pct(b),
                    ic_inf = en_pct(b - z * se_hac), ic_sup = en_pct(b + z * se_hac),
                    ic_naif_inf = en_pct(b - z * se_naif), ic_naif_sup = en_pct(b + z * se_naif),
                    t_hac = round(b / se_hac, 3),
                    p_hac = signif(2 * pnorm(-abs(b / se_hac)), 3))
  cat("\n", etiquette, "\n", sep = "")
  cat("  Écart du dernier segment au niveau d'avant :", res$ecart_pct, "%\n")
  cat("  IC 95 % HAC : [", res$ic_inf, ";", res$ic_sup, "] %",
      "| IC si mois indépendants : [", res$ic_naif_inf, ";", res$ic_naif_sup, "] %\n")
  cat("  t HAC =", res$t_hac, "| p HAC =", format(res$p_hac), "\n")
  res
}

# Périmètre 1 : réseau complet (série du script 06)
res_reseau <- ecart_hac(composantes, "Réseau complet (lecture principale)")

# Périmètre 2 : quais desservis tous les mois des deux périodes
mensuel <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)

par_quai <- mensuel %>%
  group_by(arret_code_long, date) %>%
  summarise(x = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  filter(x > 0)

quais_constants <- par_quai %>%
  group_by(arret_code_long) %>%
  summarise(n_avant = sum(date %in% MOIS_AVANT),
            n_apres = sum(date %in% MOIS_APRES), .groups = "drop") %>%
  filter(n_avant == length(MOIS_AVANT), n_apres == length(MOIS_APRES)) %>%
  pull(arret_code_long)

part_couverte <- function(tab, cle, gardes, mois) {
  t <- tab %>% filter(date %in% mois)
  round(100 * sum(t$x[t[[cle]] %in% gardes]) / sum(t$x), 1)
}

cat("\nPérimètre des quais constants :", length(quais_constants), "quais sur",
    n_distinct(par_quai$arret_code_long), "\n")
cat("  Part des montées couverte : avant", part_couverte(par_quai, "arret_code_long", quais_constants, MOIS_AVANT),
    "% | après", part_couverte(par_quai, "arret_code_long", quais_constants, MOIS_APRES), "%\n")

serie_quais <- par_quai %>%
  filter(arret_code_long %in% quais_constants) %>%
  group_by(date) %>%
  summarise(y = sum(x), .groups = "drop") %>%
  arrange(date)
if (!identical(serie_quais$date, mois_attendus))
  stop("Série des quais constants discontinue.")
res_quais <- ecart_hac(desaisonnaliser(serie_quais), "Quais constants (robustesse)")

# Périmètre 3 : lignes présentes tous les mois des deux périodes.
# Calculé pour transparence et ÉCARTÉ : les lignes qui cessent
# avant 2020 et celles qui apparaissent ensuite sont exclues, donc
# les montées passées de l'une à l'autre sont perdues. L'écart
# obtenu est tiré vers le bas par construction.
par_ligne <- mensuel %>%
  group_by(ligne, date) %>%
  summarise(x = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  filter(x > 0)
vie_lignes <- par_ligne %>% group_by(ligne) %>%
  summarise(debut = min(date), fin = max(date),
            n_avant = sum(date %in% MOIS_AVANT),
            n_apres = sum(date %in% MOIS_APRES), .groups = "drop")
lignes_constantes <- vie_lignes$ligne[vie_lignes$n_avant == length(MOIS_AVANT) &
                                        vie_lignes$n_apres == length(MOIS_APRES)]
serie_lignes <- par_ligne %>%
  filter(ligne %in% lignes_constantes) %>%
  group_by(date) %>% summarise(y = sum(x), .groups = "drop") %>% arrange(date)

cat("\nPérimètre des lignes constantes :", length(lignes_constantes), "lignes sur",
    nrow(vie_lignes), "\n")
cat("  Lignes sans service après", format(D_COVID %m-% months(1), "%m.%Y"), ":",
    sum(vie_lignes$fin < D_COVID), "| lignes apparues après :",
    sum(vie_lignes$debut >= D_COVID), "\n")
cat("  Part des montées couverte : avant", part_couverte(par_ligne, "ligne", lignes_constantes, MOIS_AVANT),
    "% | après", part_couverte(par_ligne, "ligne", lignes_constantes, MOIS_APRES), "%\n")
res_lignes <- ecart_hac(desaisonnaliser(serie_lignes), "Lignes constantes (écarté, biais vers le bas)")

# 8c. Lecture
cat("\nFormulation retenue :\n")
cat("  Sur le réseau complet, le niveau du dernier segment (depuis ",
    format(DEBUT_SEGMENT, "%m.%Y"), ") est de\n  ", res_reseau$ecart_pct,
    " % par rapport au niveau d'avant 2020, IC 95 % HAC [", res_reseau$ic_inf, " ; ",
    res_reseau$ic_sup, "] %.\n", sep = "")
if (res_reseau$ic_inf > 0) {
  cat("  L'intervalle exclut zéro : le niveau d'avant la pandémie est dépassé.\n")
} else if (res_reseau$ic_sup < 0) {
  cat("  L'intervalle est entièrement négatif : le niveau d'avant n'est pas retrouvé.\n")
} else {
  cat("  L'intervalle contient zéro : l'écart n'est pas établi, dans un sens\n")
  cat("  comme dans l'autre.\n")
}
cat("  À géographie constante (quais), l'écart vaut ", res_quais$ecart_pct,
    " %, IC [", res_quais$ic_inf, " ; ", res_quais$ic_sup, "] %.\n", sep = "")
cat("  La différence entre les deux lectures (",
    round(res_reseau$ecart_pct - res_quais$ecart_pct, 1),
    " points) donne l'ordre de grandeur\n  de ce qui tient aux quais nouveaux ou ",
    "non desservis en continu.\n", sep = "")
cat("  L'erreur type HAC suppose une autocorrélation qui s'éteint en", LAG_HAC,
    "mois environ.\n  Avec des segments de niveau aussi persistants, l'intervalle",
    "reste à lire\n  comme un ordre de grandeur.\n")

enregistrer(
  test_id = "T-006", script = "07_impact_covid.R",
  methode = paste0("Régression log(série désaisonnalisée) ~ période, avant 2020 contre ",
                   "dernier segment Bai-Perron, erreur type Newey-West (retard ", LAG_HAC, ")"),
  n = res_reseau$n, statistique = res_reseau$t_hac, p_value = res_reseau$p_hac,
  effet_nom = "Écart du dernier segment au niveau d'avant 2020 (%)",
  effet = res_reseau$ecart_pct, ic_inf = res_reseau$ic_inf, ic_sup = res_reseau$ic_sup,
  note = paste0("Réseau complet. Dernier segment depuis ", format(DEBUT_SEGMENT, "%Y-%m"),
                ". Robustesse quais constants (", length(quais_constants), " quais) : ",
                res_quais$ecart_pct, " % [", res_quais$ic_inf, " ; ", res_quais$ic_sup,
                "]. Périmètre par lignes écarté (", res_lignes$ecart_pct,
                " %, lignes supprimées et créées exclues). Remplace Mann-Whitney et TOST (S-32).")
)

# ── 9. FIGURE : SÉRIE DÉSAISONNALISÉE ET RUPTURES ───────────

ruptures_df <- if (bp_des$m > 0) data.frame(date = bp_des$dates) else NULL

p_ruptures <- ggplot(composantes, aes(x = date)) +
  geom_line(aes(y = observee / 1e6), color = COL_NEUTRE,
            linewidth = 0.4, alpha = 0.6) +
  geom_line(aes(y = desaisonnalisee / 1e6), color = ROUGE_PRINCIPAL, linewidth = 0.9) +
  geom_vline(xintercept = D_COVID, linetype = "dashed",
             color = COL_COVID, linewidth = 0.5) +
  geom_vline(xintercept = D_GRATUITE, linetype = "dashed",
             color = COL_GRATUITE, linewidth = 0.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  labs(
    title    = "Ruptures structurelles de la fréquentation",
    subtitle = paste0("Série désaisonnalisée (rouge) et série observée (gris). ",
                      bp_des$m, " rupture(s) retenue(s) par le BIC,\n",
                      "traits pleins. Traits pointillés : COVID et gratuité jeunes."),
    x = NULL, y = "Millions de montées",
    caption = SOURCE_TPG
  ) +
  theme_projet()

if (!is.null(ruptures_df)) {
  p_ruptures <- p_ruptures +
    geom_vline(data = ruptures_df, aes(xintercept = date),
               color = COL_REF, linewidth = 0.6)
}

print(p_ruptures)
ggsave(file.path(DIR_FIG, "07_ruptures_structurelles.png"),
       p_ruptures, width = 12, height = 6, dpi = 150)
message("Figure enregistrée.")

# ── 10. SAUVEGARDE ──────────────────────────────────────────

write.csv(annuel, file.path(DIR_RES, paste0("07_trajectoire_annuelle_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(resume_t002, file.path(DIR_RES, paste0("07_T002_resume_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(data.frame(serie = c("residus", "desaisonnalisee"),
                     m_optimal = c(bp_rem$m, bp_des$m),
                     dates = c(paste(format(bp_rem$dates, "%Y-%m"), collapse = "; "),
                               paste(format(bp_des$dates, "%Y-%m"), collapse = "; "))),
          file.path(DIR_RES, paste0("07_ruptures_comparaison_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)

message("Script 07 terminé. Figures dans figures/, résultats dans resultats/.")
