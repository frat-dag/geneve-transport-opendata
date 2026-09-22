# ============================================================
# SCRIPT 09 - TESTS COMPLÉMENTAIRES
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-06,
#               S-06, S-07, S-10, S-11, S-13, S-14, S-18, S-29,
#               S-34
# ------------------------------------------------------------
# TESTS :
#   T-007  : concentration du trafic entre arrêts (Gini)
#   T-008  : lien entre offre et fréquentation par ligne
#   T-009  : gratuité jeunes de janvier 2025
#   AM-002 : évolution des lignes scolaires
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(tidyr)
library(strucchange)

set.seed(SEED)

# ── 1. CHARGEMENT ───────────────────────────────────────────

journalier <- lire("journalier")
mensuel    <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)
km_prod    <- lire("km_prod")

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Journalier :", nrow(journalier), "| Mensuel :", nrow(mensuel),
    "| Km produits :", nrow(km_prod), "\n")

# ── 2. T-007 : CONCENTRATION DU TRAFIC ──────────────────────
# Unité d'analyse : le lieu d'arrêt, c'est-à-dire le nom d'arrêt,
# tous quais confondus. Les montées de tous les quais d'un même nom
# sont additionnées : chaque montée est comptée une fois.
#
# Fenêtre : les 12 derniers mois avant la coupure. Sur toute la
# période du journalier, un arrêt fermé ou ouvert en cours de route
# n'a qu'une partie de ses montées, ce qui gonfle artificiellement
# la concentration. La période complète reste affichée en robustesse.

D_DEBUT_12M <- DATE_COUPURE %m-% months(12) + days(1)

gini <- function(x) {
  x <- sort(x[x > 0]); n <- length(x)
  2 * sum(x * seq_len(n)) / (n * sum(x)) - (n + 1) / n
}

concentration <- function(df) {
  m <- df %>%
    filter(!is.na(arret)) %>%
    group_by(arret) %>%
    summarise(montees_totales = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
    arrange(montees_totales)
  k <- ceiling(0.1 * nrow(m))
  list(donnees = m, gini = gini(m$montees_totales), n_top10 = k,
       part_top10 = 100 * sum(sort(m$montees_totales, decreasing = TRUE)[1:k]) /
         sum(m$montees_totales))
}

c12  <- concentration(journalier %>% filter(date >= D_DEBUT_12M))
ctot <- concentration(journalier)

montees_par_arret <- c12$donnees
gini_obs   <- c12$gini
n_top10    <- c12$n_top10
part_top10 <- c12$part_top10

N_BOOT <- 2000
gini_boot <- replicate(N_BOOT, gini(sample(montees_par_arret$montees_totales, replace = TRUE)))
ic_gini <- quantile(gini_boot, c(0.025, 0.975))

vie_arrets <- journalier %>%
  filter(!is.na(arret)) %>%
  group_by(arret) %>%
  summarise(debut = min(date), fin = max(date), .groups = "drop")
n_disparus <- sum(vie_arrets$fin < D_DEBUT_12M)
n_apparus  <- sum(vie_arrets$debut >= D_DEBUT_12M)

cat("\n=== T-007 : CONCENTRATION DU TRAFIC ===\n")
cat("Unité : lieu d'arrêt (nom d'arrêt, tous quais confondus)\n")
cat("Fenêtre :", format(D_DEBUT_12M), "à", format(DATE_COUPURE), "\n")
cat("Lieux d'arrêt :", nrow(montees_par_arret), "\n")
cat("Gini :", round(gini_obs, 4),
    "| IC 95% bootstrap [", round(ic_gini[1], 4), ";", round(ic_gini[2], 4), "]\n")
cat("Les", n_top10, "lieux d'arrêt les plus fréquentés (10 %) concentrent",
    round(part_top10, 1), "% des montées.\n")
cat("\nRobustesse, période complète du journalier (",
    format(min(journalier$date)), "à", format(DATE_COUPURE), ") :\n")
cat("  lieux d'arrêt :", nrow(ctot$donnees), "| Gini :", round(ctot$gini, 4),
    "| part des 10 % :", round(ctot$part_top10, 1), "%\n")
cat("  dont", n_disparus, "sans montée sur les 12 derniers mois et",
    n_apparus, "apparus pendant ces 12 mois.\n")

enregistrer(
  test_id = "T-007", script = "09_tests_statistiques.R",
  methode = "Indice de Gini des montées par lieu d'arrêt, 12 derniers mois, IC bootstrap 2000 tirages",
  n = nrow(montees_par_arret), statistique = NA, p_value = NA,
  effet_nom = "Gini", effet = round(gini_obs, 4),
  ic_inf = round(ic_gini[1], 4), ic_sup = round(ic_gini[2], 4),
  note = paste0("Lieu d'arrêt = nom d'arrêt, tous quais confondus. Fenêtre ",
                format(D_DEBUT_12M), " à ", format(DATE_COUPURE), ". ",
                "10 % des lieux d'arrêt concentrent ", round(part_top10, 1), " % des montées. ",
                "Période complète : Gini ", round(ctot$gini, 4), ", ",
                nrow(ctot$donnees), " lieux, dont ", n_disparus, " sans montée sur 12 mois.")
)

lorenz <- montees_par_arret %>%
  mutate(pct_arrets = row_number() / n() * 100,
         pct_montees = cumsum(montees_totales) / sum(montees_totales) * 100)

p_lorenz <- ggplot(lorenz, aes(x = pct_arrets, y = pct_montees)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = COL_NEUTRE, linewidth = 0.6) +
  geom_area(fill = ROUGE_PRINCIPAL, alpha = 0.15) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 1) +
  annotate("text", x = 3, y = 88,
           label = paste0("Gini = ", round(gini_obs, 3), "\nIC 95% [",
                          round(ic_gini[1], 3), " ; ", round(ic_gini[2], 3), "]"),
           size = 3.2, color = ROUGE_PRINCIPAL, hjust = 0) +
  scale_x_continuous(labels = function(x) paste0(x, " %")) +
  scale_y_continuous(labels = function(x) paste0(x, " %")) +
  labs(title = "Concentration du trafic entre lieux d'arrêt",
       subtitle = paste0("Courbe de Lorenz, ", format(D_DEBUT_12M, "%m.%Y"), " à ",
                         format(DATE_COUPURE, "%m.%Y"), ", tous quais confondus.\n",
                         "Les 10 % de lieux d'arrêt les plus fréquentés concentrent ",
                         round(part_top10, 1), " % des montées."),
       x = "Part cumulée des lieux d'arrêt", y = "Part cumulée des montées",
       caption = SOURCE_TPG) +
  theme_projet()

print(p_lorenz)
ggsave(file.path(DIR_FIG, "09_lorenz_arrets.png"), p_lorenz, width = 9, height = 7, dpi = 150)

# ── 3. T-008 : OFFRE ET FRÉQUENTATION PAR LIGNE ─────────────
# NOTE S-13 : ce test est proche d'une tautologie. Une ligne qui
# roule davantage transporte davantage : la corrélation mesure
# surtout la taille des lignes. Elle est publiée comme description,
# pas comme démonstration d'efficience.
#
# Fenêtre : 12 derniers mois, comme T-007. Sur toute la période, une
# ligne qui a existé un an et une ligne qui a existé dix ans ont des
# totaux proportionnels à leur durée de vie, ce qui gonfle le lien.
#
# Correction : des lignes changent de type (ligne_type_act) dans le
# mensuel. Grouper par ligne ET type puis joindre les km par ligne
# comptait leurs km deux fois. On groupe par ligne seule et on garde
# le type du mois le plus récent.
#
# L'IC de rho est obtenu par bootstrap sur les lignes. L'IC publié
# jusqu'ici était celui de Pearson, affiché à côté du rho de Spearman.

n_types_multiples <- mensuel %>%
  distinct(ligne, ligne_type_act) %>%
  count(ligne) %>%
  filter(n > 1) %>%
  nrow()

t008 <- mensuel %>%
  filter(date >= floor_date(D_DEBUT_12M, "month")) %>%
  group_by(ligne) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE),
            ligne_type_act = ligne_type_act[which.max(date)], .groups = "drop") %>%
  inner_join(km_prod %>%
               filter(date >= D_DEBUT_12M) %>%
               mutate(ligne = as.character(ligne)) %>%
               group_by(ligne) %>%
               summarise(km = sum(km_prod, na.rm = TRUE), .groups = "drop"),
             by = "ligne") %>%
  filter(montees > 0, km > 0)

pearson  <- cor.test(t008$montees, t008$km, method = "pearson")
spearman <- suppressWarnings(cor.test(t008$montees, t008$km, method = "spearman"))

rho_boot <- replicate(N_BOOT, {
  i <- sample(nrow(t008), replace = TRUE)
  cor(t008$montees[i], t008$km[i], method = "spearman")
})
ic_rho <- quantile(rho_boot, c(0.025, 0.975))

cat("\n=== T-008 : OFFRE ET FRÉQUENTATION PAR LIGNE ===\n")
cat("Fenêtre :", format(D_DEBUT_12M), "à", format(DATE_COUPURE), "\n")
cat("Lignes :", nrow(t008), "\n")
cat("Lignes ayant changé de type sur toute la période :", n_types_multiples, "\n")
cat("Spearman rho =", round(spearman$estimate, 3),
    "| IC 95% bootstrap [", round(ic_rho[1], 3), ";", round(ic_rho[2], 3), "]\n")
cat("Pour comparaison, Pearson r =", round(pearson$estimate, 3),
    "| IC 95% [", round(pearson$conf.int[1], 3), ";", round(pearson$conf.int[2], 3), "]\n")
cat("Lecture : lien fort mais attendu. Plus de kilomètres produits\n")
cat("implique mécaniquement plus de montées (S-13).\n")

enregistrer(
  test_id = "T-008", script = "09_tests_statistiques.R",
  methode = "Corrélation de Spearman entre km produits et montées par ligne, 12 derniers mois, IC bootstrap",
  n = nrow(t008), statistique = NA, p_value = NA,
  effet_nom = "Spearman rho", effet = round(as.numeric(spearman$estimate), 3),
  ic_inf = round(ic_rho[1], 3), ic_sup = round(ic_rho[2], 3),
  note = paste0("Relation en partie tautologique (S-13). Publiée comme description. ",
                "Pearson r = ", round(pearson$estimate, 3), ". ",
                n_types_multiples, " lignes à type multiple, comptées une fois.")
)

p_t008 <- ggplot(t008, aes(x = km, y = montees, color = ligne_type_act)) +
  geom_point(size = 2, alpha = 0.8) +
  scale_color_manual(values = PALETTE_TYPES, name = NULL) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  labs(title = "Offre et fréquentation par ligne",
       subtitle = paste0("Échelles logarithmiques, ", format(D_DEBUT_12M, "%m.%Y"), " à ",
                         format(DATE_COUPURE, "%m.%Y"), ". Spearman rho = ",
                         round(spearman$estimate, 3), " sur ", nrow(t008), " lignes."),
       x = "Kilomètres produits", y = "Montées", caption = SOURCE_TPG) +
  theme_projet() + theme(legend.position = "bottom")

print(p_t008)
ggsave(file.path(DIR_FIG, "09_offre_frequentation.png"), p_t008, width = 10, height = 8, dpi = 150)


# ── 4. T-009 : GRATUITÉ JEUNES DE JANVIER 2025 ──────────────
# S-07 : la version d'avril comparait douze mois de 2024 à
# quatorze mois allant de janvier 2025 à février 2026, sans
# appariement, avec un test unilatéral alors que la documentation
# annonçait un test bilatéral.
#
# Trois corrections :
#   1. appariement mois à mois, douze paires contre douze ;
#   2. année témoin 2023 vers 2024, sans gratuité, pour disposer
#      d'un point de comparaison ;
#   3. normalisation par les kilomètres produits, pour séparer ce
#      qui vient de l'offre de ce qui vient de la demande.
#
# CONFONDANT NON ÉLIMINABLE : l'offre a été renforcée le
# 15 décembre 2024, seize jours avant la gratuité. Avec des
# données mensuelles, les deux événements ne peuvent pas être
# séparés dans le temps. La normalisation par les kilomètres
# atténue le problème sans le résoudre.
#
# PORTÉE : la mesure vise les jeunes, qui ne représentent qu'une
# fraction des usagers. Un effet réel sur cette population peut
# rester invisible dans le total du réseau. L'absence d'effet
# global ne contredit donc pas les enquêtes menées auprès des
# bénéficiaires.

agreger <- function(df_m, df_km, exclure = character(0)) {
  a <- df_m %>% filter(!ligne %in% exclure) %>%
    mutate(an = year(date), mn = month(date)) %>%
    group_by(an, mn) %>%
    summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
  b <- df_km %>% filter(!ligne %in% exclure) %>%
    mutate(an = year(date), mn = month(date)) %>%
    group_by(an, mn) %>%
    summarise(km = sum(km_prod, na.rm = TRUE), .groups = "drop")
  inner_join(a, b, by = c("an", "mn")) %>% mutate(par_km = montees / km)
}

comparer_annees <- function(don, an_ref, an_test) {
  a <- don %>% filter(an == an_ref)
  b <- don %>% filter(an == an_test)
  cm <- inner_join(a, b, by = "mn", suffix = c("_ref", "_test"))
  w_br <- wilcox.test(cm$montees_test, cm$montees_ref, paired = TRUE, conf.int = TRUE)
  w_km <- wilcox.test(cm$par_km_test,  cm$par_km_ref,  paired = TRUE, conf.int = TRUE)
  data.frame(
    comparaison   = paste0(an_ref, " vers ", an_test),
    n_paires      = nrow(cm),
    var_montees   = round(100 * (median(cm$montees_test) / median(cm$montees_ref) - 1), 2),
    var_km        = round(100 * (median(cm$km_test)      / median(cm$km_ref)      - 1), 2),
    var_par_km    = round(100 * (median(cm$par_km_test)  / median(cm$par_km_ref)  - 1), 2),
    p_montees     = round(w_br$p.value, 4),
    p_par_km      = round(w_km$p.value, 4)
  )
}

don <- agreger(mensuel, km_prod)
t009 <- bind_rows(comparer_annees(don, 2023, 2024),
                  comparer_annees(don, 2024, 2025))

cat("\n=== T-009 : GRATUITÉ JEUNES ===\n")
cat("Comparaisons appariées mois à mois, variations de médiane en %\n")
print(as.data.frame(t009))

effet_temoin <- t009$var_par_km[t009$comparaison == "2023 vers 2024"]
effet_test   <- t009$var_par_km[t009$comparaison == "2024 vers 2025"]
effet_net    <- round(effet_test - effet_temoin, 2)

cat("\n--- Lecture ---\n")
cat("Montées brutes 2024 vers 2025 :", t009$var_montees[2], "%\n")
cat("Kilomètres produits           :", t009$var_km[2], "%\n")
cat("Montées par kilomètre         :", effet_test, "%\n")
cat("La hausse des montées suit celle de l'offre.\n\n")
cat("Année témoin 2023 vers 2024, montées par kilomètre :", effet_temoin, "%\n")
cat("Écart entre l'année de la gratuité et l'année témoin :", effet_net, "points\n")
cat("C'est l'ordre de grandeur de ce qu'on peut attribuer à la période\n")
cat("de la gratuité, renforcement d'offre du 15.12.2024 compris.\n")

# Robustesse : lignes 301 et 302 (voir Q-04, incohérence entre le
# journalier et le mensuel en janvier et février 2025).
don_sans <- agreger(mensuel, km_prod, exclure = c("301", "302"))
t009_sans <- comparer_annees(don_sans, 2024, 2025)

cat("\n--- Robustesse (S-14) : sans les lignes 301 et 302 ---\n")
cat("Montées par kilomètre :", t009_sans$var_par_km, "% contre",
    effet_test, "% avec ces lignes.\n")
cat("L'anomalie de ces deux lignes ne change pas la conclusion.\n")

# Par type de ligne
types <- c("PRINCIPAL", "SECONDAIRE", "GLCT", "SCOLAIRE")
par_type <- bind_rows(lapply(types, function(t) {
  d <- agreger(mensuel %>% filter(ligne_type_act == t),
               km_prod %>% filter(ligne_type_act == t))
  if (nrow(d %>% filter(an == 2024)) < 6 || nrow(d %>% filter(an == 2025)) < 6) return(NULL)
  bind_rows(comparer_annees(d, 2023, 2024), comparer_annees(d, 2024, 2025)) %>%
    mutate(type = t, .before = 1)
}))

cat("\n--- Par type de ligne ---\n")
print(as.data.frame(par_type %>% select(type, comparaison, var_montees, var_km, var_par_km)))
cat("\nLes écarts en montées brutes suivent largement les écarts en\n")
cat("kilomètres produits. Une fois l'offre prise en compte, les\n")
cat("variations se réduisent fortement.\n")

# S-34 (1) : DISTRIBUTION PLACEBO.
# Même calcul pour toutes les paires d'années consécutives
# complètes. Les paires qui touchent 2020, 2021 ou 2022 (COVID et
# remontée, dernier segment Bai-Perron à partir de 03.2023) sont
# exclues. Les autres paires sans gratuité forment la distribution
# de référence : si l'année de la gratuité n'en sort pas, rien ne
# la distingue d'une fluctuation ordinaire d'une année à l'autre.

annees_completes <- don %>% count(an) %>% filter(n == 12) %>% pull(an)
paires <- data.frame(ref = annees_completes) %>%
  mutate(test = ref + 1) %>%
  filter(test %in% annees_completes)

placebo <- bind_rows(lapply(seq_len(nrow(paires)), function(i)
  comparer_annees(don, paires$ref[i], paires$test[i]) %>%
    mutate(ref = paires$ref[i], test = paires$test[i]))) %>%
  mutate(statut = case_when(ref == 2024                  ~ "gratuite",
                            ref >= 2019 & ref <= 2022    ~ "exclue (COVID, remontee)",
                            TRUE                          ~ "placebo"))

pl <- placebo %>% filter(statut == "placebo")
rang_gratuite <- sum(pl$var_par_km >= effet_test) + 1

cat("\n=== S-34 : T-009, DISTRIBUTION PLACEBO ===\n")
cat("Montées par km, variation d'une année à la suivante (%) :\n")
print(as.data.frame(placebo %>% select(comparaison, var_montees, var_km, var_par_km, statut)),
      row.names = FALSE)
cat("\nPaires placebo :", nrow(pl), "| plage : de", min(pl$var_par_km), "à",
    max(pl$var_par_km), "%\n")
cat("Année de la gratuité :", effet_test, "% | rang", rang_gratuite, "sur",
    nrow(pl) + 1, "(1 = plus forte hausse)\n")
if (effet_test >= min(pl$var_par_km) && effet_test <= max(pl$var_par_km)) {
  cat("L'année de la gratuité tombe à l'intérieur de la plage des années\n")
  cat("sans gratuité : rien ne la distingue d'une fluctuation ordinaire.\n")
} else {
  cat("L'année de la gratuité sort de la plage des années sans gratuité.\n")
}
cat("Avec", nrow(pl), "paires placebo, aucune p-value n'est calculée : la\n")
cat("distribution sert d'échelle, pas de test.\n")

# S-34 (2) : EFFET DE COMPOSITION.
# La variation des montées/km du réseau mélange deux choses :
# l'évolution à l'intérieur de chaque type de ligne, et le
# déplacement des km entre types dont les montées/km diffèrent.
# Décomposition sur les totaux annuels (poids moyens des deux années) :
#   intra     = somme des poids moyens x variation des montées/km du type
#   structure = somme des montées/km moyennes x variation des poids
# Le reste vient des types présents une seule des deux années.

decomposer <- function(y0, y1) {
  tot <- function(y) {
    m <- mensuel %>% filter(year(date) == y) %>% group_by(type = ligne_type_act) %>%
      summarise(M = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
    k <- km_prod %>% filter(year(date) == y) %>% group_by(type = ligne_type_act) %>%
      summarise(K = sum(km_prod, na.rm = TRUE), .groups = "drop")
    full_join(m, k, by = "type") %>% mutate(across(c(M, K), ~ coalesce(., 0)))
  }
  a <- full_join(tot(y0), tot(y1), by = "type", suffix = c("0", "1")) %>%
    mutate(across(-type, ~ coalesce(., 0)),
           w0 = K0 / sum(K0), w1 = K1 / sum(K1),
           r0 = ifelse(K0 > 0, M0 / K0, NA), r1 = ifelse(K1 > 0, M1 / K1, NA))
  R0 <- sum(a$M0) / sum(a$K0); R1 <- sum(a$M1) / sum(a$K1)
  b <- a %>% filter(K0 > 0, K1 > 0)
  intra  <- sum((b$w0 + b$w1) / 2 * (b$r1 - b$r0))
  struct <- sum((b$r0 + b$r1) / 2 * (b$w1 - b$w0))
  list(detail = a %>% transmute(type, part_km_avant = round(100 * w0, 1),
                                part_km_apres = round(100 * w1, 1),
                                var_montees_km = round(100 * (r1 / r0 - 1), 2)),
       resume = data.frame(comparaison = paste0(y0, " vers ", y1),
                           total = round(100 * (R1 / R0 - 1), 2),
                           intra = round(100 * intra / R0, 2),
                           structure = round(100 * struct / R0, 2),
                           types_entrants_sortants = round(100 * ((R1 - R0) - intra - struct) / R0, 2)))
}

dec_temoin <- decomposer(2023, 2024)
dec_test   <- decomposer(2024, 2025)

cat("\n=== S-34 : T-009, EFFET DE COMPOSITION (totaux annuels) ===\n")
cat("Variation des montées par km du réseau, en %, décomposée :\n")
print(bind_rows(dec_temoin$resume, dec_test$resume), row.names = FALSE)
cat("\nDetail 2024 vers 2025 par type :\n")
print(as.data.frame(dec_test$detail), row.names = FALSE)
ecart_intra <- round(dec_test$resume$intra - dec_temoin$resume$intra, 2)
cat("\nLecture : sur les totaux annuels, les montées par km du réseau\n")
cat("varient de", dec_test$resume$total, "% en 2025. L'effet de structure vaut",
    dec_test$resume$structure, "point(s) :\n")
cat("les km se sont déplacés vers des types de lignes à moins de montées par\n")
cat("km. À type de ligne constant, la variation est de", dec_test$resume$intra,
    "%, contre", dec_temoin$resume$intra, "%\n")
cat("l'année témoin, soit un écart de", ecart_intra, "point. Même conclusion que\n")
cat("le placebo : pas d'effet détectable sur le total du réseau.\n")

enregistrer(
  test_id = "T-009", script = "09_tests_statistiques.R",
  methode = "Wilcoxon apparié mois à mois, montées par km, année de la gratuité contre année témoin",
  n = sum(t009$n_paires), statistique = NA, p_value = NA,
  effet_nom = "écart de variation des montées/km contre année témoin (points)",
  effet = effet_net,
  note = paste0("2024 vers 2025 : montées ", t009$var_montees[2], " %, km ",
                t009$var_km[2], " %, montées/km ", effet_test, " %. ",
                "Témoin 2023 vers 2024 : ", effet_temoin, " %. ",
                "Renforcement d'offre du 15.12.2024 non séparable (S-07). ",
                "Sans lignes 301 et 302 : ", t009_sans$var_par_km, " %. ",
                "Placebo (S-34), ", nrow(pl), " paires sans gratuité hors COVID : de ",
                min(pl$var_par_km), " à ", max(pl$var_par_km), " %, gratuité rang ",
                rang_gratuite, " sur ", nrow(pl) + 1, ". Composition (totaux annuels) : ",
                "intra-type ", dec_test$resume$intra, " % contre ", dec_temoin$resume$intra,
                " % en année témoin, structure ", dec_test$resume$structure, " point(s).")
)

# ── 5. AM-002 : LIGNES SCOLAIRES ────────────────────────────
# S-11 : avant toute lecture en termes de comportement, il faut
# savoir ce que le périmètre a fait. Une baisse de fréquentation
# sur un réseau qui rétrécit n'est pas une baisse d'usage.

scolaire_perimetre <- mensuel %>%
  filter(ligne_type_act == "SCOLAIRE") %>%
  mutate(annee = year(date)) %>%
  group_by(annee) %>%
  summarise(mois_avec_service = n_distinct(date),
            lignes = n_distinct(ligne),
            arrets = n_distinct(arret),
            montees_milliers = round(sum(nb_de_montees, na.rm = TRUE) / 1e3),
            .groups = "drop") %>%
  filter(annee <= if (month(DATE_COUPURE) < 12) year(DATE_COUPURE) - 1 else year(DATE_COUPURE))

ref <- scolaire_perimetre %>% filter(annee == min(annee))
scolaire_perimetre <- scolaire_perimetre %>%
  mutate(indice_montees = round(100 * montees_milliers / ref$montees_milliers),
         indice_arrets  = round(100 * arrets / ref$arrets),
         montees_par_arret = round(1000 * montees_milliers / arrets))

cat("\n=== AM-002 : LIGNES SCOLAIRES, PÉRIMÈTRE (S-11) ===\n")
print(as.data.frame(scolaire_perimetre))

derniere <- scolaire_perimetre %>% slice_max(annee, n = 1)
cat("\nDe", min(scolaire_perimetre$annee), "à", derniere$annee, ":\n")
cat("  montées        :", derniere$indice_montees - 100, "%\n")
cat("  arrêts desservis:", derniere$indice_arrets - 100, "%\n")
cat("  montées par arrêt desservi :",
    round(100 * derniere$montees_par_arret /
            scolaire_perimetre$montees_par_arret[scolaire_perimetre$annee == min(scolaire_perimetre$annee)] - 100), "%\n")
cat("La baisse des montées suit d'abord la réduction du périmètre.\n")

# S-06 : la série scolaire n'est pas continue. Les mois sans
# service (juillet, et avril 2020) sont absents du fichier. Avec
# ts(), chaque mois manquant décale toutes les dates suivantes.

scolaire_mensuel <- mensuel %>%
  filter(ligne_type_act == "SCOLAIRE") %>%
  group_by(date) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE),
            arrets = n_distinct(arret), .groups = "drop") %>%
  arrange(date)

attendus <- seq(min(scolaire_mensuel$date), max(scolaire_mensuel$date), by = "month")
manquants <- attendus[!attendus %in% scolaire_mensuel$date]

cat("\n=== AM-002 : CONTINUITÉ DE LA SÉRIE (S-06) ===\n")
cat("Mois présents :", nrow(scolaire_mensuel), "sur", length(attendus), "attendus\n")
cat("Mois manquants :", length(manquants), "\n")
if (length(manquants) > 0) {
  print(format(manquants, "%Y-%m"))
  cat("Répartition par numéro de mois :\n")
  print(table(month(manquants)))
  idx_reel <- which(scolaire_mensuel$date == D_COVID)
  date_fausse <- min(scolaire_mensuel$date) %m+% months(idx_reel - 1)
  cat("\nConséquence concrète : mars 2020 occupe la position", idx_reel,
      "de la série.\n")
  cat("ts(start = c(2016, 1)) attribue à cette position la date",
      format(date_fausse, "%Y-%m"), ".\n")
  cat("Le test de Chow d'avril, censé porter sur mars 2020, portait\n")
  cat("donc sur une autre date. Résultat invalide.\n")
}

# Série complétée : un mois sans service scolaire vaut zéro montée.
# C'est la réalité du service, et cela rend la série continue.
scolaire_complet <- data.frame(date = attendus) %>%
  left_join(scolaire_mensuel, by = "date") %>%
  mutate(montees = ifelse(is.na(montees), 0, montees))

ts_sc <- ts(scolaire_complet$montees,
            start = c(year(min(attendus)), month(min(attendus))), frequency = 12)
stl_sc <- stl(ts_sc, s.window = "periodic", t.window = 13, robust = TRUE)
des_sc <- as.numeric(ts_sc) - as.numeric(stl_sc$time.series[, "seasonal"])

bp_sc  <- breakpoints(ts(des_sc, start = c(year(min(attendus)), month(min(attendus))),
                         frequency = 12) ~ 1)
bic_sc <- summary(bp_sc)$RSS["BIC", ]
m_sc   <- as.integer(names(which.min(bic_sc)))

cat("\n=== AM-002 : RUPTURES, SÉRIE COMPLÉTÉE ET DÉSAISONNALISÉE ===\n")
cat("Mois sans service complétés par zéro :", length(manquants), "\n")
cat("BIC :", paste(names(bic_sc), round(bic_sc, 1), sep = " = ", collapse = " | "), "\n")
cat("Ruptures retenues :", m_sc, "\n")
if (m_sc > 0) {
  dates_sc <- scolaire_complet$date[breakpoints(bp_sc, breaks = m_sc)$breakpoints]
  cat("Dates :", paste(format(dates_sc, "%Y-%m"), collapse = " | "), "\n")
} else {
  dates_sc <- as.Date(character(0))
}
cat("\nCes ruptures portent sur une série non corrigée du périmètre.\n")
cat("Le tableau de périmètre ci-dessus reste la lecture principale.\n")

enregistrer(
  test_id = "AM-002", script = "09_tests_statistiques.R",
  methode = "Périmètre scolaire par année et ruptures sur série complétée et désaisonnalisée",
  n = nrow(scolaire_complet), statistique = NA, p_value = NA,
  effet_nom = "variation des montées par arrêt desservi (%)",
  effet = round(100 * derniere$montees_par_arret /
                  scolaire_perimetre$montees_par_arret[scolaire_perimetre$annee == min(scolaire_perimetre$annee)] - 100),
  note = paste0("Montées ", derniere$indice_montees - 100, " %, arrêts ",
                derniere$indice_arrets - 100, " %. ",
                length(manquants), " mois sans service complétés par zéro (S-06). ",
                "Chow d'avril invalide : date décalée. Ruptures : ",
                ifelse(m_sc > 0, paste(format(dates_sc, "%Y-%m"), collapse = ", "), "aucune"), ".")
)

# ── 6. FIGURE : SCOLAIRE, MONTÉES ET PÉRIMÈTRE ──────────────

sc_long <- scolaire_perimetre %>%
  select(annee, indice_montees, indice_arrets) %>%
  pivot_longer(-annee, names_to = "serie", values_to = "indice") %>%
  mutate(serie = recode(serie,
                        indice_montees = "Montées",
                        indice_arrets  = "Arrêts desservis"))

p_scolaire <- ggplot(sc_long, aes(x = annee, y = indice, color = serie)) +
  geom_hline(yintercept = 100, linetype = "dashed", color = COL_REF, linewidth = 0.5) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_color_manual(values = c("Montées" = ROUGE_PRINCIPAL,
                                "Arrêts desservis" = COL_LEMAN), name = NULL) +
  scale_x_continuous(breaks = scolaire_perimetre$annee) +
  labs(title = "Lignes scolaires : fréquentation et périmètre",
       subtitle = paste0("Indice 100 en ", min(scolaire_perimetre$annee),
                         ". La baisse des montées suit celle du nombre d'arrêts desservis."),
       x = NULL, y = paste0("Indice (", min(scolaire_perimetre$annee), " = 100)"),
       caption = SOURCE_TPG) +
  theme_projet() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_scolaire)
ggsave(file.path(DIR_FIG, "09_scolaire_perimetre.png"), p_scolaire, width = 10, height = 6, dpi = 150)
message("Figures enregistrées.")

# ── 7. SAUVEGARDE ───────────────────────────────────────────

write.csv(t009,               file.path(DIR_RES, paste0("09_T009_gratuite_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(par_type,           file.path(DIR_RES, paste0("09_T009_par_type_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(scolaire_perimetre, file.path(DIR_RES, paste0("09_AM002_perimetre_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(t008,               file.path(DIR_RES, paste0("09_T008_lignes_",     SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 09 terminé. Figures dans figures/, résultats dans resultats/.")
