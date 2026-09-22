# ============================================================
# SCRIPT 13 - OFFRE ET DEMANDE
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-04, T-06,
#               S-10, S-12, S-19, S-31, S-35
# ------------------------------------------------------------
# TESTS :
#   T-013a : rapport montées / km, jours NORMAL contre VACANCES
#   T-013b : rapport montées / km selon le mode (tram, trolleybus,
#            autobus)
#   T-013c : évolution du rapport dans le temps (journalier)
#   T-013d : montées / km annuelles depuis 2016 (mensuel), avec
#            décomposition et périmètre constant (S-35)
#
# S-12 : la version d'avril qualifiait les cinq lignes de tram de
# lignes en "site propre intégral" et concluait que l'efficience
# du site propre était formellement établie. Ces données ne
# contiennent aucune information sur l'infrastructure, et aucune
# source publique consultée ne permet d'affirmer que les trams
# genevois circulent intégralement en site propre. L'affirmation
# et la conclusion qui en découle sont retirées. Le script compare
# des modes de transport, pas des types d'infrastructure.
#
# S-19 : le rapport montées / km ignore la capacité des véhicules.
# Une rame de tram transporte plusieurs fois ce que transporte un
# autobus. Un rapport plus élevé par kilomètre est donc attendu
# pour le tram, sans rien dire du taux de remplissage.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

set.seed(SEED)   # position des points dans la figure des modes

# ── 1. CHARGEMENT ───────────────────────────────────────────

journalier <- lire("journalier")
km_prod    <- lire("km_prod") %>% mutate(ligne = as.character(ligne))

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Journalier :", nrow(journalier), "| Km produits :", nrow(km_prod), "\n")

# ── 2. MODE DE CHAQUE LIGNE, DÉDUIT DES DONNÉES ─────────────
# Le dataset collisions porte une catégorie de véhicule par ligne
# (ligne_categorie_hist). On s'en sert pour identifier les lignes de
# tram et de trolleybus plutôt que de recopier une liste de mémoire.
#
# S-31 : la version précédente ne gardait que les lignes dont la
# catégorie contenait TRAMWAY, TROLLEYBUS ou AUTOBUS. Les lignes
# classées GLCT, SCOLAIRE, AUTOBUS REGIONAL ou NOCTAMBUS, et celles
# sans aucune collision, n'avaient pas de mode et sortaient du test
# (31 lignes, 5 % des montées). Ce sont toutes des lignes d'autobus.
# Règle : tram ou trolleybus si la catégorie majoritaire de la ligne
# le dit, autobus pour toutes les autres lignes. Le contrôle ci-dessous
# vérifie que tram et trolleybus sont des catégories sans ambiguïté.

cat_collisions <- lire("collisions") %>%
  filter(!is.na(ligne), !is.na(ligne_categorie_hist)) %>%
  count(ligne, ligne_categorie_hist)

controle_modes <- cat_collisions %>%
  group_by(ligne) %>%
  filter(any(ligne_categorie_hist %in% c("TRAMWAY", "TROLLEYBUS"))) %>%
  summarise(categories = n_distinct(ligne_categorie_hist), .groups = "drop")
cat("\n=== MODES IDENTIFIÉS DEPUIS LES DONNÉES ===\n")
cat("Lignes ayant une collision tram ou trolleybus :", nrow(controle_modes),
    "| dont avec plusieurs catégories :", sum(controle_modes$categories > 1), "\n")

categories <- cat_collisions %>%
  group_by(ligne) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(mode = case_when(
    ligne_categorie_hist == "TRAMWAY"    ~ "Tram",
    ligne_categorie_hist == "TROLLEYBUS" ~ "Trolleybus",
    TRUE                                 ~ "Autobus")) %>%
  select(ligne, mode)

mode_de <- function(lignes) {
  m <- categories$mode[match(lignes, categories$ligne)]
  ifelse(is.na(m), "Autobus", m)
}

lignes_actives <- sort(unique(journalier$ligne[!is.na(journalier$ligne)]))
modes_actifs   <- mode_de(lignes_actives)
for (m in c("Tram", "Trolleybus", "Autobus")) {
  lg <- lignes_actives[modes_actifs == m]
  cat(m, ":", length(lg), "lignes ->", paste(head(lg, 15), collapse = ", "),
      ifelse(length(lg) > 15, "...", ""), "\n")
}
cat("Lignes du journalier absentes du dataset collisions, classées autobus :",
    sum(!lignes_actives %in% categories$ligne), "\n")

# ── 3. JEU DE DONNEES JOURNALIER : OFFRE ET DEMANDE ─────────

offre <- km_prod %>%
  group_by(date, horaire_type) %>%
  summarise(km = sum(km_prod, na.rm = TRUE), .groups = "drop")

demande <- journalier %>%
  group_by(date, horaire_type) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

reseau_jour <- inner_join(offre, demande, by = c("date", "horaire_type")) %>%
  filter(km > 0) %>%
  mutate(ratio = montees / km)

cat("\n=== OFFRE ET DEMANDE PAR JOUR ===\n")
cat("Jours :", nrow(reseau_jour), "de", format(min(reseau_jour$date)),
    "à", format(max(reseau_jour$date)), "\n")
print(as.data.frame(reseau_jour %>%
  group_by(horaire_type) %>%
  summarise(n = n(), km_moy = round(mean(km)),
            montees_moy = round(mean(montees)),
            ratio_median = round(median(ratio), 2), .groups = "drop")))

# ── 4. T-013a : JOURS NORMAL CONTRE VACANCES ────────────────
# Question : en vacances, la fréquentation baisse-t-elle plus vite
# que l'offre ? Le rapport montées / km répond directement.

t013a_data <- reseau_jour %>% filter(horaire_type %in% c("NORMAL", "VACANCES"))

mw_a <- wilcox.test(ratio ~ horaire_type, data = t013a_data,
                    alternative = "two.sided", conf.int = TRUE)

med_normal   <- median(t013a_data$ratio[t013a_data$horaire_type == "NORMAL"])
med_vacances <- median(t013a_data$ratio[t013a_data$horaire_type == "VACANCES"])
r_a <- round(abs(qnorm(mw_a$p.value / 2)) / sqrt(nrow(t013a_data)), 3)

cat("\n=== T-013a : RAPPORT MONTÉES / KM, NORMAL CONTRE VACANCES ===\n")
cat("Médiane NORMAL   :", round(med_normal, 2), "montées par km\n")
cat("Médiane VACANCES :", round(med_vacances, 2), "montées par km\n")
cat("Écart relatif    :", round(100 * (med_vacances / med_normal - 1), 1), "%\n")
cat("Hodges-Lehmann   :", round(mw_a$estimate, 3),
    "| IC 95% [", round(mw_a$conf.int[1], 3), ";",
    round(mw_a$conf.int[2], 3), "]\n")
cat("Taille d'effet r :", r_a, "\n")
cat("p =", format(mw_a$p.value, scientific = TRUE, digits = 3),
    "(jours consécutifs autocorrélés, voir S-10)\n")
cat("\nLecture : en vacances, l'offre est réduite mais la fréquentation\n")
cat("baisse davantage, donc chaque kilomètre produit porte moins de\n")
cat("montées.\n")

enregistrer(
  test_id = "T-013a", script = "13_offre_demande.R",
  methode = "Mann-Whitney bilatéral, rapport montées/km par jour, NORMAL contre VACANCES",
  n = nrow(t013a_data), statistique = as.numeric(mw_a$statistic), p_value = NA,
  effet_nom = "Hodges-Lehmann (montées/km)", effet = round(as.numeric(mw_a$estimate), 3),
  ic_inf = round(mw_a$conf.int[1], 3), ic_sup = round(mw_a$conf.int[2], 3),
  note = paste0("Médianes ", round(med_normal, 2), " contre ",
                round(med_vacances, 2), ". r = ", r_a, ". p exclue (S-10).")
)

# ── 5. T-013b : RAPPORT SELON LE MODE ───────────────────────
# Comparaison principale : toutes les lignes. Robustesse : lignes
# régulières seulement, sans les services particuliers (scolaire,
# Noctambus), dont le rapport montées / km n'est pas comparable.
# Comparaison entre lignes à un même moment : pas d'autocorrélation
# temporelle, la p-value est publiée.

type_ligne <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne) %>%
  summarise(ligne_type_act = ligne_type_act[which.max(date)], .groups = "drop")

ratio_ligne <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop") %>%
  inner_join(km_prod %>% group_by(ligne) %>%
               summarise(km = sum(km_prod, na.rm = TRUE), .groups = "drop"),
             by = "ligne") %>%
  filter(montees > 0, km > 0) %>%
  mutate(ratio = montees / km,
         mode = factor(mode_de(ligne), levels = c("Tram", "Trolleybus", "Autobus"))) %>%
  left_join(type_ligne, by = "ligne")

resumer <- function(d) {
  d %>% group_by(mode) %>%
    summarise(n = n(), mediane = round(median(ratio), 2),
              min = round(min(ratio), 2), max = round(max(ratio), 2),
              .groups = "drop")
}

resume_mode <- resumer(ratio_ligne)
reguliers   <- ratio_ligne %>%
  filter(!ligne_type_act %in% c("SCOLAIRE", "NOCTAMBUS REGIONAL"))
resume_reg  <- resumer(reguliers)

cat("\n=== T-013b : RAPPORT MONTÉES / KM SELON LE MODE ===\n")
cat("Toutes les lignes :\n")
print(as.data.frame(resume_mode))

kw_b   <- kruskal.test(ratio ~ mode, data = ratio_ligne)
kw_reg <- kruskal.test(ratio ~ mode, data = reguliers)
cat("\nKruskal-Wallis : H =", round(kw_b$statistic, 3),
    "| ddl =", kw_b$parameter,
    "| p =", format(kw_b$p.value, digits = 3), "\n")

cat("\nRobustesse, lignes régulières (sans scolaire ni Noctambus) :\n")
print(as.data.frame(resume_reg))
cat("Kruskal-Wallis : H =", round(kw_reg$statistic, 3),
    "| p =", format(kw_reg$p.value, digits = 3), "\n")

cat("\nEffectifs : tram", resume_mode$n[resume_mode$mode == "Tram"],
    "lignes, trolleybus", resume_mode$n[resume_mode$mode == "Trolleybus"],
    "lignes. Avec des effectifs aussi faibles, la puissance du test\n")
cat("est limitée et l'estimation peu précise.\n")

cat("\n--- Ce que ce résultat dit, et ce qu'il ne dit pas ---\n")
cat("Il dit : les lignes de tram portent plus de montées par kilomètre\n")
cat("  produit que les autres modes.\n")
cat("Il ne dit pas : que le tram serait plus efficace en soi.\n")
cat("  Une rame de tram offre plusieurs fois la capacité d'un autobus,\n")
cat("  donc un rapport plus élevé par kilomètre est attendu (S-19).\n")
cat("  Les cinq lignes de tram desservent en outre les axes les plus\n")
cat("  denses du réseau : la comparaison oppose des contextes, pas\n")
cat("  seulement des modes.\n")
cat("Il ne dit rien de l'infrastructure : ces données ne contiennent\n")
cat("  aucune information sur le site propre (S-12).\n")
cat("Aucune conclusion sur l'opportunité d'étendre le réseau de tram\n")
cat("  ne peut sortir de ce calcul.\n")

enregistrer(
  test_id = "T-013b", script = "13_offre_demande.R",
  methode = "Kruskal-Wallis, rapport montées/km par ligne selon le mode",
  n = nrow(ratio_ligne), statistique = round(as.numeric(kw_b$statistic), 3),
  p_value = signif(kw_b$p.value, 3),
  effet_nom = "médiane tram / médiane autobus",
  effet = round(resume_mode$mediane[resume_mode$mode == "Tram"] /
                  resume_mode$mediane[resume_mode$mode == "Autobus"], 2),
  note = paste0("Effectifs ", paste(resume_mode$mode, resume_mode$n,
                                    sep = " = ", collapse = ", "),
                ". Lignes hors dataset collisions classées autobus (S-31). ",
                "Lignes régulières seules : médiane autobus ",
                resume_reg$mediane[resume_reg$mode == "Autobus"], ", rapport tram / autobus ",
                round(resume_reg$mediane[resume_reg$mode == "Tram"] /
                        resume_reg$mediane[resume_reg$mode == "Autobus"], 2), ". ",
                "Capacité non prise en compte (S-19), infrastructure non mesurable (S-12).")
)

# ── 6. T-013c : ÉVOLUTION DU RAPPORT DANS LE TEMPS ──────────
# Corrélation de rang entre le mois et le rapport mensuel.
# L'autocorrélation de la série rend la p-value optimiste : on
# calcule la taille d'échantillon effective et on publie rho.

ratio_mensuel <- reseau_jour %>%
  mutate(mois = floor_date(date, "month")) %>%
  group_by(mois) %>%
  summarise(km = sum(km), montees = sum(montees), .groups = "drop") %>%
  mutate(ratio = montees / km, rang_mois = row_number()) %>%
  arrange(mois)

sp_c <- suppressWarnings(cor.test(ratio_mensuel$rang_mois, ratio_mensuel$ratio,
                                  method = "spearman"))

acf_ratio <- acf(ratio_mensuel$ratio, plot = FALSE, lag.max = 3)
r1_ratio <- as.numeric(acf_ratio$acf[2])
n_eff_ratio <- round(nrow(ratio_mensuel) * (1 - r1_ratio) / (1 + r1_ratio), 1)

cat("\n=== T-013c : ÉVOLUTION DU RAPPORT DANS LE TEMPS ===\n")
cat("Mois :", nrow(ratio_mensuel), "de", format(min(ratio_mensuel$mois)),
    "à", format(max(ratio_mensuel$mois)), "\n")
cat("Rapport au premier mois :", round(ratio_mensuel$ratio[1], 2),
    "| au dernier :", round(ratio_mensuel$ratio[nrow(ratio_mensuel)], 2),
    "| variation :",
    round(100 * (ratio_mensuel$ratio[nrow(ratio_mensuel)] /
                   ratio_mensuel$ratio[1] - 1), 1), "%\n")
cat("Spearman rho =", round(as.numeric(sp_c$estimate), 3),
    "| p =", format(sp_c$p.value, digits = 3), "\n")
cat("Autocorrélation au premier retard :", round(r1_ratio, 3), "\n")
cat("Observations effectives :", n_eff_ratio, "sur", nrow(ratio_mensuel), "\n")
cat("La p-value ci-dessus suppose des mois indépendants, ce qu'ils ne\n")
cat("sont pas. C'est rho et l'amplitude de la variation qui comptent.\n")

enregistrer(
  test_id = "T-013c", script = "13_offre_demande.R",
  methode = "Corrélation de Spearman entre le rang du mois et le rapport montées/km",
  n = nrow(ratio_mensuel), statistique = NA, p_value = NA,
  effet_nom = "Spearman rho", effet = round(as.numeric(sp_c$estimate), 3),
  note = paste0("Variation du rapport sur la période : ",
                round(100 * (ratio_mensuel$ratio[nrow(ratio_mensuel)] /
                               ratio_mensuel$ratio[1] - 1), 1),
                " %. n effectif environ ", n_eff_ratio, " (S-10).")
)

# ── 6b. T-013d : MONTÉES PAR KM DEPUIS 2016 (S-35) ──────────
# T-013c ne couvre que la période du journalier (depuis 02.2023).
# Le mensuel et les km produits remontent à 2016 : on suit le
# rapport annuel sur toute la période, années complètes seulement.
# Trois lectures :
#   1. réseau complet ;
#   2. décomposition (comme S-34) : évolution à type de ligne
#      constant (intra) et déplacement des km entre types (structure) ;
#   3. périmètre constant : lignes présentes tous les mois de la
#      première et de la dernière année complètes. Réserve (S-32) :
#      ce périmètre perd les lignes supprimées ou renumérotées.
# Le type d'une ligne est celui de son mois le plus récent.

mensuel <- lire("mensuel") %>%
  filter(!is.na(ligne)) %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE)

m_ligne <- mensuel %>%
  group_by(date, ligne) %>%
  summarise(M = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")
k_ligne <- km_prod %>%
  mutate(date = floor_date(as.Date(date), "month")) %>%
  group_by(date, ligne) %>%
  summarise(K = sum(km_prod, na.rm = TRUE), .groups = "drop")
type_recent <- mensuel %>%
  group_by(ligne) %>%
  summarise(type = ligne_type_act[which.max(date)], .groups = "drop")

ligne_mois <- full_join(m_ligne, k_ligne, by = c("date", "ligne")) %>%
  mutate(M = coalesce(M, 0), K = coalesce(K, 0), an = year(date))

coherence <- ligne_mois %>%
  summarise(montees_sans_km_pct = round(100 * sum(M[K == 0]) / sum(M), 2),
            km_sans_montees_pct = round(100 * sum(K[M == 0]) / sum(K), 2))

ligne_mois <- ligne_mois %>%
  filter(M > 0, K > 0) %>%
  left_join(type_recent, by = "ligne")

annees_d <- ligne_mois %>% distinct(an, date) %>% count(an) %>%
  filter(n == 12) %>% pull(an)
AN_DEBUT <- min(annees_d); AN_FIN <- max(annees_d)

annuel_d <- ligne_mois %>%
  filter(an %in% annees_d) %>%
  group_by(an) %>%
  summarise(montees_M = sum(M) / 1e6, km_M = sum(K) / 1e6, .groups = "drop") %>%
  mutate(montees_par_km = montees_M / km_M,
         indice_montees = round(100 * montees_M / montees_M[1]),
         indice_km      = round(100 * km_M / km_M[1]),
         indice_rapport = round(100 * montees_par_km / montees_par_km[1], 1))

var_rapport <- function(a0, a1) {
  r <- annuel_d$montees_par_km
  round(100 * (r[annuel_d$an == a1] / r[annuel_d$an == a0] - 1), 1)
}

decomposer_d <- function(y0, y1) {
  s <- function(y) ligne_mois %>% filter(an == y) %>% group_by(type) %>%
    summarise(M = sum(M), K = sum(K), .groups = "drop")
  a <- full_join(s(y0), s(y1), by = "type", suffix = c("0", "1")) %>%
    mutate(across(-type, ~ coalesce(., 0)),
           w0 = K0 / sum(K0), w1 = K1 / sum(K1),
           r0 = ifelse(K0 > 0, M0 / K0, NA), r1 = ifelse(K1 > 0, M1 / K1, NA))
  R0 <- sum(a$M0) / sum(a$K0); R1 <- sum(a$M1) / sum(a$K1)
  b <- a %>% filter(K0 > 0, K1 > 0)
  intra  <- sum((b$w0 + b$w1) / 2 * (b$r1 - b$r0))
  struct <- sum((b$r0 + b$r1) / 2 * (b$w1 - b$w0))
  list(detail = a %>% transmute(type, part_km_debut = round(100 * w0, 1),
                                part_km_fin = round(100 * w1, 1),
                                var_montees_km = round(100 * (r1 / r0 - 1), 1)),
       resume = data.frame(comparaison = paste0(y0, " vers ", y1),
                           total = round(100 * (R1 / R0 - 1), 1),
                           intra = round(100 * intra / R0, 1),
                           structure = round(100 * struct / R0, 1),
                           types_entrants_sortants = round(100 * ((R1 - R0) - intra - struct) / R0, 1)))
}

dec_debut <- decomposer_d(AN_DEBUT, AN_FIN)
dec_2019  <- decomposer_d(2019, AN_FIN)

presence <- ligne_mois %>%
  filter(an %in% c(AN_DEBUT, AN_FIN)) %>%
  group_by(ligne) %>%
  summarise(n_debut = n_distinct(date[an == AN_DEBUT]),
            n_fin   = n_distinct(date[an == AN_FIN]), .groups = "drop")
lignes_cst <- presence$ligne[presence$n_debut == 12 & presence$n_fin == 12]
part_cst <- function(y) round(100 * sum(ligne_mois$M[ligne_mois$an == y & ligne_mois$ligne %in% lignes_cst]) /
                                sum(ligne_mois$M[ligne_mois$an == y]), 1)
annuel_cst <- ligne_mois %>%
  filter(ligne %in% lignes_cst, an %in% annees_d) %>%
  group_by(an) %>%
  summarise(M = sum(M), K = sum(K), .groups = "drop") %>%
  mutate(r = M / K,
         indice_montees = round(100 * M / M[1]),
         indice_km      = round(100 * K / K[1]),
         indice_rapport = round(100 * r / r[1], 1))
var_cst <- function(a0, a1) round(100 * (annuel_cst$r[annuel_cst$an == a1] /
                                           annuel_cst$r[annuel_cst$an == a0] - 1), 1)

cat("\n=== T-013d : MONTÉES PAR KM DEPUIS", AN_DEBUT, "(S-35) ===\n")
cat("Cohérence ligne par ligne : montées sans km", coherence$montees_sans_km_pct,
    "% | km sans montées", coherence$km_sans_montees_pct, "%\n")
cat("Années complètes :", AN_DEBUT, "à", AN_FIN, "\n\n")
print(as.data.frame(annuel_d %>% mutate(across(c(montees_M, km_M, montees_par_km), ~ round(., 2)))),
      row.names = FALSE)
cat("\nRéseau complet : rapport", var_rapport(AN_DEBUT, AN_FIN), "% de", AN_DEBUT, "à", AN_FIN,
    "|", var_rapport(2019, AN_FIN), "% depuis 2019\n")

cat("\nDécomposition (points de %) :\n")
print(bind_rows(dec_debut$resume, dec_2019$resume), row.names = FALSE)
cat("\nDétail", AN_DEBUT, "vers", AN_FIN, "par type (type le plus récent de chaque ligne) :\n")
print(as.data.frame(dec_debut$detail), row.names = FALSE)

cat("\nPérimètre constant :", length(lignes_cst), "lignes présentes tous les mois de",
    AN_DEBUT, "et de", AN_FIN, "\n")
cat("  Part des montées couverte :", part_cst(AN_DEBUT), "% en", AN_DEBUT, "|",
    part_cst(AN_FIN), "% en", AN_FIN, "\n")
print(as.data.frame(annuel_cst %>% select(an, indice_montees, indice_km, indice_rapport)),
      row.names = FALSE)
cat("  Rapport :", var_cst(AN_DEBUT, AN_FIN), "% de", AN_DEBUT, "à", AN_FIN, "|",
    var_cst(2019, AN_FIN), "% depuis 2019\n")

cat("\nLecture : l'offre a augmenté bien plus vite que la fréquentation.\n")
cat("Le déplacement des km vers des lignes moins chargées (structure) explique\n")
cat(-dec_debut$resume$structure, "point(s) de la baisse depuis", AN_DEBUT, "et",
    -dec_2019$resume$structure, "depuis 2019. Le reste est une baisse du rapport\n")
cat("à type de ligne constant, que le périmètre constant confirme.\n")
cat("Le niveau d'avant 2020 n'est pas retrouvé. Ces données ne permettent pas\n")
cat("d'en identifier la cause.\n\n")

enregistrer(
  test_id = "T-013d", script = "13_offre_demande.R",
  methode = paste0("Montées/km annuelles ", AN_DEBUT, "-", AN_FIN,
                   " (mensuel et km produits), décomposition intra-type et structure, périmètre constant"),
  n = length(annees_d), statistique = NA, p_value = NA,
  effet_nom = paste0("variation du rapport montées/km ", AN_DEBUT, " à ", AN_FIN, " (%)"),
  effet = var_rapport(AN_DEBUT, AN_FIN),
  note = paste0("Montées +", annuel_d$indice_montees[annuel_d$an == AN_FIN] - 100, " %, km +",
                annuel_d$indice_km[annuel_d$an == AN_FIN] - 100, " %. Depuis 2019 : ",
                var_rapport(2019, AN_FIN), " %. Décomposition ", AN_DEBUT, "-", AN_FIN,
                " : intra ", dec_debut$resume$intra, ", structure ", dec_debut$resume$structure,
                ", types disparus ", dec_debut$resume$types_entrants_sortants,
                ". Périmètre constant (", length(lignes_cst), " lignes, ", part_cst(AN_DEBUT),
                " à ", part_cst(AN_FIN), " % des montées) : ", var_cst(AN_DEBUT, AN_FIN),
                " %, depuis 2019 ", var_cst(2019, AN_FIN), " % (S-35).")
)

# ── 7. FIGURES ──────────────────────────────────────────────

p_mode <- ggplot(ratio_ligne, aes(x = mode, y = ratio, fill = mode)) +
  geom_boxplot(alpha = 0.55, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(color = mode), width = 0.12, height = 0, size = 2.2, alpha = 0.8) +
  scale_fill_manual(values = c("Tram" = ROUGE_PRINCIPAL, "Trolleybus" = COL_LEMAN,
                               "Autobus" = COL_NEUTRE), guide = "none") +
  scale_color_manual(values = c("Tram" = ROUGE_PRINCIPAL, "Trolleybus" = COL_LEMAN,
                                "Autobus" = COL_NEUTRE), guide = "none") +
  labs(title = "Montées par kilomètre produit, selon le mode",
       subtitle = paste0("Une ligne par point, ", nrow(ratio_ligne), " lignes. ",
                         "Modes identifiés dans les données tpg. Médianes : ",
                         paste(resume_mode$mode, resume_mode$mediane,
                               sep = " ", collapse = ", "), "."),
       x = NULL, y = "Montées par kilomètre",
       caption = paste(SOURCE_TPG,
                       "La capacité des véhicules n'entre pas dans ce rapport.",
                       sep = "\n")) +
  theme_projet() + theme(panel.grid.major.x = element_blank())

print(p_mode)
ggsave(file.path(DIR_FIG, "13_ratio_par_mode.png"), p_mode, width = 9, height = 7, dpi = 150)

p_temps <- ggplot(ratio_mensuel, aes(x = mois, y = ratio)) +
  geom_line(color = ROUGE_PRINCIPAL, linewidth = 0.8) +
  geom_vline(xintercept = D_ETAPE_DEC2024, linetype = "dashed",
             color = COL_NEUTRE, linewidth = 0.5) +
  geom_vline(xintercept = D_GRATUITE, linetype = "dashed",
             color = COL_GRATUITE, linewidth = 0.5) +
  scale_x_date(date_breaks = "6 months", date_labels = "%m.%Y") +
  labs(title = "Montées par kilomètre produit, mois par mois",
       subtitle = paste0("Rapport entre la fréquentation et l'offre. Spearman rho = ",
                         round(as.numeric(sp_c$estimate), 3),
                         ".\nTraits : renforcement d'offre de décembre 2024 et gratuité jeunes."),
       x = NULL, y = "Montées par kilomètre", caption = SOURCE_TPG) +
  theme_projet() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_temps)
ggsave(file.path(DIR_FIG, "13_ratio_temporel.png"), p_temps, width = 12, height = 6, dpi = 150)
message("Figures enregistrées.")

# ── 8. SAUVEGARDE ───────────────────────────────────────────

write.csv(ratio_ligne %>% mutate(ratio = round(ratio, 3)),
          file.path(DIR_RES, paste0("13_ratio_par_ligne_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(ratio_mensuel %>% mutate(ratio = round(ratio, 3)),
          file.path(DIR_RES, paste0("13_ratio_mensuel_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(resume_mode,
          file.path(DIR_RES, paste0("13_resume_modes_", SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 13 terminé. Figures dans figures/, résultats dans resultats/.")
