# ============================================================
# SCRIPT 11 - COLLISIONS ET PROXIMITÉ DES ARRÊTS
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-03, R-05, R-07, R-08, R-09, T-04,
#               T-06, S-16, S-20
# ------------------------------------------------------------
# QUESTIONS :
#   Près de quels arrêts se produisent le plus de collisions ?
#   Ce classement mesure-t-il un risque ?
#   T-011 : la part de collisions avec blessé varie-t-elle selon
#           la saison ?
#
# AVERTISSEMENT (S-20) : compter les collisions par arrêt revient
# largement à classer les arrêts par le trafic qui passe devant.
# Le script produit donc aussi un rapport à l'exposition, mais le
# seul dénominateur disponible par arrêt est le nombre de montées,
# qui mesure les voyageurs et non les passages de véhicules. Un
# arrêt peu fréquenté situé sur un axe très circulé ressort alors
# artificiellement haut. Aucun des deux classements n'est un
# classement de dangerosité, et le script le montre en comparant
# les deux.
#
# La proximité n'est pas une cause : une collision assignée à un
# arrêt s'est produite près de lui, pas nécessairement à cause de
# lui ni pendant une manœuvre de desserte.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)

# ── 1. CHARGEMENT AUTONOME (R-03) ───────────────────────────

collisions <- lire("collisions") %>%
  filter(!is.na(latitude), !is.na(longitude))

arrets <- readRDS(file.path(DIR_RAW, "arrets.rds")) %>%
  filter(actif == "Y") %>%
  separate(coordonnees, into = c("latitude", "longitude"),
           sep = ", ", convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Collisions géolocalisées :", nrow(collisions), "\n")
cat("Arrêts actifs géolocalisés :", nrow(arrets), "\n")

cat("\nEmprise des arrêts     : lat",
    round(range(arrets$latitude), 4), "| lon", round(range(arrets$longitude), 4), "\n")
cat("Emprise des collisions : lat",
    round(range(collisions$latitude), 4), "| lon", round(range(collisions$longitude), 4), "\n")

# ── 2. DISTANCES ────────────────────────────────────────────
# Haversine, calcul par blocs pour tenir en mémoire.
# Attention : pmin(sqrt(a), 1) et non pmin(1, sqrt(a)), sinon les
# dimensions de la matrice sont perdues et le résultat est faux.

RAYON_TERRE <- 6371000
rad <- pi / 180

distances_min <- function(lat1, lon1, lat2, lon2, taille_bloc = 500) {
  la1 <- lat1 * rad; lo1 <- lon1 * rad
  la2 <- lat2 * rad; lo2 <- lon2 * rad
  n <- length(la1)
  idx <- integer(n); dst <- numeric(n)
  for (i in seq(1, n, by = taille_bloc)) {
    j <- i:min(i + taille_bloc - 1, n)
    dlat <- outer(la1[j], la2, "-")
    dlon <- outer(lo1[j], lo2, "-")
    a <- sin(dlat / 2)^2 + outer(cos(la1[j]), cos(la2), "*") * sin(dlon / 2)^2
    d <- 2 * RAYON_TERRE * asin(pmin(sqrt(a), 1))
    idx[j] <- max.col(-d, ties.method = "first")
    dst[j] <- d[cbind(seq_along(j), idx[j])]
  }
  list(index = idx, distance = dst)
}

jointure <- distances_min(collisions$latitude, collisions$longitude,
                          arrets$latitude, arrets$longitude)

collisions <- collisions %>%
  mutate(arret_code = arrets$arretcodelong[jointure$index],
         arret_nom  = arrets$nomarret[jointure$index],
         distance_m = jointure$distance)

cat("\n=== DISTANCE ENTRE UNE COLLISION ET L'ARRÊT LE PLUS PROCHE ===\n")
print(round(quantile(collisions$distance_m, c(0.5, 0.75, 0.9, 0.95, 0.99, 1))))

# ── 3. CHOIX DU SEUIL ───────────────────────────────────────
# La distance entre arrêts voisins fixe la résolution possible :
# si deux arrêts sont à 32 mètres l'un de l'autre, un rayon large
# attribuerait la même collision aux deux. D'où l'assignation au
# plus proche, et un seuil qui sert seulement à écarter les
# collisions trop éloignées de tout arrêt.

voisins <- numeric(nrow(arrets))
for (i in seq(1, nrow(arrets), by = 500)) {
  j <- i:min(i + 499, nrow(arrets))
  dlat <- outer(arrets$latitude[j] * rad, arrets$latitude * rad, "-")
  dlon <- outer(arrets$longitude[j] * rad, arrets$longitude * rad, "-")
  a <- sin(dlat / 2)^2 +
    outer(cos(arrets$latitude[j] * rad), cos(arrets$latitude * rad), "*") *
    sin(dlon / 2)^2
  d <- 2 * RAYON_TERRE * asin(pmin(sqrt(a), 1))
  d[cbind(seq_along(j), j)] <- Inf
  voisins[j] <- apply(d, 1, min)
}

cat("\n=== DISTANCE ENTRE DEUX ARRÊTS VOISINS ===\n")
print(round(quantile(voisins, c(0.1, 0.25, 0.5, 0.75, 0.9))))

SEUIL_M <- 200
pct_retenu <- round(100 * mean(collisions$distance_m <= SEUIL_M), 1)
percentile_seuil <- round(100 * mean(collisions$distance_m <= SEUIL_M), 1)

cat("\nSeuil retenu :", SEUIL_M, "mètres\n")
cat("Collisions retenues :", sum(collisions$distance_m <= SEUIL_M),
    "sur", nrow(collisions), "soit", pct_retenu, "%\n")
cat("Le seuil correspond au percentile", percentile_seuil,
    "des distances observées.\n")

col_proche <- collisions %>% filter(distance_m <= SEUIL_M)

ecartees <- collisions %>% filter(distance_m > SEUIL_M)
cat("Collisions écartées :", nrow(ecartees), "\n")
if (nrow(ecartees) > 0) {
  cat("Leur répartition par type de ligne :\n")
  print(sort(table(ecartees$ligne_type_hist), decreasing = TRUE)[1:5])
}

# ── 4. CLASSEMENT BRUT DES ARRÊTS ───────────────────────────

par_arret <- col_proche %>%
  count(arret_code, arret_nom, name = "n_collisions") %>%
  arrange(desc(n_collisions))

cat("\n=== ARRÊTS PRÈS DESQUELS LE PLUS DE COLLISIONS SURVIENNENT ===\n")
print(as.data.frame(par_arret %>% slice_head(n = 15)))
cat("\nCe classement ne mesure pas un risque : il suit d'abord le\n")
cat("trafic qui passe devant chaque arrêt.\n")

# ── 5. RAPPORT À L'EXPOSITION (S-20) ────────────────────────
# Dénominateur disponible : les montées par arrêt. Il mesure les
# voyageurs, pas les passages de véhicules. On restreint aux
# arrêts suffisamment fréquentés pour que le rapport ne soit pas
# dominé par le bruit.

SEUIL_MONTEES <- 1e6

exposition <- lire("mensuel") %>%
  mutate(date = ym(mois)) %>%
  filter(date <= DATE_COUPURE) %>%
  group_by(arret_code_long) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

annee_min_expo <- 2016   # le mensuel commence en 2016

compar <- col_proche %>%
  filter(year(jour) >= annee_min_expo) %>%
  count(arret_code, arret_nom, name = "n_collisions") %>%
  inner_join(exposition, by = c("arret_code" = "arret_code_long")) %>%
  filter(montees >= SEUIL_MONTEES) %>%
  mutate(collisions_par_Mmontees = round(n_collisions / (montees / 1e6), 1))

cat("\n=== RAPPORT À L'EXPOSITION ===\n")
cat("Arrêts retenus (au moins", format(SEUIL_MONTEES, big.mark = " ", scientific = FALSE),
    "montées) :", nrow(compar), "\n")

cat("\nDix premiers par nombre de collisions :\n")
print(as.data.frame(compar %>% arrange(desc(n_collisions)) %>% slice_head(n = 10) %>%
  transmute(arret_nom, n_collisions, montees_M = round(montees / 1e6, 1),
            collisions_par_Mmontees)))

cat("\nDix premiers par collisions pour un million de montées :\n")
print(as.data.frame(compar %>% arrange(desc(collisions_par_Mmontees)) %>% slice_head(n = 10) %>%
  transmute(arret_nom, n_collisions, montees_M = round(montees / 1e6, 1),
            collisions_par_Mmontees)))

rho_classements <- cor(rank(-compar$n_collisions),
                       rank(-compar$collisions_par_Mmontees),
                       method = "spearman")

cat("\nCorrélation de rang entre les deux classements :",
    round(rho_classements, 3), "\n")
cat("Les deux classements se recoupent en partie sans se confondre :\n")
cat("certains arrêts très fréquentés sortent du haut du tableau une\n")
cat("fois la fréquentation prise en compte, et inversement.\n")
cat("Ni l'un ni l'autre ne mesure la dangerosité. Le dénominateur\n")
cat("correct serait le nombre de passages de véhicules, qui n'est\n")
cat("pas publié par arrêt (S-20).\n")

enregistrer(
  test_id = "OBS-COLLISIONS-ARRETS", script = "11_collisions_spatial.R",
  methode = "Assignation de chaque collision à l'arrêt actif le plus proche, seuil 200 m",
  n = nrow(col_proche), statistique = NA, p_value = NA,
  effet_nom = "corrélation de rang entre classement brut et classement rapporté aux montées",
  effet = round(rho_classements, 3),
  note = paste0(pct_retenu, " % des collisions dans le seuil. ",
                "Dénominateur imparfait : montées et non passages (S-20).")
)

# ── 6. T-011 : PART DE COLLISIONS AVEC BLESSÉ SELON LA SAISON ─

col_proche <- col_proche %>%
  mutate(
    mois_num = month(jour),
    saison = case_when(
      mois_num %in% c(3, 4, 5)  ~ "Printemps",
      mois_num %in% c(6, 7, 8)  ~ "Été",
      mois_num %in% c(9, 10, 11) ~ "Automne",
      TRUE                      ~ "Hiver"),
    saison = factor(saison, levels = c("Printemps", "Été", "Automne", "Hiver")),
    avec_blesse = niv_blessure_humain > 0,
    periode = ifelse(saison %in% c("Printemps", "Été"),
                     "Printemps et été", "Automne et hiver")
  )

resume_saison <- col_proche %>%
  group_by(saison) %>%
  summarise(n = n(),
            part_blesse_pct = round(100 * mean(avec_blesse, na.rm = TRUE), 2),
            n_avec_blesse = sum(avec_blesse, na.rm = TRUE),
            severite_moy = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
            .groups = "drop")

cat("\n=== T-011 : COLLISIONS AVEC BLESSÉ SELON LA SAISON ===\n")
print(as.data.frame(resume_saison))

table_chi2 <- table(col_proche$periode, col_proche$avec_blesse)
cat("\nTable de contingence :\n"); print(table_chi2)

chi2 <- chisq.test(table_chi2)
cat("\nEffectifs attendus :\n"); print(round(chi2$expected, 1))
cat("Condition des effectifs attendus supérieurs à 5 :",
    all(chi2$expected > 5), "\n")

n_chi <- sum(table_chi2)
v_cramer <- round(sqrt(as.numeric(chi2$statistic) /
                         (n_chi * (min(dim(table_chi2)) - 1))), 4)

part_beau <- round(100 * mean(col_proche$avec_blesse[col_proche$periode == "Printemps et été"], na.rm = TRUE), 2)
part_mauvais <- round(100 * mean(col_proche$avec_blesse[col_proche$periode == "Automne et hiver"], na.rm = TRUE), 2)

cat("\nchi2 =", round(chi2$statistic, 4), "| ddl =", chi2$parameter,
    "| p =", format(chi2$p.value, digits = 3), "\n")
cat("V de Cramer =", v_cramer, "|",
    ifelse(v_cramer >= 0.3, "effet moyen ou grand",
           ifelse(v_cramer >= 0.1, "effet petit", "effet négligeable")), "\n")
cat("Part de collisions avec blessé : printemps et été", part_beau,
    "% | automne et hiver", part_mauvais, "%\n")
cat("Écart :", round(part_beau - part_mauvais, 2), "point\n")

if (chi2$p.value >= 0.05) {
  cat("\nAucune différence saisonnière détectée. L'hypothèse d'un effet\n")
  cat("du beau temps, via un nombre accru de cyclistes, n'est pas\n")
  cat("soutenue par ces données. Un non-rejet ne prouve pas l'absence\n")
  cat("d'effet, mais l'écart observé est de", round(abs(part_beau - part_mauvais), 2),
      "point seulement.\n")
} else {
  cat("\nDifférence détectée, mais lire d'abord le V de Cramer : avec\n")
  cat(n_chi, "observations, un écart très faible suffit à produire une\n")
  cat("p-value petite.\n")
}

enregistrer(
  test_id = "T-011", script = "11_collisions_spatial.R",
  methode = "Chi2 de Pearson, part de collisions avec blessé, printemps et été contre automne et hiver",
  n = n_chi, statistique = round(as.numeric(chi2$statistic), 4),
  p_value = round(chi2$p.value, 4),
  effet_nom = "V de Cramer", effet = v_cramer,
  note = paste0("Parts : ", part_beau, " % contre ", part_mauvais, " %.")
)

# ── 7. SÉVÉRITÉ PAR SAISON ──────────────────────────────────
# Test de Dunn codé sur place, comme au script 05, pour ne pas
# dépendre d'un package externe (S-16).

dunn_bonferroni <- function(x, g) {
  ok <- !is.na(x) & !is.na(g)
  x <- x[ok]; g <- droplevels(factor(g[ok]))
  k <- nlevels(g); N <- length(x); r <- rank(x)
  tt <- table(x); C <- sum(tt^3 - tt) / (12 * (N - 1))
  moy <- tapply(r, g, mean); n_i <- tapply(r, g, length)
  res <- data.frame()
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    A <- levels(g)[i]; B <- levels(g)[j]
    se <- sqrt((N * (N + 1) / 12 - C) * (1 / n_i[A] + 1 / n_i[B]))
    z  <- (moy[A] - moy[B]) / se
    res <- rbind(res, data.frame(paire = paste(A, "contre", B),
                                 Z = round(z, 3),
                                 p_brut = 2 * pnorm(abs(z), lower.tail = FALSE)))
  }
  res$p_bonf <- p.adjust(res$p_brut, method = "bonferroni")
  res$signif <- res$p_bonf < 0.05
  rownames(res) <- NULL
  res
}

kw_sev <- kruskal.test(indicateur_de_severite ~ saison, data = col_proche)
cat("\n=== SÉVÉRITÉ SELON LA SAISON ===\n")
cat("Kruskal-Wallis : H =", round(kw_sev$statistic, 4),
    "| ddl =", kw_sev$parameter,
    "| p =", format(kw_sev$p.value, digits = 3), "\n")
cat("Sévérité moyenne par saison :",
    paste(resume_saison$saison, resume_saison$severite_moy,
          sep = " = ", collapse = " | "), "\n")
cat("Amplitude entre saisons :",
    round(max(resume_saison$severite_moy) - min(resume_saison$severite_moy), 3), "\n")

if (kw_sev$p.value < 0.05) {
  cat("\nPost-hoc de Dunn, correction de Bonferroni :\n")
  print(dunn_bonferroni(col_proche$indicateur_de_severite, col_proche$saison) %>%
          mutate(p_bonf = format(p_bonf, digits = 3)) %>%
          select(paire, Z, p_bonf, signif))
} else {
  cat("Pas de différence de sévérité entre saisons.\n")
}

# ── 8. FIGURES ──────────────────────────────────────────────

p_saison <- ggplot(resume_saison, aes(x = saison, y = part_blesse_pct)) +
  geom_col(fill = ROUGE_PRINCIPAL, alpha = 0.85, width = 0.6) +
  geom_text(aes(label = paste0(part_blesse_pct, " %")),
            vjust = -0.5, size = 3.5, color = "grey30") +
  scale_y_continuous(labels = function(x) paste0(x, " %"),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Part des collisions ayant fait un blessé, par saison",
       subtitle = paste0("Sur ", nrow(col_proche),
                         " collisions situées à moins de ", SEUIL_M,
                         " mètres d'un arrêt actif. V de Cramer = ", v_cramer, "."),
       x = NULL, y = "Part des collisions avec blessé",
       caption = SOURCE_TPG) +
  theme_projet() + theme(panel.grid.major.x = element_blank())

print(p_saison)
ggsave(file.path(DIR_FIG, "11_blesses_saison.png"), p_saison, width = 9, height = 6, dpi = 150)

p_comp <- ggplot(compar, aes(x = n_collisions, y = collisions_par_Mmontees)) +
  geom_point(color = ROUGE_PRINCIPAL, alpha = 0.6, size = 2) +
  scale_y_log10() +
  labs(title = "Nombre de collisions et rapport à la fréquentation",
       subtitle = paste0("Un arrêt par point, ", nrow(compar),
                         " arrêts. Corrélation de rang : ", round(rho_classements, 3),
                         ".\nUn même nombre de collisions recouvre des rapports très différents."),
       x = "Nombre de collisions",
       y = "Collisions pour un million de montées (échelle log)",
       caption = paste(SOURCE_TPG,
                       "Les montées mesurent les voyageurs, pas les passages de véhicules.",
                       sep = "\n")) +
  theme_projet()

print(p_comp)
ggsave(file.path(DIR_FIG, "11_collisions_exposition.png"), p_comp, width = 10, height = 7, dpi = 150)
message("Figures enregistrées.")

# ── 9. SAUVEGARDE ───────────────────────────────────────────

write.csv(par_arret, file.path(DIR_RES, paste0("11_collisions_par_arret_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(compar,    file.path(DIR_RES, paste0("11_collisions_exposition_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(resume_saison, file.path(DIR_RES, paste0("11_T011_saison_", SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 11 terminé. Figures dans figures/, résultats dans resultats/.")
