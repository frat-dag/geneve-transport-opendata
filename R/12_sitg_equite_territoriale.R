# =============================================================================
# SCRIPT 12 — ÉQUITÉ TERRITORIALE : SITG × DONNÉES TPG
# Projet TPG Open Data — Phase 3
# Frat DAG — avril 2026
# =============================================================================
#
# APPROCHE : inductive — les données SITG guident les questions analytiques.
# On ne pose pas les questions avant d'avoir vu les données.
#
# DOUBLE VERSION :
#   - Résultats publics : insights factuels, narration positive
#   - Observations sensibles marquées # [TPG] : version confidentielle uniquement
#
# QUESTION PRINCIPALE :
#   La fréquentation TPG est-elle corrélée à la densité de population ?
#   Existe-t-il des communes structurellement sous-desservies ?
#   Les anomalies (sur/sous-fréquentation) ont-elles une explication ?
#
# SOURCE SITG :
#   OCS_POPBATLOG_COMMUNE    — 45 communes, données population dec. 2025
#   OCS_POPBATLOG_VGE_SECTEUR — 16 secteurs ville de Genève, dec. 2025
#   Licence : Open Data SITG — mention obligatoire en publication
#   Citation : "Source : Système d'information du territoire à Genève (SITG)"
#
# =============================================================================
# ÉTAPES :
#   1. Chargement des couches SITG et reprojection
#   2. Exploration géographique — combien d'arrêts couverts ?
#   3. Jointure spatiale arrêts × communes
#   4. Calcul des indicateurs par commune
#   5. Test T-012 — corrélation densité × montées/habitant
#   6. Identification des anomalies
#   7. Jointure spatiale arrêts × secteurs (granularité fine — ville GE)
#   8. Analyse intra-ville par secteur
#   9. Visualisations
# =============================================================================

source("00_palette.R")
library(sf)
library(dplyr)
library(ggplot2)
library(scales)

# Désactiver S2 — géométries SITG contiennent des sommets dupliqués
# qui causent des erreurs avec la géométrie sphérique de sf
sf_use_s2(FALSE)

# =============================================================================
# 1. CHARGEMENT ET REPROJECTION
# =============================================================================

cat("=== CHARGEMENT DONNÉES SITG ===\n")

# Couche 1 — Communes (45 polygones, canton entier)
communes_raw <- st_read(
  "../data/raw/sitg/communes/OCS_POPBATLOG_COMMUNE-SHP/OCS_POPBATLOG_COMMUNE.shp",
  quiet = TRUE
)
communes_wgs84 <- st_transform(communes_raw, crs = 4326)

cat("Communes chargées  :", nrow(communes_raw), "\n")
cat("CRS original       :", st_crs(communes_raw)$Name, "\n")
cat("CRS après transform: WGS 84\n")
cat("Date référence     :", communes_raw$DATE_REF[1], "\n\n")

# Couche 2 — Secteurs ville de Genève (16 quartiers, granularité fine)
secteurs_raw <- st_read(
  "../data/raw/sitg/secteurs/OCS_POPBATLOG_VGE_SECTEUR-SHP/OCS_POPBATLOG_VGE_SECTEUR.shp",
  quiet = TRUE
)
secteurs_wgs84 <- st_transform(secteurs_raw, crs = 4326)

cat("Secteurs chargés   :", nrow(secteurs_raw), "\n")
cat("Date référence     :", secteurs_raw$DATE_REF[1], "\n\n")

# Arrêts actifs géolocalisés — conversion en objet sf
# Si arrets_geo n'est pas en mémoire, recharger :
# arrets_raw <- readRDS("../data/raw/arrets.rds")
# arrets_geo <- arrets_raw %>%
#   filter(actif == "Y") %>%
#   tidyr::separate(coordonnees, into = c("latitude","longitude"),
#                   sep = ",", convert = TRUE) %>%
#   filter(!is.na(latitude), !is.na(longitude))

arrets_sf <- arrets_geo %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

cat("Arrêts actifs      :", nrow(arrets_sf), "\n")

# =============================================================================
# 2. EXPLORATION GÉOGRAPHIQUE — AVANT TOUTE DÉCISION
# =============================================================================

cat("\n=== COUVERTURE GÉOGRAPHIQUE ===\n")

cat("--- Emprise communes SITG ---\n")
bb_communes <- st_bbox(communes_wgs84)
cat("Lat :", round(bb_communes["ymin"],2), "→", round(bb_communes["ymax"],2), "\n")
cat("Lon :", round(bb_communes["xmin"],2), "→", round(bb_communes["xmax"],2), "\n")

cat("\n--- Emprise arrêts TPG ---\n")
bb_arrets <- st_bbox(arrets_sf)
cat("Lat :", round(bb_arrets["ymin"],2), "→", round(bb_arrets["ymax"],2), "\n")
cat("Lon :", round(bb_arrets["xmin"],2), "→", round(bb_arrets["xmax"],2), "\n")

cat("\n--- Population totale couverte par SITG ---\n")
cat("Communes :", sum(communes_raw$POPULATION, na.rm = TRUE), "habitants\n")
cat("Secteurs :", sum(secteurs_raw$POPULATION, na.rm = TRUE), "habitants\n")

# Observation : les secteurs (ville GE uniquement) ne couvrent qu'une partie
# de la population cantonale. La couche communes est plus complète.

# =============================================================================
# 3. JOINTURE SPATIALE ARRÊTS × COMMUNES
# =============================================================================
# Note méthodologique : on utilise st_within (arrêt strictement dans le polygone)
# plutôt que st_nearest_feature (qui assignerait même les arrêts français).
# Les arrêts non assignés sont les arrêts hors canton (France, Vaud) — attendu.

cat("\n=== JOINTURE SPATIALE ARRÊTS × COMMUNES ===\n")

arrets_communes <- st_join(arrets_sf, communes_wgs84, join = st_within)

n_assignes  <- sum(!is.na(arrets_communes$COMMUNE))
n_hors      <- sum(is.na(arrets_communes$COMMUNE))

cat("Arrêts assignés à une commune :", n_assignes,
    "(", round(n_assignes/nrow(arrets_sf)*100, 1), "%)\n")
cat("Arrêts hors canton (FR/VD)    :", n_hors, "\n")

# =============================================================================
# 4. CALCUL DES INDICATEURS PAR COMMUNE
# =============================================================================

# montees_par_arret construit dans script 09 — agrégation par nom d'arrêt
# Si absent : construire depuis journalier
# montees_par_arret <- journalier %>%
#   filter(donnees_definitives == TRUE) %>%
#   group_by(arret) %>%
#   summarise(montees_totales = sum(nb_de_montees, na.rm = TRUE))

cat("\n=== INDICATEURS PAR COMMUNE ===\n")

sitg_communes <- arrets_communes %>%
  st_drop_geometry() %>%
  filter(!is.na(COMMUNE)) %>%
  left_join(montees_par_arret, by = c("nomarret" = "arret")) %>%
  group_by(COMMUNE, POPULATION, SHAPE_AREA) %>%
  summarise(
    n_arrets        = n(),
    montees_totales = sum(montees_totales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(POPULATION > 0, montees_totales > 0) %>%
  mutate(
    densite_pop     = POPULATION / (SHAPE_AREA / 1e6),
    montees_par_hab = montees_totales / POPULATION,
    arrets_par_km2  = n_arrets / (SHAPE_AREA / 1e6)
  )

cat("Communes dans l'analyse :", nrow(sitg_communes), "\n\n")

sitg_communes %>%
  dplyr::select(COMMUNE, POPULATION, densite_pop,
                n_arrets, montees_par_hab) %>%
  mutate(
    densite_pop     = round(densite_pop),
    montees_par_hab = round(montees_par_hab)
  ) %>%
  arrange(desc(montees_par_hab)) %>%
  print(n = 20)

# =============================================================================
# 5. TEST T-012 — CORRÉLATION DENSITÉ × MONTÉES PAR HABITANT
# =============================================================================
#
# Hypothèse H0 : pas de corrélation entre densité et fréquentation/habitant
# Hypothèse H1 : corrélation positive — plus dense = plus de TPG/habitant
#
# Conditions vérifiées (console 28.04.2026) :
#   Shapiro-Wilk densite_pop     : W=0.663, p=8.6×10⁻⁹ → NON normale
#   Shapiro-Wilk montees_par_hab : W=0.648, p=5.0×10⁻⁹ → NON normale
#   → Spearman retenu comme test de référence (robuste aux outliers)
#   → Pearson calculé pour référence mais biaisé par l'outlier Genève ville
#
# Note sur l'outlier Genève ville :
#   11 511 hab/km² — 5x supérieur à la 2e commune (Carouge 8 548)
#   Genève ville tire le Pearson vers le haut (r=0.856 vs Spearman r=0.695)
#   Le Spearman est l'indicateur de référence pour ces données.

cat("\n=== TEST DE NORMALITÉ ===\n")
sw_dens <- shapiro.test(sitg_communes$densite_pop)
sw_mont <- shapiro.test(sitg_communes$montees_par_hab)
cat("Shapiro densite_pop     : W =", round(sw_dens$statistic, 3),
    "p =", format(sw_dens$p.value, scientific = TRUE),
    "→", ifelse(sw_dens$p.value < 0.05, "NON normale", "normale"), "\n")
cat("Shapiro montees_par_hab : W =", round(sw_mont$statistic, 3),
    "p =", format(sw_mont$p.value, scientific = TRUE),
    "→", ifelse(sw_mont$p.value < 0.05, "NON normale", "normale"), "\n")

cat("\n=== T-012 — CORRÉLATION SPEARMAN DENSITÉ × MONTÉES/HAB ===\n")

cor_spearman <- cor.test(sitg_communes$densite_pop,
                         sitg_communes$montees_par_hab,
                         method = "spearman",
                         exact  = FALSE)

cor_pearson  <- cor.test(sitg_communes$densite_pop,
                         sitg_communes$montees_par_hab,
                         method = "pearson")

cat("n communes        :", nrow(sitg_communes), "\n")
cat("Spearman rho      :", round(cor_spearman$estimate, 3),
    "(référence — robuste aux outliers)\n")
cat("p-value Spearman  :", format(cor_spearman$p.value, scientific = TRUE), "\n")
cat("Pearson r         :", round(cor_pearson$estimate, 3),
    "(biaisé par outlier Genève — indicatif)\n")
cat("p-value Pearson   :", format(cor_pearson$p.value, scientific = TRUE), "\n")

if (cor_spearman$p.value < 0.05) {
  cat("\n-> REJET H0 — corrélation significative densité × fréquentation\n")
  cat("   Les communes denses utilisent davantage les TPG par habitant.\n")
} else {
  cat("\n-> NON-REJET H0\n")
}

cat("\n--- Ce qu'on NE peut PAS affirmer ---\n")
cat("  - Que la densité CAUSE la fréquentation.\n")
cat("    Les communes denses ont aussi plus d'offre TPG.\n")
cat("    Sans km produits par commune, on ne peut pas démêler\n")
cat("    l'effet densité de l'effet offre.\n")
cat("  - [TPG] La part de la fréquentation expliquée par l'offre vs\n")
cat("    la demande nécessite un croisement avec les km produits\n")
cat("    par commune — données disponibles en interne.\n")

# =============================================================================
# 6. IDENTIFICATION DES ANOMALIES
# =============================================================================
#
# Méthode : résidus de la régression Spearman (rang)
# Les communes au-dessus de la tendance sont sur-fréquentées vs leur densité.
# Les communes en dessous sont sous-fréquentées.
# On identifie les outliers > 1.5 × IQR des résidus.

cat("\n=== ANOMALIES — ÉCARTS À LA TENDANCE ===\n")

# Régression sur les rangs (cohérente avec Spearman)
sitg_communes <- sitg_communes %>%
  mutate(
    rang_densite = rank(densite_pop),
    rang_montees = rank(montees_par_hab),
    residu_rang  = rang_montees - rang_densite
  )

iqr_residu <- IQR(sitg_communes$residu_rang)
seuil_haut <-  1.5 * iqr_residu
seuil_bas  <- -1.5 * iqr_residu

cat("--- Sur-fréquentées (montées > densité attendue) ---\n")
sitg_communes %>%
  filter(residu_rang > seuil_haut) %>%
  dplyr::select(COMMUNE, densite_pop, montees_par_hab,
                n_arrets, residu_rang) %>%
  mutate(densite_pop = round(densite_pop),
         montees_par_hab = round(montees_par_hab)) %>%
  arrange(desc(residu_rang)) %>%
  print()

cat("\n--- Sous-fréquentées (montées < densité attendue) ---\n")
sitg_communes %>%
  filter(residu_rang < seuil_bas) %>%
  dplyr::select(COMMUNE, densite_pop, montees_par_hab,
                n_arrets, residu_rang) %>%
  mutate(densite_pop = round(densite_pop),
         montees_par_hab = round(montees_par_hab)) %>%
  arrange(residu_rang) %>%
  print()

# [TPG] Les communes sous-fréquentées par rapport à leur densité sont les
# candidates prioritaires pour une analyse d'offre :
#   - Trop peu d'arrêts ? Fréquence insuffisante ? Tracés inadaptés ?
#   - Ces questions nécessitent un croisement avec les km produits
#     et les plannings horaires par commune (données internes TPG).
# Sans ces données, on ne peut que constater l'écart, pas l'expliquer.

# =============================================================================
# 7. JOINTURE SPATIALE ARRÊTS × SECTEURS (GRANULARITÉ FINE — VILLE GE)
# =============================================================================

cat("\n=== JOINTURE SPATIALE ARRÊTS × SECTEURS (VILLE GE) ===\n")

arrets_secteurs <- st_join(arrets_sf, secteurs_wgs84, join = st_within)

n_sect <- sum(!is.na(arrets_secteurs$NOM_SECTEU))
cat("Arrêts dans un secteur :", n_sect,
    "(", round(n_sect/nrow(arrets_sf)*100, 1), "% des arrêts actifs)\n")
cat("Secteurs couverts      :", n_distinct(arrets_secteurs$NOM_SECTEU, na.rm=TRUE), "\n")

# =============================================================================
# 8. ANALYSE INTRA-VILLE PAR SECTEUR
# =============================================================================

cat("\n=== INDICATEURS PAR SECTEUR (VILLE DE GENÈVE) ===\n")

sitg_secteurs <- arrets_secteurs %>%
  st_drop_geometry() %>%
  filter(!is.na(NOM_SECTEU)) %>%
  left_join(montees_par_arret, by = c("nomarret" = "arret")) %>%
  group_by(NOM_SECTEU, POPULATION, SHAPE_AREA) %>%
  summarise(
    n_arrets        = n(),
    montees_totales = sum(montees_totales, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(POPULATION > 0, montees_totales > 0) %>%
  mutate(
    densite_pop     = round(POPULATION / (SHAPE_AREA / 1e6)),
    montees_par_hab = round(montees_totales / POPULATION),
    arrets_par_km2  = round(n_arrets / (SHAPE_AREA / 1e6), 1)
  )

sitg_secteurs %>%
  arrange(desc(montees_par_hab)) %>%
  print()

cat("\n--- Corrélation densité × montées/hab (secteurs) ---\n")
if (nrow(sitg_secteurs) >= 5) {
  cor_sect <- cor.test(sitg_secteurs$densite_pop,
                       sitg_secteurs$montees_par_hab,
                       method = "spearman", exact = FALSE)
  cat("Spearman rho :", round(cor_sect$estimate, 3),
      "| p =", format(cor_sect$p.value, scientific = TRUE), "\n")
  cat("n secteurs   :", nrow(sitg_secteurs), "\n")
} else {
  cat("Trop peu de secteurs pour un test fiable.\n")
}

# =============================================================================
# 9. VISUALISATIONS
# =============================================================================

# --- VIZ 1 : Scatter densité × montées/hab par commune ---
p_scatter_communes <- sitg_communes %>%
  mutate(
    anomalie = case_when(
      residu_rang > seuil_haut ~ "Sur-fréquentée",
      residu_rang < seuil_bas  ~ "Sous-fréquentée",
      TRUE                     ~ "Dans la tendance"
    ),
    anomalie = factor(anomalie,
                      levels = c("Sur-fréquentée",
                                 "Dans la tendance",
                                 "Sous-fréquentée"))
  ) %>%
  ggplot(aes(x = densite_pop,
             y = montees_par_hab,
             color = anomalie,
             label = COMMUNE)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.8, vjust = -0.8, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = TRUE, color = "grey50",
              linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = c(
    "Sur-fréquentée"   = "#27AE60",
    "Dans la tendance" = TPG_RED,
    "Sous-fréquentée"  = "#E67E22"
  )) +
  scale_x_continuous(labels = comma_format(big.mark = " ")) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(
    title    = "Densité de population vs fréquentation TPG par commune",
    subtitle = paste0("Spearman rho = 0.695, p = 1.65×10⁻⁷ | n = 44 communes\n",
                      "Anomalies = communes s'écartant significativement de la tendance"),
    x        = "Densité de population (hab/km²)",
    y        = "Montées TPG par habitant",
    color    = NULL,
    caption  = paste0("Sources : opendata.tpg.ch | SITG (dec. 2025) | Frat DAG 2026")
  ) +
  theme_tpg()

ggsave("../outputs/12_scatter_densite_frequentation.png",
       p_scatter_communes, width = 12, height = 7, dpi = 150)
cat("\nGraphique sauvegardé : 12_scatter_densite_frequentation.png\n")

# --- VIZ 2 : Barplot montées/hab par commune (top 20) ---
p_barplot_communes <- sitg_communes %>%
  arrange(desc(montees_par_hab)) %>%
  head(20) %>%
  mutate(COMMUNE = factor(COMMUNE, levels = rev(COMMUNE))) %>%
  ggplot(aes(x = COMMUNE,
             y = montees_par_hab,
             fill = densite_pop)) +
  geom_col(alpha = 0.85) +
  scale_fill_gradient(low = "#FEF9C3", high = TPG_RED,
                      name = "Densité\n(hab/km²)",
                      labels = comma_format(big.mark = " ")) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  coord_flip() +
  labs(
    title    = "Top 20 communes — montées TPG par habitant",
    subtitle = "Couleur = densité de population | Source SITG dec. 2025",
    x        = NULL,
    y        = "Montées TPG / habitant",
    caption  = "Sources : opendata.tpg.ch | SITG | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave("../outputs/12_barplot_montees_par_hab.png",
       p_barplot_communes, width = 10, height = 7, dpi = 150)
cat("Graphique sauvegardé : 12_barplot_montees_par_hab.png\n")

# --- VIZ 3 : Barplot secteurs ville de Genève ---
if (nrow(sitg_secteurs) > 0) {
  p_secteurs <- sitg_secteurs %>%
    arrange(desc(montees_par_hab)) %>%
    mutate(NOM_SECTEU = factor(NOM_SECTEU, levels = rev(NOM_SECTEU))) %>%
    ggplot(aes(x = NOM_SECTEU,
               y = montees_par_hab,
               fill = densite_pop)) +
    geom_col(alpha = 0.85) +
    scale_fill_gradient(low = "#FEF9C3", high = TPG_RED,
                        name = "Densité\n(hab/km²)",
                        labels = comma_format(big.mark = " ")) +
    scale_y_continuous(labels = comma_format(big.mark = " ")) +
    coord_flip() +
    labs(
      title    = "Quartiers ville de Genève — montées TPG par habitant",
      subtitle = "Granularité fine (16 secteurs) | Source SITG dec. 2025",
      x        = NULL,
      y        = "Montées TPG / habitant",
      caption  = "Sources : opendata.tpg.ch | SITG | Frat DAG 2026"
    ) +
    theme_tpg()
  
  ggsave("../outputs/12_barplot_secteurs_ville.png",
         p_secteurs, width = 10, height = 7, dpi = 150)
  cat("Graphique sauvegardé : 12_barplot_secteurs_ville.png\n")
}

# =============================================================================
# BILAN SCRIPT 12
# =============================================================================

cat("\n=== BILAN SCRIPT 12 ===\n")
cat("Couches SITG       : communes (45) + secteurs ville GE (16)\n")
cat("Arrêts assignés    : communes", n_assignes, "| secteurs", n_sect, "\n")
cat("T-012 Spearman     : rho =", round(cor_spearman$estimate, 3),
    "| p =", format(cor_spearman$p.value, scientific = TRUE), "\n")
cat("Anomalies détectées:\n")
cat("  Sur-fréquentées  :",
    sum(sitg_communes$residu_rang > seuil_haut), "communes\n")
cat("  Sous-fréquentées :",
    sum(sitg_communes$residu_rang < seuil_bas), "communes\n")
cat("3 graphiques sauvegardés dans outputs/\n")
cat("\nCitation SITG obligatoire en publication :\n")
cat("  'Source : Système d'information du territoire à Genève (SITG),\n")
cat("   extrait en avril 2026.'\n")
cat("\n[TPG] Limites analytiques documentées :\n")
cat("  - Corrélation densité/fréquentation ≠ causalité\n")
cat("  - Sans km produits par commune, on ne distingue pas\n")
cat("    effet offre de l'effet demande\n")
cat("  - Communes sous-fréquentées = candidats à une analyse d'offre interne\n")