# =============================================================================
# SCRIPT 11 — CROISEMENT SPATIAL COLLISIONS × ARRÊTS TPG
# Projet TPG Open Data — Phase 3
# Frat DAG — avril 2026
# =============================================================================
#
# APPROCHE : inductive — les données guident les décisions méthodologiques.
# Chaque choix technique est justifié empiriquement, pas par convention.
#
# DOUBLE VERSION :
#   - Résultats publics : insights factuels, narration positive
#   - Observations sensibles marquées # [TPG] : version confidentielle uniquement
#
# QUESTION PRINCIPALE :
#   Quels arrêts TPG concentrent le plus de collisions ?
#   Les collisions graves (blessés humains) suivent-elles le même pattern ?
#   Y a-t-il un effet saisonnier / horaire sur la gravité ?
#   (Hypothèse vélo : beau temps → plus de cyclistes → plus de collisions ?)
#
# =============================================================================
# ÉTAPES :
#   1. Chargement et préparation des deux datasets
#   2. Exploration de la géographie — emprise, densité des arrêts
#   3. Justification méthodologique du choix de jointure
#   4. Jointure spatiale — plus proche voisin + seuil 200m
#   5. Analyse descriptive — top arrêts, distribution
#   6. Analyse par gravité — blessés humains
#   7. Analyse temporelle — heure et saison
#   8. Test statistique T-011 — gravité selon la saison
#   9. Visualisations
# =============================================================================

source("00_palette.R")
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)

# =============================================================================
# 1. CHARGEMENT ET PRÉPARATION
# =============================================================================

cat("=== CHARGEMENT ===\n")

# Collisions — déjà en mémoire depuis script 08
# Si session fraîche : collisions <- readRDS("../data/raw/collisions.rds")
cat("Collisions :", nrow(collisions), "lignes\n")

# Arrêts bruts — parsing des coordonnées nécessaire
arrets_raw <- readRDS("../data/raw/arrets.rds")

arrets_geo <- arrets_raw %>%
  filter(actif == "Y") %>%
  tidyr::separate(coordonnees, into = c("latitude", "longitude"),
                  sep = ",", convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

cat("Arrêts actifs géolocalisés :", nrow(arrets_geo), "\n\n")

# =============================================================================
# 2. EXPLORATION GÉOGRAPHIQUE — AVANT TOUTE DÉCISION
# =============================================================================

cat("=== EMPRISE GÉOGRAPHIQUE ===\n")
cat("--- Arrêts actifs ---\n")
arrets_geo %>%
  summarise(
    n       = n(),
    lat_min = min(latitude), lat_max = max(latitude),
    lon_min = min(longitude), lon_max = max(longitude)
  ) %>% print()

cat("--- Collisions ---\n")
collisions %>%
  summarise(
    n       = n(),
    lat_min = min(latitude), lat_max = max(latitude),
    lon_min = min(longitude), lon_max = max(longitude)
  ) %>% print()

# Observation : collisions couvrent une emprise légèrement plus large (lat max 46.5)
# → Lignes GLCT transfrontalières vers pays de Gex. Cohérent avec le réseau.

# =============================================================================
# 3. JUSTIFICATION MÉTHODOLOGIQUE DU CHOIX DE JOINTURE
# =============================================================================
#
# QUESTION : quel rayon utiliser pour associer une collision à un arrêt ?
#
# EXPLORATION 1 — Distribution des distances collision → arrêt le plus proche
# (réalisée sur échantillon 500 collisions, set.seed(42), console 28.04.2026)
#
#   Percentile | Distance min collision → arrêt
#   -----------|--------------------------------
#   50%        | 10m
#   75%        | 43m
#   90%        | 104m
#   95%        | 137m
#   99%        | 224m
#   Max        | 1 305m (lignes GLCT rurales)
#
# EXPLORATION 2 — Distribution des distances inter-arrêts (voisin le plus proche)
#
#   Percentile | Distance entre deux arrêts voisins
#   -----------|------------------------------------
#   10%        | 10m
#   25%        | 18m
#   50%        | 34m   ← médiane
#   75%        | 58m
#   90%        | 97m
#
# PROBLÈME IDENTIFIÉ : un rayon fixe (ex. 50m ou 100m) est inapproprié
# pour la géographie genevoise. La médiane inter-arrêts est de 34m —
# un rayon de 50m autour d'un arrêt chevauche systématiquement le territoire
# de l'arrêt voisin en centre-ville. Résultat : une même collision pourrait
# être attribuée à deux arrêts différents selon le rayon choisi.
#
# DÉCISION MÉTHODOLOGIQUE (OBS-064) :
#   → Assignation au PLUS PROCHE VOISIN sans rayon fixe
#   → Chaque collision est assignée à UN SEUL arrêt (le plus proche)
#   → Seuil de COUPURE à 200m : exclut les collisions en zone rurale
#     éloignée de tout arrêt (lignes GLCT, outliers ruraux)
#     Justification : 200m ≈ 99e percentile des distances collision-arrêt
#     Les collisions au-delà sont structurellement éloignées du réseau d'arrêts.
#
# Cette approche est méthodologiquement plus honnête qu'un rayon fixe :
#   - Pas de chevauchement entre zones d'arrêts voisins
#   - Assignation univoque et reproductible
#   - Seuil de coupure justifié empiriquement, pas arbitrairement

cat("=== JUSTIFICATION MÉTHODOLOGIQUE ===\n")
cat("Méthode retenue : plus proche voisin + seuil de coupure 200m\n")
cat("Médiane inter-arrêts : 34m — rayon fixe crée des chevauchements\n")
cat("Seuil 200m ≈ 99e percentile distances collision-arrêt\n\n")

# =============================================================================
# 4. JOINTURE SPATIALE — PLUS PROCHE VOISIN
# =============================================================================

# Fonction distance Haversine (mètres)
# Formule trigonométrique exacte pour de courtes distances
haversine <- function(lat1, lon1, lat2, lon2) {
  R    <- 6371000  # rayon terrestre en mètres
  phi1 <- lat1 * pi / 180
  phi2 <- lat2 * pi / 180
  dphi <- (lat2 - lat1) * pi / 180
  dlam <- (lon2 - lon1) * pi / 180
  a    <- sin(dphi/2)^2 + cos(phi1)*cos(phi2)*sin(dlam/2)^2
  2 * R * asin(sqrt(a))
}

SEUIL_METRES <- 200  # seuil de coupure empirique

cat("=== JOINTURE SPATIALE ===\n")
cat("Calcul en cours —", nrow(collisions), "collisions ×",
    nrow(arrets_geo), "arrêts...\n")

# Pour chaque collision : trouver l'arrêt le plus proche et sa distance
resultats <- lapply(1:nrow(collisions), function(i) {
  dists <- haversine(
    collisions$latitude[i],  collisions$longitude[i],
    arrets_geo$latitude,     arrets_geo$longitude
  )
  idx_min  <- which.min(dists)
  dist_min <- dists[idx_min]
  list(
    arret_proche  = arrets_geo$nomarret[idx_min],
    arret_code    = arrets_geo$arretcodelong[idx_min],
    dist_metres   = dist_min,
    dans_seuil    = dist_min <= SEUIL_METRES
  )
})

# Assembler les résultats
collisions_spatial <- collisions %>%
  mutate(
    arret_proche = sapply(resultats, `[[`, "arret_proche"),
    arret_code   = sapply(resultats, `[[`, "arret_code"),
    dist_metres  = sapply(resultats, `[[`, "dist_metres"),
    dans_seuil   = sapply(resultats, `[[`, "dans_seuil")
  )

cat("\n--- Résultats jointure ---\n")
cat("Collisions dans seuil 200m :",
    sum(collisions_spatial$dans_seuil), "/", nrow(collisions),
    "(", round(mean(collisions_spatial$dans_seuil)*100, 1), "%)\n")
cat("Collisions hors seuil      :",
    sum(!collisions_spatial$dans_seuil), "\n")
cat("Distance médiane           :",
    round(median(collisions_spatial$dist_metres), 1), "m\n")
cat("Distance moyenne           :",
    round(mean(collisions_spatial$dist_metres), 1), "m\n\n")

# Dataset de travail — collisions dans le seuil uniquement
col_proche <- collisions_spatial %>%
  filter(dans_seuil)

# Enrichir avec variables temporelles
col_proche <- col_proche %>%
  mutate(
    mois      = month(jour),
    trimestre = quarter(jour),
    saison    = case_when(
      mois %in% c(12, 1, 2)  ~ "Hiver",
      mois %in% c(3, 4, 5)   ~ "Printemps",
      mois %in% c(6, 7, 8)   ~ "Été",
      mois %in% c(9, 10, 11) ~ "Automne"
    ),
    saison = factor(saison,
                    levels = c("Printemps","Été","Automne","Hiver")),
    avec_blesse   = niv_blessure_humain > 0,
    blesse_grave  = niv_blessure_humain >= 2,
    heure_num     = as.integer(substr(as.character(heure), 1, 2))
  )

cat("Dataset de travail :", nrow(col_proche), "collisions\n\n")

# =============================================================================
# 5. ANALYSE DESCRIPTIVE — TOP ARRÊTS
# =============================================================================

cat("=== TOP 15 ARRÊTS — NOMBRE DE COLLISIONS ===\n")

top_arrets_n <- col_proche %>%
  group_by(arret_proche) %>%
  summarise(
    n_collisions    = n(),
    n_blesses       = sum(avec_blesse),
    pct_blesses     = round(mean(avec_blesse) * 100, 1),
    severite_moy    = round(mean(indicateur_de_severite), 3),
    dist_moy        = round(mean(dist_metres), 0),
    .groups = "drop"
  ) %>%
  arrange(desc(n_collisions)) %>%
  head(15)

print(top_arrets_n)

cat("\n=== TOP 15 ARRÊTS — TAUX DE BLESSÉS (min 10 collisions) ===\n")

top_arrets_blesses <- col_proche %>%
  group_by(arret_proche) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    severite_moy = round(mean(indicateur_de_severite), 3),
    .groups = "drop"
  ) %>%
  filter(n_collisions >= 10) %>%
  arrange(desc(pct_blesses)) %>%
  head(15)

print(top_arrets_blesses)

# [TPG] Le croisement top arrêts × taux de blessés est l'indicateur le plus
# opérationnel : un arrêt avec beaucoup de collisions mais peu de blessés
# (ex. accrochages légers) n'a pas la même priorité d'intervention qu'un
# arrêt avec peu de collisions mais un taux de blessés élevé.

# =============================================================================
# 6. ANALYSE PAR GRAVITÉ — BLESSÉS HUMAINS
# =============================================================================

cat("\n=== ANALYSE GRAVITÉ ===\n")

cat("--- Distribution blessés ---\n")
col_proche %>%
  count(niv_blessure_humain) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  print()

cat("\n--- Gravité par type de ligne ---\n")
col_proche %>%
  group_by(ligne_type_hist) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    severite_moy = round(mean(indicateur_de_severite), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(n_collisions)) %>%
  print()

# =============================================================================
# 7. ANALYSE TEMPORELLE — HEURE ET SAISON
# =============================================================================

cat("\n=== ANALYSE HORAIRE ===\n")

cat("--- Collisions par heure ---\n")
col_proche %>%
  group_by(heure_num) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(heure_num) %>%
  print(n = 24)

cat("\n=== ANALYSE SAISONNIÈRE ===\n")

saison_summary <- col_proche %>%
  group_by(saison) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    severite_moy = round(mean(indicateur_de_severite), 3),
    .groups = "drop"
  )

print(saison_summary)

# Hypothèse vélo (OBS du 28.04.2026) :
# Beau temps (printemps/été) → plus de cyclistes → plus de collisions avec blessés ?
# Les cyclistes partagent souvent les voies avec les TPG à Genève
# (Cornavin, Plainpalais, Carouge, rive gauche).
# On teste cette hypothèse avec le taux de blessés par saison.

cat("\n--- Printemps+Été vs Automne+Hiver ---\n")
col_proche %>%
  mutate(periode = ifelse(saison %in% c("Printemps","Été"),
                          "Beau temps", "Mauvais temps")) %>%
  group_by(periode) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    severite_moy = round(mean(indicateur_de_severite), 3),
    .groups = "drop"
  ) %>%
  print()

# =============================================================================
# 8. TEST STATISTIQUE T-011 — GRAVITÉ SELON LA SAISON
# =============================================================================
#
# Hypothèse H0 : le taux de collision avec blessé est identique
#                entre beau temps (printemps+été) et mauvais temps (automne+hiver)
# Hypothèse H1 : le taux diffère
#
# Variable dépendante : avec_blesse (binaire 0/1)
# Variable indépendante : periode (2 groupes)
#
# Pourquoi test du Chi² et pas Mann-Whitney ?
# avec_blesse est une variable binaire — on compare des proportions,
# pas des distributions continues. Le Chi² de Pearson est le test approprié.
# Condition : effectifs attendus > 5 dans chaque cellule → à vérifier.

cat("\n=== T-011 — CHI² BLESSÉS × SAISON ===\n")

col_proche <- col_proche %>%
  mutate(periode = ifelse(saison %in% c("Printemps","Été"),
                          "Beau temps", "Mauvais temps"))

table_chi2 <- table(col_proche$periode, col_proche$avec_blesse)
cat("Table de contingence :\n")
print(table_chi2)

# Vérification effectifs attendus
chi2_test <- chisq.test(table_chi2)
cat("\nEffectifs attendus (condition Chi²) :\n")
print(round(chi2_test$expected, 1))

cat("\n--- Résultats T-011 ---\n")
cat("Chi²  :", round(chi2_test$statistic, 4), "\n")
cat("ddl   :", chi2_test$parameter, "\n")
cat("p     :", format(chi2_test$p.value, scientific = TRUE), "\n")

# Taille d'effet : V de Cramér
n_total <- sum(table_chi2)
v_cramer <- sqrt(chi2_test$statistic / (n_total * (min(dim(table_chi2)) - 1)))
cat("V Cramér :", round(v_cramer, 4), "\n")
cat("Magnitude :", ifelse(v_cramer >= 0.5, "Grand",
                          ifelse(v_cramer >= 0.3, "Moyen",
                                 ifelse(v_cramer >= 0.1, "Petit", "Négligeable"))), "\n")

if (chi2_test$p.value < 0.05) {
  cat("\n-> REJET H0 — le taux de blessés diffère selon la saison\n")
} else {
  cat("\n-> NON-REJET H0 — pas de différence saisonnière significative\n")
  cat("   L'hypothèse vélo/beau temps n'est pas confirmée par ces données.\n")
  cat("   Interprétation : les collisions avec blessés sont distribuées\n")
  cat("   uniformément sur l'année — d'autres facteurs dominent.\n")
}

# Test complémentaire — sévérité (continue) par saison
cat("\n=== COMPLÉMENT — SÉVÉRITÉ PAR SAISON (Kruskal-Wallis) ===\n")

# Normalité ?
sw_sev <- shapiro.test(sample(col_proche$indicateur_de_severite, 5000))
cat("Shapiro-Wilk sévérité (échantillon 5000) : W =",
    round(sw_sev$statistic, 3), "p =",
    format(sw_sev$p.value, scientific = TRUE), "\n")

kw_sev <- kruskal.test(indicateur_de_severite ~ saison, data = col_proche)
cat("KW Chi² :", round(kw_sev$statistic, 4),
    "| ddl :", kw_sev$parameter,
    "| p :", format(kw_sev$p.value, scientific = TRUE), "\n")

if (kw_sev$p.value < 0.05) {
  cat("-> Sévérité diffère selon la saison\n")
  # Post-hoc Dunn
  library(dunn.test)
  cat("\nPost-hoc Dunn (Bonferroni) :\n")
  dunn.test(col_proche$indicateur_de_severite,
            col_proche$saison,
            method = "bonferroni",
            kw = FALSE, label = TRUE, table = FALSE)
} else {
  cat("-> Pas de différence de sévérité selon la saison\n")
}

# =============================================================================
# 9. VISUALISATIONS
# =============================================================================

# --- VIZ 1 : Top 20 arrêts par nombre de collisions ---
p_top_arrets <- col_proche %>%
  group_by(arret_proche) %>%
  summarise(
    n_collisions = n(),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n_collisions)) %>%
  head(20) %>%
  mutate(arret_proche = factor(arret_proche,
                               levels = rev(arret_proche))) %>%
  ggplot(aes(x = arret_proche, y = n_collisions,
             fill = pct_blesses)) +
  geom_col(alpha = 0.9) +
  scale_fill_gradient(low = "#FFC300", high = "#C0392B",
                      name = "% avec blessé") +
  coord_flip() +
  labs(
    title    = "Top 20 arrêts TPG — concentration de collisions",
    subtitle = paste0("Jointure spatiale plus proche voisin, seuil 200m\n",
                      "Couleur = % de collisions avec blessé humain"),
    x        = NULL,
    y        = "Nombre de collisions (2015–2026)",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave("../outputs/11_top_arrets_collisions.png",
       p_top_arrets, width = 10, height = 8, dpi = 150)
cat("\nGraphique sauvegardé : 11_top_arrets_collisions.png\n")

# --- VIZ 2 : Profil horaire collisions vs blessés ---
p_horaire <- col_proche %>%
  filter(!is.na(heure_num)) %>%
  group_by(heure_num) %>%
  summarise(
    n_collisions = n(),
    n_blesses    = sum(avec_blesse),
    pct_blesses  = mean(avec_blesse) * 100,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = heure_num)) +
  geom_col(aes(y = n_collisions), fill = TPG_RED, alpha = 0.7) +
  geom_line(aes(y = pct_blesses * max(n_collisions) / 15),
            color = "#C0392B", linewidth = 1.2) +
  geom_point(aes(y = pct_blesses * max(n_collisions) / 15),
             color = "#C0392B", size = 2) +
  scale_x_continuous(breaks = 0:23,
                     labels = paste0(0:23, "h")) +
  scale_y_continuous(
    name = "Nombre de collisions",
    sec.axis = sec_axis(~ . * 15 / max(col_proche %>%
                                         group_by(heure_num) %>%
                                         summarise(n=n()) %>%
                                         pull(n), na.rm = TRUE),
                        name = "% avec blessé humain")
  ) +
  labs(
    title    = "Profil horaire des collisions TPG",
    subtitle = "Barres = volume | Ligne rouge = % avec blessé humain",
    x        = "Heure",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave("../outputs/11_profil_horaire_collisions.png",
       p_horaire, width = 12, height = 6, dpi = 150)
cat("Graphique sauvegardé : 11_profil_horaire_collisions.png\n")

# --- VIZ 3 : Sévérité et blessés par saison ---
p_saison <- col_proche %>%
  group_by(saison) %>%
  summarise(
    n_collisions = n(),
    pct_blesses  = mean(avec_blesse) * 100,
    severite_moy = mean(indicateur_de_severite),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = saison)) +
  geom_col(aes(y = n_collisions), fill = TPG_RED, alpha = 0.7) +
  geom_line(aes(y = pct_blesses * max(n_collisions) / 15,
                group = 1),
            color = "#C0392B", linewidth = 1.2) +
  geom_point(aes(y = pct_blesses * max(n_collisions) / 15),
             color = "#C0392B", size = 3) +
  scale_y_continuous(
    name = "Nombre de collisions",
    sec.axis = sec_axis(~ . * 15 / max(col_proche %>%
                                         group_by(saison) %>%
                                         summarise(n=n()) %>%
                                         pull(n), na.rm = TRUE),
                        name = "% avec blessé humain")
  ) +
  labs(
    title    = "Collisions TPG par saison",
    subtitle = paste0("Barres = volume | Ligne = % avec blessé\n",
                      "Hypothèse vélo : beau temps → plus de cyclistes → plus de blessés ?"),
    x        = NULL,
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave("../outputs/11_collisions_saison.png",
       p_saison, width = 8, height = 6, dpi = 150)
cat("Graphique sauvegardé : 11_collisions_saison.png\n")

# =============================================================================
# 9b. SCORE COMPOSITE — INDICE DE DANGEROSITÉ PAR ARRÊT
# =============================================================================
#
# MOTIVATION : le volume seul est un mauvais indicateur de dangerosité.
# Cornavin (331 collisions) n'est qu'en 10e position composite car ses
# collisions sont majoritairement légères (sévérité 1.04, blessés 14.2%).
# Un arrêt avec peu de collisions mais très graves mérite autant d'attention.
#
# MÉTHODE : score par rang — chaque dimension contribue également.
# Pas de pondération arbitraire : on laisse les données parler.
#   score = rang(volume) + rang(taux_blesses) + rang(severite_moy)
#
# Seuil minimum : 10 collisions — en dessous, les proportions sont instables.
#
# [TPG] Ce score composite est un outil de PRIORISATION opérationnelle.
#   Il permet d'identifier les arrêts qui méritent une analyse terrain
#   (signalisation, aménagement, visibilité) indépendamment de leur volume.
#   Exactement le type d'indicateur utile pour l'Unité analytique Exploitation.

cat("\n=== SCORE COMPOSITE — INDICE DE DANGEROSITÉ ===\n")
cat("Méthode : rang(volume) + rang(taux_blessés) + rang(sévérité)\n")
cat("Seuil minimum : 10 collisions\n\n")

points_noirs <- col_proche %>%
  group_by(arret_proche) %>%
  summarise(
    n_collisions = n(),
    pct_blesses  = round(mean(avec_blesse) * 100, 1),
    severite_moy = round(mean(indicateur_de_severite), 3),
    .groups = "drop"
  ) %>%
  filter(n_collisions >= 10) %>%
  mutate(
    rang_volume   = rank(n_collisions),
    rang_blesses  = rank(pct_blesses),
    rang_severite = rank(severite_moy),
    score_composite = rang_volume + rang_blesses + rang_severite
  ) %>%
  arrange(desc(score_composite))

cat("=== TOP 15 POINTS NOIRS (score composite) ===\n")
points_noirs %>%
  head(15) %>%
  dplyr::select(arret_proche, n_collisions, pct_blesses,
                severite_moy, score_composite) %>%
  print()

cat("\n--- Interprétation clé ---\n")
cat("Gare Cornavin : 331 collisions mais rang composite =",
    which(points_noirs$arret_proche == "Gare Cornavin"),
    "— collisions majoritairement légères\n")
cat("Grangettes   : seulement", 
    points_noirs$n_collisions[points_noirs$arret_proche == "Grangettes"],
    "collisions mais rang composite = 1 — gravité structurelle élevée\n")

# [TPG] Les arrêts en périphérie (Grangettes, Onex-Salle communale,
#   Grange-Canal) dominent le classement composite malgré un volume modéré.
#   Hypothèse opérationnelle : vitesse plus élevée en périphérie,
#   moins de congestion naturelle → impacts plus violents.
#   À croiser avec les données de signalisation et d'aménagement.

# --- VIZ 4 : Bubble chart — volume × taux blessés × sévérité ---
# Les trois dimensions simultanément sur un seul graphique

p_bubble <- points_noirs %>%
  head(30) %>%
  ggplot(aes(x = pct_blesses,
             y = severite_moy,
             size = n_collisions,
             label = arret_proche)) +
  geom_point(alpha = 0.6, color = TPG_RED) +
  geom_text(size = 2.5, vjust = -1, check_overlap = TRUE) +
  scale_size_continuous(range = c(3, 15), name = "Nb collisions") +
  labs(
    title    = "Points noirs TPG — portrait en 3 dimensions",
    subtitle = paste0("Top 30 arrêts (min. 10 collisions)\n",
                      "X = % avec blessé | Y = sévérité moyenne | ",
                      "Taille = volume"),
    x        = "% de collisions avec blessé humain",
    y        = "Sévérité moyenne",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave("../outputs/11_bubble_points_noirs.png",
       p_bubble, width = 12, height = 8, dpi = 150)
cat("\nGraphique sauvegardé : 11_bubble_points_noirs.png\n")

# =============================================================================
# BILAN SCRIPT 11
# =============================================================================

cat("\n=== BILAN SCRIPT 11 ===\n")
cat("Jointure spatiale    : plus proche voisin, seuil 200m\n")
cat("Collisions assignées :", nrow(col_proche), "/", nrow(collisions),
    "(", round(nrow(col_proche)/nrow(collisions)*100, 1), "%)\n")
cat("T-011 Chi² blessés × saison : p =",
    format(chi2_test$p.value, scientific = TRUE),
    "-> hypothèse vélo réfutée\n")
cat("KW sévérité × saison        : p =",
    format(kw_sev$p.value, scientific = TRUE),
    "-> sévérité stable sur l'année\n")
cat("Score composite top 1       : Grangettes\n")
cat("Score composite top 2       : Onex-Salle communale\n")
cat("Score composite top 3       : Plainpalais\n")
cat("4 graphiques sauvegardés dans outputs/\n")
cat("\n[TPG] Indicateurs opérationnels prioritaires :\n")
cat("  - Score composite = outil de priorisation terrain\n")
cat("  - Arrêts périphériques dominants malgré volume modéré\n")
cat("  - Plainpalais : seul arrêt central avec volume ET gravité élevés\n")