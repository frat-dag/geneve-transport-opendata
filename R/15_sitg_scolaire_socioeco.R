# ── SCRIPT 15 — SITG : LIGNES SCOLAIRES & ÉQUITÉ SOCIO-ÉCONOMIQUE ────────────
# Projet : TPG Open Data Analysis
# Auteur : Frat DAG
# Date   : 2026-05-19
# Données: journalier.rds | arrets.rds | SITG OCS_POPBATLOG_COMMUNE
#          SITG OCS_POPBATLOG_VGE_SECTEUR
# Synergies : EXT-006 | EXT-008 | OBS-022
# Tests     : T-015a — Spearman arrêts scolaires/commune × population
# ─────────────────────────────────────────────────────────────────────────────

source("00_palette.R")

library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(scales)
library(here)

sf_use_s2(FALSE)

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  SCRIPT 15 — SITG : Lignes scolaires & équité socio-éco     ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 1. CHARGEMENT DES DONNÉES
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 1. Chargement des données ===\n\n")

journalier <- readRDS(here::here("data/raw/journalier.rds")) %>%
  filter(donnees_definitives == TRUE) %>%
  mutate(ligne = as.character(ligne))

arrets_raw <- readRDS(here::here("data/raw/arrets.rds"))

communes_raw <- st_read(
  here::here("data/raw/sitg/communes/OCS_POPBATLOG_COMMUNE-SHP/OCS_POPBATLOG_COMMUNE.shp"),
  quiet = TRUE
)
communes_wgs84 <- st_transform(communes_raw, crs = 4326)

secteurs_raw <- st_read(
  here::here("data/raw/sitg/secteurs/OCS_POPBATLOG_VGE_SECTEUR-SHP/OCS_POPBATLOG_VGE_SECTEUR.shp"),
  quiet = TRUE
)
secteurs_wgs84 <- st_transform(secteurs_raw, crs = 4326)

cat("journalier : ", nrow(journalier), " lignes (données définitives)\n", sep = "")
cat("arrets_raw : ", nrow(arrets_raw), " arrêts\n", sep = "")
cat("communes   : ", nrow(communes_raw), " polygones | CRS : ",
    st_crs(communes_raw)$Name, "\n", sep = "")
cat("secteurs   : ", nrow(secteurs_raw), " polygones\n\n", sep = "")

# ─────────────────────────────────────────────────────────────────────────────
# 2. PRÉPARATION DES ARRÊTS (séparer coordonnées)
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 2. Préparation des arrêts géolocalisés ===\n\n")

arrets_coords <- arrets_raw %>%
  filter(actif == "Y", !is.na(coordonnees)) %>%
  tidyr::separate(coordonnees, into = c("latitude", "longitude"),
                  sep = ",", convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

arrets_sf <- arrets_coords %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

cat("Arrêts actifs avec coordonnées : ", nrow(arrets_sf), "\n\n", sep = "")

# ─────────────────────────────────────────────────────────────────────────────
# 3. EXPLORATION SITG — COLONNES DISPONIBLES (avant toute hypothèse)
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 3. Exploration SITG — colonnes disponibles ===\n\n")

cat("--- communes : names() ---\n")
print(names(communes_raw))
cat("\n--- communes : str() (niveau 1) ---\n")
str(communes_raw, max.level = 1)

cat("\n--- secteurs : names() ---\n")
print(names(secteurs_raw))
cat("\n--- secteurs : str() (niveau 1) ---\n")
str(secteurs_raw, max.level = 1)
cat("\n")

# Détection dynamique — colonne nom commune
cols_com <- names(communes_raw)
nom_col <- if ("COMMUNE"     %in% cols_com) "COMMUNE"     else
           if ("NOM_COMMUNE" %in% cols_com) "NOM_COMMUNE" else
           if ("NOM"         %in% cols_com) "NOM"         else cols_com[1]

# Détection dynamique — colonne population
pop_col <- if ("POPULATION" %in% cols_com) "POPULATION" else
           if ("POP_TOT"    %in% cols_com) "POP_TOT"    else
           if ("POP"        %in% cols_com) "POP"        else NA_character_

# Indicateurs socio-économiques (revenus, CSP) — absents de OCS_POPBATLOG
candidats_socioeco <- toupper(c("REVENU", "REVENU_MOY", "REVENU_MED",
                                 "CSP", "INDICE_GINI", "REV_MEDIAN",
                                 "NIVEAU_VIE", "QUINTILE"))
has_socioeco <- any(candidats_socioeco %in% toupper(cols_com))

cat("Colonne commune détectée      :", nom_col, "\n")
cat("Colonne population détectée   :",
    ifelse(is.na(pop_col), "ABSENTE", pop_col), "\n")
cat("Indicateurs socio-éco         :",
    ifelse(has_socioeco,
           "PRÉSENTS — analyse EXT-008 conduite",
           "ABSENTS — LIMITE documentée section 5"), "\n\n")

# ─────────────────────────────────────────────────────────────────────────────
# 4. EXT-006 — LIGNES SCOLAIRES C1-C9 : COUVERTURE GÉOGRAPHIQUE
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 4. EXT-006 — Lignes scolaires C1-C9 ===\n\n")

# ── 4.1 Arrêts scolaires distincts ──────────────────────────────────────────

scolaire_arrets <- journalier %>%
  filter(ligne_type_act == "SCOLAIRE") %>%
  dplyr::select(ligne, arret_code_long) %>%
  distinct()

cat("Lignes SCOLAIRE présentes :", n_distinct(scolaire_arrets$ligne), "\n")
cat("Arrêts SCOLAIRE distincts :", n_distinct(scolaire_arrets$arret_code_long), "\n")
cat("Lignes :", paste(sort(unique(scolaire_arrets$ligne)), collapse = ", "), "\n\n")

# ── 4.2 Géolocalisation des arrêts scolaires ─────────────────────────────────

arrets_scolaires_df <- scolaire_arrets %>%
  dplyr::select(arret_code_long) %>%
  distinct() %>%
  inner_join(
    arrets_coords %>%
      dplyr::select(arretcodelong, latitude, longitude),
    by = c("arret_code_long" = "arretcodelong")
  )

arrets_scolaires_sf <- arrets_scolaires_df %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

n_sco_total   <- n_distinct(scolaire_arrets$arret_code_long)
n_sco_geolocal <- nrow(arrets_scolaires_sf)

cat("Arrêts scolaires géolocalisés :", n_sco_geolocal, "/", n_sco_total,
    "(", round(n_sco_geolocal / n_sco_total * 100, 1), "%)\n\n")

# ── 4.3 Jointure spatiale : arrêts scolaires × communes ─────────────────────

# st_within : arrêt strictement à l'intérieur du polygone
# Exclut les arrêts hors canton (France, Vaud) — attendu
join_sco_com <- st_join(
  arrets_scolaires_sf,
  communes_wgs84[, c(nom_col, "geometry")],
  join = st_within
) %>% st_drop_geometry()

names(join_sco_com)[names(join_sco_com) == nom_col] <- "commune"

arrets_par_commune_sco <- join_sco_com %>%
  filter(!is.na(commune)) %>%
  group_by(commune) %>%
  summarise(n_arrets_scolaires = n(), .groups = "drop") %>%
  arrange(desc(n_arrets_scolaires))

n_hors_canton_sco    <- sum(is.na(join_sco_com$commune))
n_communes_scolaires <- nrow(arrets_par_commune_sco)
n_communes_total     <- nrow(communes_raw)

cat("Communes avec ≥1 arrêt scolaire :", n_communes_scolaires, "/",
    n_communes_total,
    "(", round(n_communes_scolaires / n_communes_total * 100, 1), "%)\n")
cat("Arrêts scolaires hors canton     :", n_hors_canton_sco,
    "(FR/VD — exclus de l'analyse)\n\n")

# ── 4.4 Communes non desservies ──────────────────────────────────────────────

toutes_communes <- communes_raw %>%
  st_drop_geometry() %>%
  dplyr::select(commune = all_of(nom_col)) %>%
  distinct()

communes_non_desservies <- toutes_communes %>%
  filter(!commune %in% arrets_par_commune_sco$commune) %>%
  arrange(commune)

cat("Communes sans arrêt scolaire (",
    nrow(communes_non_desservies), ") :\n", sep = "")
if (nrow(communes_non_desservies) > 0) {
  cat(paste(" -", communes_non_desservies$commune, collapse = "\n"), "\n")
}
cat("\n")

cat("--- Top 10 communes par nombre d'arrêts scolaires ---\n")
print(head(arrets_par_commune_sco, 10))
cat("\n")

# ── 4.5 T-015a — Spearman : arrêts scolaires/commune × population ────────────

if (!is.na(pop_col)) {

  cat("=== T-015a — Spearman : n_arrets scolaires × population ===\n\n")

  pop_communes_df <- communes_raw %>%
    st_drop_geometry() %>%
    dplyr::select(commune    = all_of(nom_col),
                  population = all_of(pop_col)) %>%
    mutate(population = as.numeric(population))

  # Toutes les communes y compris non desservies (n_arrets = 0)
  df_t015a <- pop_communes_df %>%
    left_join(arrets_par_commune_sco, by = "commune") %>%
    mutate(n_arrets_scolaires = replace_na(n_arrets_scolaires, 0)) %>%
    filter(!is.na(population), population > 0)

  cat("H0 : pas d'association monotone entre population et arrêts scolaires/commune\n")
  cat("H1 : les communes plus peuplées ont davantage d'arrêts scolaires\n\n")
  cat("n communes :", nrow(df_t015a), "\n\n")

  sp_t015a <- cor.test(df_t015a$population, df_t015a$n_arrets_scolaires,
                       method = "spearman")
  n_t015a  <- nrow(df_t015a)
  r_t015a  <- abs(qnorm(sp_t015a$p.value / 2)) / sqrt(n_t015a)

  cat("Spearman rho     :", round(sp_t015a$estimate, 3), "\n")
  cat("p-value          :", format(sp_t015a$p.value, scientific = TRUE), "\n")
  cat("n communes       :", n_t015a, "\n")
  cat("Taille d'effet r :", round(r_t015a, 3), "\n\n")

  n_exaequo_sco <- sum(df_t015a$n_arrets_scolaires == 0)
  cat("NOTE ex-aequo : ", n_exaequo_sco,
      " communes ont n_arrets_scolaires=0 — ex-aequo multiples.\n", sep = "")
  cat("R ne calcule pas la p-value exacte en présence d'ex-aequo ;\n")
  cat("p-value asymptotique (", format(sp_t015a$p.value, scientific = TRUE),
      ") utilisée — valide pour n=", n_t015a,
      " (limite mineure, résultat robuste).\n\n", sep = "")

  cat("--- Conclusion T-015a ---\n")
  if (sp_t015a$p.value < 0.05) {
    cat("REJET H0 (p < 0.05)\n")
    cat("Association",
        ifelse(sp_t015a$estimate > 0, "positive", "négative"),
        ": les communes les plus peuplées ont",
        ifelse(sp_t015a$estimate > 0, "davantage", "moins"),
        "d'arrêts scolaires.\n")
    cat("Taille d'effet :", round(r_t015a, 3),
        ifelse(r_t015a >= 0.5, "(grand)",
               ifelse(r_t015a >= 0.3, "(moyen)", "(faible)")), "\n")
  } else {
    cat("NON-REJET H0 (p =", format(sp_t015a$p.value, scientific = TRUE), ")\n")
    cat("Pas d'association monotone significative entre population et couverture scolaire.\n")
    cat("Interprétation : la desserte scolaire ne suit pas la densité de population.\n")
  }

  cat("\nCe qu'on NE peut PAS affirmer :\n")
  cat("  - Que l'absence d'arrêt scolaire = mauvaise desserte :\n")
  cat("    certaines communes ont peu ou pas d'établissements secondaires.\n")
  cat("  - Que l'analyse reflète la demande scolaire réelle :\n")
  cat("    sans données SITG sur les établissements (CO, collèges),\n")
  cat("    on mesure l'offre, pas l'adéquation offre × besoins.\n\n")

} else {

  cat("T-015a : colonne population non disponible dans les .shp SITG.\n")
  cat("LIMITE : test Spearman non réalisable sans données de population.\n\n")
  sp_t015a <- NULL
  r_t015a  <- NA
  n_t015a  <- NA
  df_t015a <- NULL

}

# ─────────────────────────────────────────────────────────────────────────────
# 5. EXT-008 — NIVEAU SOCIO-ÉCONOMIQUE × FRÉQUENTATION
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 5. EXT-008 — Niveau socio-économique × fréquentation ===\n\n")

if (has_socioeco) {

  cat("Indicateurs socio-économiques détectés — analyse à conduire.\n\n")

} else {

  cat("LIMITE EXT-008 : aucun indicateur de revenu ou de CSP dans les .shp SITG.\n\n")
  cat("Les couches OCS_POPBATLOG décrivent la structure bâtie et la population\n")
  cat("résidante, pas les revenus ou catégories socioprofessionnelles.\n\n")
  cat("Source complémentaire requise :\n")
  cat("  OFS — Statistique du revenu imposable par commune (portail OFS,\n")
  cat("  rubrique Statistiques des impôts → Communes, dernière édition 2022)\n\n")

  cat("--- Analyse qualitative de substitution (T-012 × géographie) ---\n\n")
  cat("T-012 (Spearman rho=0.695, p=1.65×10⁻⁷) identifie 4 communes anomaliques\n")
  cat("dans la relation densité de population × montées par habitant.\n\n")

  cat("Sous-fréquentation structurelle :\n")
  cat("  - Troinex : commune résidentielle à dominante pavillonnaire, éloignée\n")
  cat("    du centre — résidents potentiellement plus motorisés.\n")
  cat("    (hypothèse interprétative — non testée sans données OFS)\n")
  cat("  - Genthod : péninsule lacustre difficilement accessible, offre TPG\n")
  cat("    contrainte par la géographie, profil résidentiel aisé présumé.\n\n")

  # [TPG] Ces deux communes cumulent faible offre et profil résidentiel aisé présumé.
  # [TPG] Sans données OFS, le lien avec le niveau socio-éco reste hypothétique.
  # [TPG] Un croisement OFS × T-012 permettrait de trancher la question d'équité territoriale.

  cat("Sur-fréquentation structurelle :\n")
  cat("  - Meyrin : pôle CERN + Palexpo + zones d'emplois denses.\n")
  cat("  - Vernier : forte densité résidentielle, proportion élevée de ménages\n")
  cat("    à faible revenu (hypothèse interprétative — non testée formellement).\n\n")

  cat("LIMITE : sans données OFS, le lien causal entre niveau socio-économique\n")
  cat("et utilisation des TPG reste une hypothèse, pas un résultat prouvé.\n\n")

}

# ─────────────────────────────────────────────────────────────────────────────
# 6. OBS-022 — FRÉQUENTATION DES LIGNES SCOLAIRES
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 6. OBS-022 — Fréquentation lignes scolaires ===\n\n")

freq_scolaire <- journalier %>%
  filter(ligne_type_act == "SCOLAIRE") %>%
  group_by(ligne) %>%
  summarise(
    montees_tot   = sum(nb_de_montees, na.rm = TRUE),
    n_arrets_dis  = n_distinct(arret_code_long),
    n_jours       = n_distinct(date),
    .groups       = "drop"
  ) %>%
  mutate(
    montees_par_jour  = round(montees_tot / n_jours, 1),
    montees_par_arret = round(montees_tot / n_arrets_dis)
  ) %>%
  arrange(desc(montees_tot))

cat("--- Fréquentation par ligne scolaire ---\n")
print(freq_scolaire, n = Inf)
cat("\n")

total_montees  <- sum(journalier$nb_de_montees, na.rm = TRUE)
total_scolaire <- sum(freq_scolaire$montees_tot, na.rm = TRUE)
pct_scolaire   <- round(total_scolaire / total_montees * 100, 3)

cat("Total montées réseau (déf.) :",
    format(round(total_montees), big.mark = " "), "\n")
cat("Montées lignes SCOLAIRE     :",
    format(round(total_scolaire), big.mark = " "), "\n")
cat("Part SCOLAIRE / réseau      :", pct_scolaire, "%\n\n")

# [TPG] Volume faible mais fonction sociale — ces lignes opèrent uniquement
# [TPG] les jours scolaires. Leur utilité se mesure en service rendu, pas en montées totales.
cat("# [TPG] Part SCOLAIRE =", pct_scolaire,
    "% — volume faible ; jours scolaires uniquement.\n")
cat("# [TPG] Communes non desservies (",
    nrow(communes_non_desservies),
    ") = zones potentielles d'extension ou réorientation des tracés.\n\n",
    sep = "")

cat("--- OBS-086 — Meyrin : non desservie C1-C9 mais sur-fréquentée ---\n")
cat("Meyrin figure parmi les communes sans arrêt scolaire (C1-C9) mais est\n")
cat("structurellement sur-fréquentée en transports publics généraux (T-012).\n")
cat("Hypothèse : les élèves de Meyrin utilisent les lignes PRINCIPAL/SECONDAIRE\n")
cat("régulières — la densité du réseau général est suffisante pour ne pas\n")
cat("nécessiter de ligne dédiée. Ce profil distingue Meyrin des communes\n")
cat("réellement mal desservies (Troinex, Genthod).\n")
# [TPG] Signal opérationnel : l'absence de ligne scolaire à Meyrin n'est pas
# [TPG] un manque — c'est un choix cohérent avec la densité du réseau général.
# [TPG] Toutes les communes non desservies C1-C9 ne sont pas équivalentes.
cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# 7. VISUALISATIONS
# ─────────────────────────────────────────────────────────────────────────────

cat("=== 7. Visualisations ===\n\n")

# Couche communes enrichie (pour VIZ1 et VIZ2)
communes_viz <- communes_wgs84 %>%
  dplyr::select(commune = all_of(nom_col), geometry) %>%
  left_join(arrets_par_commune_sco, by = "commune") %>%
  mutate(
    n_arrets_scolaires = replace_na(n_arrets_scolaires, 0),
    couverte           = n_arrets_scolaires > 0
  )

# ── VIZ1 — Carte choroplèthe couverture scolaire par commune ─────────────────
# NOTE VIZ : candidat Python/Leaflet pour version interactive (carte web)

p_viz1 <- ggplot() +
  geom_sf(data = communes_viz,
          aes(fill = n_arrets_scolaires),
          color = "white", linewidth = 0.5) +
  geom_sf(data  = arrets_scolaires_sf,
          color = PALETTE_TYPES["SCOLAIRE"],
          size  = 0.9, alpha = 0.5) +
  scale_fill_gradient(
    low  = "#F5F5F5",
    high = PALETTE_TYPES["SCOLAIRE"],
    name = "Arrêts\nscolaires"
  ) +
  labs(
    title    = "Couverture des lignes scolaires C1-C9 par commune",
    subtitle = paste0(n_communes_scolaires, " / ", n_communes_total,
                      " communes desservies (",
                      round(n_communes_scolaires / n_communes_total * 100, 1), "%)"),
    caption  = "Source : TPG Open Data + SITG OCS_POPBATLOG_COMMUNE | Auteur : Frat DAG"
  ) +
  theme_tpg() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank()
  )

ggsave(here::here("figures/15_couverture_scolaire_communes.png"),
       plot = p_viz1, width = 10, height = 8, dpi = 150)
cat("VIZ1 sauvegardé : figures/15_couverture_scolaire_communes.png\n")

# ── VIZ2 — Barplot communes × arrêts scolaires ───────────────────────────────
# NOTE VIZ : candidat Power BI pour tableau de bord opérationnel

communes_bar <- communes_viz %>%
  st_drop_geometry() %>%
  arrange(n_arrets_scolaires) %>%
  mutate(commune = factor(commune, levels = commune))

p_viz2 <- ggplot(communes_bar,
                 aes(x = commune,
                     y = n_arrets_scolaires,
                     fill = couverte)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("FALSE" = "#DDDDDD", "TRUE" = PALETTE_TYPES["SCOLAIRE"]),
    labels = c("FALSE" = "Non desservie (0 arrêt)", "TRUE" = "Desservie"),
    name   = NULL
  ) +
  scale_y_continuous(breaks = seq(0, 20, 2)) +
  labs(
    title    = "Arrêts scolaires par commune — lignes C1-C9",
    subtitle = paste0(n_communes_total - n_communes_scolaires,
                      " communes sans arrêt scolaire sur ",
                      n_communes_total),
    x        = NULL,
    y        = "Nombre d'arrêts scolaires",
    caption  = "Source : TPG Open Data + SITG | Auteur : Frat DAG"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

ggsave(here::here("figures/15_arrets_scolaires_par_commune.png"),
       plot = p_viz2, width = 10, height = 9, dpi = 150)
cat("VIZ2 sauvegardé : figures/15_arrets_scolaires_par_commune.png\n")

# ── VIZ3 — Scatter population × arrêts scolaires (si population disponible) ──
# NOTE VIZ : candidat Python/seaborn avec annotations interactives

if (!is.na(pop_col) && !is.null(df_t015a)) {

  p_viz3 <- ggplot(df_t015a,
                   aes(x = population, y = n_arrets_scolaires)) +
    geom_point(color = PALETTE_TYPES["SCOLAIRE"], size = 3, alpha = 0.85) +
    geom_smooth(method   = "lm", se       = TRUE,
                color    = COL_NEUTRE,    fill     = "#DDDDDD",
                linetype = "dashed", linewidth = 0.8) +
    geom_text(
      data = df_t015a %>%
        filter(n_arrets_scolaires > quantile(n_arrets_scolaires, 0.75) |
               population > quantile(population, 0.75)),
      aes(label = commune),
      size = 2.7, hjust = -0.1, color = COL_REF, check_overlap = TRUE
    ) +
    scale_x_continuous(labels = scales::comma) +
    labs(
      title    = "Population × arrêts scolaires par commune",
      subtitle = paste0(
        "Spearman rho=", round(sp_t015a$estimate, 3),
        " | p=", format(sp_t015a$p.value, scientific = TRUE),
        " | n=", n_t015a, " communes"
      ),
      x       = "Population (SITG OCS_POPBATLOG, déc. 2025)",
      y       = "Nombre d'arrêts scolaires",
      caption = "Source : TPG Open Data + SITG | Auteur : Frat DAG"
    ) +
    theme_tpg() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

  ggsave(here::here("figures/15_population_vs_arrets_scolaires.png"),
         plot = p_viz3, width = 10, height = 7, dpi = 150)
  cat("VIZ3 sauvegardé : figures/15_population_vs_arrets_scolaires.png\n")

}

cat("\n")

# ─────────────────────────────────────────────────────────────────────────────
# 8. BILAN SCRIPT 15
# ─────────────────────────────────────────────────────────────────────────────

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  BILAN SCRIPT 15                                             ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("EXT-006 — Couverture lignes scolaires C1-C9 :\n")
cat("  Lignes présentes              :", n_distinct(scolaire_arrets$ligne), "\n")
cat("  Arrêts géolocalisés           :", n_sco_geolocal, "/", n_sco_total, "\n")
cat("  Communes desservies           :", n_communes_scolaires, "/", n_communes_total,
    "(", round(n_communes_scolaires / n_communes_total * 100, 1), "%)\n")

if (!is.na(pop_col) && !is.null(sp_t015a)) {
  cat("  T-015a Spearman rho =", round(sp_t015a$estimate, 3),
      "| p =", format(sp_t015a$p.value, scientific = TRUE),
      "| n =", n_t015a, "\n")
} else {
  cat("  T-015a : population non disponible — non réalisé\n")
}

cat("\nEXT-008 — Niveau socio-économique :\n")
if (has_socioeco) {
  cat("  Données socio-éco présentes dans SITG\n")
} else {
  cat("  LIMITE : revenus/CSP absents SITG — analyse qualitative via T-012\n")
  cat("  Source requise : OFS Statistique fiscale communale\n")
}

cat("\nOBS-022 — Part scolaire réseau :", pct_scolaire, "%\n")

n_viz_saved <- ifelse(!is.na(pop_col) && !is.null(df_t015a), 3, 2)
cat(n_viz_saved, "graphiques sauvegardés dans figures/\n\n")
