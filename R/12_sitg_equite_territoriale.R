# ============================================================
# SCRIPT 12 - ÉQUITÉ TERRITORIALE (DONNÉES SITG)
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-03, R-05, R-07, R-08, R-09, T-04,
#               T-06, S-21, S-22, S-33
# ------------------------------------------------------------
# OBJECTIF : croiser les arrêts et la fréquentation tpg avec la
# population des communes genevoises et des secteurs de la Ville
# de Genève.
#
# S-22 : la version d'avril joignait les montées aux arrêts par le
# NOM d'arrêt. Or 1989 arrêts actifs ne portent que 877 noms
# distincts, et un même nom peut couvrir jusqu'à douze quais. La
# jointure multipliait donc les montées d'un arrêt par le nombre
# de ses quais dans la commune. La jointure se fait désormais par
# le code d'arrêt, qui est unique.
#
# S-21 : les montées rapportées à la population résidente ne
# mesurent pas l'usage des transports par les habitants. Une
# montée à Cornavin est le fait de n'importe quel voyageur du
# canton, pas d'un habitant de la Ville de Genève. Les communes
# qui concentrent emplois, gares et commerces ressortent donc
# très haut par construction. L'indicateur est conservé sous son
# vrai nom, charge du réseau rapportée à la population, et
# l'équité territoriale est mesurée par la couverture en arrêts,
# qui ne souffre pas de ce biais.
#
# S-33 : deux corrections.
#   1. SURFACE. Le SHAPE_AREA des communes riveraines inclut leur
#      part du lac (Hermance 4.91 km2 contre 1.44 km2 de terre).
#      Les polygones sont désormais découpés par la couche SITG
#      GEO_LAC (Léman, Rhône, Arve) et la surface est recalculée
#      sur la terre. Les arrêts restent rattachés aux polygones
#      administratifs complets (Bel-Air est sur le Rhône).
#   2. MÉTHODE. La corrélation de Spearman entre densité et arrêts
#      pour mille habitants met en relation deux rapports qui ont
#      la population en commun : une partie de la corrélation vient
#      de la construction. Elle est remplacée par la régression
#      log(arrets) ~ log(population) + log(surface), dont les
#      coefficients sont des élasticités. Le repérage des communes
#      atypiques se fait sur les résidus de ce modèle.
#
# DÉCALAGE TEMPOREL : la population SITG porte une date de
# référence propre, relevée à l'exécution, qui n'est pas celle du
# snapshot tpg. Le script affiche les deux.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(scales)
library(sandwich)

# ── 1. CHARGEMENT AUTONOME (R-03) ───────────────────────────

# Le shapefile peut être rangé directement dans son dossier ou dans
# un sous-dossier livré par le SITG. On le cherche au lieu de coder
# un chemin qui dépendrait de la façon dont l'archive a été ouverte.
trouver_shapefile <- function(dossier, motif) {
  chemins <- list.files(file.path(DIR_SITG, dossier), pattern = motif,
                        recursive = TRUE, full.names = TRUE)
  if (length(chemins) == 0)
    stop("Shapefile introuvable sous ", file.path(DIR_SITG, dossier),
         " pour le motif ", motif)
  if (length(chemins) > 1)
    warning("Plusieurs shapefiles trouvés, le premier est retenu : ",
            paste(basename(chemins), collapse = ", "))
  chemins[1]
}

chemin_communes <- trouver_shapefile("communes", "^OCS_POPBATLOG_COMMUNE\\.shp$")
chemin_secteurs <- trouver_shapefile("secteurs", "^OCS_POPBATLOG_VGE_SECTEUR\\.shp$")
chemin_lac      <- trouver_shapefile("lac", "^GEO_LAC\\.shp$")

cat("Shapefile communes :", chemin_communes, "\n")
cat("Shapefile secteurs :", chemin_secteurs, "\n")
cat("Shapefile lac      :", chemin_lac, "\n\n")

communes_raw <- st_read(chemin_communes, quiet = TRUE)
secteurs_raw <- st_read(chemin_secteurs, quiet = TRUE)
lac_raw      <- st_read(chemin_lac, quiet = TRUE)

DATE_REF_SITG <- as.character(unique(communes_raw$DATE_REF))[1]

cat("Snapshot tpg :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Population SITG, date de référence :", DATE_REF_SITG, "\n")
cat("Les deux sources n'ont pas la même date. À rappeler dans toute\n")
cat("légende et dans la méthodologie.\n")

cat("\nCommunes :", nrow(communes_raw), "| population totale :",
    format(sum(communes_raw$POPULATION, na.rm = TRUE), big.mark = " "), "\n")
cat("Secteurs :", nrow(secteurs_raw), "| population totale :",
    format(sum(secteurs_raw$POPULATION, na.rm = TRUE), big.mark = " "), "\n")
cat("Les secteurs ne couvrent que la Ville de Genève.\n")
cat("Système de coordonnées SITG :", st_crs(communes_raw)$input, "\n")

arrets <- readRDS(file.path(DIR_RAW, "arrets.rds")) %>%
  filter(actif == "Y") %>%
  separate(coordonnees, into = c("latitude", "longitude"),
           sep = ", ", convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

arrets_sf <- st_as_sf(arrets, coords = c("longitude", "latitude"), crs = 4326)

# Certaines géométries du fichier secteurs comportent des sommets
# dupliqués, ce qui fait échouer la jointure spatiale. st_make_valid
# les répare. On vérifie ce qui a été corrigé plutôt que de réparer
# en silence.
n_invalides_c <- sum(!st_is_valid(communes_raw))
n_invalides_s <- sum(!st_is_valid(secteurs_raw))
cat("Géométries invalides avant réparation : communes", n_invalides_c,
    "| secteurs", n_invalides_s, "\n")

# S-33 : retrait du lac, du Rhône et de l'Arve. Le découpage se fait
# dans le système suisse (mètres), avant la conversion en degrés.
if (st_crs(lac_raw) != st_crs(communes_raw))
  lac_raw <- st_transform(lac_raw, st_crs(communes_raw))
lac <- st_union(st_make_valid(lac_raw))
cat("Surfaces d'eau retirées :", paste(lac_raw$NOM, collapse = ", "), "\n")

retirer_eau <- function(polygones) {
  p <- st_make_valid(polygones)
  st_agr(p) <- "constant"   # les attributs valent pour la partie terrestre
  p <- st_make_valid(st_difference(p, lac))
  p$surface_terre_km2 <- as.numeric(st_area(p)) / 1e6
  p
}

communes_terre <- retirer_eau(communes_raw)
secteurs_terre <- retirer_eau(secteurs_raw)

comparaison_surfaces <- communes_terre %>%
  st_drop_geometry() %>%
  transmute(COMMUNE, surface_sitg_km2 = SHAPE_AREA / 1e6, surface_terre_km2,
            part_eau_pct = round(100 * (1 - surface_terre_km2 / surface_sitg_km2), 1))

cat("\n=== S-33 : SURFACE TERRESTRE ===\n")
cat("Communes dont la surface SITG comprend de l'eau :",
    sum(comparaison_surfaces$part_eau_pct >= 0.1), "sur", nrow(comparaison_surfaces), "\n")
cat("Cinq communes les plus touchées (km2) :\n")
print(as.data.frame(comparaison_surfaces %>%
  arrange(desc(part_eau_pct)) %>% slice_head(n = 5) %>%
  mutate(across(c(surface_sitg_km2, surface_terre_km2), ~ round(., 2)))))
cat("Contrôle externe : Hermance 1.44 km2 et Versoix 10.51 km2 selon la\n")
cat("statistique de la superficie.\n")

# Les ARRÊTS sont rattachés aux polygones administratifs complets :
# un arrêt sur un pont ou une île du Rhône (Bel-Air) appartient bien
# à sa commune. Seules les SURFACES viennent des polygones découpés.
communes <- st_make_valid(st_transform(st_make_valid(communes_raw), 4326))
secteurs <- st_make_valid(st_transform(st_make_valid(secteurs_raw), 4326))
communes$surface_terre_km2 <- communes_terre$surface_terre_km2[match(communes$COMMUNE, communes_terre$COMMUNE)]
secteurs$surface_terre_km2 <- secteurs_terre$surface_terre_km2[match(secteurs$NOM_SECTEU, secteurs_terre$NOM_SECTEU)]
if (anyNA(communes$surface_terre_km2) || anyNA(secteurs$surface_terre_km2))
  stop("Surface terrestre manquante pour une commune ou un secteur.")
communes_carte_terre <- st_make_valid(st_transform(communes_terre, 4326))

cat("\nGéométries invalides après réparation : communes",
    sum(!st_is_valid(communes)), "| secteurs", sum(!st_is_valid(secteurs)),
    "| communes découpées", sum(!st_is_valid(communes_carte_terre)), "\n")

cat("Arrêts actifs géolocalisés :", nrow(arrets_sf), "\n")

# ── 2. MONTÉES PAR ARRÊT, PAR CODE (S-22) ───────────────────

montees_par_code <- lire("journalier") %>%
  filter(!is.na(arret_code_long)) %>%
  group_by(arret_code_long) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE), .groups = "drop")

cat("\nCodes d'arrêt dans le journalier :", nrow(montees_par_code), "\n")
cat("Noms d'arrêt distincts parmi les arrêts actifs :",
    n_distinct(arrets$nomarret), "pour", nrow(arrets), "arrêts.\n")
cat("C'est pourquoi la jointure se fait par code et non par nom.\n")

# ── 3. JOINTURE SPATIALE ARRÊTS ET COMMUNES ─────────────────
# st_within : l'arrêt doit être strictement dans le polygone. Les
# arrêts non assignés sont hors canton (France, Vaud), ce qui est
# attendu et vérifié ci-dessous. Jointure sur les polygones
# administratifs complets (voir section 1).

arrets_communes <- st_join(arrets_sf, communes, join = st_within)

n_assignes <- sum(!is.na(arrets_communes$COMMUNE))
cat("\n=== JOINTURE ARRÊTS ET COMMUNES ===\n")
cat("Arrêts assignés à une commune :", n_assignes,
    "(", round(100 * n_assignes / nrow(arrets_sf), 1), "% )\n")
cat("Arrêts hors canton :", nrow(arrets_sf) - n_assignes, "\n")
cat("Répartition par pays des arrêts non assignés :\n")
print(table(arrets_communes$pays[is.na(arrets_communes$COMMUNE)]))

# ── 4. INDICATEURS PAR COMMUNE ──────────────────────────────

sitg_communes <- arrets_communes %>%
  st_drop_geometry() %>%
  filter(!is.na(COMMUNE)) %>%
  left_join(montees_par_code, by = c("arretcodelong" = "arret_code_long")) %>%
  group_by(COMMUNE, POPULATION, surface_terre_km2) %>%
  summarise(n_arrets = n(),
            montees_totales = sum(montees, na.rm = TRUE),
            .groups = "drop") %>%
  filter(POPULATION > 0) %>%
  mutate(
    surface_km2      = surface_terre_km2,
    densite_pop      = POPULATION / surface_km2,
    arrets_par_km2   = n_arrets / surface_km2,
    arrets_par_1000h = 1000 * n_arrets / POPULATION,
    charge_par_hab   = montees_totales / POPULATION
  ) %>%
  select(-surface_terre_km2)

cat("\n=== INDICATEURS PAR COMMUNE ===\n")
cat("Communes retenues :", nrow(sitg_communes), "sur", nrow(communes_raw), "\n")
cat("Commune(s) sans arrêt actif, hors analyse :",
    paste(setdiff(communes_raw$COMMUNE, sitg_communes$COMMUNE), collapse = ", "), "\n")
cat("Surface terrestre totale :", round(sum(sitg_communes$surface_km2)), "km2\n")

cat("\nDix communes à la plus forte charge rapportée à la population :\n")
print(as.data.frame(sitg_communes %>%
  arrange(desc(charge_par_hab)) %>% slice_head(n = 10) %>%
  transmute(COMMUNE, POPULATION, densité = round(densite_pop),
            n_arrets, charge_par_hab = round(charge_par_hab))))
cat("\nCe classement reflète d'abord la concentration des déplacements,\n")
cat("pas l'usage des transports par les habitants de ces communes (S-21).\n")

# ── 5. COUVERTURE EN ARRÊTS : L'INDICATEUR D'ÉQUITÉ ─────────
# Le nombre d'arrêts par habitant et par km2 mesure ce que la
# collectivité met à disposition, sans dépendre de qui utilise le
# réseau ni d'où viennent les voyageurs.

cat("\n=== COUVERTURE EN ARRÊTS ===\n")
cat("Dix communes les mieux dotées pour mille habitants :\n")
print(as.data.frame(sitg_communes %>%
  arrange(desc(arrets_par_1000h)) %>% slice_head(n = 10) %>%
  transmute(COMMUNE, POPULATION, n_arrets,
            arrets_par_1000h = round(arrets_par_1000h, 2),
            arrets_par_km2 = round(arrets_par_km2, 1))))

cat("\nDix communes les moins bien dotées pour mille habitants :\n")
print(as.data.frame(sitg_communes %>%
  arrange(arrets_par_1000h) %>% slice_head(n = 10) %>%
  transmute(COMMUNE, POPULATION, n_arrets,
            arrets_par_1000h = round(arrets_par_1000h, 2),
            arrets_par_km2 = round(arrets_par_km2, 1))))

cat("\nLes communes peu peuplées ont mécaniquement plus d'arrêts par\n")
cat("habitant : le réseau doit les traverser quelle que soit leur\n")
cat("population. Les deux extrémités du classement se lisent ensemble.\n")

# ── 6. T-012 : POPULATION, SURFACE ET NOMBRE D'ARRÊTS (S-33) ─
# Modèle : log(arrets) = a + b_pop * log(population) + b_surf * log(surface)
# b_pop et b_surf sont des élasticités : +1 % de population, à
# surface égale, donne b_pop % d'arrêts en plus.
# Si b_pop + b_surf = 1, le nombre d'arrêts par habitant ne dépend
# que de la densité, avec une élasticité b_pop - 1. On le teste.
# Erreurs types HC3 (robustes à l'hétéroscédasticité, adaptées à
# un petit échantillon), à côté des erreurs types classiques.

modele <- lm(log(n_arrets) ~ log(POPULATION) + log(surface_km2), data = sitg_communes)
V_hc3  <- vcovHC(modele, type = "HC3")
coefs  <- coef(modele)
se_cl  <- sqrt(diag(vcov(modele)))
se_hc3 <- sqrt(diag(V_hc3))
t_crit <- qt(0.975, df = df.residual(modele))

tab_t012 <- data.frame(
  terme      = c("constante", "log(population)", "log(surface terrestre)"),
  estimation = round(coefs, 3),
  ic_inf_hc3 = round(coefs - t_crit * se_hc3, 3),
  ic_sup_hc3 = round(coefs + t_crit * se_hc3, 3),
  p_hc3      = signif(2 * pt(-abs(coefs / se_hc3), df.residual(modele)), 3),
  p_classique = signif(2 * pt(-abs(coefs / se_cl), df.residual(modele)), 3),
  row.names = NULL)

somme   <- sum(coefs[2:3])
se_som  <- sqrt(sum(V_hc3[2:3, 2:3]))
ic_som  <- round(somme + c(-1, 1) * t_crit * se_som, 3)
r2      <- summary(modele)$r.squared
cor_reg <- cor(log(sitg_communes$POPULATION), log(sitg_communes$surface_km2))

cat("\n=== T-012 : RÉGRESSION DU NOMBRE D'ARRÊTS (S-33) ===\n")
cat("n communes :", nrow(sitg_communes), "| R2 :", round(r2, 3),
    "| corrélation entre les deux régresseurs :", round(cor_reg, 3), "\n")
print(tab_t012)
cat("\nSomme des élasticités :", round(somme, 3),
    "| IC 95 % HC3 : [", ic_som[1], ";", ic_som[2], "]\n")
if (ic_som[1] <= 1 && ic_som[2] >= 1) {
  cat("L'intervalle contient 1 : les données sont compatibles avec un nombre\n")
  cat("d'arrêts par habitant qui ne dépend que de la densité, avec une\n")
  cat("élasticité d'environ", round(coefs[2] - 1, 2), "(une densité doublée donne\n")
  cat("environ", round(100 * (2^(coefs[2] - 1) - 1)), "% d'arrêts par habitant).\n")
} else {
  cat("L'intervalle exclut 1 : la taille de la commune compte en plus de sa\n")
  cat("densité.\n")
}
cat("\nLecture : à surface égale, doubler la population ajoute environ",
    round(100 * (2^coefs[2] - 1)), "% d'arrêts ;\nà population égale, doubler",
    "la surface en ajoute environ", round(100 * (2^coefs[3] - 1)), "%.\n")

sp_charge <- suppressWarnings(
  cor.test(sitg_communes$densite_pop, sitg_communes$charge_par_hab,
           method = "spearman"))
sp_couverture <- suppressWarnings(
  cor.test(sitg_communes$densite_pop, sitg_communes$arrets_par_1000h,
           method = "spearman"))
cat("\nÀ titre descriptif seulement : Spearman densité et arrêts pour mille\n")
cat("habitants, rho =", round(as.numeric(sp_couverture$estimate), 3),
    "(rapports qui partagent la population, S-33) ; densité et charge par\n")
cat("habitant, rho =", round(as.numeric(sp_charge$estimate), 3),
    "(indicateur biaisé, S-21).\n")

enregistrer(
  test_id = "T-012", script = "12_sitg_equite_territoriale.R",
  methode = "Régression log(arrets) ~ log(population) + log(surface terrestre), par commune, erreurs types HC3",
  n = nrow(sitg_communes), statistique = round(r2, 3), p_value = tab_t012$p_hc3[2],
  effet_nom = "Élasticité du nombre d'arrêts à la population",
  effet = tab_t012$estimation[2], ic_inf = tab_t012$ic_inf_hc3[2], ic_sup = tab_t012$ic_sup_hc3[2],
  note = paste0("Élasticité à la surface ", tab_t012$estimation[3], " [",
                tab_t012$ic_inf_hc3[3], " ; ", tab_t012$ic_sup_hc3[3], "]. Somme ",
                round(somme, 3), " [", ic_som[1], " ; ", ic_som[2], "]. R2 ", round(r2, 3),
                ". Surface terrestre (lac retiré, S-33). Population SITG au ", DATE_REF_SITG,
                ", arrêts au ", SNAPSHOT_ID, ". Remplace le Spearman densité/couverture.")
)

# ── 7. COMMUNES ATYPIQUES : RÉSIDUS DU MODÈLE (S-33) ────────
# Un résidu négatif : moins d'arrêts que ne le voudraient la
# population et la surface. Résidus studentisés ; seuil de
# repérage |r| > 2 fixé à l'avance. Ce n'est pas un test : avec
# 44 communes, on attend environ deux dépassements par hasard.
# (Remplace l'écart de rangs de S-23.)

sitg_communes$residu     <- round(residuals(modele), 3)
sitg_communes$residu_stu <- round(rstudent(modele), 2)
SEUIL_RESIDU <- 2

cat("\n=== COMMUNES ATYPIQUES (repérage, pas un test) ===\n")
cat("Résidus studentisés du modèle de la section 6, seuil |r| >", SEUIL_RESIDU, "\n")
cat("\nCinq communes les moins dotées relativement au modèle :\n")
print(as.data.frame(sitg_communes %>% arrange(residu_stu) %>% slice_head(n = 5) %>%
  transmute(COMMUNE, POPULATION, surface_km2 = round(surface_km2, 2), n_arrets,
            attendu = round(exp(log(n_arrets) - residu), 1), residu_stu)))
cat("\nCinq communes les mieux dotées relativement au modèle :\n")
print(as.data.frame(sitg_communes %>% arrange(desc(residu_stu)) %>% slice_head(n = 5) %>%
  transmute(COMMUNE, POPULATION, surface_km2 = round(surface_km2, 2), n_arrets,
            attendu = round(exp(log(n_arrets) - residu), 1), residu_stu)))
cat("\nCommunes au-delà du seuil :",
    ifelse(any(abs(sitg_communes$residu_stu) > SEUIL_RESIDU),
           paste(sitg_communes$COMMUNE[abs(sitg_communes$residu_stu) > SEUIL_RESIDU], collapse = ", "),
           "aucune"), "\n")

cat("\nUn résidu ne dit pas si la desserte est insuffisante : la fréquence\n")
cat("des passages, les tracés et la demande réelle ne sont pas mesurés ici.\n")

# ── 8. SECTEURS DE LA VILLE DE GENEVE ───────────────────────

arrets_secteurs <- st_join(arrets_sf, secteurs, join = st_within)
n_sect <- sum(!is.na(arrets_secteurs$NOM_SECTEU))

cat("\n=== SECTEURS DE LA VILLE DE GENÈVE ===\n")
cat("Arrêts dans un secteur :", n_sect,
    "(", round(100 * n_sect / nrow(arrets_sf), 1), "% des arrêts actifs )\n")
cat("Secteurs couverts :", n_distinct(arrets_secteurs$NOM_SECTEU, na.rm = TRUE),
    "sur", nrow(secteurs_raw), "\n")

sitg_secteurs <- arrets_secteurs %>%
  st_drop_geometry() %>%
  filter(!is.na(NOM_SECTEU)) %>%
  left_join(montees_par_code, by = c("arretcodelong" = "arret_code_long")) %>%
  group_by(NOM_SECTEU, POPULATION, surface_terre_km2) %>%
  summarise(n_arrets = n(), montees_totales = sum(montees, na.rm = TRUE),
            .groups = "drop") %>%
  filter(POPULATION > 0) %>%
  mutate(surface_km2 = surface_terre_km2,
         densite_pop = POPULATION / surface_km2,
         arrets_par_1000h = 1000 * n_arrets / POPULATION,
         charge_par_hab = montees_totales / POPULATION) %>%
  select(-surface_terre_km2)

cat("\nIndicateurs par secteur (surface terrestre) :\n")
print(as.data.frame(sitg_secteurs %>%
  arrange(desc(densite_pop)) %>%
  transmute(NOM_SECTEU, POPULATION, densité = round(densite_pop),
            n_arrets, arrets_par_1000h = round(arrets_par_1000h, 2),
            charge_par_hab = round(charge_par_hab))))

# ── 9. FIGURES ──────────────────────────────────────────────

p_couverture <- ggplot(sitg_communes, aes(x = densite_pop, y = arrets_par_1000h)) +
  geom_point(color = ROUGE_PRINCIPAL, size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = COL_NEUTRE, linewidth = 0.6, linetype = "dashed") +
  scale_x_log10(labels = label_number(big.mark = " ")) +
  scale_y_log10() +
  labs(title = "Densité de population et couverture en arrêts",
       subtitle = paste0("Une commune par point, ", nrow(sitg_communes),
                         " communes. Surface terrestre, lac retiré. Élasticités du nombre\n",
                         "d'arrêts : population ", round(coefs[2], 2), ", surface ",
                         round(coefs[3], 2), " (R2 = ", round(r2, 2),
                         "). Échelles logarithmiques."),
       x = "Habitants par km2 de terre", y = "Arrêts pour mille habitants",
       caption = paste(SOURCE_TPG, SOURCE_SITG,
                       paste0("Population au ", DATE_REF_SITG,
                              ". Surfaces recalculées après retrait des surfaces d'eau."),
                       sep = "\n")) +
  theme_projet()

print(p_couverture)
ggsave(file.path(DIR_FIG, "12_densite_couverture.png"), p_couverture,
       width = 10, height = 7, dpi = 150)

communes_carte <- communes_carte_terre %>%
  left_join(sitg_communes %>% select(COMMUNE, arrets_par_1000h), by = "COMMUNE")

p_carte <- ggplot(communes_carte) +
  geom_sf(aes(fill = arrets_par_1000h), color = "white", linewidth = 0.2) +
  scale_fill_gradient(low = "#F5E4E6", high = ROUGE_PRINCIPAL,
                      name = "Arrêts pour\nmille habitants",
                      na.value = "grey90") +
  labs(title = "Couverture en arrêts par commune",
       subtitle = paste0("Arrêts actifs pour mille habitants. Population SITG, état ",
                         DATE_REF_SITG, ".\nEn gris : commune sans arrêt actif."),
       caption = paste(SOURCE_TPG, SOURCE_SITG,
                       paste0("Population au ", DATE_REF_SITG,
                              ". Surfaces recalculées après retrait des surfaces d'eau."),
                       sep = "\n")) +
  theme_projet() +
  # theme_projet() règle axis.text.x (rotation), plus spécifique que
  # axis.text : il faut donc neutraliser chaque axe explicitement.
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

print(p_carte)
ggsave(file.path(DIR_FIG, "12_carte_couverture.png"), p_carte,
       width = 10, height = 8, dpi = 150)
message("Figures enregistrées.")

# ── 10. SAUVEGARDE ──────────────────────────────────────────

write.csv(sitg_communes %>%
            mutate(across(where(is.numeric), ~ round(., 3))),
          file.path(DIR_RES, paste0("12_communes_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(sitg_secteurs %>%
            mutate(across(where(is.numeric), ~ round(., 3))),
          file.path(DIR_RES, paste0("12_secteurs_", SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 12 terminé. Population SITG au ", DATE_REF_SITG,
        ", montées tpg au ", format(DATE_COUPURE), ".")
