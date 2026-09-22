# ============================================================
# SCRIPT 15 - LIGNES SCOLAIRES ET TERRITOIRE (DONNÉES SITG)
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-02, T-04,
#               T-06, S-13, S-21, S-22, S-24, S-27, S-28
# 22.09.2026 : carte au point 1 de la passation (lac non decoupe,
# contrairement a la carte du script 12). Meme logique que le
# script 12 (S-33) : le lac, le Rhone et l'Arve (couche SITG
# GEO_LAC) sont retires UNIQUEMENT pour l'affichage de la carte.
# La jointure spatiale (section 2) reste sur les polygones
# administratifs complets : un arret sur le Rhone appartient a sa
# commune. Sans impact statistique, ce script n'utilise ni surface
# ni densite.
# ------------------------------------------------------------
# QUESTIONS :
#   Où s'arrêtent les lignes scolaires ?
#   T-015a : leur couverture suit-elle la population jeune ?
#   Combien de voyageurs transportent-elles ?
#
# S-27 : la version d'avril calculait une « taille d'effet r » à
# partir de la p-value d'une corrélation de Spearman. Pour une
# corrélation, la taille d'effet est rho lui-même. Le script publie
# rho avec un intervalle de confiance bootstrap.
#
# S-28 : la version d'avril recopiait en dur des résultats de T-012
# obtenus avec un indicateur biaisé (S-21) et une jointure fautive
# (S-22), et en tirait des affirmations sur le profil social de
# communes nommées (« profil résidentiel aisé présumé », « ménages
# à faible revenu »). Aucune donnée du projet ne porte sur le revenu.
# Ces affirmations sont retirées.
#
# PÉRIMÈTRE : lignes de type SCOLAIRE sur les douze derniers mois
# avant la coupure, pour décrire le réseau scolaire actuel.
# Population SITG a sa date de référence propre (DATE_REF).
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(scales)
library(lubridate)

set.seed(SEED)

# ── 1. CHARGEMENT ───────────────────────────────────────────

trouver_shapefile <- function(dossier, motif) {
  chemins <- list.files(file.path(DIR_SITG, dossier), pattern = motif,
                        recursive = TRUE, full.names = TRUE)
  if (length(chemins) == 0)
    stop("Shapefile introuvable sous ", file.path(DIR_SITG, dossier))
  chemins[1]
}

communes_raw <- st_read(trouver_shapefile("communes", "^OCS_POPBATLOG_COMMUNE\\.shp$"),
                        quiet = TRUE) %>%
  st_make_valid()

DATE_REF_SITG <- as.character(unique(communes_raw$DATE_REF))[1]

# Decoupage du lac pour la carte (voir note en tete de script). Meme
# logique que le script 12 : union des polygones GEO_LAC, difference
# dans le systeme suisse (metres) avant transformation en degres.
chemin_lac <- trouver_shapefile("lac", "^GEO_LAC\\.shp$")
lac_raw <- st_read(chemin_lac, quiet = TRUE)
if (st_crs(lac_raw) != st_crs(communes_raw))
  lac_raw <- st_transform(lac_raw, st_crs(communes_raw))
lac <- st_union(st_make_valid(lac_raw))

st_agr(communes_raw) <- "constant"
communes_carte_terre <- st_make_valid(st_difference(communes_raw, lac))
communes_carte_terre <- st_transform(communes_carte_terre, 4326)

communes <- st_transform(communes_raw, 4326)

DEBUT_FENETRE <- DATE_COUPURE %m-% months(12) + 1

scolaire <- lire("journalier") %>%
  filter(ligne_type_act == "SCOLAIRE", date >= DEBUT_FENETRE)

arrets <- readRDS(file.path(DIR_RAW, "arrets.rds")) %>%
  separate(coordonnees, into = c("latitude", "longitude"),
           sep = ", ", convert = TRUE) %>%
  filter(!is.na(latitude), !is.na(longitude))

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Fenêtre des lignes scolaires :", format(DEBUT_FENETRE), "à",
    format(DATE_COUPURE), "\n")
cat("Population SITG, date de référence :", DATE_REF_SITG, "\n")

# ── 2. LIGNES ET ARRETS SCOLAIRES ───────────────────────────
# Jointure sur les polygones administratifs complets (communes),
# pas sur communes_carte_terre : un arret sur le Rhone (Bel-Air)
# appartient a sa commune (meme principe qu'au script 12).

cat("\n=== LIGNES SCOLAIRES ===\n")
cat("Lignes :", paste(sort(unique(scolaire$ligne)), collapse = ", "), "\n")
codes_sco <- unique(scolaire$arret_code_long)
cat("Codes d'arrêt desservis :", length(codes_sco), "\n")

arrets_sco <- arrets %>%
  filter(arretcodelong %in% codes_sco) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

cat("Arrêts géolocalisés :", nrow(arrets_sco), "sur", length(codes_sco), "\n")

jointure <- st_join(arrets_sco, communes["COMMUNE"], join = st_within) %>%
  st_drop_geometry()
cat("Arrêts hors canton :", sum(is.na(jointure$COMMUNE)), "\n")

par_commune <- communes %>%
  st_drop_geometry() %>%
  select(COMMUNE, POPULATION, AGE_0_19) %>%
  left_join(jointure %>% filter(!is.na(COMMUNE)) %>%
              count(COMMUNE, name = "arrets_scolaires"),
            by = "COMMUNE") %>%
  mutate(arrets_scolaires = replace_na(arrets_scolaires, 0))

# Desserte générale, pour ne pas confondre « sans arrêt scolaire »
# et « sans desserte » (arrêts actifs, toutes lignes confondues).
desserte <- st_join(arrets %>% filter(actif == "Y") %>%
                      st_as_sf(coords = c("longitude", "latitude"), crs = 4326),
                    communes["COMMUNE"], join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(COMMUNE)) %>%
  count(COMMUNE, name = "arrets_actifs")

par_commune <- par_commune %>%
  left_join(desserte, by = "COMMUNE") %>%
  mutate(arrets_actifs = replace_na(arrets_actifs, 0))

avec <- sum(par_commune$arrets_scolaires > 0)
cat("\nCommunes avec au moins un arrêt scolaire :", avec, "sur", nrow(par_commune), "\n")

sans <- par_commune %>% filter(arrets_scolaires == 0) %>% arrange(desc(AGE_0_19))
cat("\nCommunes sans arrêt scolaire (", nrow(sans), "), par population de 0 à 19 ans :\n", sep = "")
print(as.data.frame(sans %>% transmute(COMMUNE, POPULATION, AGE_0_19, arrets_actifs)))
cat("\nSans arrêt scolaire ne veut pas dire sans desserte : la colonne\n")
cat("arrets_actifs compte les arrêts de toutes les lignes. Les lignes\n")
cat("scolaires complètent le réseau là où il ne suffit pas ; leur\n")
cat("absence peut simplement signifier que le réseau général suffit.\n")

cat("\nDix communes au plus grand nombre d'arrêts scolaires :\n")
print(as.data.frame(par_commune %>% arrange(desc(arrets_scolaires)) %>%
  slice_head(n = 10) %>%
  transmute(COMMUNE, POPULATION, AGE_0_19, arrets_scolaires)))

# ── 3. T-015a : COUVERTURE SCOLAIRE ET POPULATION JEUNE ─────
# Variable principale : population de 0 à 19 ans, la plus proche de
# la population scolaire disponible dans le SITG. La population
# totale est donnée en comparaison. Toutes les communes entrent dans
# le calcul, y compris celles sans arrêt scolaire (valeur 0).

rho_boot <- function(x, y, B = 2000) {
  n <- length(x)
  replicate(B, {
    i <- sample.int(n, n, replace = TRUE)
    suppressWarnings(cor(x[i], y[i], method = "spearman"))
  })
}

calcul_rho <- function(var) {
  x <- par_commune[[var]]; y <- par_commune$arrets_scolaires
  rho <- suppressWarnings(cor(x, y, method = "spearman"))
  b   <- rho_boot(x, y)
  ic  <- quantile(b, c(0.025, 0.975), na.rm = TRUE)
  list(rho = round(rho, 3), inf = round(ic[1], 3), sup = round(ic[2], 3))
}

r_jeunes <- calcul_rho("AGE_0_19")
r_total  <- calcul_rho("POPULATION")
n_zero   <- sum(par_commune$arrets_scolaires == 0)

cat("\n=== T-015a : ARRÊTS SCOLAIRES ET POPULATION PAR COMMUNE ===\n")
cat("Communes :", nrow(par_commune), "dont", n_zero, "sans arrêt scolaire (ex aequo à 0)\n")
cat("Population de 0 à 19 ans : rho =", r_jeunes$rho,
    "| IC 95% bootstrap [", r_jeunes$inf, ";", r_jeunes$sup, "]\n")
cat("Population totale        : rho =", r_total$rho,
    "| IC 95% bootstrap [", r_total$inf, ";", r_total$sup, "]\n")
cat("\nLecture : association positive et modérée. Les communes où vivent\n")
cat("plus de jeunes reçoivent en général plus d'arrêts scolaires, avec\n")
cat("de nombreuses exceptions. Les données ne disent pas où sont les\n")
cat("établissements scolaires, qui déterminent les trajets réels :\n")
cat("on mesure une couverture, pas une adéquation à la demande.\n")

enregistrer(
  test_id = "T-015a", script = "15_sitg_scolaire_socioeco.R",
  methode = "Spearman, arrêts scolaires par commune et population de 0 à 19 ans, IC bootstrap 2000 tirages",
  n = nrow(par_commune), statistique = NA, p_value = NA,
  effet_nom = "Spearman rho", effet = r_jeunes$rho,
  ic_inf = r_jeunes$inf, ic_sup = r_jeunes$sup,
  note = paste0("Population totale : rho = ", r_total$rho, " [", r_total$inf, " ; ",
                r_total$sup, "]. ", n_zero, " communes sans arrêt scolaire. ",
                "Population SITG ", DATE_REF_SITG, ", lignes scolaires ",
                format(DEBUT_FENETRE), " à ", format(DATE_COUPURE),
                ". r calculé depuis la p-value retiré (S-27).")
)

# ── 4. FREQUENTATION DES LIGNES SCOLAIRES ───────────────────

freq <- scolaire %>%
  group_by(ligne) %>%
  summarise(montees = sum(nb_de_montees, na.rm = TRUE),
            jours_de_service = n_distinct(date),
            arrets = n_distinct(arret_code_long), .groups = "drop") %>%
  mutate(montees_par_jour = round(montees / jours_de_service)) %>%
  arrange(desc(montees))

total_reseau <- lire("journalier") %>%
  filter(date >= DEBUT_FENETRE) %>%
  summarise(t = sum(nb_de_montees, na.rm = TRUE)) %>%
  pull(t)
part <- round(100 * sum(freq$montees) / total_reseau, 2)

cat("\n=== FRÉQUENTATION DES LIGNES SCOLAIRES (12 derniers mois) ===\n")
print(as.data.frame(freq %>% mutate(montees = round(montees))))
cat("\nPart des lignes scolaires dans les montées du réseau :", part, "%\n")
cat("Ces lignes ne circulent que les jours d'école et visent un public\n")
cat("précis : leur volume ne se compare pas à celui des lignes régulières.\n")

# ── 5. NIVEAU SOCIO-ÉCONOMIQUE ──────────────────────────────
# Les couches OCS_POPBATLOG décrivent le bâti et la population
# résidante (âge, sexe, nationalité), pas le revenu. La nationalité
# n'est pas un indicateur de revenu et n'est pas utilisée comme tel.
# Aucune conclusion socio-économique n'est donc tirée ici. Un
# croisement avec une statistique communale du revenu (par exemple
# celle de l'administration fiscale ou de l'OFS, à identifier et
# vérifier) serait nécessaire.

cat("\n=== NIVEAU SOCIO-ÉCONOMIQUE ===\n")
cat("Colonnes disponibles dans la couche communes :\n")
cat(paste(setdiff(names(communes), "geometry"), collapse = ", "), "\n")
cat("Aucune ne mesure le revenu. Pas d'analyse socio-économique possible\n")
cat("avec ces données ; voir la note en tête de section.\n")

# ── 6. FIGURES ──────────────────────────────────────────────
# Carte : communes_carte_terre (lac retire, voir section 1). Nuage
# de points : par_commune, indicateurs non affectes par le decoupage.

carte <- communes_carte_terre %>% left_join(par_commune %>% select(COMMUNE, arrets_scolaires), by = "COMMUNE")

p_carte <- ggplot() +
  geom_sf(data = carte, aes(fill = arrets_scolaires), color = "white", linewidth = 0.2) +
  geom_sf(data = arrets_sco, color = COL_REF, size = 0.4, alpha = 0.6) +
  scale_fill_gradient(low = "#F2F2F2", high = ROUGE_PRINCIPAL, name = "Arrêts\nscolaires") +
  labs(title = "Arrêts des lignes scolaires par commune",
       subtitle = paste0("Lignes ", paste(sort(unique(scolaire$ligne)), collapse = ", "),
                         ", ", format(DEBUT_FENETRE, "%m.%Y"), " à ",
                         format(DATE_COUPURE, "%m.%Y"), ". Points : arrêts."),
       caption = paste(SOURCE_TPG, SOURCE_SITG,
                       paste0("Population au ", DATE_REF_SITG, "."),
                       sep = "\n")) +
  theme_projet() +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(), axis.title = element_blank(),
        panel.grid = element_blank())

print(p_carte)
ggsave(file.path(DIR_FIG, "15_carte_scolaire.png"), p_carte, width = 10, height = 8, dpi = 150)

p_nuage <- ggplot(par_commune, aes(x = AGE_0_19, y = arrets_scolaires)) +
  geom_point(color = ROUGE_PRINCIPAL, size = 2.5, alpha = 0.8) +
  scale_x_log10(labels = label_number(big.mark = " ")) +
  labs(title = "Arrêts scolaires et population de 0 à 19 ans, par commune",
       subtitle = paste0("Une commune par point. Spearman rho = ", r_jeunes$rho,
                         ", IC 95% [", r_jeunes$inf, " ; ", r_jeunes$sup, "].\n",
                         n_zero, " communes sans arrêt scolaire, sur l'axe du bas."),
       x = "Habitants de 0 à 19 ans (échelle logarithmique)",
       y = "Arrêts des lignes scolaires",
       caption = paste(SOURCE_TPG, SOURCE_SITG,
                       paste0("Population au ", DATE_REF_SITG, "."),
                       sep = "\n")) +
  theme_projet()

print(p_nuage)
ggsave(file.path(DIR_FIG, "15_scolaire_population.png"), p_nuage, width = 10, height = 7, dpi = 150)
message("Figures enregistrées.")

# ── 7. SAUVEGARDE ───────────────────────────────────────────

write.csv(par_commune, file.path(DIR_RES, paste0("15_scolaire_communes_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(freq,        file.path(DIR_RES, paste0("15_scolaire_lignes_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)

message("Script 15 terminé. Figures dans figures/, résultats dans resultats/.")
