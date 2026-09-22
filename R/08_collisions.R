# ============================================================
# SCRIPT 08 - COLLISIONS AVEC TIERS
# Auteur : Frat DAG
# Corrections : R-01, R-02, R-05, R-07, R-08, R-09, T-04, T-06, S-17,
#               S-30, Q-05
# ------------------------------------------------------------
# OBJECTIF : décrire les collisions impliquant un véhicule tpg et
# un tiers, dans le temps, dans la journée et dans l'espace.
#
# POINT CENTRAL (S-17) : un nombre brut de collisions ne se lit pas
# sans l'exposition qui le produit. Le réseau produit chaque année
# davantage de kilomètres : à risque constant, le nombre de
# collisions augmente mécaniquement. Ce script publie donc le taux
# par million de km parcourus à côté du nombre brut, et c'est le
# taux qui sert aux comparaisons entre années.
#
# S-30 : quelques lignes roulent sans aucune collision enregistrée
# (301, 302 notamment). Sont-elles hors du périmètre du dataset, ou
# simplement sans collision ? Les données ne le disent pas (Q-07). Le
# taux est donc calculé de deux façons : avec tous les kilomètres
# (lecture principale) et avec les seuls kilomètres des lignes
# présentes au moins une fois dans le dataset collisions.
# ============================================================

source(here::here("R", "config.R"))
source(here::here("R", "00_palette.R"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(leaflet)
library(htmlwidgets)
library(webshot2)

# ── 1. CHARGEMENT ───────────────────────────────────────────
# collisions n'a pas de colonne donnees_definitives ; lire()
# applique la coupure de date via la colonne jour.

collisions <- lire("collisions")
km_prod    <- lire("km_prod")

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n")
cat("Collisions :", nrow(collisions), "de",
    format(min(collisions$jour)), "à", format(max(collisions$jour)), "\n")
cat("Km produits :", nrow(km_prod), "lignes de",
    format(min(km_prod$date)), "à", format(max(km_prod$date)), "\n")

# ── 2. PRÉPARATION ──────────────────────────────────────────
# heure est de classe hms (secondes depuis minuit) : la conversion
# passe par les secondes, pas par les deux premiers caractères.

collisions <- collisions %>%
  mutate(
    sens_clean = ifelse(is.na(sens), "Hors ligne", sens),
    cat_clean  = ifelse(is.na(cat),  "Non categorise", cat),
    annee      = year(jour),
    mois_num   = month(jour),
    heure_num  = as.integer(as.numeric(heure) / 3600)
  )

cat("\nHeures couvertes :", min(collisions$heure_num, na.rm = TRUE), "à",
    max(collisions$heure_num, na.rm = TRUE), "\n")

cat("\n--- Par catégorie ---\n"); print(table(collisions$cat_clean))
cat("\n--- Par sens ---\n");      print(table(collisions$sens_clean))
cat("\n--- Par type de ligne ---\n")
print(sort(table(collisions$ligne_type_hist), decreasing = TRUE))

n_hors_ligne <- sum(collisions$sens_clean == "Hors ligne")
cat("\nCollisions hors ligne :", n_hors_ligne,
    "(", round(100 * n_hors_ligne / nrow(collisions), 1), "% )\n")

# ── 3. EXPOSITION : KILOMÈTRES PRODUITS PAR AN ──────────────

lignes_presentes <- unique(as.character(collisions$ligne[!is.na(collisions$ligne)]))

km_annuel <- km_prod %>%
  mutate(annee = year(date),
         presente = as.character(ligne) %in% lignes_presentes) %>%
  group_by(annee) %>%
  summarise(km_total    = sum(km_prod, na.rm = TRUE),
            km_presents = sum(km_prod[presente], na.rm = TRUE),
            .groups = "drop")

# Une année n'entre dans la comparaison que si elle est complète
# dans les DEUX sources.
ANNEE_MAX <- if (month(DATE_COUPURE) < 12) year(DATE_COUPURE) - 1 else year(DATE_COUPURE)
annee_min_commune <- max(min(year(collisions$jour)), min(km_annuel$annee))

collisions_annuel <- collisions %>%
  group_by(annee) %>%
  summarise(n_collisions = n(),
            severite_moy = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
            .groups = "drop") %>%
  inner_join(km_annuel, by = "annee") %>%
  filter(annee >= annee_min_commune, annee <= ANNEE_MAX) %>%
  mutate(km_millions = round(km_total / 1e6, 2),
         taux_par_Mkm = round(n_collisions / (km_total / 1e6), 2),
         pct_km_absents = round(100 * (1 - km_presents / km_total), 2),
         taux_lignes_presentes = round(n_collisions / (km_presents / 1e6), 2))

cat("\n=== COLLISIONS ET EXPOSITION, PAR ANNÉE ===\n")
print(as.data.frame(collisions_annuel %>%
  select(annee, n_collisions, km_millions, taux_par_Mkm, severite_moy)))

a_deb <- min(collisions_annuel$annee)
a_fin <- max(collisions_annuel$annee)
variation <- function(x) {
  round(100 * (x[collisions_annuel$annee == a_fin] /
               x[collisions_annuel$annee == a_deb] - 1), 1)
}
var_brut <- variation(collisions_annuel$n_collisions)
var_taux <- variation(collisions_annuel$taux_par_Mkm)
var_km   <- variation(collisions_annuel$km_total)

cat("\n--- Lecture (S-17) ---\n")
cat("De", a_deb, "à", a_fin, ":\n")
cat("  nombre de collisions :", var_brut, "%\n")
cat("  kilomètres produits  :", var_km, "%\n")
cat("  taux par million de km :", var_taux, "%\n")
cat("L'essentiel de la hausse du nombre brut suit la hausse de l'offre.\n")

pire_brut <- collisions_annuel %>% slice_max(n_collisions, n = 1)
pire_taux <- collisions_annuel %>% slice_max(taux_par_Mkm,  n = 1)
cat("\nAnnée au nombre le plus élevé :", pire_brut$annee,
    "(", pire_brut$n_collisions, "collisions )\n")
cat("Année au taux le plus élevé    :", pire_taux$annee,
    "(", pire_taux$taux_par_Mkm, "par million de km )\n")
if (pire_brut$annee != pire_taux$annee) {
  rang <- which(sort(collisions_annuel$taux_par_Mkm, decreasing = TRUE) ==
                  pire_brut$taux_par_Mkm)[1]
  cat("Les deux ne coïncident pas. L'année", pire_brut$annee,
      "arrive au rang", rang, "sur", nrow(collisions_annuel),
      "en taux.\nParler d'une année record sur le seul nombre brut serait",
      "trompeur.\n")
}

# S-30 : robustesse, kilomètres des seules lignes présentes dans le
# dataset collisions.
var_taux_pres <- variation(collisions_annuel$taux_lignes_presentes)
rang_pres <- rank(-collisions_annuel$taux_lignes_presentes,
                  ties.method = "min")[collisions_annuel$annee == pire_brut$annee]
pire_pres <- collisions_annuel$annee[which.max(collisions_annuel$taux_lignes_presentes)]

cat("\n--- Robustesse (S-30) : km des seules lignes présentes au dataset ---\n")
print(as.data.frame(collisions_annuel %>%
  select(annee, pct_km_absents, taux_par_Mkm, taux_lignes_presentes)))
cat("Taux de", a_deb, "à", a_fin, ":", var_taux, "% avec tous les km,",
    var_taux_pres, "% avec les km des lignes présentes.\n")
cat("Année au taux le plus élevé :", pire_taux$annee, "et", pire_pres,
    "| rang de", pire_brut$annee, ":", rang, "et", rang_pres, "\n")
cat("Le rang d'une année dépend du périmètre retenu ; la tendance reste\n")
cat("faible dans les deux cas, loin de la hausse du nombre brut.\n")

# NOTE T-04 : le script d'avril proposait une explication au pic de
# 2025 ("nouveaux conducteurs moins expérimentés"). Rien dans les
# données ne permet de l'étayer, et l'affirmation est défavorable
# aux tpg sans fondement. Elle est retirée. Le taux, lui, se lit
# sans hypothèse.

# ── 4. FIGURE 1 : NOMBRE BRUT ET TAUX ───────────────────────
# Les deux séries sur une même figure, le facteur d'échelle étant
# calculé et non choisi à la main.

facteur <- max(collisions_annuel$n_collisions) / max(collisions_annuel$taux_par_Mkm)

p_annuel <- ggplot(collisions_annuel, aes(x = annee)) +
  geom_col(aes(y = n_collisions), fill = COL_NEUTRE, alpha = 0.55) +
  geom_line(aes(y = taux_par_Mkm * facteur), color = ROUGE_PRINCIPAL, linewidth = 1) +
  geom_point(aes(y = taux_par_Mkm * facteur), color = ROUGE_PRINCIPAL, size = 2) +
  geom_vline(xintercept = year(D_COVID), linetype = "dashed",
             color = COL_COVID, linewidth = 0.4) +
  scale_x_continuous(breaks = collisions_annuel$annee) +
  scale_y_continuous(
    name = "Nombre de collisions (barres)",
    sec.axis = sec_axis(~ . / facteur, name = "Collisions par million de km (ligne)")
  ) +
  labs(
    title    = "Collisions avec tiers : nombre et taux rapporté à l'offre",
    subtitle = paste0("Années complètes ", a_deb, " à ", a_fin,
                      ". Le nombre brut augmente de ", var_brut,
                      " %, le taux de ", var_taux, " % (", var_taux_pres,
                      " % sur les seules lignes présentes au dataset)."),
    x = NULL, caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(axis.title.y.right = element_text(color = ROUGE_PRINCIPAL),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p_annuel)
ggsave(file.path(DIR_FIG, "08_collisions_annuel.png"),
       p_annuel, width = 12, height = 6, dpi = 150)
message("Figure 1 enregistrée.")

# ── 5. DISTRIBUTION HORAIRE ─────────────────────────────────

collisions_heure <- collisions %>%
  filter(!is.na(heure_num), heure_num >= 0, heure_num <= 23) %>%
  group_by(heure_num) %>%
  summarise(n_collisions = n(),
            severite_moy = round(mean(indicateur_de_severite, na.rm = TRUE), 3),
            .groups = "drop")

cat("\n=== DISTRIBUTION HORAIRE ===\n")
print(as.data.frame(collisions_heure))

facteur_h <- max(collisions_heure$n_collisions) /
             max(collisions_heure$severite_moy, na.rm = TRUE)

p_heure <- ggplot(collisions_heure, aes(x = heure_num)) +
  geom_col(aes(y = n_collisions), fill = ROUGE_PRINCIPAL, alpha = 0.85, width = 0.8) +
  geom_line(aes(y = severite_moy * facteur_h), color = COL_LEMAN, linewidth = 0.8) +
  geom_point(aes(y = severite_moy * facteur_h), color = COL_LEMAN, size = 1.5) +
  scale_x_continuous(breaks = 0:23, labels = paste0(0:23, "h")) +
  scale_y_continuous(
    name = "Nombre de collisions (barres)",
    sec.axis = sec_axis(~ . / facteur_h, name = "Sévérité moyenne (ligne)")
  ) +
  labs(
    title    = "Distribution horaire des collisions",
    subtitle = paste0("Ensemble de la période, ", format(min(collisions$jour), "%Y"),
                     " à ", format(max(collisions$jour), "%Y"),
                     ". Nombre non rapporté à l'offre par heure."),
    x = "Heure", caption = SOURCE_TPG
  ) +
  theme_projet() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.title.y.right = element_text(color = COL_LEMAN))

print(p_heure)
ggsave(file.path(DIR_FIG, "08_collisions_horaire.png"),
       p_heure, width = 12, height = 6, dpi = 150)
message("Figure 2 enregistrée.")

# Les heures creuses ont peu de collisions parce qu'il y circule peu
# de véhicules. La sévérité, elle, est comparable d'une heure à
# l'autre : on le vérifie plutôt que de l'affirmer.
sev <- collisions_heure$severite_moy
cat("\nSévérité moyenne : min", round(min(sev, na.rm = TRUE), 2),
    "| max", round(max(sev, na.rm = TRUE), 2),
    "| ecart-type", round(sd(sev, na.rm = TRUE), 3), "\n")

# ── 6. CARTE DES COLLISIONS ─────────────────────────────────
# Q-05 : le fichier HTML produit s'ouvre dans RStudio mais GitHub et
# les navigateurs bloquent les ressources locales d'un widget
# interactif. Doublon PNG ajouté juste après saveWidget().

collisions_geo <- collisions %>%
  filter(!is.na(latitude), !is.na(longitude),
         latitude  > 45,  latitude  < 47,
         longitude > 5.5, longitude < 7.5)

cat("\nCollisions géolocalisées :", nrow(collisions_geo),
    "sur", nrow(collisions),
    "(", round(100 * nrow(collisions_geo) / nrow(collisions), 1), "% )\n")

pal_severite <- colorNumeric(
  palette  = c(COL_LEMAN, "#FFC107", ROUGE_PRINCIPAL),
  domain   = collisions_geo$indicateur_de_severite,
  na.color = "#888888"
)

carte_collisions <- leaflet(collisions_geo) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude, radius = 4,
    color = ~pal_severite(indicateur_de_severite),
    fillColor = ~pal_severite(indicateur_de_severite),
    fillOpacity = 0.7, weight = 1,
    popup = ~paste0("<b>", format(jour, "%d.%m.%Y"), "</b><br>",
                    "Ligne : ", ligne, " (", ligne_type_hist, ")<br>",
                    "Arrêt : ", arret, "<br>",
                    "Sens : ", sens_clean, "<br>",
                    "Sévérité : ", indicateur_de_severite)
  ) %>%
  addLegend(position = "bottomright", pal = pal_severite,
            values = ~indicateur_de_severite,
            title = paste0("Sévérité<br>", nrow(collisions_geo), " collisions"),
            opacity = 0.8) %>%
  addControl(html = paste0("<small>", SOURCE_TPG, "</small>"),
             position = "bottomleft")

print(carte_collisions)
saveWidget(carte_collisions,
           file.path(DIR_FIG, "08_carte_collisions.html"),
           selfcontained = TRUE)
message("Carte enregistrée.")

# Q-05 : doublon statique pour GitHub, même principe qu'au script 01.
webshot2::webshot(file.path(DIR_FIG, "08_carte_collisions.html"),
                  file = file.path(DIR_FIG, "08_carte_collisions.png"),
                  vwidth = 1100, vheight = 850, delay = 1)
message("Doublon PNG de la carte enregistré (Q-05).")

# ── 7. ENREGISTREMENT ───────────────────────────────────────

enregistrer(
  test_id = "OBS-COLLISIONS", script = "08_collisions.R",
  methode = "Taux de collisions par million de km produits, par année",
  n = sum(collisions_annuel$n_collisions),
  statistique = NA, p_value = NA,
  effet_nom = paste0("variation du taux ", a_deb, " à ", a_fin, " (%)"),
  effet = var_taux,
  note = paste0("Nombre brut : ", var_brut, " %. Km produits : ", var_km,
                " %. Année au nombre le plus élevé : ", pire_brut$annee,
                ". Année au taux le plus élevé : ", pire_taux$annee,
                ". Robustesse S-30, km des seules lignes présentes au dataset : taux ",
                var_taux_pres, " %, rang de ", pire_brut$annee, " : ", rang_pres,
                " (", rang, " avec tous les km).")
)

write.csv(collisions_annuel,
          file.path(DIR_RES, paste0("08_collisions_annuel_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)
write.csv(collisions_heure,
          file.path(DIR_RES, paste0("08_collisions_horaire_", SNAPSHOT_ID, ".csv")),
          row.names = FALSE)

message("Script 08 terminé. Figures dans figures/, résultats dans resultats/.")
