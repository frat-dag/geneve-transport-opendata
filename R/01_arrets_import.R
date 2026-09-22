# ============================================================
# SCRIPT 01 : IMPORT ET NETTOYAGE DES ARRÊTS
# Auteur : Frat DAG
# Corrections appliquées : R-01, R-02, R-09, R-10, Q-05 (voir CORRECTIONS.md)
# ------------------------------------------------------------
# OBJECTIF : charger le référentiel des arrêts, le nettoyer,
# produire la carte de référence et le fichier arrets_clean.rds
# utilisé par les scripts suivants.
# RÈGLE : aucun effectif n'est écrit en dur. Tous les nombres
# affichés, y compris dans la légende de la carte, sont calculés.
# ============================================================

source(here::here("R", "config.R"))


library(dplyr)
library(tidyr)
library(leaflet)
library(htmlwidgets)
library(webshot2)

# ── 1. CHARGEMENT ───────────────────────────────────────────
# Le référentiel des arrêts n'a pas de colonne de date : lecture
# directe du snapshot, sans passer par lire().

arrets <- readRDS(file.path(DIR_RAW, "arrets.rds"))
cat("Snapshot   :", SNAPSHOT_ID, "\n")
cat("Dimensions :", nrow(arrets), "lignes x", ncol(arrets), "colonnes\n")
cat("Codes d'arrêt en double :", sum(duplicated(arrets$arretcodelong)), "\n")

# ── 2. COORDONNÉES ──────────────────────────────────────────
# "46.220238, 6.226213" -> latitude + longitude numériques

arrets_coord <- arrets %>%
  separate(coordonnees, into = c("latitude", "longitude"),
           sep = ", ", convert = TRUE)

stopifnot(is.numeric(arrets_coord$latitude), is.numeric(arrets_coord$longitude))

# ── 3. INVENTAIRE AVANT EXCLUSION ───────────────────────────
# Ne jamais exclure sans avoir d'abord compté et caractérisé.

sans_coords <- arrets_coord %>% filter(is.na(latitude))

cat("\n--- Arrêts sans coordonnées ---\n")
cat("Total         :", nrow(sans_coords), "\n")
cat("Dont actifs   :", sum(sans_coords$actif == "Y"), "\n")
cat("Dont inactifs :", sum(sans_coords$actif == "N"), "\n")

# L'exclusion n'est sans impact que si aucun arrêt actif n'est concerné.
# On le vérifie au lieu de le supposer.
if (sum(sans_coords$actif == "Y") > 0) {
  warning("Des arrêts ACTIFS n'ont pas de coordonnées : exclusion à réexaminer.")
  print(sans_coords %>% filter(actif == "Y") %>%
          select(arretcodelong, nomarret, commune))
}

arrets_clean <- arrets_coord %>% filter(!is.na(latitude))

cat("\n--- Dataset nettoyé ---\n")
cat("Arrêts géolocalisés :", nrow(arrets_clean), "\n")
cat("Exclus              :", nrow(sans_coords), "\n")
cat("Actifs              :", sum(arrets_clean$actif == "Y"), "\n")
cat("Inactifs            :", sum(arrets_clean$actif == "N"), "\n")
cat("Sans nom            :", sum(is.na(arrets_clean$nomarret)), "\n")

# ── 4. ARRÊTS SANS CODEDIDOC ────────────────────────────────

sans_didoc <- arrets_clean %>% filter(is.na(codedidoc))

cat("\n--- Arrêts sans codedidoc ---\n")
cat("Total         :", nrow(sans_didoc), "\n")
cat("Dont actifs   :", sum(sans_didoc$actif == "Y"), "\n")
cat("Dont inactifs :", sum(sans_didoc$actif == "N"), "\n")

# Seuls les actifs comptent pour la suite. Classement par règle explicite :
#   douane : code commençant par "_"
#   dépôt  : nom commençant par "Dépôt"
#   autre  : tout le reste, à examiner un par un s'il y en a
actifs_sans_didoc <- sans_didoc %>%
  filter(actif == "Y") %>%
  mutate(categorie = case_when(
    grepl("^_", arretcodelong)  ~ "douane",
    grepl("^D.p.t ", nomarret)  ~ "depot",   # "Dépôt", écrit sans accent : robuste à l'encodage
    TRUE                        ~ "autre"
  )) %>%
  select(arretcodelong, nomarret, commune, pays, categorie)

cat("\n--- Arrêts ACTIFS sans codedidoc, par catégorie ---\n")
print(table(actifs_sans_didoc$categorie))

cat("\nDouanes par pays :\n")
print(table(actifs_sans_didoc$pays[actifs_sans_didoc$categorie == "douane"]))

cat("\nDépôts :\n")
print(actifs_sans_didoc$nomarret[actifs_sans_didoc$categorie == "depot"])

n_autres <- sum(actifs_sans_didoc$categorie == "autre")
if (n_autres > 0) {
  warning(n_autres, " arrêt(s) actif(s) sans codedidoc hors douanes et dépôts : à examiner.")
  print(actifs_sans_didoc %>% filter(categorie == "autre"))
}

# ── 5. CARTE ────────────────────────────────────────────────

actifs   <- arrets_clean %>% filter(actif == "Y")
inactifs <- arrets_clean %>% filter(actif == "N")

lab_actifs   <- paste0("Arrêts actifs (",   format(nrow(actifs),   big.mark = " "), ")")
lab_inactifs <- paste0("Arrêts inactifs (", format(nrow(inactifs), big.mark = " "), ")")

carte_arrets <- leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = actifs, lng = ~longitude, lat = ~latitude,
    radius = 4, color = "#B23A48", fillColor = "#B23A48",
    fillOpacity = 0.8, weight = 1,
    popup = ~paste0("<b>", nomarret, "</b><br>Commune : ", commune,
                    "<br>Pays : ", pays, "<br>Code : ", arretcodelong),
    group = lab_actifs
  ) %>%
  addCircleMarkers(
    data = inactifs, lng = ~longitude, lat = ~latitude,
    radius = 3, color = "#4A6FA5", fillColor = "#4A6FA5",
    fillOpacity = 0.5, weight = 1,
    popup = ~paste0("<b>", nomarret, "</b><br>Commune : ", commune,
                    "<br>Pays : ", pays, "<br>Code : ", arretcodelong,
                    "<br><i>Arrêt inactif</i>"),
    group = lab_inactifs
  ) %>%
  addLayersControl(overlayGroups = c(lab_actifs, lab_inactifs),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addLegend(position = "bottomright",
            colors = c("#B23A48", "#4A6FA5"),
            labels = c(lab_actifs, lab_inactifs),
            title  = paste0("Arrêts, état au ",
                            format(as.Date(SNAPSHOT_ID), "%d.%m.%Y")),
            opacity = 0.8) %>%
  addControl(html = paste0("<small>", SOURCE_TPG, "</small>"),
             position = "bottomleft")

saveWidget(carte_arrets, file.path(DIR_FIG, "01_carte_arrets.html"),
           selfcontained = TRUE)

# Q-05 : GitHub n'affiche pas les widgets HTML interactifs. Doublon
# statique (capture du widget) pour la consultation directe sur
# GitHub, où le HTML self-contained n'est vu que comme du texte.
webshot2::webshot(file.path(DIR_FIG, "01_carte_arrets.html"),
                  file = file.path(DIR_FIG, "01_carte_arrets.png"),
                  vwidth = 1100, vheight = 850, delay = 1)
message("Doublon PNG de la carte enregistré (Q-05).")

# ── 6. SAUVEGARDE ───────────────────────────────────────────
# arrets_clean est relu par les scripts suivants : plus aucun
# script ne dépend d'un objet laissé en mémoire par un autre.

saveRDS(arrets_clean, file.path(DIR_PROC, "arrets_clean.rds"))
message("Script 01 terminé. Fichiers : ",
        file.path(DIR_PROC, "arrets_clean.rds"), " ; ",
        file.path(DIR_FIG, "01_carte_arrets.html"), " ; ",
        file.path(DIR_FIG, "01_carte_arrets.png"))
