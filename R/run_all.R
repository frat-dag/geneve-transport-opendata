# ============================================================
# RUN_ALL - RELANCE COMPLÈTE DE LA CHAÎNE D'ANALYSE
# Auteur : Frat DAG
# ------------------------------------------------------------
# Lance chaque script dans un processus R neuf, dans l'ordre.
# Un script qui dépendrait d'un objet laissé en mémoire par un
# autre échoue ici : c'est le test d'autonomie de la chaîne.
#
# À lancer DEPUIS RSTUDIO (bouton Source) : les cartes HTML ont
# besoin de pandoc, que RStudio fournit aux processus qu'il lance.
#
# Ne relance PAS 00_download.R : le snapshot est figé.
#
# Sorties :
#   logs/<script>.log   sortie complète de chaque script
#   tableau récapitulatif et contrôle des fichiers attendus
# ============================================================

source(here::here("R", "config.R"))

SCRIPTS <- c(
  "00b_verification_definitif.R",
  "01_arrets_import.R",
  "02_frequentation_import.R",
  "03_evolution_temporelle.R",
  "04_heatmap_horaire.R",
  "05_profil_journalier.R",
  "06_saisonnalite.R",
  "07_impact_covid.R",
  "08_collisions.R",
  "09_tests_statistiques.R",
  "10_efficience_lignes.R",
  "11_collisions_spatial.R",
  "12_sitg_equite_territoriale.R",
  "13_offre_demande.R",
  "14_noctambus_ligne10.R",
  "15_sitg_scolaire_socioeco.R"
)

FIGURES_ATTENDUES <- c(
  "01_carte_arrets.html", "01_carte_arrets.png",
  "02_top10_arrets.png", "02_top10_lignes.png",
  "03_evolution_temporelle.png", "03_evolution_par_type.png",
  "03_recuperation_par_type.png",
  "04_heatmap_horaire.png",
  "05_profil_journalier.png", "05_ecart_mercredi.png",
  "06_stl_decomposition.png", "06_saison_mensuelle.png",
  "07_ruptures_structurelles.png",
  "08_collisions_annuel.png", "08_collisions_horaire.png", "08_carte_collisions.html", "08_carte_collisions.png",
  "09_lorenz_arrets.png", "09_offre_frequentation.png", "09_scolaire_perimetre.png",
  "10_efficience_boxplot.png", "10_efficience_taille.png",
  "11_blesses_saison.png", "11_collisions_exposition.png",
  "12_densite_couverture.png", "12_carte_couverture.png",
  "13_ratio_par_mode.png", "13_ratio_temporel.png",
  "14_noctambus_regional.png", "14_nuit_1h_4h.png",
  "15_carte_scolaire.png", "15_scolaire_population.png"
)

RESULTATS_ATTENDUS <- paste0(c(
  "02_parts_par_type", "02_top10_arrets", "02_top10_lignes",
  "03_global_mensuel", "03_recuperation",
  "05_T001_dunn", "05_T001_resume_jours", "05_T005b_matrice",
  "05_T005_global", "05_T005_intrasemaine",
  "06_profil_saisonnier", "06_stabilite_saisonnalite",
  "07_ruptures_comparaison", "07_T002_resume", "07_trajectoire_annuelle",
  "08_collisions_annuel", "08_collisions_horaire",
  "09_AM002_perimetre", "09_T008_lignes", "09_T009_gratuite", "09_T009_par_type",
  "10_efficience_lignes", "10_resume_groupes",
  "11_collisions_exposition", "11_collisions_par_arret", "11_T011_saison",
  "12_communes", "12_secteurs",
  "13_ratio_mensuel", "13_ratio_par_ligne", "13_resume_modes",
  "14_noctambus_annuel", "14_nuit_par_jour", "14_nuit_par_mois", "14_principal_reprise",
  "14_principal_vacances",
  "15_scolaire_communes", "15_scolaire_lignes",
  "recoupement_ligne", "recoupement_mois",
  "resultats", "verification_definitif"
), "_", SNAPSHOT_ID, ".csv")

# ── EXECUTION ───────────────────────────────────────────────

rscript <- file.path(R.home("bin"), ifelse(.Platform$OS.type == "windows",
                                           "Rscript.exe", "Rscript"))
dir_logs <- here::here("logs")
dir.create(dir_logs, showWarnings = FALSE)

debut_global <- Sys.time()
cat("Relance complète, snapshot", SNAPSHOT_ID, "| début",
    format(debut_global, "%H:%M:%S"), "\n\n")

bilan <- data.frame()
for (s in SCRIPTS) {
  chemin <- here::here("R", s)
  journal <- file.path(dir_logs, sub("\\.R$", ".log", s))
  t0 <- Sys.time()
  cat(sprintf("%-34s ... ", s)); flush.console()
  if (!file.exists(chemin)) {
    code <- NA
  } else {
    code <- system2(rscript, shQuote(chemin), stdout = journal, stderr = journal)
  }
  duree <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  statut <- ifelse(is.na(code), "ABSENT", ifelse(code == 0, "OK", "ERREUR"))
  cat(statut, "(", duree, "s )\n")
  bilan <- rbind(bilan, data.frame(script = s, statut = statut, duree_s = duree))
}

cat("\n=== BILAN DES SCRIPTS ===\n")
print(bilan, row.names = FALSE)
cat("Durée totale :",
    round(as.numeric(difftime(Sys.time(), debut_global, units = "mins")), 1), "minutes\n")

# ── CONTRÔLE DES FICHIERS PRODUITS ──────────────────────────
# Un fichier compte seulement s'il a été écrit pendant CETTE
# exécution : un fichier plus ancien du même nom ne suffit pas.

controler <- function(dossier, attendus, etiquette) {
  chemins <- file.path(dossier, attendus)
  existe  <- file.exists(chemins)
  recent  <- existe & file.info(chemins)$mtime >= debut_global
  cat("\n===", etiquette, "===\n")
  cat("Attendus :", length(attendus), "| produits par cette exécution :", sum(recent), "\n")
  if (any(!existe)) cat("ABSENTS :", paste(attendus[!existe], collapse = ", "), "\n")
  if (any(existe & !recent))
    cat("ANCIENS, NON RÉÉCRITS :", paste(attendus[existe & !recent], collapse = ", "), "\n")
  presents <- list.files(dossier, pattern = "\\.(png|html|csv)$")
  en_trop  <- setdiff(presents, attendus)
  if (length(en_trop) > 0) cat("NON ATTENDUS DANS LE DOSSIER :", paste(en_trop, collapse = ", "), "\n")
  invisible(all(recent))
}

ok_fig <- controler(DIR_FIG, FIGURES_ATTENDUES, "FIGURES")
ok_res <- controler(DIR_RES, RESULTATS_ATTENDUS, "RESULTATS")

cat("\n=== CONCLUSION ===\n")
if (all(bilan$statut == "OK") && ok_fig && ok_res) {
  cat("Chaîne complète : tous les scripts ont tourné et toutes les sorties\n")
  cat("attendues ont été produites par cette exécution.\n")
} else {
  cat("Chaîne incomplète : voir les lignes ci-dessus et les journaux dans logs/.\n")
}
