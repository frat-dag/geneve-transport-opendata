# ============================================================
# R/00b_verification_definitif.R
# VÉRIFICATION DE L'ÉTAT "DÉFINITIF" DU SNAPSHOT
# ------------------------------------------------------------
# Rôle : documenter, de façon reproductible et datée, jusqu'à quel
# mois chaque dataset du snapshot courant est marqué définitif
# par les tpg (colonne donnees_definitives).
# Sert de justification tracée à DATE_COUPURE (config.R) et de
# garde-fou : si un futur snapshot change ce schéma, ce script
# le signale au lieu de laisser une DATE_COUPURE obsolète passer
# inaperçue.
#
# À RELANCER À CHAQUE NOUVEAU SNAPSHOT (octobre 2026, novembre...),
# avant toute analyse, pour confirmer ou ajuster DATE_COUPURE.
# Produit : resultats/verification_definitif_<SNAPSHOT_ID>.csv
# ============================================================

source(here::here("R", "config.R"))

datasets <- c("journalier", "mensuel", "horaire", "mn", "km_prod", "collisions")

rapport <- list()

for (nom in datasets) {
  f <- file.path(DIR_RAW, paste0(nom, ".rds"))
  if (!file.exists(f)) {
    message(nom, " : fichier absent, ignoré.")
    next
  }
  df <- readRDS(f)

  col_date <- if ("date" %in% names(df)) "date" else if ("mois" %in% names(df)) "mois" else NA

  if (is.na(col_date)) {
    message(nom, " : aucune colonne date/mois reconnue, ignoré.")
    next
  }

  if (!"donnees_definitives" %in% names(df)) {
    rapport[[nom]] <- data.frame(dataset = nom, mois = NA,
                                 n_definitif = NA, n_non_definitif = NA,
                                 note = "pas de colonne donnees_definitives")
    next
  }

  mois_vec <- if (col_date == "date") format(df$date, "%Y-%m") else as.character(df$mois)
  tab    <- table(mois_vec, df$donnees_definitives)
  df_tab <- as.data.frame.matrix(tab)
  if (!"TRUE"  %in% names(df_tab)) df_tab[["TRUE"]]  <- 0
  if (!"FALSE" %in% names(df_tab)) df_tab[["FALSE"]] <- 0

  rapport[[nom]] <- data.frame(
    dataset         = nom,
    mois            = rownames(df_tab),
    n_definitif     = df_tab[["TRUE"]],
    n_non_definitif = df_tab[["FALSE"]],
    note            = ""
  )
}

rapport <- do.call(rbind, rapport)
rownames(rapport) <- NULL

# ── Dernier mois 100% définitif, par dataset ────────────────
complet <- subset(rapport, n_non_definitif == 0 & n_definitif > 0 & !is.na(mois))
dernier_mois_ok <- aggregate(mois ~ dataset, data = complet, FUN = max)

cat("Snapshot vérifié :", SNAPSHOT_ID, "\n\n")
cat("Dernier mois entièrement marqué 'définitif', par dataset :\n")
print(dernier_mois_ok)

cat("\nDATE_COUPURE actuelle dans config.R :", format(DATE_COUPURE), "\n")
mois_coupure <- format(DATE_COUPURE, "%Y-%m")
incoherents  <- dernier_mois_ok$dataset[dernier_mois_ok$mois != mois_coupure]

if (length(incoherents) > 0) {
  cat("\n\u26a0 DIVERGENCE : les datasets suivants ne s'accordent pas avec DATE_COUPURE (",
      mois_coupure, ") :\n", sep = "")
  print(dernier_mois_ok[dernier_mois_ok$dataset %in% incoherents, ])
  cat("À examiner avant de faire confiance aux résultats.\n")
} else {
  cat("\nOK : tous les datasets datés sont cohérents avec DATE_COUPURE.\n")
}

sans_colonne <- unique(rapport$dataset[is.na(rapport$mois)])
if (length(sans_colonne) > 0) {
  cat("\nDatasets sans colonne donnees_definitives (filtrage par date seule) :",
      paste(sans_colonne, collapse = ", "), "\n")
}

# ── Sauvegarde datée, versionnable, en annexe du travail ────
f_out <- file.path(DIR_RES, paste0("verification_definitif_", SNAPSHOT_ID, ".csv"))
write.csv(rapport, f_out, row.names = FALSE)
cat("\nRapport détaillé enregistré :", f_out, "\n")
