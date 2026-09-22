# ============================================================
# SCRIPT 00_download : GEL DES DONNÉES (SNAPSHOT DATÉ)
# Projet : analyse open data tpg
# ------------------------------------------------------------
# RÔLE : seul script du projet autorisé à contacter l'API.
# Il se lance UNE fois par snapshot, à la main. Il :
#   1. télécharge les CSV bruts dans data/raw/<SNAPSHOT_ID>/
#   2. compare le nombre de lignes reçu au compteur de l'API
#      (détecte un export tronqué, cf. les 1 750 000 lignes
#      "rondes" du journalier en avril)
#   3. écrit un manifeste : date UTC, lignes, SHA-256, état API
#   4. refuse d'écraser un snapshot existant
# Tous les autres scripts lisent data/raw/<SNAPSHOT_ID>/ via
# R/config.R et ne téléchargent jamais rien.
# ============================================================

library(httr2)
library(jsonlite)
library(readr)
library(digest)   # install.packages("digest") si absent
library(here)

# ── 1. PARAMÈTRES ───────────────────────────────────────────

SNAPSHOT_ID <- format(Sys.Date(), "%Y-%m-%d")     # ex. "2026-09-21"
BASE_URL    <- "https://opendata.tpg.ch/api/explore/v2.1/catalog/datasets"
DIR_SNAP    <- here("data", "raw", SNAPSHOT_ID)

DATASETS <- c(
  arrets     = "arrets",
  journalier = "montees-par-arret-par-ligne",
  mensuel    = "montees-mensuelles-par-arret-par-ligne",
  horaire    = "frequentation-journaliere-par-tranche-horaire",
  mn         = "mn_montees-par-arret-par-ligne-par-tranchehoraire",
  km_prod    = "kilometres-produits-journaliers-par-ligne",
  collisions = "collisions-tpg-avec-tiers"
)

# ── 2. GARDE-FOU : ne jamais écraser un snapshot ────────────

if (dir.exists(DIR_SNAP)) {
  stop("Le snapshot ", SNAPSHOT_ID, " existe déjà. ",
       "Un snapshot ne se modifie pas : supprime le dossier à la main ",
       "si tu veux vraiment le refaire.")
}
dir.create(DIR_SNAP, recursive = TRUE)
options(timeout = 3600)   # le journalier est volumineux

# ── 3. MÉTADONNÉES API (compteur de lignes, date de traitement)

lire_metas <- function(id) {
  m <- tryCatch(
    request(paste0(BASE_URL, "/", id)) |> req_perform() |>
      resp_body_json(),
    error = function(e) NULL
  )
  d <- m$metas$default
  list(
    records_count  = if (is.null(d$records_count))  NA else d$records_count,
    data_processed = if (is.null(d$data_processed)) NA else d$data_processed,
    modified       = if (is.null(d$modified))       NA else d$modified
  )
}

# ── 4. TÉLÉCHARGEMENT SUR DISQUE, PUIS LECTURE ──────────────
# On écrit d'abord le CSV brut (c'est lui la pièce d'archive),
# puis on le relit depuis le disque. Lire directement l'URL
# peut tronquer silencieusement en cas de coupure réseau.

manifeste <- list()

for (nom in names(DATASETS)) {
  id      <- DATASETS[[nom]]
  f_csv   <- file.path(DIR_SNAP, paste0(nom, ".csv"))
  url     <- paste0(BASE_URL, "/", id,
                    "/exports/csv?delimiter=%3B&timezone=Europe%2FZurich")

  message("Téléchargement : ", id)
  metas <- lire_metas(id)
  t_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  download.file(url, f_csv, mode = "wb", quiet = TRUE)

  # na = "" : seule une cellule vide compte comme manquante. Avec le
  # reglage par defaut, le nom de la ligne Noctambus "NA" devient une
  # valeur manquante et la ligne disparait des analyses (F-07).
  df <- read_delim(f_csv, delim = ";", show_col_types = FALSE,
                   locale = locale(encoding = "UTF-8", tz = "Europe/Zurich"),
                   guess_max = 100000, na = "")
  saveRDS(df, file.path(DIR_SNAP, paste0(nom, ".rds")))

  complet <- is.na(metas$records_count) || nrow(df) == metas$records_count
  if (!complet) {
    warning(nom, " : ", nrow(df), " lignes reçues pour ",
            metas$records_count, " annoncées par l'API : EXPORT INCOMPLET.")
  }

  manifeste[[nom]] <- data.frame(
    snapshot_id        = SNAPSHOT_ID,
    dataset            = nom,
    identifiant_api    = id,
    telecharge_utc     = t_utc,
    lignes_recues      = nrow(df),
    lignes_api         = metas$records_count,
    export_complet     = complet,
    api_data_processed = as.character(metas$data_processed),
    api_modified       = as.character(metas$modified),
    taille_octets      = file.size(f_csv),
    sha256_csv         = digest(f_csv, algo = "sha256", file = TRUE)
  )
  message("  -> ", nrow(df), " lignes | complet : ", complet)
}

manifeste <- do.call(rbind, manifeste)
write_csv(manifeste, file.path(DIR_SNAP, "MANIFESTE.csv"))
writeLines(capture.output(sessionInfo()),
           file.path(DIR_SNAP, "sessionInfo.txt"))

# ── 5. VERROUILLAGE EN LECTURE SEULE ────────────────────────

Sys.chmod(list.files(DIR_SNAP, full.names = TRUE), mode = "0444")

print(manifeste[, c("dataset", "lignes_recues", "lignes_api", "export_complet")])
message("\nSnapshot gelé : ", DIR_SNAP,
        "\nÀ reporter dans R/config.R : SNAPSHOT_ID <- \"", SNAPSHOT_ID, "\"",
        "\nMention obligatoire (CGU tpg) : « Source : transports publics ",
        "genevois (tpg), état en date du ", format(Sys.Date(), "%d.%m.%Y"), " ».")
