# ============================================================
# R/config.R : CONFIGURATION CENTRALE
# À sourcer en tête de CHAQUE script : source(here::here("R", "config.R"))
# Changer de snapshot = changer UNE ligne ici, nulle part ailleurs.
# ============================================================

library(here)

# ── Snapshot analysé (dossier créé par 00_download.R) ───────
# Valeur affichée par 00_download.R à la fin du téléchargement.
SNAPSHOT_ID <- "2026-09-18"

# ── Date de coupure analytique ──────────────────────────────
# Dernier mois COMPLET et DÉFINITIF inclus dans les analyses.
# Tout ce qui est postérieur est ignoré, même si présent dans le snapshot.
DATE_COUPURE <- as.Date("2026-06-30")

# Confirmé sur donnees_definitives, snapshot 2026-09-18 :
# juin 2026 = 100% définitif sur journalier/horaire/mn/km_prod.
# juillet 2026 = partiellement définitif (à exclure).
# mensuel diverge (juillet marqué 100% définitif) : écart signalé aux tpg (Q-01).


# ── Dates d'événements (une seule définition pour tout le projet)
D_LEMAN_EXPRESS <- as.Date("2019-12-15")
D_COVID         <- as.Date("2020-03-01")
D_RESEAU_NUIT   <- as.Date("2023-12-10")
D_ETAPE_DEC2024 <- as.Date("2024-12-15")  # hausse d'offre, 2 semaines avant la gratuité
D_GRATUITE      <- as.Date("2025-01-01")

# ── Graine unique (bootstrap, échantillons) ─────────────────
SEED <- 42

# ── Chemins ─────────────────────────────────────────────────
DIR_RAW     <- here("data", "raw", SNAPSHOT_ID)
DIR_SITG    <- here("data", "raw", "sitg")
DIR_PROC    <- here("data", "processed", SNAPSHOT_ID)
DIR_FIG     <- here("figures")
DIR_RES     <- here("resultats")
for (d in c(DIR_PROC, DIR_FIG, DIR_RES)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Mention de source obligatoire (CGU tpg, art. 6 al. 1) ───
# À mettre en légende (caption) de CHAQUE figure publiée.
# Formulation imposée par les CGU (source + date d'état). La note sur
# les traitements va dans la méthodologie (README), pas sur chaque figure.
SOURCE_TPG <- paste0("Source : transports publics genevois (tpg), état en date du ",
                     format(as.Date(SNAPSHOT_ID), "%d.%m.%Y"),
                     ". Ce travail n'est pas la propriété des tpg.")

# Mention de source imposée par les conditions d'utilisation du SITG,
# article 9 (T-08). Dates d'extraction des couches téléchargées.
SOURCE_SITG <- paste0("Source : Système d'information du territoire à Genève (SITG), ",
                      "extrait en date du 20.03.2026 (communes et secteurs) et du ",
                      "21.08.2026 (emprise du lac).")

# ── Lecture standardisée d'un dataset du snapshot ───────────
# Applique partout les mêmes conventions : données définitives,
# ligne en character, coupure analytique.
lire <- function(nom, col_date = "date") {
  df <- readRDS(file.path(DIR_RAW, paste0(nom, ".rds")))
  if ("donnees_definitives" %in% names(df)) df <- df[df$donnees_definitives %in% TRUE, ]
  if ("ligne" %in% names(df)) df$ligne <- as.character(df$ligne)
  
  # Détection robuste : certains datasets (collisions) utilisent "jour"
  # au lieu de "date". On essaie col_date, puis les alias connus, plutôt
  # que de filtrer silencieusement sur rien si le nom ne correspond pas.
  candidats   <- unique(c(col_date, "date", "jour", "mois"))
  col_trouvee <- candidats[candidats %in% names(df)][1]
  
  # F-08 : le mensuel porte une colonne mois en texte AAAA-MM. On
  # compare le premier jour du mois à la coupure (juin 2026 garde,
  # juillet 2026 retire). La colonne reste du texte : les scripts
  # qui font ym(mois) ne changent pas.
  valeurs <- if (!is.na(col_trouvee)) df[[col_trouvee]] else NULL
  if (is.factor(valeurs)) valeurs <- as.character(valeurs)
  est_mois_texte <- is.character(valeurs) &&
    all(grepl("^[0-9]{4}-[0-9]{2}$", valeurs[!is.na(valeurs)]))
  
  if (is.na(col_trouvee)) {
    warning(nom, " : aucune colonne de date trouvée parmi (",
            paste(candidats, collapse = ", "), ") : DATE_COUPURE NON appliquée.")
  } else if (inherits(df[[col_trouvee]], "Date")) {
    df <- df[df[[col_trouvee]] <= DATE_COUPURE, ]
  } else if (est_mois_texte) {
    debut_mois <- as.Date(paste0(valeurs, "-01"))
    df <- df[!is.na(debut_mois) & debut_mois <= DATE_COUPURE, ]
  } else {
    warning(nom, " : colonne '", col_trouvee, "' trouvée mais n'est pas de type Date : DATE_COUPURE NON appliquée.")
  }
  
  df
}

# ── Registre des résultats ──────────────────────────────────
# Chaque test appelle enregistrer() : le README cite ce fichier,
# plus jamais un chiffre recopié à la main.
enregistrer <- function(test_id, script, methode, n, statistique = NA,
                        p_value = NA, effet_nom = NA, effet = NA,
                        ic_inf = NA, ic_sup = NA, note = "") {
  f <- file.path(DIR_RES, paste0("resultats_", SNAPSHOT_ID, ".csv"))
  ligne <- data.frame(snapshot_id = SNAPSHOT_ID, date_coupure = DATE_COUPURE,
                      test_id, script, methode, n, statistique, p_value,
                      effet_nom, effet, ic_inf, ic_sup, note)
  if (file.exists(f)) {
    ancien <- read.csv(f)
    ancien <- ancien[ancien$test_id != test_id, ]   # un test = une ligne
    ligne  <- rbind(ancien, ligne)
  }
  write.csv(ligne, f, row.names = FALSE)
}
