# ============================================================
# SCRIPT 02 - FRÉQUENTATION JOURNALIÈRE : CONTRÔLES ET STRUCTURE
# Auteur : Frat DAG
# Corrections appliquées : R-01, R-02, R-04, R-05, R-09, T-04 (voir CORRECTIONS.md)
# ------------------------------------------------------------
# OBJECTIF :
#   A. Contrôler le dataset journalier avant tout usage :
#      continuité des dates, clé unique, valeurs manquantes,
#      recoupement avec le dataset mensuel.
#   B. Décrire la structure du réseau : parts par type de ligne,
#      dix premiers arrêts, dix premières lignes.
# RÈGLE : aucun chiffre écrit en dur. Les constats sont lus dans
# la sortie et dans resultats/, pas dans les commentaires.
# ============================================================

source(here::here("R", "config.R"))

library(dplyr)
library(ggplot2)
library(scales)

# ── A1. CHARGEMENT ET INVENTAIRE DU FILTRE ──────────────────

brut <- readRDS(file.path(DIR_RAW, "journalier.rds"))
journalier <- lire("journalier")   # définitives + coupure + ligne en texte

cat("Snapshot :", SNAPSHOT_ID, "| coupure :", format(DATE_COUPURE), "\n\n")
cat("Lignes brutes                :", nrow(brut), "\n")
cat("  dont non définitives       :", sum(!brut$donnees_definitives), "\n")
cat("  dont postérieures à coupure:", sum(brut$date > DATE_COUPURE), "\n")
cat("Lignes retenues              :", nrow(journalier),
    "(", round(100 * nrow(journalier) / nrow(brut), 1), "% )\n")
rm(brut)

# ── A2. PÉRIODE ET CONTINUITÉ ───────────────────────────────

d_min <- min(journalier$date)
d_max <- max(journalier$date)
jours_attendus  <- seq(d_min, d_max, by = "day")
jours_manquants <- as.Date(setdiff(jours_attendus, unique(journalier$date)),
                           origin = "1970-01-01")

cat("\nPériode          :", format(d_min), "au", format(d_max), "\n")
cat("Jours attendus   :", length(jours_attendus), "\n")
cat("Jours présents   :", n_distinct(journalier$date), "\n")
cat("Jours manquants  :", length(jours_manquants), "\n")
if (length(jours_manquants) > 0) print(jours_manquants)

PERIODE_TXT <- paste(format(d_min, "%d.%m.%Y"), "au", format(d_max, "%d.%m.%Y"))

# ── A3. CLÉ, VALEURS MANQUANTES, VALEURS IMPOSSIBLES ────────
# Une observation = un jour x une ligne x un point d'arrêt (code long).
# Un même NOM d'arrêt regroupe plusieurs codes (quais, directions).

cat("\nDoublons sur (date, ligne, arret_code_long) :",
    sum(duplicated(journalier[, c("date", "ligne", "arret_code_long")])), "\n")
cat("Noms d'arrêt distincts  :", n_distinct(journalier$arret), "\n")
cat("Codes d'arrêt distincts :", n_distinct(journalier$arret_code_long), "\n")
cat("Lignes distinctes       :", n_distinct(journalier$ligne, na.rm = TRUE), "\n")

total_montees <- sum(journalier$nb_de_montees, na.rm = TRUE)
na_ligne      <- is.na(journalier$ligne)

cat("\nMontées manquantes (NA) :", sum(is.na(journalier$nb_de_montees)), "\n")
cat("Montées négatives       :", sum(journalier$nb_de_montees < 0, na.rm = TRUE), "\n")
cat("Observations sans ligne :", sum(na_ligne),
    "(", round(100 * mean(na_ligne), 2), "% des observations,",
    round(100 * sum(journalier$nb_de_montees[na_ligne]) / total_montees, 4),
    "% des montées )\n")

# ── A4. RECOUPEMENT AVEC LE DATASET MENSUEL ─────────────────
# Si le journalier est complet, ses sommes mensuelles doivent
# retrouver celles du dataset mensuel, publié séparément.
# C'est le contrôle qui aurait détecté un export tronqué.

mensuel <- readRDS(file.path(DIR_RAW, "mensuel.rds")) %>%
  mutate(ligne = as.character(ligne))

recoup_mois <- journalier %>%
  mutate(mois = format(date, "%Y-%m")) %>%
  group_by(mois) %>%
  summarise(montees_journalier = sum(nb_de_montees), .groups = "drop") %>%
  inner_join(mensuel %>% group_by(mois) %>%
               summarise(montees_mensuel = sum(nb_de_montees, na.rm = TRUE),
                         .groups = "drop"),
             by = "mois") %>%
  mutate(ecart_pct = round(100 * (montees_journalier - montees_mensuel) /
                             montees_mensuel, 3))

cat("\n=== RECOUPEMENT JOURNALIER / MENSUEL, PAR MOIS ===\n")
cat("Mois comparés :", nrow(recoup_mois), "\n")
cat("Ecart relatif (%) - min, mediane, max :",
    min(recoup_mois$ecart_pct), median(recoup_mois$ecart_pct),
    max(recoup_mois$ecart_pct), "\n")
cat("Mois avec un écart supérieur à 0,01 % :\n")
print(as.data.frame(recoup_mois %>% filter(abs(ecart_pct) > 0.01)))

mois_communs <- recoup_mois$mois
recoup_ligne <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne) %>%
  summarise(montees_journalier = sum(nb_de_montees), .groups = "drop") %>%
  full_join(mensuel %>% filter(mois %in% mois_communs, !is.na(ligne)) %>%
              group_by(ligne) %>%
              summarise(montees_mensuel = sum(nb_de_montees, na.rm = TRUE),
                        .groups = "drop"),
            by = "ligne") %>%
  mutate(ecart_pct = round(100 * (montees_journalier - montees_mensuel) /
                             montees_mensuel, 2))

cat("\nLignes présentes dans un seul des deux datasets :",
    sum(is.na(recoup_ligne$montees_journalier) | is.na(recoup_ligne$montees_mensuel)), "\n")
cat("Lignes avec un écart supérieur à 1 % sur la période :\n")
print(as.data.frame(recoup_ligne %>% filter(abs(ecart_pct) > 1) %>%
                      arrange(desc(abs(ecart_pct)))))

write.csv(recoup_mois,  file.path(DIR_RES, paste0("recoupement_mois_",  SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(recoup_ligne, file.path(DIR_RES, paste0("recoupement_ligne_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
rm(mensuel)

# ── B1. STRUCTURE PAR TYPE DE LIGNE ─────────────────────────

freq_par_type <- journalier %>%
  group_by(ligne_type_act) %>%
  summarise(montees  = sum(nb_de_montees),
            n_lignes = n_distinct(ligne, na.rm = TRUE),
            n_arrets = n_distinct(arret),
            .groups  = "drop") %>%
  mutate(pct_montees = round(100 * montees / sum(montees), 2)) %>%
  arrange(desc(montees))

cat("\n=== FREQUENTATION PAR TYPE DE LIGNE -", PERIODE_TXT, "===\n")
print(as.data.frame(freq_par_type))

# ── B2. DIX PREMIERS ARRÊTS ET DIX PREMIÈRES LIGNES ─────────
# Arrêt = nom d'arrêt (tous quais confondus). Les montées incluent
# les correspondances : un pôle d'échange est mécaniquement haut.

top_arrets <- journalier %>%
  group_by(arret) %>%
  summarise(montees  = sum(nb_de_montees),
            n_lignes = n_distinct(ligne, na.rm = TRUE),
            n_codes  = n_distinct(arret_code_long),
            .groups  = "drop") %>%
  arrange(desc(montees)) %>%
  slice_head(n = 10) %>%
  mutate(pct_du_total = round(100 * montees / total_montees, 1))

top_lignes <- journalier %>%
  filter(!is.na(ligne)) %>%
  group_by(ligne, ligne_type_act) %>%
  summarise(montees = sum(nb_de_montees), n_arrets = n_distinct(arret),
            .groups = "drop") %>%
  arrange(desc(montees)) %>%
  slice_head(n = 10) %>%
  mutate(pct_du_total = round(100 * montees / total_montees, 1))

cat("\n=== DIX PREMIERS ARRÊTS ===\n"); print(as.data.frame(top_arrets))
cat("Part cumulée :", sum(top_arrets$pct_du_total), "%\n")
cat("\n=== DIX PREMIÈRES LIGNES ===\n"); print(as.data.frame(top_lignes))
cat("Part cumulée :", sum(top_lignes$pct_du_total), "%\n")

write.csv(freq_par_type, file.path(DIR_RES, paste0("02_parts_par_type_", SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(top_arrets,    file.path(DIR_RES, paste0("02_top10_arrets_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)
write.csv(top_lignes,    file.path(DIR_RES, paste0("02_top10_lignes_",   SNAPSHOT_ID, ".csv")), row.names = FALSE)

# ── B3. FIGURES ─────────────────────────────────────────────

barres_top10 <- function(df, var, titre, note = NULL) {
  df$etiquette <- df[[var]]
  ggplot(df, aes(x = reorder(etiquette, montees), y = montees / 1e6)) +
    geom_col(fill = "#B23A48", alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f %%", pct_du_total)),
              hjust = -0.1, size = 3.5, color = "grey30") +
    coord_flip() +
    scale_y_continuous(labels = label_number(suffix = " M"),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = titre,
         subtitle = paste0("Montées cumulées du ", PERIODE_TXT, ", données définitives"),
         x = NULL, y = "Millions de montées",
         caption = paste(c(SOURCE_TPG, "% = part des montées du réseau", note),
                         collapse = "\n")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          panel.grid.major.y = element_blank())
}

p_arrets <- barres_top10(top_arrets, "arret", "Les dix arrêts les plus fréquentés",
                         "Montées, correspondances comprises ; tous quais d'un même nom regroupés")
p_lignes <- barres_top10(top_lignes, "ligne", "Les dix lignes les plus fréquentées")

ggsave(file.path(DIR_FIG, "02_top10_arrets.png"), p_arrets, width = 10, height = 6, dpi = 150)
ggsave(file.path(DIR_FIG, "02_top10_lignes.png"), p_lignes, width = 10, height = 6, dpi = 150)

message("Script 02 terminé. Résultats dans resultats/, figures dans figures/.")
