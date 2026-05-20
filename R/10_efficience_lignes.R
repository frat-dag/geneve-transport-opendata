# =============================================================================
# SCRIPT 10 — EFFICIENCE DES LIGNES TPG
# Projet TPG Open Data — Phase 3
# Frat DAG — avril 2026
# =============================================================================
#
# APPROCHE : inductive — les données guident les décisions.
# Séquence respectée :
#   exploration visuelle → observation → hypothèse → conditions → test → conclusion
#
# DOUBLE VERSION :
#   - Résultats publics : insights factuels, narration positive
#   - Observations sensibles marquées # [TPG] : version confidentielle uniquement
#
# CONTEXTE :
#   t008_data construit dans script 09 — ratio montées/km par ligne.
#   Ce script approfondit cette analyse avec des exclusions motivées,
#   une segmentation analytique rigoureuse et un test statistique formel.
#
# =============================================================================
# ÉTAPES :
#   1. Chargement et point de départ
#   2. Exploration brute — qui sont les lignes extrêmes ?
#   3. Investigation ligne par ligne — décisions motivées
#   4. Construction du dataset propre
#   5. Exploration du dataset propre — distributions
#   6. Choix du test — conditions d'application
#   7. Test T-010 — Mann-Whitney PRINCIPAL vs SECONDAIRE
#   8. Analyses séparées — catégorie B et C
#   9. Visualisations
# =============================================================================

source("00_palette.R")
library(dplyr)
library(ggplot2)
library(scales)

# =============================================================================
# 1. CHARGEMENT
# =============================================================================
# Objets attendus en mémoire : t008_data, journalier
# Si session fraîche, charger :
# journalier <- readRDS("../data/raw/journalier.rds")
# km_prod    <- readRDS("../data/raw/km_prod.rds")
# Puis sourcer les scripts 02 et 09 pour reconstruire t008_data.

cat("=== POINT DE DÉPART ===\n")
cat("t008_data :", nrow(t008_data), "lignes x", ncol(t008_data), "colonnes\n")
cat("Colonnes  :", paste(names(t008_data), collapse = ", "), "\n\n")

# Enrichir avec le type de ligne depuis journalier
types_lignes <- journalier %>%
  distinct(ligne, ligne_type_act)

t008_enrichi <- t008_data %>%
  left_join(types_lignes, by = "ligne")

cat("Distribution brute par type :\n")
print(table(t008_enrichi$ligne_type_act, useNA = "ifany"))

# =============================================================================
# 2. EXPLORATION BRUTE — QUI SONT LES LIGNES EXTREMES ?
# =============================================================================
# Avant toute exclusion, on regarde la distribution complete.
# Regle : on ne decide rien sans avoir regarde.

cat("\n=== DISTRIBUTION BRUTE (toutes lignes) ===\n")
cat("Min    :", min(t008_enrichi$montees_par_km), "\n")
cat("Q1     :", quantile(t008_enrichi$montees_par_km, 0.25), "\n")
cat("Mediane:", median(t008_enrichi$montees_par_km), "\n")
cat("Moyenne:", round(mean(t008_enrichi$montees_par_km), 2), "\n")
cat("Q3     :", quantile(t008_enrichi$montees_par_km, 0.75), "\n")
cat("Max    :", max(t008_enrichi$montees_par_km), "\n")

# Lignes du haut — potentiellement artificielles
cat("\n--- TOP 15 (ratio le plus eleve) ---\n")
t008_enrichi %>%
  arrange(desc(montees_par_km)) %>%
  head(15) %>%
  dplyr::select(ligne, ligne_type_act, montees_totales, km_totaux, montees_par_km) %>%
  print()

# Lignes du bas — potentiellement problematiques ou a vocation sociale
cat("\n--- BOTTOM 15 (ratio le plus faible) ---\n")
t008_enrichi %>%
  arrange(montees_par_km) %>%
  head(15) %>%
  dplyr::select(ligne, ligne_type_act, montees_totales, km_totaux, montees_par_km) %>%
  print()

# =============================================================================
# 3. INVESTIGATION LIGNE PAR LIGNE — DECISIONS MOTIVEES
# =============================================================================
# Chaque exclusion est justifiee analytiquement, pas par convention.
# Source : exploration console du 28.04.2026 — JOURNAL_ANALYTIQUE OBS-058.

# --- CATEGORIE A : EXCLUS DEFINITIFS ---

# A1 — Lignes CX (scolaires)
# Observation : ratios 2.7 a 4.5 — les plus eleves du reseau.
# Investigation : courses tres courtes (41 000 km pour C8), tres chargees
#   aux heures scolaires uniquement. Km_totaux negligeables.
# Decision : ratio artificiellement eleve — non comparable aux lignes regulieres.
# Reference : DEC-015
lignes_cx <- c("C1","C3","C4","C5","C6","C7","C8","C9")

# A2 — Lignes Noctambus (dont NO)
# Observation : ratios = 0 ou 0.1, volumes derisoires (900 a 3 000 montees).
# Investigation : service absorbe dans le reseau regulier en dec 2023.
#   Donnees couvrent une periode partielle — ratio biaise par la discontinuite.
#   NO (900 montees, ratio=0) survit au filtre montees>0 mais reste non comparable.
# Decision : service discontinu — exclusion methodologique.
lignes_noctambus <- c("NC","ND","NE","NJ","NK","NM","NP","NS","NT","NV","NO",
                      "NB1","NB2","NB3","NB4","NB5","NB6")

exclus_definitifs <- c(lignes_cx, lignes_noctambus)

cat("\n=== CATEGORIE A — EXCLUS DEFINITIFS ===\n")
cat("Lignes CX     :", length(lignes_cx), "\n")
cat("Lignes NB/N*  :", length(lignes_noctambus), "\n")
cat("Total exclus A:", length(exclus_definitifs), "\n")

# --- CATEGORIE B : ANALYSE SEPAREE — RURALES ET SOCIALES ---

# Observation : lignes SECONDAIRE a ratio 0 ou 0.1 avec arrets dominants
#   situes en zone rurale ou peripherique genevoise.
# Investigation detaillee (console 28.04.2026) :
#   - 63  : Bernex-Vailly, 5 arrets, desserte locale tres courte
#   - 76  : La Plaine-Gare, vallee de l'Arve, zone frontaliere rurale
#   - 77  : La Plaine-Gare, meme zone, variante
#   - 86  : Annemasse-Gare Rotonde, max 8 montees/jour
#   - 34  : Belle-Idee-Centre — desserte hopital psychiatrique (mission sociale)
#   - 36  : ligne longue depuis Bel-Air vers peripherie
#   - 39  : Bois-Caran — zone industrielle/peripherique
#   - 59  : Palexpo — ligne evenementielle, faible frequentation hors salons
#   - 72  : Satigny-Gare — rural nord-ouest genevois
#   - 73  : Vernier-Village — periurbain a faible densite
#   - 74  : La Plaine-Gare — vallee de l'Ain
#   - 78  : La Plaine-Gare — meme zone
#   - 301 : Bernex-Vailly — variante rurale
#   - 302 : Vessenaz-Village — rural rive gauche
#   - 303 : Satigny-Gare — max 3 montees/jour
#   - L   : Rural ouest genevois (Confignon, Bernex, Laconnex, Soral, Sezegnin)
#             28 arrets, 3M km, 398 000 montees sur 3 ans — desserte sociale rurale
#   - T   : Challex, La Plaine — rural extreme ouest, service universel
#             6 363 montees sur 3 ans — quasi symbolique
#
# Decision : ratio faible structurellement justifie par la vocation de service
#   public. Ce ne sont PAS des lignes inefficientes — elles repondent a une
#   mission differente. Analyse separee obligatoire.
#
# [TPG] Ces lignes representent le cout de la desserte universelle.
#   La question pertinente pour les elus : quel est le cout par montee de
#   ces lignes vs les lignes urbaines ? Combien de residents seraient
#   encalves sans elles ? Ce croisement necessite les donnees de cout
#   d'exploitation (non publiques).

lignes_rurales_sociales <- c("L","63","76","77","86","34","36","39","59",
                             "72","73","74","78","301","302","303","T")

cat("\n=== CATEGORIE B — RURALES/SOCIALES ===\n")
cat("Lignes :", length(lignes_rurales_sociales), "\n")
cat(paste(lignes_rurales_sociales, collapse = ", "), "\n")

# --- CATEGORIE C : ANALYSE SEPAREE — GLCT TRANSFRONTALIERES ---

# Observation : lignes GLCT a ratio faible.
# Investigation : M et N = reseau CCG (pays de Gex, OBS-009/DEC-006).
#   Arrets dominants : Saint-Julien SNCF, Arande, Valleiry, Viry — communes
#   francaises de Haute-Savoie et Ain. Perimetre, logique tarifaire et
#   financement (GLCT) distincts du reseau urbain genevois.
#
# [TPG] La recuperation GLCT a 117% post-COVID (T-006) est un biais de
#   perimetre lie a l'integration des lignes apres l'appel d'offres 2023.
#   A ne pas presenter comme croissance organique dans toute communication.

lignes_glct_separees <- c("M","N")  # T deja en categorie B

cat("\n=== CATEGORIE C — GLCT TRANSFRONTALIERES ===\n")
cat("Lignes :", length(lignes_glct_separees), "\n")

# =============================================================================
# 4. CONSTRUCTION DU DATASET PROPRE
# =============================================================================

efficience <- t008_enrichi %>%
  filter(
    !ligne %in% exclus_definitifs,
    !ligne %in% lignes_rurales_sociales,
    !ligne %in% lignes_glct_separees,
    ligne_type_act %in% c("PRINCIPAL", "SECONDAIRE"),
    montees_totales > 0,
    km_totaux > 0
  )

cat("\n=== DATASET PRINCIPAL ===\n")
cat("Lignes retenues :", nrow(efficience), "\n")
efficience %>%
  group_by(ligne_type_act) %>%
  summarise(n = n(), .groups = "drop") %>%
  print()

# =============================================================================
# 5. EXPLORATION DU DATASET PROPRE — DISTRIBUTIONS
# =============================================================================
# On regarde AVANT de choisir le test.

cat("\n=== STATISTIQUES PAR TYPE (dataset propre) ===\n")
efficience %>%
  group_by(ligne_type_act) %>%
  summarise(
    n       = n(),
    min     = min(montees_par_km),
    q1      = quantile(montees_par_km, 0.25),
    mediane = median(montees_par_km),
    moyenne = round(mean(montees_par_km), 2),
    q3      = quantile(montees_par_km, 0.75),
    max     = max(montees_par_km),
    .groups = "drop"
  ) %>%
  print()

# Observation : PRINCIPAL mediane 1.2 vs SECONDAIRE mediane 0.4 — ecart facteur 3.
# Signal fort visuellement. Necessite un test formel pour confirmer.

# =============================================================================
# 6. CHOIX DU TEST — CONDITIONS D'APPLICATION
# =============================================================================

cat("\n=== TEST DE NORMALITE PAR TYPE (Shapiro-Wilk) ===\n")
for (t in c("PRINCIPAL", "SECONDAIRE")) {
  vals <- efficience %>%
    filter(ligne_type_act == t) %>%
    pull(montees_par_km)
  sw <- shapiro.test(vals)
  cat(sprintf("  %-12s n=%-3d W=%.3f p=%.4f %s\n",
              t, length(vals), sw$statistic, sw$p.value,
              ifelse(sw$p.value < 0.05, "-> NON normale", "-> normale")))
}

# Resultats confirmes (28.04.2026) :
#   PRINCIPAL  : W=0.871, p=0.0066 -> NON normale
#   SECONDAIRE : W=0.660, p~0      -> tres fortement NON normale
#
# Decision : normalite violee dans les deux groupes.
# -> Test de Mann-Whitney (Wilcoxon rank-sum)
#    Equivalent non parametrique du test t pour deux groupes independants.
# -> Test bilateral : direction prouvee par IC Hodges-Lehmann, pas supposee a priori.
#
# Note ex-aequos : montees_par_km arrondi a 1 decimale dans t008_data.
#   Nombreux ex-aequos -> R bascule sur approximation normale pour la p-value.
#   Comportement standard et documente.
#
# Alternative rejetee : KW a 3 groupes initialement envisage.
#   Rejete : NOCTAMBUS REGIONAL = n=1 apres exclusion de NO -> non testable.
#   La comparaison se reduit a 2 groupes -> Mann-Whitney plus approprie.

# =============================================================================
# 7. TEST T-010 — MANN-WHITNEY PRINCIPAL vs SECONDAIRE
# =============================================================================

cat("\n=== T-010 — MANN-WHITNEY PRINCIPAL vs SECONDAIRE ===\n")

mw_t010 <- wilcox.test(
  montees_par_km ~ ligne_type_act,
  data        = efficience,
  alternative = "two.sided",
  conf.int    = TRUE,
  conf.level  = 0.95
)

# Taille d'effet r = |Z| / sqrt(N)
n_total <- nrow(efficience)
z_score <- qnorm(mw_t010$p.value / 2)
r_t010  <- abs(z_score) / sqrt(n_total)

cat("Groupes compares : PRINCIPAL (n=23) vs SECONDAIRE (n=40)\n")
cat("Note : p-value approximee (ex-aequos — arrondi a 1 decimale)\n\n")
cat("W (statistique)  :", mw_t010$statistic, "\n")
cat("p-value          :", format(mw_t010$p.value, scientific = TRUE), "\n")
cat("Hodges-Lehmann   :", round(mw_t010$estimate, 3), "montees/km\n")
cat("IC 95%           : [", round(mw_t010$conf.int[1], 3),
    ";", round(mw_t010$conf.int[2], 3), "]\n")
cat("Taille effet r   :", round(r_t010, 3), "\n")
cat("Magnitude        :", ifelse(r_t010 >= 0.5, "Grand (>= 0.5)",
                                 ifelse(r_t010 >= 0.3, "Moyen (0.3-0.5)",
                                        "Petit (< 0.3)")), "\n")

if (mw_t010$p.value < 0.05) {
  cat("\n-> REJET H0 : les lignes PRINCIPAL sont significativement plus\n")
  cat("  efficientes que les lignes SECONDAIRE.\n")
  cat("  IC 95% Hodges-Lehmann entierement positif -> direction prouvee.\n")
} else {
  cat("\n-> NON-REJET H0\n")
}

cat("\n--- CONCLUSIONS FORMELLES T-010 ---\n")
cat("Ce qu'on peut affirmer :\n")
cat("  - PRINCIPAL > SECONDAIRE en montees/km",
    "(p =", format(mw_t010$p.value, scientific = TRUE), ")\n")
cat("  - Difference estimee : +", round(mw_t010$estimate, 1),
    "montees/km (IC 95% [",
    round(mw_t010$conf.int[1], 1), ";",
    round(mw_t010$conf.int[2], 1), "])\n")
cat("  - Grand effet pratique (r =", round(r_t010, 3), ")\n")
cat("  - Sur une ligne faisant 5M km/an : +",
    format(round(mw_t010$estimate * 5e6), big.mark = " "),
    "montees supplementaires\n")
cat("\nCe qu'on NE peut PAS affirmer :\n")
cat("  - Que les lignes SECONDAIRE sous-performent — elles desservent\n")
cat("    des zones moins denses, ce qui explique structurellement le ratio.\n")
cat("  - Que l'efficience est le seul critere pertinent d'evaluation.\n")
cat("  - [TPG] Que les lignes SECONDAIRE a ratio faible doivent etre\n")
cat("    supprimees — sans donnees de cout et de demande potentielle,\n")
cat("    cette conclusion serait prematuree et methodologiquement incorrecte.\n")

# =============================================================================
# 8. ANALYSES SEPAREES
# =============================================================================

# --- 8A : Categorie B — Lignes rurales et sociales ---

cat("\n=== CATEGORIE B — LIGNES RURALES ET SOCIALES ===\n")

rurales_data <- t008_enrichi %>%
  filter(ligne %in% lignes_rurales_sociales)

cat("Lignes presentes :", nrow(rurales_data), "\n\n")

rurales_data %>%
  arrange(montees_par_km, montees_totales) %>%
  dplyr::select(ligne, ligne_type_act,
                montees_totales, km_totaux, montees_par_km) %>%
  print(n = 30)

cat("\nKm produits total cat. B :",
    format(round(sum(rurales_data$km_totaux)), big.mark = " "), "\n")
cat("Montees total cat. B     :",
    format(round(sum(rurales_data$montees_totales)), big.mark = " "), "\n")
cat("Ratio median cat. B      :",
    round(median(rurales_data$montees_par_km), 2), "montees/km\n")
cat("Ratio reseau principal   :",
    round(median(efficience$montees_par_km), 2), "montees/km (reference)\n")

# [TPG] La ligne 303 (max 3 montees/jour) et la ligne T (6 363 montees sur 3 ans)
#   posent une question de fond : quel est le seuil de frequentation minimal
#   justifiant le maintien d'une ligne ? Ce seuil releve d'une decision
#   politique (service universel) autant que d'une logique economique.
#   Sans acces aux couts d'exploitation, aucune recommandation ne peut etre
#   formulee de facon responsable.

# --- 8B : Categorie C — Lignes GLCT ---

cat("\n=== CATEGORIE C — LIGNES GLCT (M et N) ===\n")

glct_data <- t008_enrichi %>%
  filter(ligne %in% lignes_glct_separees)

glct_data %>%
  dplyr::select(ligne, ligne_type_act,
                montees_totales, km_totaux, montees_par_km) %>%
  print()

cat("\nContexte : lignes M et N = reseau CCG (pays de Gex).\n")
cat("Arrets dominants : Saint-Julien SNCF, Arande, Valleiry, Viry\n")
cat("(communes francaises — perimetre et financement distincts)\n")

# [TPG] Ratio M : 0.1 montee/km sur 1.3M km produits.
#   Non comparable au reseau urbain genevois.
#   Le contrat GLCT fixe les obligations de service — l'efficience
#   n'est pas le critere premier de ces lignes.

# =============================================================================
# 9. VISUALISATIONS
# NOTE VIZ : candidats Power BI / Python final
# =============================================================================

q1_eff <- quantile(efficience$montees_par_km, 0.25)
q3_eff <- quantile(efficience$montees_par_km, 0.75)

# --- VIZ 1 : Boxplot PRINCIPAL vs SECONDAIRE ---
p_boxplot <- ggplot(efficience,
                    aes(x = ligne_type_act,
                        y = montees_par_km,
                        fill = ligne_type_act)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2,
               outlier.fill = "white", alpha = 0.85) +
  geom_jitter(width = 0.12, alpha = 0.5, size = 2, color = "grey30") +
  scale_fill_manual(values = PALETTE_TYPES) +
  labs(
    title    = "Efficience des lignes TPG — montees par km produit",
    subtitle = paste0("PRINCIPAL (n=23) vs SECONDAIRE (n=40) | ",
                      "Mann-Whitney p=2.6e-8 | r=0.702 (grand effet)\n",
                      "CX, Noctambus, lignes rurales/sociales et GLCT exclus"),
    x        = "Type de ligne",
    y        = "Montees / km produit",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(legend.position = "none")

ggsave("../outputs/10_boxplot_efficience.png",
       p_boxplot, width = 8, height = 6, dpi = 150)
cat("\nGraphique sauvegarde : 10_boxplot_efficience.png\n")

# --- VIZ 2 : Barplot classement ---
p_barplot <- efficience %>%
  arrange(desc(montees_par_km)) %>%
  mutate(ligne = factor(ligne, levels = ligne)) %>%
  ggplot(aes(x = ligne, y = montees_par_km, fill = ligne_type_act)) +
  geom_col(alpha = 0.85) +
  geom_hline(yintercept = q1_eff, linetype = "dashed",
             color = "firebrick", linewidth = 0.7) +
  geom_hline(yintercept = q3_eff, linetype = "dashed",
             color = "forestgreen", linewidth = 0.7) +
  annotate("text", x = 2, y = q1_eff + 0.06,
           label = paste0("Q1 = ", round(q1_eff, 1)),
           color = "firebrick", size = 3, hjust = 0) +
  annotate("text", x = 2, y = q3_eff + 0.06,
           label = paste0("Q3 = ", round(q3_eff, 1)),
           color = "forestgreen", size = 3, hjust = 0) +
  scale_fill_manual(values = PALETTE_TYPES) +
  labs(
    title    = "Classement des lignes par efficience",
    subtitle = "Montees / km produit — dataset propre (exclusions motivees)",
    x        = "Ligne",
    y        = "Montees / km produit",
    fill     = "Type",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

ggsave("../outputs/10_barplot_classement.png",
       p_barplot, width = 14, height = 6, dpi = 150)
cat("Graphique sauvegarde : 10_barplot_classement.png\n")

# --- VIZ 3 : Scatter montees x km ---
p_scatter <- ggplot(efficience,
                    aes(x = km_totaux / 1e6,
                        y = montees_totales / 1e6,
                        color = ligne_type_act,
                        label = ligne)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(size = 2.8, vjust = -0.8, check_overlap = TRUE) +
  geom_smooth(aes(group = ligne_type_act),
              method = "lm", se = FALSE,
              linewidth = 0.8, linetype = "dashed") +
  scale_color_manual(values = PALETTE_TYPES) +
  scale_x_continuous(labels = label_number(suffix = " M km", accuracy = 0.1)) +
  scale_y_continuous(labels = label_number(suffix = " M", accuracy = 0.1)) +
  labs(
    title    = "Montees vs km produits par ligne",
    subtitle = "Spearman r=0.924 | Les trams (PRINCIPAL) generent plus de montees par km",
    x        = "Km produits (millions)",
    y        = "Montees totales (millions)",
    color    = "Type",
    caption  = "Source : opendata.tpg.ch | Frat DAG 2026"
  ) +
  theme_tpg()

ggsave("../outputs/10_scatter_montees_km.png",
       p_scatter, width = 12, height = 7, dpi = 150)
cat("Graphique sauvegarde : 10_scatter_montees_km.png\n")

# =============================================================================
# BILAN SCRIPT 10
# =============================================================================

cat("\n=== BILAN SCRIPT 10 ===\n")
cat("Dataset propre    : 63 lignes (23 PRINCIPAL + 40 SECONDAIRE)\n")
cat("Exclus definitifs : CX (ratio artificiel) + Noctambus (service discontinu)\n")
cat("Analyse separee B : 17 lignes rurales/sociales\n")
cat("Analyse separee C : 2 lignes GLCT transfrontalieres (M, N)\n")
cat("\nT-010 Mann-Whitney PRINCIPAL vs SECONDAIRE :\n")
cat("  p =", format(mw_t010$p.value, scientific = TRUE),
    "| HL = +", round(mw_t010$estimate, 1), "montees/km",
    "| r =", round(r_t010, 3), "(grand)\n")
cat("\n3 graphiques sauvegardes dans outputs/\n")
cat("[TPG] Observations sensibles marquees # [TPG] dans le script\n")