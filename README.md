# 🚌 TPG Open Data Analysis

> **10 ans de données des Transports Publics Genevois — une analyse statistique complète.**

Analyse statistique rigoureuse des données open data des TPG (Transports Publics Genevois),
couvrant la fréquentation, la saisonnalité, l'impact COVID, les collisions et l'efficience
des lignes. Projet réalisé à Genève, avril 2026.

---

## 📊 Résultats clés

### Concentration du trafic
- **Indice de Gini = 0.843** — très forte concentration
- 10% des arrêts concentrent **74.2%** de toutes les montées
- Cornavin seul représente **9.2%** du trafic réseau

### Impact COVID
- Plancher avril 2020 : **3.48M montées** (-80.6% vs niveau habituel)
- Récupération **statistiquement complète** en 2022+ (Mann-Whitney p=0.506)
- Le COVID est un **choc transitoire**, pas un changement de régime permanent

### Saisonnalité
- Juillet = creux le plus profond : **-2.60M montées** vs tendance
- Novembre = mois le plus fort : **+1.72M montées** vs tendance
- La saisonnalité n'a **pas changé** après COVID — les habitudes sont stables

### Profils horaires
- Le pic de 17h transporte **22% de plus** que le pic de 8h (r=0.866, p<10⁻²²⁵)
- Le mercredi a une **identité horaire propre** : bosse 11h-15h (+17.8% à 14h)
- Le lundi post-COVID est structurellement plus creux que les autres jours

### Efficience des lignes
- Les **trams** sont structurellement plus efficients que les bus
- Corrélation fréquentation × km produits : r=0.924 (Spearman)
- Le site propre = efficience mesurable — argument quantifié pour l'extension du réseau

### Gratuité jeunes (janvier 2025)
- +4.6% de fréquentation médiane descriptiblement
- Non significatif statistiquement (p=0.137) — données insuffisantes (14 mois)
- À réévaluer en 2027 avec 2-3 ans de recul

### Collisions
- **9 975 collisions** documentées sur 11 ans (2015-2026)
- Record 2025 : **1 024 collisions** — à contextualiser avec l'augmentation de l'offre
- Distribution horaire corrélée à la fréquentation — pic 16h-17h

---

## 📁 Structure du projet

```
tpg-opendata-analysis/
│
├── R/                          ← Scripts d'analyse (numérotés dans l'ordre)
│   ├── 00_palette.R            ← Palette officielle du projet (couleurs, thème)
│   ├── 00_exploration_generale.R  ← EDA complet — inventaire des 7 datasets
│   ├── 01_arrets_import.R      ← Import arrêts + carte Leaflet interactive
│   ├── 02_frequentation_import.R  ← Fréquentation journalière, top arrêts/lignes
│   ├── 03_evolution_temporelle.R  ← Évolution 2016-2026, récupération COVID
│   ├── 04_heatmap_horaire.R    ← Heatmap heure × jour, T-003
│   ├── 05_profil_journalier.R  ← Profils par jour, T-001, T-005
│   ├── 06_saisonnalite.R       ← Décomposition STL
│   ├── 07_impact_covid.R       ← T-002, T-004, T-004b, T-006
│   ├── 08_collisions.R         ← Analyse collisions, carte Leaflet
│   └── 09_tests_statistiques.R ← T-007 Gini, T-008 corrélation, T-009 gratuité
│
├── data/
│   ├── raw/                    ← Données brutes (.rds) — non versionnées
│   └── processed/              ← Données traitées (.rds) — non versionnées
│
├── outputs/                    ← Graphiques et cartes — non versionnés
└── README.md                   ← Ce fichier
```

---

## 🔬 Tests statistiques réalisés

| Test | Hypothèse | Méthode | Résultat |
|------|-----------|---------|---------|
| T-001 | Jours de semaine ≠ | KW + Dunn Bonferroni | Lundi seul diffère — η²=1.1% |
| T-002 | NORMAL vs VACANCES | Mann-Whitney | -28.2%, r=0.549 |
| T-003 | Pic soir > pic matin | Wilcoxon apparié | r=0.866, p<10⁻²²⁵ |
| T-004 | COVID = rupture structurelle | Chow + CUSUM + Bai-Perron | 3 ruptures prouvées |
| T-004b | COVID = rupture résidus STL | Chow + CUSUM + BP | Choc transitoire confirmé |
| T-005 | Mercredi ≠ autres jours | KW par heure + Bonferroni | 8/19 heures significatives |
| T-006 | Pré vs post-COVID | Mann-Whitney bilatéral | p=0.506 — récupération complète |
| T-007 | Concentration arrêts | Gini + bootstrap | Gini=0.843, IC[0.811;0.872] |
| T-008 | Corrélation km × montées | Pearson + Spearman | r=0.924 |
| T-009 | Gratuité jeunes jan 2025 | Mann-Whitney | p=0.137 — non significatif |

---

## 📊 Datasets utilisés

Source : [opendata.tpg.ch](https://opendata.tpg.ch)

| Dataset | Période | Granularité |
|---------|---------|-------------|
| Arrêts du réseau | Historique | Par arrêt |
| Fréquentation journalière | Avr 2023 – Fév 2026 | Jour × arrêt × ligne |
| Fréquentation mensuelle | Jan 2016 – Fév 2026 | Mois × arrêt × ligne |
| Fréquentation horaire | Jan 2019 – Avr 2026 | Heure × jour |
| Kilomètres produits | Jan 2016 – Avr 2026 | Jour × ligne |
| Collisions avec tiers | Jan 2015 – Avr 2026 | Par événement |

---

## 🚀 Reproduire l'analyse

### Prérequis
- R 4.5+ et RStudio
- Packages R : `httr2`, `dplyr`, `ggplot2`, `lubridate`, `leaflet`,
  `htmlwidgets`, `readr`, `tidyr`, `janitor`, `viridis`, `scales`,
  `strucchange`, `zoo`, `sandwich`, `dunn.test`, `here`

### Installation des packages
```r
install.packages(c("httr2", "dplyr", "ggplot2", "lubridate", "leaflet",
                   "htmlwidgets", "readr", "tidyr", "janitor", "viridis",
                   "scales", "strucchange", "zoo", "sandwich",
                   "dunn.test", "here"))
```

### Exécution
1. Cloner le repo
2. Ouvrir le projet dans RStudio
3. Définir le répertoire de travail vers `R/`
4. Exécuter les scripts dans l'ordre numérique (00 → 09)
5. Le script 00 télécharge et sauvegarde les données depuis l'API TPG

> **Note :** les données brutes ne sont pas versionnées.
> Le script 00 les télécharge automatiquement depuis l'API TPG open data.
> La sauvegarde locale en `.rds` accélère les sessions suivantes.

---

## 🛠️ Stack technique

- **R 4.5** — analyse statistique principale
- **ggplot2** — visualisations
- **leaflet** — cartographie interactive
- **strucchange** — tests de rupture structurelle (Chow, CUSUM, Bai-Perron)
- **Python 3.11** — visualisations finales (à venir)
- **Power BI** — dashboard interactif (à venir)

---

## 📚 Sources et contexte

- **TPG Open Data** : [opendata.tpg.ch](https://opendata.tpg.ch)
- **Rapports annuels TPG** : [tpg.ch](https://www.tpg.ch/fr/nous-connaitre/publications/rapports-annuels)
- **SITG** (données géographiques genevoises) : [sitg.ch](https://sitg.ch)

**Contexte clé :**
- 2019 : mise en service du Léman Express — réorganisation majeure du réseau
- Mars 2020 : plancher COVID (-80.6% en avril 2020)
- Décembre 2023 : intégration du réseau Noctambus dans les lignes diurnes
- Janvier 2025 : entrée en vigueur de la gratuité pour les jeunes (<18 ans + 18-24 ans en formation)

---

## 👤 Auteur

**Frat DAG** — Statisticien et Data Analyst, Genève
- Analyse statistique rigoureuse des données publiques
- R, Python, Power BI, SQL

---

*Projet en cours — Phase 3 (analyses avancées) et visualisations finales à venir.*
