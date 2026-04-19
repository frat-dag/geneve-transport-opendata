# 🚌 TPG Open Data Analysis

**600 000 voyageurs par jour. 10 ans de données. Que nous disent vraiment les chiffres ?**

À Genève, les Transports Publics Genevois (TPG) publient leurs données en open data. Ce projet les analyse rigoureusement — pas pour produire de jolis graphiques, mais pour répondre à des questions concrètes : le réseau a-t-il vraiment récupéré après le COVID ? La gratuité pour les jeunes a-t-elle changé quelque chose ? Quelles lignes sont efficientes, et lesquelles ne le sont pas ?

L'analyse couvre 6 datasets, 11 tests statistiques formels, et jusqu'à 11 ans de données sur certains indicateurs. Elle est conçue pour être utile à trois types de lecteurs : les opérationnels TPG qui planifient l'offre, les élus qui votent les budgets, et les data analysts qui veulent comprendre la démarche.

> ⚠️ **Version provisoire** — Les analyses de Phase 3 (efficience des lignes, croisements géographiques, clustering) et les visualisations Python/Power BI enrichiront ce README au fur et à mesure.

---

## 📖 Glossaire

Avant d'aller plus loin, voici les termes clés du projet.

### Types de lignes TPG

| Type | Description | Exemples |
|------|-------------|---------|
| **PRINCIPAL** | Lignes urbaines à haute fréquence — l'épine dorsale du réseau. Circulent toute la journée, 7j/7. | 1, 3, 5, 6, 7, 8, 10, 12, 14, 15, 17, 18... |
| **SECONDAIRE** | Lignes de quartier ou de desserte complémentaire — fréquence plus faible, zones moins denses. | 20, 21, 22, 23, 25, 28... |
| **SCOLAIRE** | Courses dédiées aux élèves de Cycles d'Orientation et Collèges spécifiques. Circulent uniquement en semaine, hors vacances, aux heures d'entrée et sortie des classes. | C1, C3, C4, C5, C6, C7, C8, C9 |
| **GLCT** | Lignes transfrontalières franco-genevoises gérées par le Groupement Local de Coopération Transfrontalière. | 60, 61, 64, 66, 68, 80... |
| **REGIONAL** | Lignes régionales existant avant la mise en service du Léman Express (disparu après déc. 2019). | — |
| **NOCTAMBUS** | Service nocturne le week-end — absorbé dans les lignes régulières en décembre 2023. | NB1, NB2... |

### Indicateurs statistiques

| Terme | Définition simple | Exemple dans ce projet |
|-------|-------------------|----------------------|
| **Montée** | Un passager qui monte dans un véhicule TPG. Unité de base de la fréquentation. | 600 000 montées/jour en semaine |
| **p-value** | Probabilité d'observer un résultat aussi extrême si l'hypothèse nulle est vraie. En dessous de 0.05, on rejette l'hypothèse nulle. | T-003 : p < 10⁻²²⁵ |
| **Taille d'effet (r, η²)** | Mesure de l'importance pratique d'une différence, indépendamment de la significativité. r > 0.5 = grand effet. | T-002 : r = 0.549 (grand) |
| **Indice de Gini** | Mesure de concentration entre 0 (égalité parfaite) et 1 (tout concentré sur un seul élément). | Gini = 0.843 sur la distribution des arrêts |
| **Courbe de Lorenz** | Représentation graphique du Gini — l'écart entre la courbe réelle et la diagonale d'égalité est proportionnel au Gini. | 10% des arrêts = 74.2% du trafic |
| **Décomposition STL** | Méthode statistique qui sépare une série temporelle en trois composantes : tendance, saisonnalité, résidus. | Isole le choc COVID des patterns normaux |
| **Test de Chow** | Test de rupture structurelle — vérifie si une série a changé de régime à une date donnée. | Rupture prouvée en mars 2020 |
| **Bai-Perron** | Détection automatique du nombre et des dates de ruptures structurelles dans une série. | 3 ruptures : fév. 2020, août 2021, fév. 2023 |
| **Mann-Whitney** | Test non paramétrique pour comparer deux groupes indépendants quand la normalité est violée. | T-002 : NORMAL vs VACANCES |
| **Wilcoxon apparié** | Test non paramétrique pour comparer deux mesures sur les mêmes observations (ex : matin et soir du même jour). | T-003 : pic 8h vs pic 17h |
| **Bootstrap** | Technique de rééchantillonnage pour estimer un intervalle de confiance sans hypothèse de normalité. | IC 95% du Gini : [0.811 ; 0.872] |
| **Km produits** | Kilomètres effectivement parcourus par les véhicules TPG. Proxy de l'offre de transport. | ~31M km/an en 2025 |
| **Taux normalisé** | Indicateur divisé par le volume d'activité pour permettre des comparaisons équitables entre années. | Collisions / million de km produits |

### Abréviations

| Abréviation | Signification |
|-------------|---------------|
| **CO** | Cycle d'Orientation (école secondaire genevoise) |
| **ECG** | École de Commerce de Genève |
| **GLCT** | Groupement Local de Coopération Transfrontalière |
| **STL** | Seasonal and Trend decomposition using Loess |
| **KW** | Test de Kruskal-Wallis |
| **BP** | Test de Bai-Perron |
| **IC 95%** | Intervalle de confiance à 95% |
| **BH** | Correction de Benjamini-Hochberg (pour tests multiples) |
| **BF** | Correction de Bonferroni (pour tests multiples) |

---

## 🔍 La démarche

Ce projet suit une règle simple : **on n'affirme rien sans l'avoir testé, et on ne teste rien sans avoir d'abord regardé les données.**

La séquence est toujours la même — exploration visuelle, formulation d'une hypothèse, choix d'un test statistique adapté, conclusion documentée avec ses limites. Chaque test justifie pourquoi cette méthode et pas une autre. Les résultats non significatifs sont documentés au même titre que les résultats significatifs — l'absence de preuve est aussi une information.

**Pourquoi des tests non paramétriques ?** Les tests classiques (ANOVA, t de Student) supposent que les données suivent une loi normale. Sur des données de transport agrégées sur plusieurs années incluant le COVID, cette hypothèse est systématiquement violée — les distributions sont asymétriques et contiennent des valeurs extrêmes. Les tests non paramétriques (Kruskal-Wallis, Mann-Whitney, Wilcoxon) ne font pas cette hypothèse et sont plus honnêtes dans ce contexte.

**Pourquoi la décomposition STL ?** Parce que la fréquentation TPG obéit à plusieurs patterns superposés : une tendance de long terme (croissance, COVID, récupération), une saisonnalité annuelle (été creux, automne fort), et des chocs ponctuels (grèves, pandémies). Sans décomposer ces composantes, on ne peut pas distinguer un vrai changement de régime d'un simple effet saisonnier.

---

## 📊 Ce que les données révèlent

### 1. Le réseau est extrêmement concentré

L'indice de Gini de la distribution du trafic entre arrêts est de **0.843** (IC 95% bootstrap : [0.811 ; 0.872]). Pour donner une intuition : un Gini de 0 signifierait que tous les arrêts ont la même fréquentation, un Gini de 1 que tout le trafic passe par un seul arrêt. À 0.843, on est très proches de l'extrême.

Concrètement : **10% des arrêts (environ 100 sur 1 012) concentrent 74.2% de toutes les montées.** Les 90% restants se partagent le quart restant. Cornavin seul représente 9.2% du trafic réseau.

Ce résultat a une implication opérationnelle directe : protéger et fiabiliser les 100 arrêts les plus chargés, c'est protéger les trois quarts du service public.

![Courbe de Lorenz](outputs/09_lorenz_concentration.png)

### 2. Le COVID était un choc transitoire, pas un changement de régime

Pour tester si le COVID a durablement modifié le niveau de fréquentation, trois approches complémentaires ont été utilisées.

Un **test de Chow** et des **tests CUSUM** sur la série brute ont confirmé l'existence de ruptures structurelles — trois précisément, en février 2020, août 2021 et février 2023.

Ensuite, la série a été décomposée avec la méthode **STL**, qui sépare la fréquentation en trois composantes : tendance, saisonnalité, résidus. En relançant les mêmes tests sur les seuls résidus — c'est-à-dire sur ce qui reste après avoir retiré la tendance et la saisonnalité — les ruptures disparaissent (Chow p=0.164). Le COVID a été absorbé par la tendance, pas par le niveau résiduel.

Enfin, en comparant directement les distributions de fréquentation avant COVID (jan. 2016 – fév. 2020) et après (jan. 2022 – fév. 2026), on ne trouve aucune différence statistiquement significative (Mann-Whitney p=0.506). **Le réseau a retrouvé son niveau d'avant COVID.**

![Décomposition STL](outputs/06_stl_decomposition.png)

### 3. Les lignes scolaires, elles, n'ont pas récupéré

Le réseau global a récupéré. Les lignes scolaires, non. Un test de Chow spécifique sur la série SCOLAIRE révèle une rupture structurelle en **juin 2021** (F=29.6, p=3.26×10⁻⁷) — au moment de la sortie de crise, pas pendant le confinement. En 2022, les lignes scolaires ne transportaient que 59.7% de leur niveau 2019, soit un niveau inférieur à leur plancher de 2020 (78.2%).

Ce résultat est contre-intuitif et mérite d'être documenté soigneusement. Les lignes SCOLAIRE TPG (C1 à C9) sont des courses dédiées à des établissements spécifiques — Cycles d'Orientation et Collèges — qui ne circulent qu'en semaine, hors vacances, aux heures d'entrée et de sortie des classes.

Plusieurs hypothèses peuvent expliquer ce déclin, sans qu'on puisse en privilégier une sans données complémentaires :

- **Nouvelles habitudes de mobilité** — pendant la crise, des familles ont développé d'autres pratiques (voiture, vélo, accompagnement à pied) qui ont perduré après la réouverture.
- **Réorganisation du réseau** — en décembre 2023, la modification du tracé de la ligne 22 a entraîné la création de nouvelles courses scolaires vers Le Rolliet, avec une redirection partielle des élèves vers le tram 15. D'autres réorganisations similaires ont pu réduire mécaniquement le volume des lignes CX dans les données.
- **Glissement vers les lignes régulières** — certains élèves qui utilisaient les lignes CX dédiées se reportent peut-être sur les lignes SECONDAIRE ou PRINCIPAL, ce qui expliquerait partiellement la hausse de ces types après 2021.

**Question ouverte aux décideurs TPG :** Y a-t-il eu des réorganisations spécifiques des lignes scolaires entre 2020 et 2023 qui pourraient expliquer ce déclin statistiquement prouvé ? Une partie de ce déclin reflète-t-elle un vrai désengagement des élèves, ou un effet de périmètre lié aux changements de nomenclature des lignes ?

### 4. La gratuité jeunes a trouvé son public, mais pas là où on l'attendait

Lorsqu'on teste l'effet de la gratuité jeunes (janvier 2025) sur l'ensemble du réseau, on trouve une hausse de +4.6% qui n'est pas statistiquement significative (p=0.137). L'effet est noyé dans la variabilité naturelle du réseau.

En revanche, en segmentant par type de ligne, le tableau change radicalement. Les lignes **SECONDAIRE** montrent une hausse de **+13.1%** (p=0.004) — significative. Ce sont les lignes qui desservent les zones résidentielles, les hautes écoles, les centres de formation. Les 18-24 ans en formation ont adopté ces lignes.

Les lignes PRINCIPAL ne montrent pas de hausse significative — non pas parce que la gratuité n'y a pas eu d'effet, mais parce que transporter 85% du trafic total rend statistiquement très difficile la détection d'un signal de quelques points de pourcentage. Un +5% sur PRINCIPAL représenterait pourtant ~800 000 montées supplémentaires par mois.

> **À retenir :** un effet réel peut être invisible statistiquement si la variabilité de la série est trop grande. L'absence de preuve n'est pas une preuve d'absence.

### 5. Le soir transporte 22% de plus que le matin — partout, tout le temps

Sur les jours de semaine normaux, la fréquentation à 17h est systématiquement supérieure à la fréquentation à 8h. Un test de Wilcoxon apparié sur 1 370 jours donne r=0.866 (très grand effet, p<10⁻²²⁵). **99.9% des jours respectent cette règle.**

La différence médiane est de 13 807 montées — soit l'équivalent de 23 bus remplis de plus le soir que le matin, chaque jour.

Le mercredi fait exception dans le détail : une bosse de fréquentation entre 11h et 15h (+17.8% à 14h comparé aux autres jours) combinée à un creux prononcé à 16h (-11.9%). C'est la signature de l'organisation scolaire genevoise — les enfants qui rentrent à midi déplacent la demande vers le milieu de journée.

![Heatmap horaire](outputs/04_heatmap_horaire.png)

### 6. Les trams sont structurellement plus efficients que les bus

La corrélation entre km produits et montées par ligne est très forte (Spearman r=0.924, p<10⁻³⁴). Mais sur le graphique de dispersion, les trams forment une droite parallèle au-dessus de la droite de régression principale — ils génèrent plus de montées par km que les bus, quel que soit leur volume.

C'est la traduction statistique du **site propre intégral** des trams : une vitesse commerciale supérieure et une fréquence plus élevée à infrastructure égale. Chaque km de tram produit structurellement plus de service que chaque km de bus — un argument chiffré pour les décisions d'investissement dans l'extension du réseau ferré.

![Corrélation km × montées](outputs/09_correlation_km_montees.png)

### 7. 2025 n'est pas l'année la plus dangereuse — une fois normalisé

En nombre brut, 2025 est l'année record avec 1 024 collisions. Mais le réseau a aussi produit plus de kilomètres que jamais (31.7 millions). En normalisant par les km produits — ce qui donne le taux de collision pour un million de km parcourus — 2025 est dans la moyenne historique à 32.3/Mkm.

Les années vraiment dangereuses étaient **2018 et 2019**, avec des taux de 37.2 et 37.6/Mkm. Depuis le COVID, le taux oscille entre 28 et 33 — en dessous des niveaux pré-COVID.

![Taux de collision normalisé](outputs/09_taux_collision_normalise.png)

---

## 📁 Structure du projet

```
tpg-opendata-analysis/
│
├── R/                             ← Scripts d'analyse (numérotés dans l'ordre)
│   ├── 00_palette.R               ← Palette officielle du projet
│   ├── 00_exploration_generale.R  ← EDA — inventaire des 7 datasets
│   ├── 01_arrets_import.R         ← Arrêts + carte Leaflet interactive
│   ├── 02_frequentation_import.R  ← Fréquentation journalière, top arrêts/lignes
│   ├── 03_evolution_temporelle.R  ← Évolution 2016-2026, récupération COVID par type
│   ├── 04_heatmap_horaire.R       ← Heatmap heure × jour, T-003
│   ├── 05_profil_journalier.R     ← Profils par jour, T-001, T-005, T-005b
│   ├── 06_saisonnalite.R          ← Décomposition STL
│   ├── 07_impact_covid.R          ← T-002, T-004, T-004b, T-006
│   ├── 08_collisions.R            ← Collisions, carte Leaflet, taux normalisé
│   └── 09_tests_statistiques.R    ← T-007 Gini, T-008, T-009, AM-001 à AM-005
│
├── data/
│   ├── raw/                       ← Données brutes (.rds) — non versionnées
│   └── processed/                 ← Données traitées — non versionnées
│
├── outputs/                       ← Graphiques et cartes — non versionnés
└── README.md                      ← Ce fichier
```

---

## 🔬 Tests statistiques réalisés

| # | Hypothèse testée | Méthode choisie | Pourquoi cette méthode | Résultat |
|---|-----------------|-----------------|----------------------|---------|
| T-001 | Les jours de semaine ont-ils des fréquentations différentes ? | Kruskal-Wallis + Dunn Bonferroni | Normalité violée (5 groupes) | Lundi seul diffère — η²=1.1% |
| T-002 | NORMAL vs VACANCES | Mann-Whitney unilatéral | 2 groupes indépendants, non normaux | -28.2%, r=0.549 (grand effet) |
| T-003 | Le pic du soir (17h) dépasse-t-il le pic du matin (8h) ? | Wilcoxon apparié unilatéral | Mesures liées (même jour), direction fixée a priori | r=0.866, p<10⁻²²⁵ — très grand effet |
| T-004 | COVID = rupture structurelle dans la série brute ? | Chow + CUSUM + Bai-Perron | Tests complémentaires (rupture connue, stabilité, détection auto) | 3 ruptures : fév. 2020, août 2021, fév. 2023 |
| T-004b | La rupture persiste-t-elle dans les résidus STL ? | Chow + CUSUM sur résidus | Distinguer choc transitoire de changement de régime | Rupture absente → COVID = choc transitoire |
| T-005 | Le mercredi a-t-il un profil horaire distinct ? | KW par heure + Bonferroni + BH | Tests multiples (19 heures), deux niveaux de correction | 8/19 heures sig. Bonferroni — bosse 11h-15h |
| T-005b | Matrice complète : chaque jour a-t-il une identité horaire ? | KW 5×19 + Bonferroni + BH | Exhaustivité — tester toutes les combinaisons | 52/95 sig. Bonferroni (54.7%) |
| T-006 | La fréquentation pré et post-COVID est-elle identique ? | Mann-Whitney bilatéral | Pas de direction a priori, 2 périodes indépendantes | p=0.506, r=0.067 — récupération complète |
| T-007 | Le trafic est-il très concentré sur quelques arrêts ? | Gini + bootstrap IC | Mesure de concentration standard, IC sans hypothèse de normalité | Gini=0.843, IC [0.811 ; 0.872] |
| T-008 | Les km produits prédisent-ils les montées par ligne ? | Pearson + Spearman | Pearson (linéaire) + Spearman (monotone) pour comparaison | r=0.924 — trams = efficience structurelle |
| T-009 | La gratuité jeunes a-t-elle augmenté la fréquentation ? | Mann-Whitney par type de ligne | Segmentation nécessaire — effet dilué sur le réseau global | SECONDAIRE +13.1% p=0.004 ✅ |

---

## 📊 Datasets utilisés

Source : [opendata.tpg.ch](https://opendata.tpg.ch)

| Dataset | Période | Granularité | Lignes |
|---------|---------|-------------|--------|
| Arrêts du réseau | Historique | Par arrêt | 4 634 |
| Fréquentation journalière | Avr. 2023 – Fév. 2026 | Jour × arrêt × ligne | 1 668 267 |
| Fréquentation mensuelle | Jan. 2016 – Fév. 2026 | Mois × arrêt × ligne | 489 657 |
| Fréquentation horaire | Jan. 2019 – Avr. 2026 | Heure × jour | 58 723 |
| Kilomètres produits | Jan. 2016 – Avr. 2026 | Jour × ligne | 272 345 |
| Collisions avec tiers | Jan. 2015 – Avr. 2026 | Par événement | 9 975 |

---

## 🚀 Reproduire l'analyse

### Prérequis

R 4.5+ et RStudio.

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
4. Exécuter les scripts dans l'ordre — de `00_exploration_generale.R` à `09_tests_statistiques.R`
5. Le script `00` télécharge les données depuis l'API TPG et les sauvegarde en `.rds` — les sessions suivantes chargent depuis le disque en quelques secondes

---

## 🛠️ Stack technique

| Outil | Usage |
|-------|-------|
| **R 4.5** | Analyse statistique, visualisations, cartographie |
| **ggplot2** | Graphiques statiques |
| **leaflet** | Cartes interactives |
| **strucchange** | Tests de rupture structurelle (Chow, CUSUM, Bai-Perron) |
| **dunn.test** | Test post-hoc de Dunn (non paramétrique) |
| **Python 3.11** | Visualisations finales *(à venir)* |
| **Power BI** | Dashboard interactif *(à venir)* |
| **ML/clustering** | Typologies d'arrêts, prévision fréquentation *(à venir)* |

---

## 📚 Contexte et événements clés

| Date | Événement | Impact dans les données |
|------|-----------|------------------------|
| Déc. 2019 | Mise en service du Léman Express | Réorganisation du réseau — disparition des types REGIONAL et REGIONAL COMMUNE |
| Mars 2020 | Premier confinement COVID | Plancher à 3.48M montées en avril (-80.6%) |
| Juin 2021 | Sortie de crise | Rupture structurelle permanente sur les lignes SCOLAIRE |
| Déc. 2023 | Intégration du réseau Noctambus | Disparition du type NOCTAMBUS REGIONAL dans les données |
| Jan. 2025 | Gratuité jeunes | +13.1% sur les lignes SECONDAIRE (p=0.004) |

**Sources :**
- TPG Open Data : [opendata.tpg.ch](https://opendata.tpg.ch)
- Rapports annuels TPG : [tpg.ch](https://www.tpg.ch/fr/nous-connaitre/publications/rapports-annuels)
- SITG (données géographiques genevoises) : [sitg.ch](https://sitg.ch) *(Phase 3)*

---

## 👤 Auteur

**Frat DAG** — Statisticien et Data Analyst, Genève

---

*Projet en cours. Phase 3 (efficience des lignes, croisements géographiques SITG, clustering) et visualisations finales Python/Power BI à venir.*
