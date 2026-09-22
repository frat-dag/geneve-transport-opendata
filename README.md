# Données ouvertes et transport public à Genève : une analyse statistique

Analyse statistique des données ouvertes de fréquentation, d'offre et de collisions du réseau de transports publics à Genève, de 2016 à juin 2026.

> Source : transports publics genevois (tpg), état en date du 18.09.2026. Ce travail n'est pas la propriété des tpg.
>
> Couches géographiques : Source : Système d'information du territoire à Genève (SITG), extrait en date du 20.03.2026 (communes, secteurs) et du 21.08.2026 (emprise du lac).

Le projet part d'une règle : rien n'est affirmé avant d'avoir été calculé, et chaque résultat est publié avec sa taille d'effet, son incertitude et ses limites. Tous les chiffres de ce document proviennent du registre de résultats produit par les scripts (`resultats/resultats_2026-09-18.csv`) ou des tableaux enregistrés dans `resultats/`. Aucun n'est recopié à la main.

---

## Lexique

Trois tableaux pour rendre la suite lisible sans connaissance préalable du réseau ni des statistiques.

**Ce qui est compté**

| Terme | Sens dans ce projet |
|---|---|
| Montée | Un embarquement dans un véhicule. Une personne qui change de véhicule compte deux montées : ce sont des embarquements, pas des voyageurs. |
| Km produits | Kilomètres parcourus par les véhicules en service. C'est la mesure de l'offre. |
| Montées par km | Fréquentation rapportée à l'offre. Ne dit rien du remplissage : la taille des véhicules n'entre pas dans le calcul. |
| Lieu d'arrêt | Un nom d'arrêt, tous ses quais regroupés. Un même nom peut couvrir jusqu'à douze quais. |
| Instantané | Copie figée des données, téléchargée à une date donnée et jamais relue ensuite. Celui-ci date du 18.09.2026. |

**Catégories du réseau, telles qu'elles figurent dans les données**

| Terme | Sens |
|---|---|
| PRINCIPAL, SECONDAIRE | Classement des lignes régulières par l'opérateur. Les lignes principales desservent les axes les plus denses. |
| GLCT | Lignes transfrontalières. |
| SCOLAIRE | Lignes C1 à C9, qui ne circulent que les jours d'école. |
| NOCTAMBUS REGIONAL | Lignes régionales de nuit, remplacées le 10.12.2023 par le prolongement nocturne des lignes de jour. |
| REGIONAL, REGIONAL COMMUNE | Types disparus en janvier 2020, regroupés avec SECONDAIRE pour comparer sur dix ans. |
| SITG | Système d'information du territoire à Genève, source des couches géographiques (communes, secteurs, lac). |

**Termes statistiques employés dans les résultats**

| Terme | Ce qu'il veut dire |
|---|---|
| Taille d'effet | L'importance pratique d'un écart, indépendamment du nombre d'observations. Avec beaucoup de données, un écart minuscule peut être « significatif » sans être important. |
| IC 95 % | Intervalle de confiance : la plage de valeurs compatibles avec les données. S'il contient zéro, l'écart n'est pas établi. |
| Hodges-Lehmann (HL) | Estimation robuste de l'écart entre deux groupes, associée aux tests de Mann-Whitney et de Wilcoxon. |
| p-value | Probabilité d'observer un tel écart si rien ne se passait. Elle suppose des observations indépendantes, ce que des jours ou des mois successifs ne sont pas : elle est souvent absente ici, volontairement. |
| Autocorrélation | Le fait qu'une observation ressemble à la précédente. Un mardi chargé suit souvent un lundi chargé. |
| Gini | Concentration, entre 0 (tout le monde à égalité) et 1 (tout concentré sur un seul élément). |
| STL | Décomposition d'une série en trois morceaux : tendance, saisonnalité et reste. |
| Bai-Perron | Méthode qui cherche combien de ruptures de niveau contient une série, et à quelles dates. |
| Newey-West, HC3 | Erreurs types robustes : la première à l'autocorrélation, la seconde à l'hétérogénéité des variances. |
| Placebo | Le même calcul appliqué là où l'événement étudié n'a pas eu lieu, pour voir à quoi ressemble une variation ordinaire. |
| Élasticité | De combien de pour cent varie une grandeur quand une autre varie de 1 %. |

---

## Données et périmètre

### Jeux de données

| Jeu | Contenu | Période retenue | Utilisé dans |
|---|---|---|---|
| Arrêts | Référentiel des arrêts, coordonnées, statut actif | État au 18.09.2026, 4 655 lignes | 01, 11, 12, 15 |
| Montées journalières | Jour x ligne x arrêt | 02.2023 à 06.2026, 4 581 814 lignes reçues | 02, 09, 12, 13, 14, 15 |
| Montées mensuelles | Mois x ligne x arrêt | 01.2016 à 06.2026, 514 799 lignes reçues | 02, 03, 06, 07, 09, 10, 11, 13, 14 |
| Fréquentation horaire | Jour x tranche horaire, réseau entier | 01.2019 à 06.2026, 65 479 lignes reçues | 04, 05, 07, 14 |
| Montées par tranche horaire, arrêt et ligne | Jour x heure x ligne x arrêt | 03.2024 à 06.2026, 581 660 lignes reçues | 00b (contrôle) |
| Kilomètres produits | Jour x ligne | 01.2016 à 06.2026, 287 941 lignes reçues | 08, 09, 10, 13 |
| Collisions avec tiers | Par événement | 01.2015 à 06.2026, 10 359 lignes reçues | 08, 11, 13 |
| SITG, population par commune | 45 communes | Population au 12.2025, extrait du 20.03.2026 | 12, 15 |
| SITG, population par secteur | Secteurs de la Ville de Genève | Population au 12.2025, extrait du 20.03.2026 | 12 |
| SITG, emprise du lac (GEO_LAC) | Léman, Rhône, Arve | Extrait du 21.08.2026 | 12, 15 |

Source tpg : [opendata.tpg.ch](https://opendata.tpg.ch). Source SITG : [sitg.ch](https://sitg.ch).

Chaque téléchargement est consigné dans un manifeste (nombre de lignes reçues contre nombre annoncé par l'interface, empreinte SHA-256 de chaque fichier). Les sept jeux tpg sont arrivés complets.

### Traitements appliqués aux données

Les conditions d'utilisation des deux sources demandent de signaler les traitements. Ceux de ce projet sont les suivants.

- **Instantané figé.** Toutes les données tpg ont été téléchargées le 18.09.2026 et ne sont plus relues depuis l'API. Le portail étant une source vivante, une nouvelle extraction peut donner des chiffres légèrement différents.
- **Données définitives et coupure.** Seules les lignes marquées définitives sont retenues, et l'analyse s'arrête au 30.06.2026 (dernière date où les jeux concordent, vérifiée par `00b_verification_definitif.R`).
- **Lecture des valeurs manquantes.** Les fichiers sont lus avec `na = ""`. Sans ce réglage, le nom de la ligne de nuit « NA » était pris pour une valeur manquante et ses montées disparaissaient.
- **Lignes 301 et 302.** Leurs montées divergent fortement entre le jeu mensuel et le jeu journalier en janvier et février 2025. Les résultats qui couvrent cette période sont aussi donnés sans ces deux lignes.
- **Types de ligne.** Le type de chaque ligne est celui fourni dans les données. Certaines lignes changent de type au cours du temps ; les scripts qui en dépendent retiennent une règle écrite (type le plus récent ou le plus fréquent) et la signalent. Les types REGIONAL et REGIONAL COMMUNE, qui disparaissent en janvier 2020, sont regroupés avec SECONDAIRE dans le script 03.
- **Mode de transport.** Le mode (tram, trolleybus, autobus) est déduit de la catégorie de véhicule du jeu des collisions ; les lignes absentes de ce jeu sont classées autobus.
- **Surfaces communales.** Les polygones SITG des communes riveraines incluent leur part du lac. Pour les surfaces, les densités et l'affichage des cartes, le lac, le Rhône et l'Arve sont retirés avec la couche GEO_LAC. Les arrêts restent rattachés aux communes par les polygones complets, pour qu'un arrêt situé sur un pont reste dans sa commune.
- **Géométries.** Quelques géométries SITG invalides sont réparées avec `st_make_valid()`, avec un comptage avant et après.

### Ce que les données ne contiennent pas

- Les navettes lacustres (Mouettes genevoises) ne figurent dans aucun jeu : les 143 lignes du jeu mensuel relèvent toutes du réseau terrestre. Le nom d'arrêt « Carouge, Mouettes » est une rue de Carouge, sans rapport.
- La catégorie NOCTAMBUS REGIONAL ne contient que les douze lignes régionales de nuit. Les lignes urbaines du Noctambus n'y sont pas.
- Les montées comptent des embarquements, pas des voyageurs : une personne qui change de véhicule est comptée deux fois.
- Les montées publiées ne sont pas des nombres entiers (par exemple 209,51 sur un mois et une ligne). Ce sont donc des estimations, mais la méthode de comptage n'est pas documentée dans le jeu.
- Aucune donnée sur la capacité des véhicules, l'infrastructure (site propre), les coûts, les origines et destinations des voyageurs, ni le revenu des communes.
- La population SITG est celle de décembre 2025, les montées vont jusqu'à juin 2026 : les deux dates ne coïncident pas.

---

## Résultats

### 1. Un réseau très concentré

Sur les douze derniers mois (07.2025 à 06.2026), les 931 lieux d'arrêt (tous quais d'un même nom regroupés) ont un indice de Gini de **0,828**, IC 95 % bootstrap [0,791 ; 0,861]. Les 10 % de lieux les plus fréquentés concentrent **72,2 %** des montées (T-007).

Les 23 lignes de type PRINCIPAL portent 84,8 % des montées sur la période du jeu journalier.

![Courbe de Lorenz](figures/09_lorenz_arrets.png)

### 2. La semaine, la journée et l'année

Un jour de semaine ordinaire compte en médiane **741 767 montées**, contre 534 304 un jour de vacances scolaires, soit **-28 %** (T-002).

Sur 1 443 jours de semaine ordinaires, 17h dépasse 8h dans **99,9 %** des cas, de 13 748 montées selon l'estimateur de Hodges-Lehmann (T-003). Ce résultat porte sur le réseau entier ; il n'a pas été vérifié ligne par ligne.

Les cinq jours ouvrables diffèrent peu en volume : le jour explique 1,2 % de la variance (T-001). Le mercredi se distingue par son profil horaire. Comparé aux autres jours de sa propre semaine, il diffère à 17 heures sur 19 : **+15,4 %** à 12h, **+16,3 %** à 14h, **-11,6 %** à 16h, -5,9 % à 8h (T-005).

La saisonnalité est marquée : mars et novembre dépassent la tendance d'environ 1,7 million de montées, juillet et août sont en dessous de 2,6 et 2,3 millions. Le profil saisonnier est quasiment identique avant 2020 et depuis 2022 (corrélation 0,994).

![Écart du mercredi](figures/05_ecart_mercredi.png)

### 3. La pandémie et le niveau actuel

La série mensuelle désaisonnalisée (01.2016 à 06.2026) présente trois ruptures retenues par le BIC ; les segments se terminent en février 2020, août 2021 et février 2023 (T-004b). Le plancher est avril 2020, avec 3,48 millions de montées.

Le dernier segment, depuis mars 2023, se situe **+4,21 %** au-dessus du niveau d'avant 2020, IC 95 % [0,91 ; 7,63] avec une erreur type robuste à l'autocorrélation (T-006). À géographie constante (1 172 quais desservis tous les mois des deux périodes), l'écart est de +1,97 %, IC [-1,17 ; 5,22] : il n'est pas établi. Le niveau d'avant la pandémie est retrouvé ; le léger dépassement du réseau complet tient en partie aux quais nouveaux.

![Ruptures structurelles](figures/07_ruptures_structurelles.png)

### 4. L'offre a crû plus vite que la fréquentation

De 2016 à 2025, les montées augmentent de **9 %** et les kilomètres produits de **27 %**. Chaque kilomètre porte donc **13,8 %** de montées en moins (T-013d).

La décomposition attribue 6,7 points au déplacement des kilomètres vers des types de lignes moins chargés, 5,4 points à une baisse à l'intérieur des types, et 1,6 point aux types disparus. Sur les 40 lignes présentes en 2016 comme en 2025, la baisse est de 6,8 %. Les données ne permettent pas d'en identifier la cause.

Sur la période récente (02.2023 à 06.2026), le rapport est stable : -1,3 % sur 41 mois (T-013c). En vacances, chaque kilomètre porte 6,67 montées contre 7,83 en période scolaire : la fréquentation baisse plus que l'offre (T-013a).

![Montées par kilomètre](figures/13_ratio_temporel.png)

### 5. Gratuité pour les jeunes (janvier 2025) : pas d'effet visible sur le total

Entre 2024 et 2025, mois par mois, les montées augmentent de 4,61 % et les kilomètres de 5,33 % : les montées par kilomètre baissent de 0,57 %. L'année témoin 2023 à 2024, sans gratuité, donne -2,41 % ; l'écart est de 1,84 point (T-009).

Pour juger si cet écart sort de l'ordinaire, le même calcul a été fait sur les autres paires d'années hors pandémie : elles varient de -2,41 % à +0,48 %, et l'année de la gratuité arrive au rang 3 sur 5. À type de ligne constant, l'évolution est de +0,60 % contre +0,06 % l'année témoin. Aucun effet n'est détectable sur le total du réseau.

Deux réserves : l'offre a été renforcée le 15.12.2024, seize jours avant la gratuité, et les données mensuelles ne séparent pas les deux ; la mesure vise les jeunes, que ces données ne distinguent pas des autres voyageurs. Ce résultat ne dit donc rien de l'effet sur les jeunes eux-mêmes.

### 6. Lignes scolaires : une baisse de périmètre, pas d'usage

De 2016 à 2025, les montées des lignes scolaires baissent de **37 %** et le nombre d'arrêts desservis de **34 %**. Les montées par arrêt desservi ne baissent que de 4 % (AM-002). La baisse suit la réduction du service.

Sur les douze derniers mois, huit lignes (C1, C3 à C9) desservent 275 arrêts et portent 0,18 % des montées du réseau. Le nombre d'arrêts scolaires d'une commune est associé à sa population de 0 à 19 ans, rho = 0,517, IC 95 % [0,225 ; 0,741] (T-015a). 24 communes sur 45 n'ont pas d'arrêt scolaire ; toutes sauf Céligny sont desservies par le réseau régulier.

![Lignes scolaires](figures/09_scolaire_perimetre.png)

### 7. Lignes et modes

Les lignes PRINCIPAL portent en médiane **8,25** montées par kilomètre, les lignes SECONDAIRE **1,68** (Hodges-Lehmann 6,46, IC 95 % [5,10 ; 7,76], T-010). Cet écart retrouve en partie le critère de classement des lignes, qui suit la densité desservie. La dispersion interne est large : de 2,43 à 24,48 chez PRINCIPAL, de 0,08 à 9,13 chez SECONDAIRE.

Par mode, les médianes sont de 6,43 montées par kilomètre pour le tram (5 lignes), 3,15 pour le trolleybus (6) et 0,85 pour l'autobus (101), T-013b. Une rame de tram offre plusieurs fois la capacité d'un autobus, et les trams desservent les axes les plus denses : l'écart ne mesure ni le remplissage ni une efficacité propre au mode.

![Montées par km selon le mode](figures/13_ratio_par_mode.png)

### 8. Collisions : le nombre suit l'offre

De 2016 à 2025, le nombre de collisions avec tiers augmente de 29,6 %, les kilomètres produits de 26,9 %, et le taux par million de kilomètres de **2,1 %** seulement. L'année au taux le plus élevé est 2019 (37,6 par million de km). 2025, année au nombre le plus élevé, arrive au rang 5 sur 10 en taux (OBS-COLLISIONS). Calculé sur les seules lignes présentes dans le jeu des collisions, le taux augmente de 3,9 % et 2025 passe au rang 3.

La part de collisions avec blessé ne varie pas selon la saison : 7,87 % au printemps et en été, 8,15 % en automne et en hiver, V de Cramer 0,0048 (T-011).

97,5 % des collisions se produisent à moins de 200 m d'un arrêt actif. Le classement des arrêts par nombre de collisions et celui rapporté aux montées ne se recoupent qu'en partie (corrélation de rang 0,664). Aucun des deux ne mesure la dangerosité : le bon dénominateur serait le nombre de passages de véhicules, qui n'est pas publié par arrêt.

![Collisions et taux](figures/08_collisions_annuel.png)

### 9. Couverture du territoire

Pour les 44 communes qui ont au moins un arrêt actif, le nombre d'arrêts se modélise par la population et la surface terrestre (R² = 0,923). Les élasticités sont de **0,509** pour la population, IC 95 % [0,453 ; 0,566], et de **0,520** pour la surface, IC [0,388 ; 0,652]. Leur somme, 1,029 [0,908 ; 1,15], est compatible avec 1 : le nombre d'arrêts par habitant dépend alors surtout de la densité (T-012).

Deux communes ont nettement moins d'arrêts que ne le prévoit le modèle : Troinex et Versoix (résidus studentisés -2,06 et -2,24). C'est un repérage, pas un test : sur 44 communes, environ deux dépassements sont attendus par hasard. Le modèle ne mesure ni la fréquence des passages ni la demande.

Les montées rapportées à la population ne mesurent pas l'usage par les habitants : une montée dans une gare centrale est le fait de voyageurs venus de tout le canton.

![Couverture en arrêts](figures/12_carte_couverture.png)

### 10. Le réseau de nuit

Le 10.12.2023, le Noctambus est remplacé par le prolongement nocturne des lignes de jour. De janvier à novembre, 2023 contre 2024, les montées entre 1h et 4h des nuits du vendredi et du samedi augmentent de **17,1 %** par week-end, contre 3,1 % en journée. Rapporté à l'année témoin, l'écart est de 16,8 points. Appliqué à chaque heure de 6h à 23h, le même calcul ne dépasse jamais 2,5 points : la nuit arrive au rang 1 sur 19 (T-014b).

Sur les années civiles, le gain de 2024 est de 109 820 montées entre 1h et 4h, ce qui retrouve les 110 000 voyageurs supplémentaires annoncés par les tpg. Le pourcentage diffère : 16,6 % ici contre 21,6 % annoncé, la base de l'annonce ne pouvant pas être reconstituée avec ces données.

Avant la pandémie, le Noctambus régional baissait déjà : 2019 est à -15,7 % de 2016, Spearman rho = -0,55 (T-014a). En 2022, dernière année complète avant son remplacement, il était à -41,9 % de 2019.

![Montées de nuit](figures/14_nuit_1h_4h.png)

---

## Limites transversales

- **Autocorrélation.** Les séries journalières et mensuelles sont fortement autocorrélées. Les p-values des tests qui supposent l'indépendance sont trop optimistes : elles ne sont pas publiées, au profit des tailles d'effet et des intervalles. Quand c'est possible, une erreur type robuste (Newey-West, HC3) ou une taille d'échantillon effective est calculée.
- **Capacité.** Le rapport montées par kilomètre ne tient pas compte de la taille des véhicules. Il mesure l'usage par kilomètre offert, pas le remplissage.
- **Causalité.** Aucun résultat de ce projet n'établit une cause. Les comparaisons avant et après un événement contrôlent ce qu'elles peuvent (année témoin, placebo, périmètre constant) et disent ce qu'elles ne contrôlent pas.
- **Descriptif ou test.** Certains résultats sont purement descriptifs (ligne 10, communes atypiques). Ils sont signalés comme tels.

---

## Tests et observations

| ID | Question | Méthode | Résultat principal | Script |
|---|---|---|---|---|
| T-001 | Les jours ouvrables diffèrent-ils en volume ? | Kruskal-Wallis, post-hoc de Dunn (Bonferroni) | eta² = 0,012 ; 5 paires sur 10 | 05 |
| T-002 | Jours ordinaires contre vacances | Mann-Whitney | -28 % ; HL 188 160 montées/jour | 07 |
| T-003 | 17h dépasse-t-il 8h ? | Wilcoxon apparié | HL 13 748 ; 99,9 % des jours | 04 |
| T-004b | Ruptures de la série | Bai-Perron sur série désaisonnalisée | 3 ruptures (02.2020, 08.2021, 02.2023) | 07 |
| T-005 | Profil horaire du mercredi | Wilcoxon apparié intra-semaine, par heure | 17 heures sur 19 | 05 |
| T-005b | Profil horaire de chaque jour | Kruskal-Wallis par jour et par heure | 54 tests sur 95 (Bonferroni) | 05 |
| T-006 | Niveau actuel contre avant 2020 | Régression, erreur type Newey-West | +4,21 % [0,91 ; 7,63] ; quais constants +1,97 % [-1,17 ; 5,22] | 07 |
| T-007 | Concentration entre lieux d'arrêt | Gini, bootstrap | 0,828 [0,791 ; 0,861] | 09 |
| T-008 | Offre et fréquentation par ligne | Spearman, bootstrap | rho = 0,904 [0,83 ; 0,942], relation en partie mécanique | 09 |
| T-009 | Gratuité pour les jeunes | Wilcoxon apparié, année témoin, placebo, décomposition | écart 1,84 point, rang 3 sur 5, pas d'effet détectable | 09 |
| AM-002 | Lignes scolaires | Périmètre annuel | montées -37 %, arrêts -34 %, par arrêt -4 % | 09 |
| T-010 | PRINCIPAL contre SECONDAIRE | Mann-Whitney | médianes 8,25 et 1,68 montées/km | 10 |
| OBS-COLLISIONS | Collisions rapportées à l'offre | Taux par million de km | taux +2,1 % de 2016 à 2025 | 08 |
| OBS-COLLISIONS-ARRETS | Collisions près des arrêts | Arrêt le plus proche, seuil 200 m | corrélation de rang 0,664 | 11 |
| T-011 | Blessés selon la saison | Chi² | V de Cramer 0,0048 | 11 |
| T-012 | Arrêts, population et surface | Régression log-log, erreurs HC3 | élasticités 0,509 et 0,520, R² 0,923 | 12 |
| T-013a | Montées par km en vacances | Mann-Whitney | 6,67 contre 7,83 | 13 |
| T-013b | Montées par km selon le mode | Kruskal-Wallis | tram 6,43, trolleybus 3,15, autobus 0,85 | 13 |
| T-013c | Évolution récente des montées par km | Spearman sur le rang du mois | -1,3 % sur 41 mois | 13 |
| T-013d | Montées par km depuis 2016 | Série annuelle, décomposition, périmètre constant | -13,8 % de 2016 à 2025 | 13 |
| T-014a | Noctambus régional avant la pandémie | supF d'Andrews, Spearman | 2019 à -15,7 % de 2016 ; rho = -0,55 | 14 |
| T-014b | Réseau de nuit de décembre 2023 | Apparié par mois, année témoin, placebo horaire | nuit +17,1 %, jour +3,1 %, rang 1 sur 19 | 14 |
| T-014c | Reprise de la ligne 10 (descriptif) | Rang parmi les lignes principales | -7,5 % contre 2019, rang 5 sur 22 | 14 |
| T-014d | Ligne 10 en vacances (descriptif) | Rang parmi les lignes principales | rapport 0,807, rang 2 sur 22 | 14 |
| T-015a | Arrêts scolaires et population jeune | Spearman, bootstrap | rho = 0,517 [0,225 ; 0,741] | 15 |

Le détail de chaque ligne (effectifs, statistiques, notes) est dans `resultats/resultats_2026-09-18.csv`.

---

## Reproduire l'analyse

### Prérequis

R 4.5 ou plus récent, et les packages suivants :

```r
install.packages(c("here", "httr2", "jsonlite", "digest", "readr",
                   "dplyr", "tidyr", "lubridate", "ggplot2", "scales", "viridis",
                   "strucchange", "sandwich", "sf", "leaflet", "htmlwidgets",
                   "webshot2"))
```

`webshot2` produit la version PNG des deux cartes interactives et demande Chrome ou Edge sur la machine. L'analyse publiée a tourné sous R 4.5.3, Windows 11, fuseau Europe/Zurich.

### Données

1. **Données tpg.** `R/00_download.R` télécharge les sept jeux et les range dans `data/raw/<date du jour>/`, avec un manifeste. L'analyse publiée utilise l'instantané du 18.09.2026 (`SNAPSHOT_ID` dans `R/config.R`). Un nouveau téléchargement crée un nouvel instantané, dont les chiffres peuvent différer légèrement.
2. **Couches SITG.** À télécharger à la main sur [sitg.ch](https://sitg.ch) et à décompresser dans `data/raw/sitg/` : `OCS_POPBATLOG_COMMUNE` dans `communes/`, `OCS_POPBATLOG_VGE_SECTEUR` dans `secteurs/`, `GEO_LAC` dans `lac/`.

### Exécution

`R/run_all.R` lance chaque script dans un processus R neuf, dans l'ordre, et vérifie ensuite que chaque figure et chaque tableau attendu a bien été écrit **pendant cette exécution** : un fichier ancien portant le bon nom ne compte pas. Un script qui dépendrait d'un objet laissé en mémoire par un autre échoue donc ici. Sur l'instantané du 18.09.2026, les 16 scripts passent et toutes les sorties attendues sont produites.

À chaque nouvel instantané, relancer d'abord `00b_verification_definitif.R`, qui vérifie la date jusqu'à laquelle les jeux sont définitifs et concordants.

---

## Structure du dépôt

```
R/
  config.R                         instantané, coupure, dates d'événements, lire(), enregistrer()
  00_palette.R                     couleurs et thème des figures
  00_download.R                    téléchargement des jeux tpg
  00b_verification_definitif.R     contrôle des données définitives
  01_arrets_import.R               arrêts, carte interactive
  02_frequentation_import.R        contrôles du jeu journalier, structure du réseau
  03_evolution_temporelle.R        évolution 2016-2026
  04_heatmap_horaire.R             T-003
  05_profil_journalier.R           T-001, T-005, T-005b
  06_saisonnalite.R                décomposition STL
  07_impact_covid.R                T-002, T-004b, T-006
  08_collisions.R                  collisions et exposition, carte interactive
  09_tests_statistiques.R          T-007, T-008, T-009, AM-002
  10_efficience_lignes.R           T-010
  11_collisions_spatial.R          collisions près des arrêts, T-011
  12_sitg_equite_territoriale.R    T-012
  13_offre_demande.R               T-013a à T-013d
  14_noctambus_ligne10.R           T-014a à T-014d
  15_sitg_scolaire_socioeco.R      T-015a
  run_all.R                        exécution complète et contrôle des sorties
data/
  raw/<instantané>/                jeux tpg (.csv et .rds)
  raw/sitg/                        couches SITG
  processed/<instantané>/          fichiers intermédiaires
figures/                           figures PNG et cartes HTML
resultats/                         registre de résultats et tableaux CSV
logs/                              journaux d'exécution (non publiés)
LICENSE                            licence MIT (code seulement)
```

---

## Méthode de travail

Une première version de cette analyse, datant d'avril 2026, a été entièrement reprise en septembre 2026 : plusieurs de ses conclusions ne résistaient pas à un examen méthodique. Tous les tests ont été réécrits sur un instantané figé des données, et aucun résultat de la version précédente n'a été repris tel quel.

La reprise s'est faite avec l'assistance de Claude (Anthropic). Chaque script a été exécuté deux fois de façon indépendante, dans deux environnements distincts, et n'a été validé que si les deux sorties concordaient. Chaque correction, sa justification et son effet sur les résultats sont consignés dans un registre tenu à part.

Le code est publié sous licence MIT. Les données, elles, restent la propriété de leurs producteurs et sont soumises aux conditions d'utilisation de leurs portails respectifs.

### Suites prévues

- Valider les résultats hors échantillon, sur les données d'octobre 2026 à mars 2027, à chaque nouvel instantané.
- Comparer deux instantanés pour vérifier si des données marquées définitives sont révisées après coup.

---

## Note sur les couleurs

Les couleurs des figures ont été choisies pour ce projet. Elles ne reprennent l'identité visuelle d'aucune entreprise, et ce travail est indépendant de tout opérateur de transport.

---

## Auteur

**Frat DAG**, statisticien, Genève.
