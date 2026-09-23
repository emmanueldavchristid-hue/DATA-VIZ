# BRFSS Interactive Health Dashboard

## Présentation

Ce projet consiste en la conception et le développement d’un tableau de bord interactif dédié à l’exploration, à l’analyse et à la visualisation des données du **Behavioral Risk Factor Surveillance System (BRFSS)**, une enquête américaine portant sur les comportements, les facteurs de risque et les principaux indicateurs liés à la santé de la population.

Développée avec **R et Shiny**, l’application propose une interface interactive permettant d’explorer les données selon différentes dimensions sociodémographiques, sanitaires et comportementales. L’objectif est de faciliter l’identification des tendances, des disparités et des relations entre les caractéristiques individuelles et les indicateurs de santé.

## Fonctionnalités

### Tableau de bord

La page d’accueil présente une synthèse des principaux indicateurs issus des données BRFSS, notamment le nombre d’observations, l’état de santé perçu, la couverture par une assurance santé et la pratique d’une activité physique.

### Analyse de la population

Cette section permet d’examiner la structure de la population selon plusieurs caractéristiques sociodémographiques :

* sexe ;
* âge ;
* niveau d’éducation ;
* niveau de revenu ;
* état de santé perçu ;
* répartition de la population selon l’âge et le sexe.

### Accès aux soins

L’application analyse les relations entre les caractéristiques socio-économiques et l’accès aux soins, notamment :

* la couverture par une assurance santé ;
* la relation entre revenu et couverture d’assurance ;
* l’état de santé perçu selon le statut d’assurance.

### Comportements et facteurs de risque

Cette partie s'intéresse à plusieurs facteurs comportementaux et indicateurs de santé, notamment :

* la pratique d’une activité physique ;
* le tabagisme ;
* l’indice de masse corporelle (IMC) ;
* les variations de ces indicateurs selon les caractéristiques sociodémographiques.

### Analyses croisées

Des analyses multivariées permettent d'étudier les interactions entre plusieurs variables et d'explorer notamment les relations entre l'âge, l'activité physique et l'état de santé perçu.

### Visualisation avancée

L’application intègre également des techniques avancées de composition graphique permettant d'organiser plusieurs visualisations au sein d'une même mise en page et de construire des représentations adaptées à l'exploration interactive des données.

### Analyse de la structure interne de ggplot2

Une partie spécifique du projet est consacrée à l'exploration de la structure interne des objets graphiques produits avec **ggplot2**, notamment à travers l'analyse et la manipulation des objets de type `gtable` et `grob`.

### Géométrie personnalisée avec ggproto

Le projet comprend également la création d'une géométrie personnalisée avec `ggproto`. La fonction `geom_labeled_bar` permet de produire des graphiques en barres intégrant directement les valeurs au sein des représentations graphiques, en s'appuyant notamment sur les primitives graphiques de `grid` telles que `rectGrob`, `textGrob` et `gTree`.

## Interface interactive

L’application dispose d’une interface graphique personnalisée intégrant un système de filtres permettant d'explorer dynamiquement les données selon :

* l’année ;
* l’indicateur de santé ;
* les variables sociodémographiques ;
* les comportements étudiés.

L'interface repose également sur un design personnalisé inspiré du **glassmorphism**, avec des effets de transparence, des animations et une organisation visuelle destinée à améliorer l'expérience d'exploration.

## Technologies utilisées

* **R**
* **Shiny**
* **ggplot2**
* **dplyr**
* **tidyr**
* **grid**
* **gtable**
* **ggproto**
* **CSS**

## Contexte académique

Projet réalisé dans le cadre du **Master 2 Data Science**, avec un objectif centré sur la visualisation interactive, l'analyse exploratoire des données et l'utilisation avancée de l'écosystème graphique de R.

## Structure du projet

```text
DATA-VIZ/
│
├── app.R
├── README.md
│
├── data/
│   ├── 2014_light.csv
│   ├── 2015_light.csv
│   └── 2015_formats.json
│
├── data_analyse.csv
├── data_model.csv
│
├── rapport_SHINY_M2.pdf
│
└── rsconnect/
    └── shinyapps.io/
```

## Objectif

Au-delà de la présentation graphique des données, ce projet vise à démontrer la capacité à construire une application de visualisation complète, combinant **préparation des données, analyse exploratoire, visualisation statistique, interactivité Shiny et personnalisation avancée des graphiques avec R**.
