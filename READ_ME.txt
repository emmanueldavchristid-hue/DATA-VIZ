================================================================================
                           READ_ME.txt
                  Projet Master 2 Data Science - DATAVIZ
               Analyse BRFSS - Tableau de bord Shiny interactif
================================================================================

Date de soumission : 20 janvier 2026 (minuit)

Auteurs / Membres du groupe :
- MOUHI Christ-Emmanuel
- N'GUESSAN Yakpa Ludivine
- KOUIHAON Tosseu Larissa

Titre du projet :
Tableau de bord interactif d'analyse des comportements et perceptions de santé à partir des données BRFSS (2014-2015)

Lien de l'application déployée sur shinyapps.io :

https://mouhishinydashboard.shinyapps.io/BRFSS-Analyse-Sante-2026/

================================================================================
INSTRUCTIONS POUR EXÉCUTER L'APPLICATION LOCALEMENT
================================================================================

1. Prérequis
   - R (version 4.0 ou supérieure) installé
   - RStudio recommandé (mais pas obligatoire)

2. Installation des packages nécessaires
   Ouvrir R ou RStudio et exécuter une seule fois :
   install.packages(c("shiny", "shinydashboard", "plotly", "dplyr", "tidyr", 
                      "ggplot2", "DT", "grid", "gridExtra", "scales", 
                      "shinycssloaders"))

3. Lancement de l'application
   - Placer tous les fichiers dans un  dossier :
     - app.R (fichier principal)

On rééchantillonne les CSV directement (plus léger pour shinyapps) :
Ouvre RStudio
Charge et rééchantillonne :Rlibrary(dplyr)
data2014 <- read.csv("C:/Users/Dell/Desktop/DATA VIZ/data/2014.csv") %>%
  slice_sample(n = 50000)
data2015 <- read.csv("C:/Users/Dell/Desktop/DATA VIZ/data/2015.csv") %>%
  slice_sample(n = 50000)
write.csv(data2014, "data/2014_light.csv", row.names = FALSE)
write.csv(data2015, "data/2015_light.csv", row.names = FALSE)

     - data
       - 2014_light.csv (données)
       - 2015_light.csv (données)
   - Dans R/RStudio, définir le répertoire de travail sur ce dossier :
     setwd("C:/chemin/vers/votre/dossier")
   - Lancer l'application :
     shiny::runApp()

4. Accès aux données
   - Les fichiers 2014_light.csv et 2015_light.csv doivent être présents dans le dossier data.

================================================================================
DESCRIPTION 
================================================================================
- Analyse pondérée des données BRFSS 2014-2015
- Tableau de bord interactif avec filtres (année, âge, sexe, éducation, revenu, etc.)
- Visualisations dynamiques (plotly) et composition avancée manuelle (grid/grob)
- Respect des exigences : pas de patchwork/cowplot, géom personnalisé via ggproto

================================================================================
CONTACT
================================================================================
Pour toute question ou problème technique :
- Christ-Emmanuel MOUHI
- Email : christ.mouhi24@inphb.ci
- Téléphone : 0101058267

Bonne lecture et bonne exploration !

================================================================================
Fin du fichier READ_ME.txt
================================================================================