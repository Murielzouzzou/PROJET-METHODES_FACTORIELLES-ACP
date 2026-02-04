# PROJET-METHODES_FACTORIELLES-ACP
Ce projet consistait à travailler seul ou en binôme avec une eprsonne pour mettre en pratique nos connaissance théoriques sur le cours des méthodes factorielles. Pour ma part, j'ai travaillé toute seule.  j’ai travaillé sur un jeu de données provenant de Kaggle qui porte sur les habitudes de  sommeil de 373 individus. 
L’objectif de cette étude était de mieux comprendre les profils de sommeil à 
partir de ces données, pour identifier des comportements ou facteurs associés 
à un bon ou un mauvais état de santé. La question centrale était : quels sont 
les profils types de sommeil et de santé dans cette population, et 
comment sont-ils influencés par des facteurs comme le genre ou l’IMC 
? En d’autres termes, est-ce qu’il existe des groupes de personnes ayant des 
habitudes similaires en matière de sommeil, d’activité et de santé, et ces 
groupes sont-ils liés à des facteurs comme le sexe ou le poids ? 
Pour répondre à cette question, j’ai utilisé une méthode statistique appelée 
"Analyse en Composantes Principales", qui permet de simplifier un grand nombre de données complexes en 
quelques grandes catégories, tout en conservant l’essentiel. Cette méthode a permis de dégager deux axes 
principaux qui expliquent près de 74 % des différences observées dans les données. Le premier axe oppose une 
bonne qualité de sommeil et un faible stress à un stress élevé et une fréquence cardiaque plus élevée. Le second 
axe met en lumière l’opposition entre un mode de vie actif, avec beaucoup de pas quotidiens et d’activité physique, 
et un mode de vie plus sédentaire. 

# 😴 Analyse des habitudes de sommeil — ACP & Profils de santé

## 🎯 Objectif du projet
Projet individuel réalisé dans le cadre du BUT Science des Données.  
L’objectif : **identifier des profils types de sommeil et de santé** à partir d’un jeu de données de 373 individus (Kaggle), en utilisant une **Analyse en Composantes Principales (ACP)** et en intégrant des facteurs sociodémographiques (genre, IMC).

Ce projet explore les liens entre **sommeil, activité physique, stress et signes vitaux**, et montre comment l’ACP permet de simplifier un jeu de données complexe pour révéler des comportements de santé.

---

## 🛠️ Compétences mobilisées
- **Préparation et nettoyage de données** : centrage-réduction, gestion des variables qualitatives, transformation factorielle.
- **Analyse multivariée** :  
  - ACP sur variables quantitatives  
  - Interprétation des axes factoriels  
  - Intégration de variables qualitatives (genre, IMC)
- **Datavisualisation** :  
  - Graphiques des variables  
  - Graphiques des individus  
  - Représentation des groupes (genre, IMC)
- **Interprétation statistique** : identification de profils, analyse des comportements de santé.
- **Communication scientifique** : rédaction d’un rapport structuré, vulgarisation des résultats.

---

## 📂 Contenu du projet
- **Base de données Kaggle** : sommeil, activité physique, stress, fréquence cardiaque, pression artérielle, pas quotidiens.
- **ACP complète** :  
  - centrage-réduction  
  - extraction des axes  
  - visualisation des contributions
- **Analyse sociodémographique** :  
  - comparaison hommes/femmes  
  - comparaison selon l’IMC (normal, surpoids, obèse)
- **Interprétation des profils** :  
  - Profil “reposé/actif”  
  - Profil “sous tension”

---

## 📊 Résultats clés
- Les deux premiers axes expliquent **près de 74 % de la variance totale**.  
- **Axe 1 (47 %) :** opposition entre bonne qualité de sommeil / faible stress et stress élevé / fréquence cardiaque élevée.  
- **Axe 2 (27 %) :** opposition entre mode de vie actif (beaucoup de pas, activité physique) et mode de vie sédentaire.  
- Les personnes en **surpoids/obésité** sont plus souvent associées à :  
  - un stress plus élevé  
  - une activité physique plus faible  
  - une fréquence cardiaque plus importante  
- Hommes et femmes se répartissent globalement de manière homogène.

---

## 🧠 Ce que ce projet démontre
- Capacité à **mener une analyse statistique complète**, de la préparation des données à l’interprétation.  
- Maîtrise des **méthodes multivariées** et de leur visualisation.  
- Intérêt pour les **données de santé** et compréhension des enjeux liés au sommeil et au bien‑être.  
- Aptitude à **communiquer clairement** des résultats complexes.  
- Rigueur dans la **sélection des variables** et la justification méthodologique.

---

## 📁 Organisation du dépôt
```
📦 ACP-Sommeil-Sante
 ┣ 📄 README.md
 ┣ 📊 data/
 ┃   ┗ sleep_health_dataset.csv
 ┣ 📈 visualisations/
 ┃   ┗ *.png
 ┗ 📘 rapport/
     ┣ Note_methodologique.pdf
     ┗ Rapport_ACP.pdf
```

---

## 🔗 Source des données
Dataset Kaggle : *Sleep Health and Lifestyle Dataset*  
`https://www.kaggle.com/datasets/uom190346a/sleep-health-and-lifestyle-dataset` [(kaggle.com in Bing)](https://www.bing.com/search?q="https%3A%2F%2Fwww.kaggle.com%2Fdatasets%2Fuom190346a%2Fsleep-health-and-lifestyle-dataset")

