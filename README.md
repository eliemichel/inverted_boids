!style = adapt

Inversion de boids
==================




Problèmes d'origine
-------------------

Analyse de foules.

Exemples d'application :

 - Détection de comportements à risque de certains individus
 - Détection de comportement global imprévu (accident)

Méthode : Aproximation de la foule par un comportement inspiré des boids.




Points de recherche
-------------------

### Modèle comportemental général

Règles de simulation :

 - Séparation
 - Cohésion
 - Alignement : Les individus ont tenance à aller dans la même direction.
 - Suivi : Un individu peut avoir tendance à en suivre un autre.
 - Un peu d'aléa
 - Classes d'individus qui ont des règles internes (ex : regroupement au sein du groupe mais éloignement des autres)
 - Comportement : matrice

Poids **constants** entre ces différentes règles défini pour chaque individu.

Combinaision des différentes règles : linéaire ?

### Inférence des paramètres individuels

Entrée : position succésive de tous les agents.

 - Trouver comment corriger les paramètres (corriger étape par étape)
 - Correction par approximation successives : On calcule une étape de simulation avec les paramètres actuels et on compare à l'évolution réelle pour corriger les paramètres. Difficulté : Trouver la bonne fonction de correction. 
 - Détection de grandeurs statistiques caractéristiques (ex : matrice des distances moyennes de toutes les paires d'agents, etc)

Informations *a priori* :

 - Environnement géographique connu (routes, murs, etc)
 - Événements connus (points d'attrait, etc)

### Caractérisation des paramètres

 - Traduire en comportement qualitatif les paramètres utilisés pour la simulation (ex : comportement louche, hésitation, etc)





Protocole
---------

Premiers essais : Le même moteur génére puis analyse des mouvements

Essais avec des relevés réels : Vérifier que le modèle a bien du sens par rapport à des mouvements réels
^ Comment trouver des données ?

Utiliser ces données réelles et le sentiment que l'on a en regardant certains agents pour en déduire des règles de traduction paramètres -> comportement qualitatif.

Vérifier le degré d'erreur expérimental.

Donner des arguments un peu plus mathématiques, notamment pour la convergence de l'inférence des paramètres.


Références bibliographiques
---------------------------

Bernard Chazelle



