!style = adapt

Inversion de boids
==================


**Attention :** Ce document est écrit dans une version étendue de Markdown permettant d'écrire des expressions mathématiques : [TeXdown](https://github.com/eliemichel/texdown). Vous pouvez en voir une version PDF [ici](http://www.exppad.com/public/cours/inverted_boids/README.pdf) mais elle a moins de chances d'être à jour que ce document.



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


Brainstorming du 28 mars
------------------------

Inverser des boids sur des agents tous différents est difficile à appréhender mathématiquement et l'objectif d'origine demande également des échantillons de vraies valeurs, la capacité à caractériser à partir des paramètres le comportement qualitatif des agents, etc.

Nous allons donc commencer par tester nos méthodes sur des objectifs moins ambitieux en faisant des hypothèses supplémentaires *a priori*. Nous pouvons par exemple supposer que tous les agents ont le même comportement fondamental et tenter simplement de reconnaître les agents (plutôt poissons, plutôt oiseaux, etc).

Un autre sous-objectif consiste à essayer de repérer dans un ensemble d'agents tous identiques un ou quelques agents différents : des intrus. Un exemple est celui du prédateur que tous les autres fuient et que l'on peut retrouver en constatant qu'il est en moyenne plus loin des autres agents.

Les objectifs principaux du projet restent globalement les mêmes :

 - Trouver un modèle de simulation à la fois simple (aussi bien pour l'analyse mathématique que pour l'implémentation) et expressif pour pouvoir reproduire des comportements très variés et que l'invertion ait donc un enjeux réel.

 - Déterminer des propriétés mathématiques de ce modèle. Ce peut être des propriétés de convergence et de stabilité de la méthode d'inversion ou bien des liens, éventuellement asymptotiques, entre règles locales et comportement global (distances moyennes, direction moyenne, etc) permettant d'accélérer l'inversion.

 - Expérimenter en implémentant la simulation et l'inversion et en testant différents jeux de résultats.

### Modèle

Après avoir donné quelques exemples de ce que le modèle devrait être capable de gérer, nous avons défini les règles élémentaires suivantes :

 - Alignement :

$F_a = \displaystyle\sum_jM_{ij}f_{\lambda_{ij}, \alpha_{ij}}(||x_j - x_i||)v_i$

 - Cohésion :

$F_c = \displaystyle\sum_jM_{ij}f_{\lambda_{ij}, \alpha_{ij}}(||x_j - x_i||)(x_j - x_i)$


On regroupe les différentes règles comme si elles étaient des forces, par le principe de la dynamique discrétisé :

$v_{n+1} = v_n + \delta (\displaystyle\sum_{\text{règle} r}F_r)$

Où $\delta$ (qui est homogène à une masse fois un temps) est déterminé arbitrairement car éventuellement corrigé par la matrice $M$, bien que cette correction soit abusive d'un point de vue physique (qui n'est pas notre point de vue ici).

**Notes :**

 - Les matrices $M$, $\alpha$ et $\lambda$ sont définies pour chaque règle.
 - La séparation est la même règle que la cohésion en changeant $M$ en $-M$.
 - Le nombre et de règle de chaque type en vigueur est fixé (à trois dans le cas de boids) et connu à l'avance.

La fonction $f_{\lambda, \alpha}$ utilisée est $f_{\lambda, \alpha} : x \mapsto \frac 1 {1 + \exp(\alpha(x - \lambda))}$

Les matrices permettent de gérer plusieurs catégories de population, le suivi d'un individu en particulier, etc. Elles peuvent avoir des formes très particulières.

Le nombre de règles élémentaires est très restreint et on a par exemple abandonné des notions comme un champ de vision non isotrope (non sans en débattre).

Les propriétés de l'environnement peuvent être gérées par des agents particuliers qui seraient immobiles mais tout de même dotés de certaines propriétés (attraction, répulsion, etc).

Une gravité peut être simulée par le biais d'un individu statique ayant tout de même une vitesse non nulle dans une direction.

Une idée d'extension de ce modèle qui a été proposée consiste à imaginer des agents à états qui possèderaient plusieurs jeux de règles ainsi qu'un loi de transition d'une règle à l'autre. Elle restera certainement à l'état d'idée mais permet théoriquement d'étendre de façon efficace l'expressivité du modèle.

### Propriétés

L'approche mathématique n'est, dans le cas le plus général, vraiment pas évidente. Aussi nous n'hésiterons vraiment pas à faire, au moins localement, des hypothèses drastiques du moment qu'elles nous permettent d'obtenir un résultat intéressant. Nous tenterons de généraliser dans un second temps.

Ces hypothèses ne se feront cependant pas sans perdre de vue le but du modèle etcorrespondront donc autant que possible à une propriété qualitative, à une particularisation du problème d'origine.

Les hypothèses auxquelles nous avons pensé sont par exemple :

 - Les agents sont tous identiques, c'est-à-dire que les matrices sont toutes uniformes.

 - Il y a un intru parmis les agents : Il existe $i$ tel que les matrices privées de leurs colonne et ligne d'indice $i$ soient uniformes. On peut aussi supposer que les colonnes (resp. lignes) d'indice $i$ sont également uniformes.

 - Il y a deux populations distinctes. Les matrices sont donc de la forme $\left(\begin{array}{c}AB \\ CD\end{array}\right)$ où $A$, $B$, $C$ et $D$ sont uniforme. $A$ et $D$ représentent respectivement le comportement interne de la première et de la seconde population et $B$ et $C$ définissent leur façon d'intéragir.

On peut bien sûr trouver d'autres idées.

### Implémentation

Maintenant que le modèle est bien défini, nous allons recoder la simulation et prévoir sa double utilisation : à la fois pour générer les données et pour faire l'inversion.

Le code sera divisé en trois modules :

 - Affichage
 - Simulation
 - Inversion





Références bibliographiques
---------------------------

 - Cours de Bernard Chazelle

 - [Thèses de Jérémy Patrix](http://exppad.com/public/cours/these.pdf) (Détection de comportements à travers des modèles multi-agents
collaboratifs, appliquée à l’évaluation de la situation, notamment en
environnement asymétrique avec des données imprécises et incertaines)






