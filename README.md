ocaml-seamcarving
=================

# Compilation
```
 ocamlopt graphics.cmxa ppm.ml main.ml -o output
```

# Lancement
```
./output nom_fichier_entree.ppm nom_fichier_sortie.ppm (rgb4|rgb8|lum4|lum8)
```
rgb4 ou rgb8 ou lum4 ou lum8 suivant que l'on veuille utiliser une distance rgb ou lumineuse, avec 4 ou 8 voisins.

Avant de lancer l'animation, on peut dessiner :
-en vert pour conserver les pixels
-en rouge pour supprimer les pixels
Pour passer au vert, appuyer sur la touche '+'.
Pour passer au rouge, appuyer sur la touche '-'.

Lancer l'animation en appuyant sur n'importe quelle autre touche.

Pour mettre en pause l'animation, appuyer sur la touche espace,
Pour sauvegarder l'image, appuyer sur la touche 's'.

=================

L2 Programmation Fonctionelle - Paris Diderot 2012
