# GMRC Shiny stat

## Procédure d'installation

### Téléchargement du logiciel R :

Télécharger R au lien suivant https://cloud.r-project.org/

### Utilisateur windows : installation de rTools :

https://cran.r-project.org/bin/windows/Rtools/

### Installation de l'application et mise à jour :

Lancer le logiciel R.

Coller les lignes de codes suivantes 
```r
if(!require(remotes)){install.packages('remotes',quiet=T,dep = FALSE)}
if(!require('GmrcShinyStats')){
  remotes::install_github('DrFabach/GmrcShinyStats',dep = T, INSTALL_opts = c('--no-lock'))  }

 }
```
Appuyer sur "entrée", après quelques minutes d'installation le logiciel devrait pouvoir être lancé.

### Lancement du logiciel 

Lancer le logiciel R.

Coller les lignes de codes suivantes 
```r
GmrcShinyStats::run_app()
```


---
### Citation

Thibaut Fabacher, Michael Schaeffer, Nicolas Tuzin, François Séverac, François Lefebvre, Marie Mielcarek, Erik-André Sauleau, Nicolas Meyer, Julien Godet. Biostatistiques médicales avec GMRC Shiny Stats - un outil de formation par la pratique, Annales Pharmaceutiques Françaises,2020.
ISSN 0003-4509, https://doi.org/10.1016/j.pharma.2020.06.001.

---
### License

This package is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License, version 3, as
published by the Free Software Foundation.

This program is distributed in the hope that it will be useful, but
without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  See the GNU
General Public License for more details.

A copy of the GNU General Public License, version 3, is available at
<https://www.r-project.org/Licenses/GPL-3>

