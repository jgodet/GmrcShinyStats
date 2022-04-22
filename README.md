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
  remotes::install_gitHUB('https://github.com/DrFabach/GmrcShinyStats',dep = T)  }
```
Appuyer sur "entrée", après quelques minutes d'installation le logiciel devrait pouvoir être lancé.

### Lancement du logiciel 

Lancer le logiciel R.

Coller les lignes de codes suivantes 
```r
GmrcShinyStats::run_app()
```

