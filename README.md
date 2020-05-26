# GmrcShinyStats

GMRC Shiny Stats permet de faire facilement des analyses statistiques sans coder.

---
## How to install and run GmrcShinyStats

```r
if(!require(devtools){install.packages("devtools")}
devtools::install_github("jgodet/gmrcfun")
devtools::install_github("jgodet/GmrcShinyStats")
require(GmrcShinyStats)
GO()
```


Alternative way
```r
install.packages("devtools")
library(devtools)
install_github("jgodet/gmrcfun")
install_github("jgodet/GmrcShinyStats", INSTALL_opts = c('--no-lock'))
require(gmrcfun)
require(shiny)
GO()
```

Once installed, anytime you want to run GmrcShinyStats - just type the two last lines
```r
require(shiny)
GO()
```

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
