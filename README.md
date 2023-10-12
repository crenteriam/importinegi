<!-- badges: start -->
[![Travis build status](https://travis-ci.org/crenteriam/importinegi.svg?branch=master)](https://travis-ci.org/crenteriam/importinegi) [![](https://www.r-pkg.org/badges/version-last-release/importinegi)](https://cran.r-project.org/package=importinegi) [![](https://cranlogs.r-pkg.org/badges/importinegi)](https://cran.r-project.org/package=importinegi)
<!-- badges: end -->
 
## Introduccion

`importinegi` es un paquete de R para descargar y gestionar bases de datos abiertas del INEGI. El paquete facilita la búsqueda, descarga y gestión de los datos abiertos del INEGI. Con la version `1.2.1` del paquete se pueden descargar y gestionar los datos de los siguientes proyectos estadisticos:

- Censo (y Conteo) Poblacion y Vivienda (1990-2010)
- Censo Nacional de Gobiernos Municipales y Delegacionales (2011-2017)
- Encuesta Nacional de Ingreso y Gasto (ENIGH) (2008-2014)
- Encuesta Nacional de Ocupación y Empleo (ENOE) (2005-2018)
- Red Nacional Carretera (2014-2016)
- Marco Geoestadistico Nacional (1995-2018)

## Instalacion

Última versión estable `1.2.1` (desde CRAN):
```
install.packages("importinegi")
library(importinegi)
```

- Nota de la versión `1.2.1`. Se actualizó la nueva URL del Repositorio Nacional de Metadatos. Se reemplazo el paquete rgdal por sf.
- Nota de la versión `1.2.0`. Se arreglan problemas con las funciones catalogo_inegi() y enoe(). Cortesía de Villiem (https://twitter.com/villicanaem).
- Nota de la versión `1.1.3`. INEGI terminó de trasladar algunos repositorios de su versión beta a su versión estable. Esto afectó los URL de las funciones para el Censo y la ENOE. La versión beta `1.1.2` arregla este problema.

```
install.packages("devtools")
devtools::install_github("crenteriam/importinegi")
library(importinegi)
```
