<!-- badges: start -->
[![](https://www.r-pkg.org/badges/version-last-release/importinegi)](https://cran.r-project.org/package=importinegi) [![](https://cranlogs.r-pkg.org/badges/importinegi)](https://cran.r-project.org/package=importinegi) [![R-CMD-check](https://github.com/crenteriam/importinegi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/crenteriam/importinegi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
 
## Introduccion

`importinegi` es un paquete de R para descargar y gestionar bases de datos abiertas del INEGI. El paquete facilita la búsqueda, descarga y gestión de los datos abiertos del INEGI. Con la version `1.2.1` del paquete se pueden descargar y gestionar los datos de los siguientes proyectos estadisticos:

- Censo (y Conteo) Poblacion y Vivienda (1990-2010)
- Censo Nacional de Gobiernos Municipales y Delegacionales (2011-2017)
- Encuesta Nacional de Ingreso y Gasto (ENIGH) (2008-2014)
- Encuesta Nacional de Ocupación y Empleo (ENOE) (2005-2018)
- Red Nacional Carretera (2014-2016)
- Marco Geoestadistico Nacional (1995-2018)

La versión `2.0.0` está en desarrollo en este repositorio. La última versión estable es la `1.2.1`, disponible en CRAN.

## Instalacion

Última versión estable `1.2.1` (desde CRAN):
```
install.packages("importinegi")
library(importinegi)
```

Versión en desarrollo `2.0.0` (desde GitHub):
```
install.packages("devtools")
devtools::install_github("crenteriam/importinegi")
library(importinegi)
```

La versión `2.0.0` requiere R 4.1 o posterior. Las funciones de mapas `sig_marcogeo()` y `sig_caminos_extrae()` necesitan, además, el paquete `sf`, que se instala aparte con `install.packages("sf")`.

## Uso

El siguiente ejemplo, con la versión en desarrollo `2.0.0`, consulta el catálogo de proyectos estadísticos del INEGI:

```
# Descarga el catalogo de proyectos estadisticos del INEGI
catalogo <- catalogo_inegi()

# Busca proyectos por nombre o por siglas, y por año
catalogo_inegi(buscar = "enoe", year = 2020)

# Accede a la documentacion de un proyecto con su numero (id)
catalogo_inegi(id = 489)
```

La sintaxis de todas las funciones, con ejemplos, se explica en la [viñeta del paquete](https://github.com/crenteriam/importinegi/blob/master/vignettes/importinegi.Rmd). Con el paquete instalado desde CRAN, la viñeta también se consulta con `vignette("importinegi")`.

## Novedades

Los cambios de cada versión se registran en [NEWS.md](https://github.com/crenteriam/importinegi/blob/master/NEWS.md).
