<!-- badges: start -->
  [![Travis build status](https://travis-ci.org/crenteriam/importinegi.svg?branch=master)](https://travis-ci.org/crenteriam/importinegi)
<!-- badges: end -->
  
## Introduccion

`importinegi` es un paquete de R para descargar y gestionar bases de datos abiertas del INEGI. El paquete contribuye a facilitar la búsqueda, descarga y gestión de los datos abiertos del INEGI. La version `1.0.0` del paquete permite descargar y gestionar los datos de los siguientes proyectos estadisticos:

- Censo (y Conteo) Poblacion y Vivienda (1990-2010)
- Censo Nacional de Gobiernos Municipales y Delegacionales (2011-2017)
- Encuesta Nacional de Ingreso y Gasto (ENIGH) (2008-2014)
- Encuesta Nacional de Ocupación y Empleo (ENOE) (2005-2018)
- Red Nacional Carretera (2014-2016)
- Marco Geoestadistico Nacional (1995-2018)

## Instalacion

Instalacion de la ultima version estable, `1.0.0`, a traves del CRAN:
```
install.packages("importinegi")
library(importinegi)
```

Instalacion de la ultima version beta a traves de Github:

```
install.packages("devtools")
devtools::install_github("crenteriam/importinegi")
library(importinegi)
```

## Otros paquetes

Otrosp paquetes de R para descargar y gestionar datos del INEGI:

- [INEGI API](https://www.diegovalle.net/mxmaps/articles/articles/inegi_api.html)
- [inegiR](https://github.com/Eflores89/inegiR/)
