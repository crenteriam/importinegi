## Introduccion

Paquete de R para la importacion de datos de INEGI. La version 1.0 del paquete se concentra en los principales censos, encuestas y mapas. Con `importinegi` version 1.0 puedes descargar:


Paquete en R para descargar y gestionar bases de datos del INEGI. Las pases de datos disponibles para descarga y gestion en la version 1.0.0 de *importinegi*  son:
- Censo Nacional de Poblacion y Vivienda (1990-2010)
- Censo Nacional de Gobiernos Municipales y Delegacionales
- Encuesta Nacional de Ingreso y Gasto (ENIGH) (2008-2014)
Encuesta Nacional de Ocupación y Empleo (ENOE) (2005-2018)
- Shapefiles de la Red Nacional Carretera (2014-2016)
- Shapefiles del Marco Geoestadistico Nacional (1995-2018)

## Instalacion

Instalacion de la ultima version estable a traves del CRAN:
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
## Ejemplo

Descarga la base de datos ITER del censo 2010 para el estado de Jalisco.
```
censo.jalisco10 = censo_poblacion_iter(year = 2010, estado = "Jalisco")
```
## Otros paquetes

Otros paquetes para gestionar datos del INEGI:
- [INEGI API](https://www.diegovalle.net/mxmaps/articles/articles/inegi_api.html)
- [inegiR](https://github.com/Eflores89/inegiR/)
