#' @section Funciones:
#'
#' Catalogo y documentacion de los proyectos estadisticos:
#' \itemize{
#'   \item \code{\link{catalogo_inegi}}: catalogo de proyectos de la Red Nacional de Metadatos del INEGI y acceso a la ficha de cada proyecto.
#'   \item \code{\link{bases_importinegi}}: bases de datos que se pueden descargar con el paquete, por proyecto y año, con su cobertura y su tamaño. No requiere conexion a internet.
#' }
#'
#' Censo y Conteo de Poblacion y Vivienda, y Encuesta Intercensal:
#' \itemize{
#'   \item \code{\link{cpv_censo}}: todas las bases de datos de los Censos de Poblacion y Vivienda de 1990 a 2020 (ITER, AGEB y manzana, muestra, localidades, entorno urbano y alojamientos de asistencia social) en una sola funcion.
#'   \item \code{\link{cpv_conteo}}: bases de datos de los Conteos de Poblacion y Vivienda de 1995 y 2005 (ITER, muestra y viviendas) en una sola funcion.
#'   \item \code{\link{cpv_intercensal}}: bases de datos de las Encuestas Intercensales de 2015 y 2025 (muestra y, en 2025, principales resultados por localidad de 50 000 y mas habitantes) en una sola funcion.
#' }
#'
#' Censos de gobierno y encuestas en hogares:
#' \itemize{
#'   \item \code{\link{cngmd}}: Censo Nacional de Gobiernos Municipales y Demarcaciones Territoriales de la Ciudad de Mexico, de 2011 a 2025, por modulo y tema.
#'   \item \code{\link{cndhf}}: Censo Nacional de Derechos Humanos Federal, de 2017 a 2025, por modulo y tema.
#'   \item \code{\link{enigh_nuevaconstruccion}}: Encuesta Nacional de Ingreso y Gasto de los Hogares, Nueva Construccion.
#'   \item \code{\link{enigh_tradicional}}: Encuesta Nacional de Ingresos y Gastos de los Hogares, serie tradicional, de 1984 a 2014, por tabla.
#'   \item \code{\link{enoe}}: Encuesta Nacional de Ocupacion y Empleo, de 2005 a 2026, por trimestre.
#' }
#'
#' Informacion georreferenciada:
#' \itemize{
#'   \item \code{\link{sig_marcogeo}}: Marco Geoestadistico Nacional.
#'   \item \code{\link{sig_caminos_descarga}} y \code{\link{sig_caminos_extrae}}: Red Nacional de Caminos.
#' }
#'
#' @section Paquetes opcionales:
#'
#' Las funciones \code{\link{sig_marcogeo}} y \code{\link{sig_caminos_extrae}} requieren el paquete \code{sf}, que no se instala junto con \code{importinegi}. Se instala con \code{install.packages("sf")}. La base \code{"resultados"} de \code{\link{cpv_intercensal}} requiere el paquete \code{readxl}, que se instala con \code{install.packages("readxl")}.
#'
#' @section Guia de uso:
#'
#' La viñeta del paquete explica la sintaxis de las funciones con ejemplos: \code{vignette("importinegi")}.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
