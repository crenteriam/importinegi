#' @section Funciones:
#'
#' Catalogo y documentacion de los proyectos estadisticos:
#' \itemize{
#'   \item \code{\link{catalogo_inegi}}: catalogo de proyectos de la Red Nacional de Metadatos del INEGI y acceso a la ficha de cada proyecto.
#' }
#'
#' Censo y Conteo de Poblacion y Vivienda:
#' \itemize{
#'   \item \code{\link{censo_poblacion_iter}}: principales resultados por localidad (ITER).
#'   \item \code{\link{censo_poblacion_ageb}}: resultados por AGEB y manzana urbana.
#'   \item \code{\link{censo_poblacion_rural}}: localidades con menos de 5 mil habitantes.
#'   \item \code{\link{censo_poblacion_urbano}}: infraestructura y caracteristicas del entorno urbano.
#'   \item \code{\link{censo_poblacion_muestra}}: muestra (cuestionario ampliado).
#' }
#'
#' Censos de gobierno y encuestas en hogares:
#' \itemize{
#'   \item \code{\link{censo_municipal}}: Censo Nacional de Gobiernos Municipales y Delegacionales.
#'   \item \code{\link{enigh_nuevaconstruccion}}: Encuesta Nacional de Ingreso y Gasto de los Hogares, Nueva Construccion.
#'   \item \code{\link{enoe}}: Encuesta Nacional de Ocupacion y Empleo.
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
#' Las funciones \code{\link{sig_marcogeo}} y \code{\link{sig_caminos_extrae}} requieren el paquete \code{sf}, que no se instala junto con \code{importinegi}. Se instala con \code{install.packages("sf")}.
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
