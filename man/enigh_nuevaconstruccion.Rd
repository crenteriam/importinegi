% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enigh_nuevaconstruccion.R
\name{enigh_nuevaconstruccion}
\alias{enigh_nuevaconstruccion}
\title{ENIGH Nueva Construccion (2008-2014)}
\usage{
enigh_nuevaconstruccion(year = NA, datos = NA)
}
\arguments{
\item{year}{Año de levantamiento de la encuesta en formato numerico. Los años disponibles son 2008, 2010, 2012 y 2014}

\item{datos}{Base de datos a descargar "viviendas" "hogares" "concentrado" "erogaciones" "gastohogar" "gastotarjetas" "poblacion" "ingresos" "gastopersona" "trabajos" "agro" "noagro"}
}
\value{
Data.frame
}
\description{
Descarga datos de la Encuesta Nacional de Ingreso y Gasto de los Hogares, Nueva Construccion (2008-2014).
}
\details{
La ENIGH provee informacion estadisticas sobre los ingresos y gastos de los hogares en cuanto a su monto, procedencia y distribucion. Adicionalmente, la ENIGH provee informacion sobre las caracteristicas socio-demograficas de los integrantes del hogar.
}
\examples{

# Descargar datos de hogares
\dontrun{hogares14 = enigh_nuevaconstruccion(year = 2014,  datos = "hogares")}
}
