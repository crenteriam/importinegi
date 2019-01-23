#' Red Nacional de Caminos
#'
#' @param year An~o (1995-2018)
#' @param capa Opciones: entidades, municipios, ageb, urbano, rural
#' @param version Algunos an~ tienen multiples versiones. Si no la sabes, el mensaje de error te dara las opcines.
# shell.exec("https://www.inegi.org.mx/temas/mapas/mg/")

#marco_geoestadistico <- function(year = NA, capa = NA, version = NA){
library(rgdal)
# Informacion de la version
message("marco_geoestadistico() Versión 1.0.
        \rMarco Geoestadistico Nacional
        \rAños disponible: 1995-2018
        \r\n")

# Get Help and More Information
if (is.na(year) & is.na(capa) & is.na(version)) {base::shell.exec("https://www.inegi.org.mx/temas/mapas/mg/")}

# Links:
url_base = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/"
# 1995 - "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292836_s.zip"

# Codigo Year
if      (year == 1995) {codigo_year = "702825292836_s"}
else if (year == XXXX) {codigo_year = }
else if (year == XXXX) {codigo_year = }
else if (year == XXXX) {codigo_year = }
else if (year == XXXX) {codigo_year = }
else if (year == XXXX) {codigo_year = }
else if (year == XXXX) {codigo_year = }

# Codigo de la Capa
if (capa == entidades)  {codigo_capa = "mge"}
if (capa == municipios) {codigo_capa = "mgm"}
if (capa == ageb)       {codigo_capa = "mgau"}
if (capa == urbano)     {codigo_capa = "mgr"}
if (capa == rural)      {codigo_capa = "mgu"}


#Download File
  url.marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292836_s.zip"
  tempmarcogeo = tempfile()
  download.file(url.marcogeo, tempmarcogeo)

  # Unzip and open
  zipdir = tempdir()
  zipdir_entidades  = tempdir()
  zipdir_municipios = tempdir()
  utils::unzip(tempmarcogeo, exdir = zipdir)

  # 1995
  utils::unzip(paste0(zipdir, "\\\\mge1995.zip"), exdir = zipdir_entidades)
  utils::unzip(paste0(zipdir, "\\\\mgm1995.zip"), exdir = zipdir_municipios)
  map_output = readOGR(dsn = zipdir_entidades, layer = "Entidades_1995")
  map_output = readOGR(dsn = zipdir_municipios, layer = "Municipios_1995")

  # 2000
  # agebs_urb_2000
  utils::unzip(paste0(zipdir, "\\\\mgau2000.zip"), exdir = zipdir_entidades)
  utils::unzip(paste0(zipdir, "\\\\mge2000.zip"), exdir = zipdir_entidades)
  utils::unzip(paste0(zipdir, "\\\\mgm2000.zip"), exdir = zipdir_municipios)
  map_output = readOGR(dsn = zipdir_entidades, layer = "Entidades_1995")
  map_output = readOGR(dsn = zipdir_municipios, layer = "Municipios_1995")
