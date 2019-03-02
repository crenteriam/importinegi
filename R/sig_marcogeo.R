#' Marco Geoestadistico Nacional
#'
#' @param year An~o (1995-2018)
#' @param capa Opciones: entidades, municipios, ageb, urbano, rural
#' @param version Algunos an~ tienen multiples versiones. Si no la sabes, el mensaje de error te dara las opcines.
# shell.exec("https://www.inegi.org.mx/temas/mapas/mg/")

sig_marcogeo <- function(year = NA, capa = NA, version = NA){
library(rgdal)

# Generales -----------------------------------------------------------------
# Informacion de la version
message("sig_marcogeo() Versión 1.0.
        \rMarco Geoestadistico Nacional
        \rAños disponible: 1995-2018
        \r\n")

# Get Help and More Information
if (is.na(year) & is.na(capa) & is.na(version)) {base::shell.exec("https://www.inegi.org.mx/temas/mapas/mg/")}

# Inicio --------------------------------------------------------------------

# Links:
url_base = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/"

# URL por An~o
if      (year == 1995) {url_marcogeo   = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292836_s.zip"}
else if (year == 2000) {codigo_year = ""}
else if (year == 2000) {codigo_year = ""}
else if (year == 2000) {codigo_year = ""}
else if (year == 2000) {codigo_year = ""}
else if (year == 2000) {codigo_year = ""}
else if (year == 2000) {codigo_year = ""}

# Codigo de la Capa
if (capa == "entidades"){
    codigo_capa = "mge"
    nombre_capa = "Entidades"
    }
if (capa == "municipios"){
    codigo_capa = "mgm"
    nombre_capa = "Municipios"
    }
if (capa == "ageb")       {codigo_capa = "mgau"}
if (capa == "urbano")     {codigo_capa = "mgr"}
if (capa == "rural")      {codigo_capa = "mgu"}

  # Desgargas  ------------------------------------------------------------------
  # Download File
  tempmarcogeo = tempfile()
  download.file(url_marcogeo, tempmarcogeo)

  # Unzip and open
  zipdir = tempdir()
  zipdir_level  = tempdir()
  utils::unzip(tempmarcogeo, exdir = zipdir)

  # 1995
  if (year == 1995) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, ".zip")), exdir = zipdir_level)}
  else {}

  # 2000
  # agebs_urb_2000
  utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, ".zip")), exdir = zipdir_level)
  #utils::unzip(paste0(zipdir, "\\\\mge2000.zip"), exdir = zipdir_entidades)
  #utils::unzip(paste0(zipdir, "\\\\mgm2000.zip"), exdir = zipdir_municipios)

  # Extract Map
  if (year == 1995) {map_output = readOGR(dsn = zipdir_level, layer = paste0(nombre_capa, "_", year))}
  #map_output = readOGR(dsn = zipdir_municipios, layer = "Municipios_1995")

  return(map_output)
} # End of Function
