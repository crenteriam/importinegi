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
else if (is.na(year)) {stop("Debes introducir el an~o del mapa")}
else if (is.na(capa)) {stop("Debes introducir la capa del mapa")}
else if (year == 2010 & is.na(version)) {stop("Para el an~ 2010, debes especificar la version. Las versiones disponibles son \"4.3\", \"5.0\" y \"5.0.A\" ")}
else if (year == 2017 & is.na(version)) {warning("El an~o 2017 contiene dos versiones: 2017 (deja la version en blanco) y \"2010.0\" ")}
else if (year == 2018 & is.na(version)) {warning("El an~o 2018 contiene dos versiones: 2018 (deja la version en blanco) y \"2010.0\" ")}
# Inicio --------------------------------------------------------------------

# Links:
url_base = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/"

# URL por An~o
if      (year == 1995) {url_marcogeo   = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292836_s.zip"}
else if (year == 2000) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292843_s.zip"}
else if (year == 2005) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292850_s.zip"}
else if (year == 2007) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292867_s.zip"}
else if (year == 2009) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292782_s.zip"}
else if (year == 2010 & version == "4.3") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825296520_s.zip"}
else if (year == 2010 & version == "5.0") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292812_s.zip"}
else if (year == 2010 & version == "5.0.A") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292805_s.zip"}
else if (year == 2013) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825292829_s.zip"}
else if (year == 2016) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825217341_s.zip"}
else if (year == 2017) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463171829_s.zip"}
else if (year == 2017 & version == "2010.0") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463142683_s.zip"}
else if (year == 2018) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463674658_s.zip"}
else if (year == 2018 & version == "2010.0") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463592587_s.zip"}
else {stop("El an~o elegido no esta disponible. Para ver los an~os disponibles usa teclea sig_marcogeo() en la consola")}

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

  return(map_output)
} # End of Function
