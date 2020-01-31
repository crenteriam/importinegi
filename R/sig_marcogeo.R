#' Marco Geoestadistico Nacional
#'
#' Extrae los mapas del Marco Geoestadistico Nacional.
#'
#' El Marco Geoestadistico Nacional (MGN) es un proyecto geoestadistico que presenta informacion sobre la division política del territorio mexicano en sus diferentes niveles de gobierno (nacional, estatal y municipal), asi como otras formas de clasificacion del territorio nacional.
#'
#' @param year Año de referencia del mapa, en formato numerico. Años disponibles: 1995, 2000, 2005, 2007, 2009, 2010 y 2013.
#' @param mapa Mapa en formato alfanumerico. Las opciones son: entidades, municipios, ageb, urbano, y rural.
#' @param version Especificar, en formato alfanumerico, la version para los años 2010 (4.3, 5.0, 5.0.A), 2017 (2010.0 o dejar en blanco) y 2018 (2010.0 o dejar en blanco). Para el resto de los años, dejar en blanco.
#'
#' @examples
#'
#' # Consultar los metadatos del Marco Geoestadistico Nacional
#' \donttest{sig_marcogeo()}
#'
#' # Descargar el mapa de munucipios para 2009
#' \donttest{mapa09 = sig_marcogeo(year = 2009, mapa = "municipios")}
#' @return Data.frame
#' @export


sig_marcogeo <- function(year = NA, mapa = NA, version = NA){

# Get Help and More Information
if (is.na(year) & is.na(mapa) & is.na(version)) {
  shell.exec("https://www.inegi.org.mx/temas/mg/")
  message("\n Cargando pagina... \n")
  # Halt the function without error message
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  stop()
}
else if (is.na(year)) {stop("Debes introducir el a\u00f1o del mapa")}
else if (is.na(mapa)) {stop("Debes introducir el nombre del mapa")}
else if (year == 2010 & is.na(version)) {stop("Para el a\u00f1o 2010, debes especificar la version. Las versiones disponibles son \"4.3\", \"5.0\" y \"5.0.A\" ")}
else if (year == 2017 & is.na(version)) {warning("El a\u00f1o 2017 contiene dos versiones: 2017 (deja la version en blanco) y \"2010.0\" ")}
else if (year == 2018 & is.na(version)) {warning("El a\u00f1o 2018 contiene dos versiones: 2018 (deja la version en blanco) y \"2010.0\" ")}

# Mapas no disponibles por a\u00f1o
else if (year == 1995 & (mapa == "ageb" | mapa == "urbano" | mapa == "rural")) {stop("Los mapas ageb, urbano o rural no estan disponibles para el a\u00f1o 1995.")}
  else if (year == 2000 & (mapa == "urbano" | mapa == "rural")) {stop("Los mapas urbano o rural no estan disponibles para el a\u00f1o 2000.")}
  else if (year == 2005 & (mapa == "urbano" | mapa == "rural")) {stop("Los mapas urbano o rural no estan disponibles para el a\u00f1o 2005.")}
  else if (year == 2010 & mapa == "ageb" & version == "5.0.A") {stop("El mapa ageb no esta disponible para el a\u00f1o 2010 5.0.A")}
  #else if (mapa != "entidades" | mapa != "municipios" | mapa != "ageb" | mapa != "urbano" | mapa != "rural") {stop(message("Nombre de mapa no reconocido."))}

# Inicio --------------------------------------------------------------------

# Links:
url_base = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/"

# URL por A\u00f1o - Estructura anterior
if      (year == 1995) {url_marcogeo = paste0(url_base, "702825292836_s.zip")}
else if (year == 2000) {url_marcogeo = paste0(url_base, "702825292843_s.zip")}
else if (year == 2005) {url_marcogeo = paste0(url_base, "702825292850_s.zip")}
else if (year == 2007) {url_marcogeo = paste0(url_base, "702825292867_s.zip")}
else if (year == 2009) {url_marcogeo = paste0(url_base, "702825292782_s.zip")}
#else if (year == 2010 & version == "4.3") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825296520_s.zip"}
else if (year == 2010 & version == "5.0") {url_marcogeo = paste0(url_base, "702825292812_s.zip")}
else if (year == 2010 & version == "5.0.A") {url_marcogeo = paste0(url_base, "702825292805_s.zip")}
else if (year == 2013) {url_marcogeo = paste0(url_base, "702825292829_s.zip")}

# URL por a\u00f1o - Estructura nueva
#else if (year == 2016) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc_geo/702825217341_s.zip"}
#2.3 GB else if (year == 2017) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463171829_s.zip"}
#else if (year == 2017 & version == "2010.0") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463142683_s.zip"}
# 2.6GB else if (year == 2018) {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463674658_s.zip"}
# 2.4GB else if (year == 2018 & version == "2010.0") {url_marcogeo = "http://internet.contenidos.inegi.org.mx/contenidos/Productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marcogeo/889463592587_s.zip"}
else {stop("El a\u00f1o elegido no esta disponible. Para ver los a\u00f1os disponibles usa teclea sig_marcogeo() en la consola")}

# Recodificar mapa AGEB
if      (mapa == "ageb" & (year == 2000 | year == 2005 | year == 2009 | year == 2013 | year == 2010)){mapa = "ageb_urbana"}
else if (mapa == "ageb" & year == 2007) {mapa = "ageb_rural"}

# Codigos para mapa
if (mapa == "entidades"){
    codigo_mapa = "mge"
    nombre_mapa = "Entidades"
    }
if (mapa == "municipios"){
    codigo_mapa = "mgm"
    nombre_mapa = "Municipios"
    }
if (mapa == "ageb_urbana"){
    codigo_mapa = "mgau"
    nombre_mapa = "agebs_urb"
    }
if (mapa == "urbano"){
    codigo_mapa = "mglu"
    nombre_mapa = "Localidades urbanas"
    }
if (mapa == "rural"){
    codigo_mapa = "mglr"
    nombre_mapa = "Localidades rurales"
    }
if (mapa == "ageb_rural"){
    codigo_mapa = "mgar"
    nombre_mapa = "AGEB_rural"
}

# Ediciones especiales
if      (year == 2009 & mapa == "ageb_urbana") {nombre_mapa = "ageb_urb"}
else if (year == 2010 & mapa == "entidades"  & version == "5.0.A") {nombre_mapa = "estados"}
else if (year == 2010 & mapa == "rural"  & version == "5.0.A") {nombre_mapa = "localidades_rurales"}
else if (year == 2010 & mapa == "urbano"  & version == "5.0.A") {nombre_mapa = "poligonos_urbanos"}
else if (year == 2010 & mapa == "municipios"  & version == "5.0.A") {nombre_mapa = "municipios"}
else if (year == 2013 & mapa == "ageb_urbana") {nombre_mapa = "agebs_urbanos"}
else if (year == 2010 & mapa == "ageb_urbana" & version == "5.0") {nombre_mapa = "AGEB_urb"}

  # Desgargas  ------------------------------------------------------------------
  # Download File
  tempmarcogeo = tempfile()
  utils::download.file(url_marcogeo, tempmarcogeo)

  # Unzip and open
  zipdir = tempdir()
  zipdir_level  = tempdir()
  utils::unzip(tempmarcogeo, exdir = zipdir)

  # 1995
  if (year == 1995) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, ".zip")), exdir = zipdir_level)}
  else {}

  # 2000
  # agebs_urb_2000
  if (year == 2005) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, "v_1_0.zip")), exdir = zipdir_level)}
  else if (year == 2009) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, "v4_3.zip")), exdir = zipdir_level)}
  else if (year == 2010 & (version == "5.0" | version == "5.0.A")) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, "v5_0.zip")), exdir = zipdir_level)}
  else if (year == 2013) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, "v6_0.zip")), exdir = zipdir_level)}
  else {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_mapa, year, ".zip")), exdir = zipdir_level)}

  # Renombrar nombre_mapa
  if (year == 2010 & version == "5.0" & mapa == "rural") {nombre_mapa = "Localidades_rurales"}
  else if (year == 2010 & version == "5.0" & mapa == "urbano") {nombre_mapa = "Localidades_urbanas"}
  else {}

  # Extract Map
  if (year == 2010 & version == "5.0") {map_output = rgdal::readOGR(dsn = zipdir_level, layer = paste0(nombre_mapa, "_", year, "_5"))}
  else if (year == 2010 & version == "5.0.A") {map_output = rgdal::readOGR(dsn = zipdir_level, layer = nombre_mapa)}
  else {map_output = rgdal::readOGR(dsn = zipdir_level, layer = paste0(nombre_mapa, "_", year))}

  return(map_output)
} # End of Function
