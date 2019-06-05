#' Marco Geoestadistico Nacional
#'
#' El Marco Geoestadístico es un sistema único y de carácter nacional diseñado por el INEGI, el cual presenta la división del territorio nacional en diferentes niveles de desagregación para referir geográficamente la información estadística de los censos y encuestas institucionales
#'
#' @param year Año de referencia del mapa, en formato numerico (1995-2018).
#' @param capa Capa (Shpafile) en formato alfanumerico. Las opciones son: entidades, municipios, ageb, urbano, y rural.
#' @param version Especificar, en formato alfanumerico, la version para los años 2010 (4.3, 5.0, 5.0.A), 2017 (2010.0 o dejar en blanco) y 2018 (2010.0 o dejar en blanco). Para el resto de los años, dejar en blanco.
#'
#' @examples
#'
#' # Consulta los metadatos del Marco Geoestadistico Macional
#' \dontrun{sig_marcogeo()}
#'
#' # Descarga el mapa de munucipios para 2009
#' \dontrun{mapa09 = sig_marcogeo(year = 2009, capa = "municipios")}
#' @export


sig_marcogeo <- function(year = NA, capa = NA, version = NA){
# Get Help and More Information
if (is.na(year) & is.na(capa) & is.na(version)) {base::shell.exec("https://www.inegi.org.mx/temas/mapas/mg/")}
else if (is.na(year)) {stop("Debes introducir el a\u00f1o del mapa")}
else if (is.na(capa)) {stop("Debes introducir la capa del mapa")}
else if (year == 2010 & is.na(version)) {stop("Para el a\u00f1o 2010, debes especificar la version. Las versiones disponibles son \"4.3\", \"5.0\" y \"5.0.A\" ")}
else if (year == 2017 & is.na(version)) {warning("El a\u00f1o 2017 contiene dos versiones: 2017 (deja la version en blanco) y \"2010.0\" ")}
else if (year == 2018 & is.na(version)) {warning("El a\u00f1o 2018 contiene dos versiones: 2018 (deja la version en blanco) y \"2010.0\" ")}

# Capas no disponibles por a\u00f1o
else if (year == 1995 & (capa == "ageb" | capa == "urbano" | capa == "rural")) {stop("Las capas ageb, urbano o rural no estan disponibles para el a\u00f1o 1995.")}
  else if (year == 2000 & (capa == "urbano" | capa == "rural")) {stop("Las capas urbano o rural no estan disponibles para el a\u00f1o 2000.")}
  else if (year == 2005 & (capa == "urbano" | capa == "rural")) {stop("Las capas urbano o rural no estan disponibles para el a\u00f1o 2005.")}
  else if (year == 2010 & capa == "ageb" & version == "5.0.A") {stop("La capa ageb no esta disponible para el a\u00f1o 2010 5.0.A")}
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

# Recodificar capa AGEB
if      (capa == "ageb" & (year == 2000 | year == 2005 | year == 2009 | year == 2013 | year == 2010)){capa = "ageb_urbana"}
else if (capa == "ageb" & year == 2007) {capa = "ageb_rural"}

# Codigos para Capa
if (capa == "entidades"){
    codigo_capa = "mge"
    nombre_capa = "Entidades"
    }
if (capa == "municipios"){
    codigo_capa = "mgm"
    nombre_capa = "Municipios"
    }
if (capa == "ageb_urbana"){
    codigo_capa = "mgau"
    nombre_capa = "agebs_urb"
    }
if (capa == "urbano"){
    codigo_capa = "mglu"
    nombre_capa = "Localidades urbanas"
    }
if (capa == "rural"){
    codigo_capa = "mglr"
    nombre_capa = "Localidades rurales"
    }
if (capa == "ageb_rural"){
    codigo_capa = "mgar"
    nombre_capa = "AGEB_rural"
}

# Ediciones especiales
if      (year == 2009 & capa == "ageb_urbana") {nombre_capa = "ageb_urb"}
else if (year == 2010 & capa == "entidades"  & version == "5.0.A") {nombre_capa = "estados"}
else if (year == 2010 & capa == "rural"  & version == "5.0.A") {nombre_capa = "localidades_rurales"}
else if (year == 2010 & capa == "urbano"  & version == "5.0.A") {nombre_capa = "poligonos_urbanos"}
else if (year == 2010 & capa == "municipios"  & version == "5.0.A") {nombre_capa = "municipios"}
else if (year == 2013 & capa == "ageb_urbana") {nombre_capa = "agebs_urbanos"}
else if (year == 2010 & capa == "ageb_urbana" & version == "5.0") {nombre_capa = "AGEB_urb"}

  # Desgargas  ------------------------------------------------------------------
  # Download File
  tempmarcogeo = tempfile()
  utils::download.file(url_marcogeo, tempmarcogeo)

  # Unzip and open
  zipdir = tempdir()
  zipdir_level  = tempdir()
  utils::unzip(tempmarcogeo, exdir = zipdir)

  # 1995
  if (year == 1995) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, ".zip")), exdir = zipdir_level)}
  else {}

  # 2000
  # agebs_urb_2000
  if (year == 2005) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, "v_1_0.zip")), exdir = zipdir_level)}
  else if (year == 2009) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, "v4_3.zip")), exdir = zipdir_level)}
  else if (year == 2010 & (version == "5.0" | version == "5.0.A")) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, "v5_0.zip")), exdir = zipdir_level)}
  else if (year == 2013) {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, "v6_0.zip")), exdir = zipdir_level)}
  else {utils::unzip(paste0(zipdir, paste0("\\\\", codigo_capa, year, ".zip")), exdir = zipdir_level)}

  # Renombrar nombre_capa
  if (year == 2010 & version == "5.0" & capa == "rural") {nombre_capa = "Localidades_rurales"}
  else if (year == 2010 & version == "5.0" & capa == "urbano") {nombre_capa = "Localidades_urbanas"}
  else {}

  # Extract Map
  if (year == 2010 & version == "5.0") {map_output = rgdal::readOGR(dsn = zipdir_level, layer = paste0(nombre_capa, "_", year, "_5"))}
  else if (year == 2010 & version == "5.0.A") {map_output = rgdal::readOGR(dsn = zipdir_level, layer = nombre_capa)}
  else {map_output = rgdal::readOGR(dsn = zipdir_level, layer = paste0(nombre_capa, "_", year))}

  return(map_output)
} # End of Function
