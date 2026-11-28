# Genera los archivos de muestra con los que se prueba
# sig_marcogeoestadistico() sin conexion (tests/testthat/fixtures/sig).
#
# Uso, desde la carpeta raiz del paquete, en Linux o macOS:
#
#   Rscript data-raw/muestras_sig.R
#
# Requiere el paquete sf y el programa zip. Dos muestras necesitan ademas otros
# programas, y se omiten si no estan: capa_deflate64.zip (7z) y zip64.zip
# (python3).
#
# Los datos son inventados: unos cuantos cuadros, lineas y puntos por capa. Lo
# que imitan es la forma de los archivos del Marco Geoestadistico, que se
# reviso el 24 y el 27 de noviembre de 2026 en el indice de los zip del INEGI,
# en el encabezado de sus DBF y en sus archivos .prj y .cpg, sin descargar los
# productos. El texto de cada .prj es el de un archivo real del INEGI.
#
# Descarga por capa (capa_<archivo del INEGI>):
#
#   capa_2025_1_00_MUN        Archivo de todo el pais: un shapefile en la raiz
#                             del zip, con .cpg en UTF-8 y con archivos que no
#                             se usan (.sbn, .xml).
#   capa_2025_1_01_MUN        Una entidad.
#   capa_2025_1_06_MUN        Otra entidad, con .cpg 1252 y con la misma
#                             proyeccion escrita de otra manera en el .prj.
#   capa_2025_1_01_M          Manzanas, con el texto "NULL" en una columna.
#   capa_2025_1_01_FM y _06_  Frentes de manzana (lineas) de dos entidades.
#   capa_2020_1_06_TI         Territorio insular.
#   capa_deflate64            Una capa comprimida con Deflate64, como el
#                             archivo nacional de frentes de manzana de 2025.
#
# Productos completos (producto_<archivo del INEGI>):
#
#   702825292836  1995: un zip por capa, guardado sin comprimir dentro del
#                 producto. Capas sin .cpg y sin pagina de codigos en el DBF,
#                 con el texto en Latin-1.
#   702825292850  2005: como el anterior, pero las entidades y los municipios
#                 no traen .prj, y la clave de la entidad se llama CVE_EDO.
#   702825296520  2010, version 4.3: capas en carpetas, zonas interestatales
#                 con el DBF en la pagina de codigos 850 de DOS (declarada), y
#                 un archivo cuyo nombre no es UTF-8 valido.
#   889463084105  2014 (producto 702825004386): capas en una carpeta, en
#                 coordenadas geograficas y con la pagina de codigos declarada
#                 en el DBF.
#   889463526636  Febrero de 2018: un zip por entidad, guardado sin comprimir,
#                 con las capas en la carpeta "conjunto de datos" y un .CPG en
#                 mayusculas. Sin zip de todo el pais.
#   794551067284  Censo Agropecuario 2022: como el anterior, con la carpeta
#                 "conjunto_de_datos" y un zip de todo el pais.
#
# Otros:
#
#   zip64.zip     Zip con el indice en formato zip64, el de los zip de mas de
#                 4 GB o de mas de 65 535 archivos.

if (!file.exists("DESCRIPTION")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Este script requiere el paquete sf.")
}
destino <- file.path("tests", "testthat", "fixtures", "sig")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)
destino <- normalizePath(destino)
source("data-raw/muestras_comun.R")

# Proyecciones: texto de archivos .prj del INEGI ------------------------------------

prj <- list(
  # Descarga por capa, 2025, municipios
  itrf2008 = paste0(
    'PROJCS["MEXICO_ITRF_2008_LCC",GEOGCS["GCS_ITRF_2008",DATUM["D_ITRF_2008",',
    'SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],',
    'UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],',
    'PARAMETER["False_Easting",2500000.0],PARAMETER["False_Northing",0.0],',
    'PARAMETER["Central_Meridian",-102.0],PARAMETER["Standard_Parallel_1",17.5],',
    'PARAMETER["Standard_Parallel_2",29.5],PARAMETER["Latitude_Of_Origin",12.0],UNIT["Meter",1.0]]'
  ),
  # Descarga por capa, 2025, AGEB rurales: la misma proyeccion con otros nombres
  itrf2008_b = paste0(
    'PROJCS["Mexico_ITRF2008_LCC",GEOGCS["GCS_Mexico_ITRF2008",DATUM["D_Mexico_ITRF2008",',
    'SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],',
    'UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],',
    'PARAMETER["False_Easting",2500000.0],PARAMETER["False_Northing",0.0],',
    'PARAMETER["Central_Meridian",-102.0],PARAMETER["Standard_Parallel_1",17.5],',
    'PARAMETER["Standard_Parallel_2",29.5],PARAMETER["Latitude_Of_Origin",12.0],UNIT["Meter",1.0]]'
  ),
  # Producto de 1995, municipios
  itrf92 = paste0(
    'PROJCS["North_America_Lambert_Conformal_Conic",GEOGCS["ITRF92",DATUM["D_ITRF_1992",',
    'SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],',
    'UNIT["Degree",0.0174532925199433]],PROJECTION["Lambert_Conformal_Conic"],',
    'PARAMETER["False_Easting",2500000.0],PARAMETER["False_Northing",0.0],',
    'PARAMETER["Central_Meridian",-102.0],PARAMETER["Standard_Parallel_1",17.5],',
    'PARAMETER["Standard_Parallel_2",29.5],PARAMETER["Latitude_Of_Origin",12.0],UNIT["Meter",1.0]]'
  ),
  # Producto de 2014, entidades: coordenadas geograficas
  geograficas = paste0(
    'GEOGCS["ITRF_1992",DATUM["D_ITRF_1992",SPHEROID["GRS_1980",6378137.0,298.257222101]],',
    'PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
  )
)

# Geometrias inventadas ----------------------------------------------------------------

cuadro <- function(x, y, lado = 1000) {
  sf::st_polygon(list(rbind(c(x, y), c(x + lado, y), c(x + lado, y + lado), c(x, y + lado), c(x, y))))
}
linea <- function(x, y, largo = 500) {
  sf::st_linestring(rbind(c(x, y), c(x + largo, y + largo)))
}
# Un mapa: los atributos y una geometria por fila, sin proyeccion (el .prj se
# escribe aparte, con el texto del INEGI).
mapa <- function(atributos, geometrias) {
  sf::st_sf(atributos, geometry = sf::st_sfc(geometrias), stringsAsFactors = FALSE)
}
cuadros <- function(n, x = 2400000, y = 1100000, lado = 1000) {
  lapply(seq_len(n), function(i) cuadro(x + (i - 1) * 2 * lado, y, lado))
}
lineas <- function(n, x = 2400000, y = 1100000) {
  lapply(seq_len(n), function(i) linea(x + (i - 1) * 1000, y))
}

# Escribe un shapefile en `carpeta` y devuelve sus archivos.
#
#   codificacion  Codificacion del texto del DBF.
#   cpg           Texto del archivo .cpg, o NULL para no incluirlo.
#   pagina        Byte de pagina de codigos del encabezado del DBF (0 = no
#                 declara ninguna; 0x02 = 850 de DOS; 0x57 = ANSI).
#   proyeccion    Texto del .prj, o NULL para no incluirlo.
#   extension_cpg Como se escribe la extension del .cpg.
escribir_capa <- function(carpeta, nombre, datos, proyeccion, codificacion = "UTF-8", cpg = "UTF-8",
                          pagina = NULL, extension_cpg = "cpg") {
  dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
  base <- file.path(carpeta, nombre)
  # sf avisa que el mapa no tiene proyeccion: el .prj se escribe despues
  suppressMessages(
    sf::st_write(datos, paste0(base, ".shp"), quiet = TRUE, layer_options = paste0("ENCODING=", codificacion))
  )
  unlink(paste0(base, c(".prj", ".cpg")))
  if (!is.null(proyeccion)) {
    cat(proyeccion, file = paste0(base, ".prj"))
  }
  if (!is.null(cpg)) {
    cat(cpg, file = paste0(base, ".", extension_cpg))
  }
  if (!is.null(pagina)) {
    conexion <- file(paste0(base, ".dbf"), open = "r+b")
    seek(conexion, where = 29, rw = "write")
    writeBin(as.raw(pagina), conexion)
    close(conexion)
  }
  invisible(list.files(carpeta, pattern = paste0("^", nombre, "[.]"), full.names = TRUE))
}

# Comprime el contenido de `carpeta` en un zip. Con guardar = TRUE lo guarda
# sin comprimir, como los zip que el INEGI incluye dentro de sus productos.
comprimir <- function(carpeta, zip, guardar = FALSE) {
  unlink(zip)
  anterior <- setwd(carpeta)
  on.exit(setwd(anterior))
  utils::zip(zip, list.files(".", recursive = TRUE), flags = if (guardar) "-r0Xq" else "-r9Xq")
  invisible(zip)
}

temporal <- function() {
  carpeta <- tempfile("muestra_sig_")
  dir.create(carpeta)
  carpeta
}

# Descarga por capa ---------------------------------------------------------------------

municipios <- function(claves, nombres) {
  mapa(
    data.frame(
      CVEGEO = claves, CVE_ENT = substr(claves, 1, 2), CVE_MUN = substr(claves, 3, 5), NOMGEO = nombres,
      stringsAsFactors = FALSE
    ),
    cuadros(length(claves))
  )
}

carpeta <- temporal()
escribir_capa(
  carpeta, "2025_1_00_MUN",
  municipios(
    c("01001", "01002", "06001", "06002", "18001"),
    c("Aguascalientes", "Asientos", "Armer\u00eda", "Colima", "Acaponeta")
  ),
  prj$itrf2008
)
# Archivos que el INEGI incluye y que la funcion no extrae
writeBin(as.raw(1:16), file.path(carpeta, "2025_1_00_MUN.sbn"))
writeLines("<metadata>Muestra inventada</metadata>", file.path(carpeta, "2025_1_00_MUN.xml"))
comprimir(carpeta, file.path(destino, "capa_2025_1_00_MUN.zip"))

carpeta <- temporal()
escribir_capa(carpeta, "2025_1_01_MUN", municipios(c("01001", "01002"), c("Aguascalientes", "Asientos")), prj$itrf2008)
comprimir(carpeta, file.path(destino, "capa_2025_1_01_MUN.zip"))

carpeta <- temporal()
escribir_capa(
  carpeta, "2025_1_06_MUN", municipios(c("06001", "06002"), c("Armer\u00eda", "Colima")), prj$itrf2008_b,
  codificacion = "CP1252", cpg = "1252"
)
comprimir(carpeta, file.path(destino, "capa_2025_1_06_MUN.zip"))

carpeta <- temporal()
escribir_capa(
  carpeta, "2025_1_01_M",
  mapa(
    data.frame(
      CVEGEO = c("0100100010229001", "0100100010229002", "0100100010233001"),
      CVE_ENT = "01", CVE_MUN = "001", CVE_LOC = "0001", CVE_AGEB = c("0229", "0229", "0233"),
      CVE_MZA = c("001", "002", "001"), AMBITO = c("Urbana", "Urbana", "NULL"),
      TIPOMZA = c("T\u00edpica", "T\u00edpica", "Edificio-Manzana"), stringsAsFactors = FALSE
    ),
    cuadros(3, lado = 100)
  ),
  prj$itrf2008
)
comprimir(carpeta, file.path(destino, "capa_2025_1_01_M.zip"))

frentes <- function(entidad, vialidades) {
  n <- length(vialidades)
  mapa(
    data.frame(
      CVEGEO = paste0(entidad, "00100010229", sprintf("%03d", seq_len(n))), CVE_ENT = entidad, CVE_MUN = "001",
      CVE_LOC = "0001", CVE_AGEB = "0229", CVE_MZA = sprintf("%03d", seq_len(n)), CVEFT = as.character(seq_len(n)),
      NOMVIAL = vialidades, TIPOVIAL = "Calle", stringsAsFactors = FALSE
    ),
    lineas(n)
  )
}
carpeta <- temporal()
escribir_capa(carpeta, "2025_1_01_FM", frentes("01", c("Francisco I. Madero", "Jos\u00e9 Mar\u00eda Ch\u00e1vez")), prj$itrf2008)
comprimir(carpeta, file.path(destino, "capa_2025_1_01_FM.zip"))
carpeta <- temporal()
escribir_capa(carpeta, "2025_1_06_FM", frentes("06", c("Rey Colim\u00e1n", "Ni\u00f1os H\u00e9roes", "Pino Su\u00e1rez")), prj$itrf2008)
comprimir(carpeta, file.path(destino, "capa_2025_1_06_FM.zip"))

carpeta <- temporal()
escribir_capa(
  carpeta, "2020_1_06_TI",
  mapa(
    data.frame(
      CVEGEO = c("060070199", "060070200"), CVE_ENT = "06", CVE_MUN = "007", CVE_AGEB = c("0199", "0200"),
      NOMGEO = c("Isla Socorro", "Isla San Benedicto"), stringsAsFactors = FALSE
    ),
    cuadros(2, x = 1500000, y = 800000)
  ),
  prj$itrf2008
)
comprimir(carpeta, file.path(destino, "capa_2020_1_06_TI.zip"))

# Deflate64: el metodo de compresion del archivo nacional de frentes de manzana
# de 2025, que utils::unzip() no abre.
if (nzchar(Sys.which("7z"))) {
  carpeta <- temporal()
  escribir_capa(carpeta, "2025_1_00_FM", frentes("01", c("Francisco I. Madero", "Jos\u00e9 Mar\u00eda Ch\u00e1vez")), prj$itrf2008)
  unlink(file.path(destino, "capa_deflate64.zip"))
  anterior <- setwd(carpeta)
  system2("7z", c("a", "-tzip", "-mm=Deflate64", "-bso0", "-bsp0", shQuote(file.path(destino, "capa_deflate64.zip")), "."))
  setwd(anterior)
} else {
  message("No se genero capa_deflate64.zip: falta el programa 7z.")
}

# Productos completos: un zip por capa (1995 y 2005) ------------------------------------

# Zip de una capa, como los que el INEGI guarda dentro del producto.
zip_de_capa <- function(carpeta_producto, zip, ...) {
  carpeta <- temporal()
  escribir_capa(carpeta, ...)
  comprimir(carpeta, file.path(carpeta_producto, zip))
}

# 1995. Sin .cpg y sin pagina de codigos en el DBF; texto en Latin-1.
producto <- temporal()
zip_de_capa(
  producto, "mge1995.zip", "Entidades_1995",
  mapa(
    data.frame(
      CVE_ENT = c("01", "06", "16"), NOM_ENT = c("Aguascalientes", "Colima", "Michoac\u00e1n de Ocampo"),
      stringsAsFactors = FALSE
    ),
    cuadros(3, lado = 50000)
  ),
  prj$itrf92, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
zip_de_capa(
  producto, "mgm1995.zip", "Municipios_1995",
  mapa(
    data.frame(
      CVE_ENT = c("01", "06", "06", "16"), CVE_MUN = c("001", "001", "007", "053"),
      NOM_MUN = c("Aguascalientes", "Armer\u00eda", "Manzanillo", "Morelia"), stringsAsFactors = FALSE
    ),
    cuadros(4)
  ),
  prj$itrf92, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
comprimir(producto, file.path(destino, "producto_702825292836_s.zip"), guardar = TRUE)

# 2005. Entidades y municipios sin .prj; la clave de la entidad es CVE_EDO. Las
# AGEB urbanas si traen .prj y solo tienen la clave concatenada (CLAVE).
producto <- temporal()
zip_de_capa(
  producto, "mge2005v_1_0.zip", "Entidades_2005",
  mapa(
    data.frame(CVE_EDO = c("01", "06"), NOMBRE_EDO = c("Aguascalientes", "Colima"), stringsAsFactors = FALSE),
    cuadros(2, lado = 50000)
  ),
  NULL, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
zip_de_capa(
  producto, "mgm2005v_1_0.zip", "Municipios_2005",
  mapa(
    data.frame(
      CVE_EDO = c("01", "06", "06"), CVE_MUN = c("001", "001", "007"),
      NOMBRE_MUN = c("Aguascalientes", "Armer\u00eda", "Manzanillo"), CVE_CONCA = c("01001", "06001", "06007"),
      stringsAsFactors = FALSE
    ),
    cuadros(3)
  ),
  NULL, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
zip_de_capa(
  producto, "mgau2005v_1_0.zip", "agebs_urb_2005",
  mapa(
    data.frame(CLAVE = c("010010001287A", "0600200010153", "0600700010327"), stringsAsFactors = FALSE),
    cuadros(3, lado = 300)
  ),
  prj$itrf92, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
comprimir(producto, file.path(destino, "producto_702825292850_s.zip"), guardar = TRUE)

# Productos completos: capas en carpetas (2010 version 4.3 y 2014) -------------------------

# 2010, version 4.3. El zip se escribe byte por byte para incluir un archivo
# con el nombre en la pagina de codigos 850 de DOS, como el del INEGI.
producto <- temporal()
escribir_capa(
  file.path(producto, "32_Entidades_Federativas"), "ESTADOS",
  mapa(
    data.frame(CVE_ENT = c("01", "06"), NOM_ENT = c("Aguascalientes", "Colima"), stringsAsFactors = FALSE),
    cuadros(2, lado = 50000)
  ),
  prj$itrf92, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
escribir_capa(
  file.path(producto, "32_Entidades_Federativas"), "MUNICIPIOS",
  mapa(
    data.frame(
      CVE_ENT = c("01", "06", "06"), CVE_MUN = c("001", "001", "007"),
      NOM_MUN = c("Aguascalientes", "Armer\u00eda", "Manzanillo"), stringsAsFactors = FALSE
    ),
    cuadros(3)
  ),
  prj$itrf92, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0
)
# Zonas interestatales: la clave de la entidad son letras y el DBF declara la
# pagina de codigos 850
escribir_capa(
  file.path(producto, "Zonas_Interestatales"), "ESTADOS",
  mapa(
    data.frame(
      CVE_ENT = c("CY", "CQ"), NOM_ENT = c("Campeche - Yucat\u00e1n", "Campeche - Quintana Roo"),
      stringsAsFactors = FALSE
    ),
    cuadros(2, x = 3700000, y = 900000, lado = 20000)
  ),
  prj$itrf92, codificacion = "CP850", cpg = NULL, pagina = 0x02
)
archivos <- list.files(producto, recursive = TRUE)
entradas <- lapply(archivos, function(archivo) {
  ruta <- file.path(producto, archivo)
  list(nombre = charToRaw(archivo), contenido = readBin(ruta, what = "raw", n = file.size(ruta)))
})
leeme <- iconv("LEEME MARCO GEOESTAD\u00cdSTICO 2010 VERSION 4.3.docx", from = "UTF-8", to = "CP850", toRaw = TRUE)[[1]]
entradas <- c(entradas, list(list(nombre = leeme, contenido = charToRaw("Muestra inventada"))))
unlink(file.path(destino, "producto_702825296520_s.zip"))
escribir_zip_crudo("producto_702825296520_s.zip", entradas)

# 2014 (version 6.2). Coordenadas geograficas; el DBF declara la pagina de
# codigos y no hay .cpg. El archivo del producto 702825004386 se llama
# 889463084105_s.zip.
producto <- temporal()
geograficos <- function(n) cuadros(n, x = -104.5, y = 19, lado = 0.25)
escribir_capa(
  file.path(producto, "CONTINUO_NACIONAL"), "ESTADOS",
  mapa(
    data.frame(CVE_ENT = c("01", "06"), NOM_ENT = c("Aguascalientes", "Colima"), stringsAsFactors = FALSE),
    geograficos(2)
  ),
  prj$geograficas, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0x57
)
escribir_capa(
  file.path(producto, "CONTINUO_NACIONAL"), "MUNICIPIOS",
  mapa(
    data.frame(
      CVE_ENT = c("01", "06", "06"), CVE_MUN = c("001", "001", "007"),
      NOM_MUN = c("Aguascalientes", "Armer\u00eda", "Manzanillo"), stringsAsFactors = FALSE
    ),
    geograficos(3)
  ),
  prj$geograficas, codificacion = "ISO-8859-1", cpg = NULL, pagina = 0x57
)
writeLines("<html>Muestra inventada</html>", file.path(producto, "MGN6.2.html"))
comprimir(producto, file.path(destino, "producto_889463084105_s.zip"))

# Productos por entidad (febrero de 2018 y Censo Agropecuario 2022) ----------------------

# Zip de una entidad. `capas` es una lista con nombre (el sufijo del archivo:
# "mun", "ent" ...) y `opciones`, argumentos de escribir_capa() por sufijo.
zip_de_entidad <- function(carpeta_producto, zip, clave, carpeta_datos, capas, opciones = list()) {
  carpeta <- temporal()
  for (sufijo in names(capas)) {
    argumentos <- c(
      list(file.path(carpeta, carpeta_datos), paste0(clave, sufijo), capas[[sufijo]]),
      utils::modifyList(list(proyeccion = prj$itrf2008), opciones[[sufijo]] %||% list())
    )
    do.call(escribir_capa, argumentos)
  }
  dir.create(file.path(carpeta, "catalogos"))
  writeLines("Muestra inventada", file.path(carpeta, "catalogos", "leeme.txt"))
  comprimir(carpeta, file.path(carpeta_producto, zip))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

entidad <- function(clave, nombre) {
  mapa(data.frame(CVE_GEO = clave, CVE_ENT = clave, NOM_ENT = nombre, stringsAsFactors = FALSE), cuadros(1, lado = 50000))
}
ageb_urbana <- function(clave) {
  mapa(
    data.frame(
      CVE_ENT = clave, CVE_MUN = "001", CVE_LOC = "0001", CVE_AGEB = c("0229", "0233"), stringsAsFactors = FALSE
    ),
    cuadros(2, lado = 300)
  )
}
islas <- function(clave) {
  mapa(
    data.frame(
      CVEGEO = paste0(clave, "0070199"), CVE_ENT = clave, CVE_MUN = "007", CVE_AGEB = "0199",
      NOMGEO = "Isla Socorro", stringsAsFactors = FALSE
    ),
    cuadros(1, x = 1500000, y = 800000)
  )
}

# Febrero de 2018
producto <- temporal()
zip_de_entidad(
  producto, "01_aguascalientes.zip", "01", "conjunto de datos",
  list(
    ent = entidad("01", "Aguascalientes"),
    mun = municipios(c("01001", "01002"), c("Aguascalientes", "Asientos")),
    a = ageb_urbana("01")
  ),
  list(
    ent = list(codificacion = "ISO-8859-1", cpg = "ISO 88591"),
    a = list(codificacion = "ISO-8859-1", cpg = "ISO 88591", extension_cpg = "CPG")
  )
)
zip_de_entidad(
  producto, "06_colima.zip", "06", "conjunto de datos",
  list(
    ent = entidad("06", "Colima"),
    mun = municipios(c("06001", "06002"), c("Armer\u00eda", "Colima")),
    a = ageb_urbana("06"),
    territorio_insular = islas("06")
  ),
  list(
    ent = list(codificacion = "ISO-8859-1", cpg = "ISO 88591"),
    a = list(codificacion = "ISO-8859-1", cpg = "ISO 88591", extension_cpg = "CPG"),
    territorio_insular = list(codificacion = "ISO-8859-1", cpg = "88591")
  )
)
comprimir(producto, file.path(destino, "producto_889463526636_s.zip"), guardar = TRUE)

# Censo Agropecuario 2022
producto <- temporal()
zip_de_entidad(
  producto, "01_aguascalientes.zip", "01", "conjunto_de_datos",
  list(mun = municipios(c("01001", "01002"), c("Aguascalientes", "Asientos")))
)
zip_de_entidad(
  producto, "06_colima.zip", "06", "conjunto_de_datos",
  list(mun = municipios(c("06001", "06002"), c("Armer\u00eda", "Colima")), ti = islas("06")),
  list(mun = list(proyeccion = prj$itrf2008_b))
)
zip_de_entidad(
  producto, "mgagro2022_integrado.zip", "00", "conjunto_de_datos",
  list(
    mun = municipios(
      c("01001", "01002", "06001", "06002", "18001"),
      c("Aguascalientes", "Asientos", "Armer\u00eda", "Colima", "Acaponeta")
    ),
    ent = mapa(
      data.frame(
        CVEGEO = c("01", "06", "18"), CVE_ENT = c("01", "06", "18"),
        NOMGEO = c("Aguascalientes", "Colima", "Nayarit"), stringsAsFactors = FALSE
      ),
      cuadros(3, lado = 50000)
    )
  )
)
comprimir(producto, file.path(destino, "producto_794551067284_s.zip"), guardar = TRUE)

# Indice en formato zip64 -------------------------------------------------------------------

# Python escribe el indice en formato zip64 solo cuando el zip rebasa los
# limites del formato original; se le bajan esos limites para obtenerlo con un
# archivo pequeño.
if (nzchar(Sys.which("python3"))) {
  programa <- c(
    "import sys, zipfile",
    "zipfile.ZIP64_LIMIT = 0",
    "zipfile.ZIP_FILECOUNT_LIMIT = 0",
    "with zipfile.ZipFile(sys.argv[1], 'w', zipfile.ZIP_DEFLATED) as z:",
    "    z.writestr('carpeta/uno.txt', 'Muestra inventada\\n' * 20)",
    "    z.writestr('dos.txt', 'Muestra inventada\\n')"
  )
  guion <- tempfile(fileext = ".py")
  writeLines(programa, guion)
  unlink(file.path(destino, "zip64.zip"))
  system2("python3", c(shQuote(guion), shQuote(file.path(destino, "zip64.zip"))))
} else {
  message("No se genero zip64.zip: falta el programa python3.")
}

message("Muestras escritas en ", destino, ": ", paste(list.files(destino), collapse = ", "))
