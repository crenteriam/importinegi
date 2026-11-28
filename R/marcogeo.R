# Marco Geoestadistico: ediciones, mapas, descarga y lectura --------------------
#
# El INEGI publica el Marco Geoestadistico por dos vias:
#
#   A. Productos completos: un zip por edicion con todos sus mapas. Los 26
#      productos estan en el objeto interno `mg_productos`.
#   B. Descarga por capa: un zip por edicion, mapa y area (todo el pais o una
#      entidad), de 2017 a 2026. Sus direcciones siguen un patron; el objeto
#      interno `mg_capas` dice cuales existen y cuanto pesa el nacional.
#
# Los dos objetos se generan con data-raw/marcogeo.R.
#
# Cada edicion se descarga por una de tres rutas (columna `via`):
#
#   "A"  Producto completo (de 1995 a junio de 2017, de 35 a 202 MB).
#   "B"  Descarga por capa (diez ediciones, de diciembre de 2017 a 2026).
#   "P"  Producto parcial: tres ediciones posteriores a 2017 que solo existen
#        como producto completo de 2.4 a 2.9 GB. El zip de cada entidad esta
#        guardado sin comprimir dentro del producto y se descarga solo ese
#        tramo (ver utils-zip-remoto.R).
#
# La funcion devuelve cada mapa como lo publica el INEGI: no cambia los nombres
# de las columnas, la proyeccion ni el contenido.

# Ediciones -----------------------------------------------------------------------

# Una fila por edicion. `year` es el año de edicion del INEGI. `version` es la
# distincion que hace el INEGI entre las ediciones de un mismo año (vacia si el
# año tiene una sola); `etiqueta`, el texto con que el INEGI identifica una
# edicion unica, que tambien se acepta en el argumento `version`. `clave` es la
# clave del producto en `mg_productos`.
marcogeo_ediciones <- function() {
  utils::read.table(
    text = "
1995||conteo 1995|702825292836|A
2000||censo 2000|702825292843|A
2005||1.0|702825292850|A
2007||censos agropecuarios 2007|702825292867|A
2009||4.3|702825292782|A
2010|4.3||702825296520|A
2010|5.0||702825292812|A
2010|5.0.A||702825292805|A
2010|5.0.A 1:20 000||702825296629|A
2013||6.0|702825292829|A
2014||6.2|702825004386|A
2016||junio|702825217341|A
2017|junio||889463142683|A
2017|diciembre||889463171829|B
2018|febrero||889463526636|P
2018|junio||889463592587|P
2018|diciembre||889463674658|B
2019||septiembre|889463776079|B
2020||censo de poblacion y vivienda 2020|889463807469|B
2021||diciembre|889463849568|B
2022||diciembre|889463770541|B
2023|censo agropecuario 2022||794551067284|P
2023|diciembre||794551067314|B
2024||2024|794551132173|B
2025||2025|794551163061|B
2026||encuesta intercensal 2025|794551196649|B",
    sep = "|", quote = "", strip.white = TRUE, stringsAsFactors = FALSE,
    col.names = c("year", "version", "etiqueta", "clave", "via"),
    colClasses = c("integer", "character", "character", "character", "character"),
    na.strings = character()
  )
}

# Texto para comparar versiones: sin mayusculas, acentos, puntuacion ni espacios.
marcogeo_normalizar <- function(texto) {
  gsub(" ", "", normalizar_texto(texto), fixed = TRUE)
}

# Nombre de una edicion para los mensajes: "2018 (febrero)", "2024".
marcogeo_nombre_edicion <- function(edicion) {
  if (nzchar(edicion$version)) paste0(edicion$year, " (", edicion$version, ")") else as.character(edicion$year)
}

# Fila de la edicion que corresponde a `year` y `version`.
marcogeo_edicion <- function(year, version = NULL) {
  ediciones <- marcogeo_ediciones()
  if (missing(year) || is.null(year)) {
    detener(
      "Debes indicar el a\u00f1o de edici\u00f3n del Marco Geoestad\u00edstico en `year`. ",
      "A\u00f1os disponibles: ", enumerar(unique(ediciones$year)), "."
    )
  }
  validar_escalar(year, "year")
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero)) {
    detener("`year` debe ser un a\u00f1o en formato num\u00e9rico (por ejemplo, 2020).")
  }
  del_year <- ediciones[ediciones$year == numero, ]

  if (is.null(version)) {
    if (nrow(del_year) == 0) {
      detener(
        "No hay una edici\u00f3n del Marco Geoestad\u00edstico con `year = ", numero, "`. ",
        "A\u00f1os disponibles: ", enumerar(unique(ediciones$year)), "."
      )
    }
    if (nrow(del_year) > 1) {
      detener(
        "`year = ", numero, "` tiene ", nrow(del_year), " ediciones del Marco Geoestad\u00edstico. ",
        "Indica una en `version`: ", enumerar(del_year$version), "."
      )
    }
    return(del_year)
  }

  validar_escalar(version, "version")
  buscada <- marcogeo_normalizar(version)
  coincide <- function(filas) {
    textos <- cbind(marcogeo_normalizar(filas$version), marcogeo_normalizar(filas$etiqueta))
    exacta <- which(textos[, 1] == buscada | textos[, 2] == buscada)
    if (length(exacta) > 0 || !nzchar(buscada)) {
      return(exacta)
    }
    which(grepl(buscada, textos[, 1], fixed = TRUE) | grepl(buscada, textos[, 2], fixed = TRUE))
  }
  halladas <- coincide(del_year)
  if (length(halladas) == 1) {
    return(del_year[halladas, ])
  }

  # La version no corresponde a ese año: puede ser de otro (el Marco
  # Geoestadistico de la Encuesta Intercensal 2025 tiene año de edicion 2026).
  otras <- ediciones[ediciones$year != numero, ]
  otras <- otras[coincide(otras), ]
  sugerencia <- if (length(halladas) == 0 && nrow(otras) == 1) {
    paste0(
      " El INEGI public\u00f3 esa edici\u00f3n con el a\u00f1o ", otras$year, ": usa `year = ", otras$year, "`",
      if (nzchar(otras$version)) paste0(" y `version = ", comillas(otras$version), "`"), "."
    )
  } else {
    ""
  }
  if (nrow(del_year) == 0) {
    detener(
      "No hay una edici\u00f3n del Marco Geoestad\u00edstico con `year = ", numero, "`.", sugerencia,
      " A\u00f1os disponibles: ", enumerar(unique(ediciones$year)), "."
    )
  }
  opciones <- if (nrow(del_year) > 1) {
    paste0(" Versiones de ", numero, ": ", enumerar(del_year$version), ".")
  } else {
    paste0(" `year = ", numero, "` tiene una sola edici\u00f3n y no necesita `version`.")
  }
  detener(
    "`version = ", comillas(version), "` ",
    if (length(halladas) > 1) "coincide con varias ediciones" else "no es una edici\u00f3n",
    " de `year = ", numero, "`.", sugerencia, opciones
  )
}

# Fila de `mg_productos` de una edicion.
marcogeo_producto <- function(edicion) {
  producto <- mg_productos[mg_productos$clave == edicion$clave, ]
  if (nrow(producto) != 1) {
    detener("El registro del paquete no tiene el producto ", edicion$clave, " del Marco Geoestad\u00edstico.")
  }
  producto
}

marcogeo_url_producto <- function(edicion) {
  inegi_url(marcogeo_producto(edicion)$ruta)
}

# Direccion de una capa en la descarga por capa del INEGI.
marcogeo_url_capa <- function(year, clave, capa) {
  inegi_url(paste0(
    "/contenidos/descargadenue/MGdescarga/MGN", year, "/", if (clave != "00") paste0(clave, "/"),
    year, "_1_", clave, "_", capa, ".zip"
  ))
}

# Mapas -----------------------------------------------------------------------------

# Vocabulario de `mapa`. `capa` es el codigo del INEGI en la descarga por capa y
# `sufijo`, el del nombre del archivo en los productos por entidad ("06mun").
marcogeo_mapas <- function() {
  data.frame(
    mapa = c(
      "entidades", "municipios", "ageb_rural", "ageb_urbana", "localidades_urbanas",
      "localidades_amanzanadas", "localidades_rurales", "integracion_territorial",
      "territorio_insular", "manzanas", "frentes", "vialidades", "servicios_area",
      "servicios_linea", "servicios_punto", "caserio_disperso", "poligono_externo",
      "poligono_externo_manzana"
    ),
    capa = c(
      "ENT", "MUN", "AR", "A", NA, "L", "LPR", NA, "TI", "M", "FM", "E", "SIA", "SIL", "SIP", "CD",
      "PE", "PEM"
    ),
    sufijo = c(
      "ent", "mun", "ar", "a", NA, "l", "lpr", NA, "ti|territorio_insular", "m", "fm", "e", "sia",
      "sil", "sip", "cd", "pe", "pem"
    ),
    stringsAsFactors = FALSE
  )
}

# Mapas con archivo de todo el pais en el zip nacional de los productos por
# entidad. El zip nacional trae ademas una capa con las AGEB urbanas y rurales
# juntas, que no se usa.
marcogeo_mapas_nacional <- function() {
  c("entidades", "municipios", "localidades_amanzanadas", "localidades_rurales")
}

# Capas de los productos completos (via "A"): ubicacion de cada mapa dentro del
# zip, sin la extension. "interno.zip!/capa" indica una capa que esta dentro de
# un zip interno. Los nombres son los del INEGI y cambian con cada edicion.
marcogeo_capas_producto <- function(clave) {
  por_capa <- function(sufijo, entidades, municipios, ...) {
    otras <- c(...)
    capas <- c(entidades = entidades, municipios = municipios, otras)
    prefijo <- c(
      entidades = "mge", municipios = "mgm", ageb_urbana = "mgau", ageb_rural = "mgar",
      localidades_urbanas = "mglu", localidades_rurales = "mglr"
    )
    stats::setNames(paste0(prefijo[names(capas)], sufijo, ".zip!/", capas), names(capas))
  }
  en_carpeta <- function(carpeta, ..., zonas = NULL, capas_zonas = NULL) {
    capas <- c(...)
    rutas <- stats::setNames(paste0(carpeta, "/", capas), names(capas))
    if (!is.null(zonas)) {
      rutas <- c(rutas, stats::setNames(paste0(zonas, "/", capas_zonas), paste0("zonas_", names(capas_zonas))))
    }
    rutas
  }
  switch(
    clave,
    "702825292836" = por_capa("1995", "Entidades_1995", "Municipios_1995"),
    "702825292843" = por_capa("2000", "Entidades_2000", "Municipios_2000", ageb_urbana = "agebs_urb_2000"),
    "702825292850" = por_capa("2005v_1_0", "Entidades_2005", "Municipios_2005", ageb_urbana = "agebs_urb_2005"),
    "702825292867" = por_capa(
      "2007", "Entidades_2007", "Municipios_2007", ageb_rural = "AGEB_rural_2007",
      localidades_urbanas = "Localidades urbanas_2007", localidades_rurales = "Localidades rurales_2007"
    ),
    "702825292782" = por_capa(
      "2009v4_3", "Entidades_2009", "Municipios_2009", ageb_urbana = "ageb_urb_2009",
      localidades_urbanas = "Localidades urbanas_2009", localidades_rurales = "Localidades rurales_2009"
    ),
    "702825296520" = en_carpeta(
      "32_Entidades_Federativas",
      entidades = "ESTADOS", municipios = "MUNICIPIOS", ageb_rural = "AGEB",
      localidades_urbanas = "POLIGONOS_URBANOS", localidades_rurales = "LOCALIDADES_RURALES",
      zonas = "Zonas_Interestatales",
      capas_zonas = c(
        entidades = "ESTADOS", municipios = "MUNICIPIOS", ageb_rural = "AGEB",
        localidades_urbanas = "POLIGONOS_URBANOS", localidades_rurales = "LOCALIDADES_RURALES"
      )
    ),
    "702825292812" = por_capa(
      "2010v5_0", "Entidades_2010_5", "Municipios_2010_5", ageb_urbana = "AGEB_urb_2010_5",
      localidades_urbanas = "Localidades_urbanas_2010_5", localidades_rurales = "Localidades_rurales_2010_5"
    ),
    "702825292805" = por_capa(
      "2010v5_0", "estados", "municipios",
      localidades_urbanas = "poligonos_urbanos", localidades_rurales = "localidades_rurales"
    ),
    "702825296629" = en_carpeta(
      "32_Entidades_Federativas/SHAPE",
      entidades = "ESTADOS", municipios = "MUNICIPIOS", localidades_urbanas = "POLIGONOS_URBANOS",
      localidades_rurales = "LOCALIDADES_RURALES", territorio_insular = "ISLAS",
      zonas = "Zonas_Pendientes_de_Asignar/SHAPE",
      capas_zonas = c(
        entidades = "ESTADOS", municipios = "MUNICIPIOS", localidades_urbanas = "POLIGONOS_URBANOS",
        localidades_rurales = "LOCALIDADES_RURALES"
      )
    ),
    "702825292829" = por_capa(
      "2013v6_0", "Entidades_2013", "Municipios_2013", ageb_urbana = "agebs_urbanos_2013",
      localidades_urbanas = "Localidades urbanas_2013", localidades_rurales = "Localidades rurales_2013"
    ),
    "702825004386" = en_carpeta(
      "CONTINUO_NACIONAL",
      entidades = "ESTADOS", municipios = "MUNICIPIOS", ageb_rural = "AGEB",
      integracion_territorial = "INTEGRACION_TERRITORIAL", territorio_insular = "TERRITORIO_INSULAR"
    ),
    "702825217341" = en_carpeta(
      "conjunto_de_datos",
      entidades = "areas_geoestadisticas_estatales", municipios = "areas_geoestadisticas_municipales",
      ageb_rural = "areas_geoestadisticas_basicas",
      localidades_amanzanadas = "poligonos_localidades_urbanas_y_rurales",
      integracion_territorial = "integracion_territorial", territorio_insular = "territorio_insular"
    ),
    "889463142683" = en_carpeta(
      "conjunto_de_datos",
      entidades = "areas_geoestadisticas_estatales", municipios = "areas_geoestadisticas_municipales",
      ageb_rural = "areas_geoestadisticas_basicas_rurales",
      localidades_amanzanadas = "poligonos_localidades_urbanas_y_rurales",
      localidades_rurales = "localidades_rurales_puntuales", territorio_insular = "territorio_insular"
    ),
    detener("El producto ", clave, " del Marco Geoestad\u00edstico no tiene una lista de capas.")
  )
}

# Mapas de una edicion, en el orden del vocabulario.
marcogeo_mapas_edicion <- function(edicion) {
  mapas <- marcogeo_mapas()
  if (edicion$via == "A") {
    capas <- names(marcogeo_capas_producto(edicion$clave))
    return(c(intersect(mapas$mapa, capas), capas[startsWith(capas, "zonas_")]))
  }
  con_capa <- mapas[!is.na(mapas$capa), ]
  if (edicion$via == "B") {
    publicadas <- mg_capas$capa[mg_capas$year == edicion$year & mg_capas$faltan != "todas"]
    return(con_capa$mapa[con_capa$capa %in% publicadas])
  }
  # Productos por entidad: los poligonos externos existen desde diciembre de 2018
  if (edicion$year < 2019) con_capa$mapa[!con_capa$capa %in% c("PE", "PEM")] else con_capa$mapa
}

# Valida `mapa` y lo devuelve con el nombre del vocabulario.
marcogeo_mapa <- function(mapa, edicion) {
  mapas <- marcogeo_mapas()
  disponibles <- marcogeo_mapas_edicion(edicion)
  if (missing(mapa) || is.null(mapa)) {
    detener(
      "Debes indicar el mapa en `mapa`. Mapas de la edici\u00f3n ", marcogeo_nombre_edicion(edicion), ": ",
      enumerar(disponibles), "."
    )
  }
  validar_escalar(mapa, "mapa")
  if (!is.character(mapa)) {
    detener("`mapa` debe ser el nombre de un mapa, entre comillas (por ejemplo, \"municipios\").")
  }
  buscado <- gsub(" ", "_", normalizar_texto(mapa), fixed = TRUE)
  codigo <- match(toupper(buscado), mapas$capa)
  if (!is.na(codigo)) {
    buscado <- mapas$mapa[codigo]
  }
  anteriores <- c(
    ageb = "\"ageb_urbana\" o \"ageb_rural\"", urbano = "\"localidades_urbanas\"",
    rural = "\"localidades_rurales\""
  )
  if (buscado %in% names(anteriores)) {
    detener(
      "`mapa = ", comillas(mapa), "` era una opci\u00f3n de sig_marcogeo(). En sig_marcogeoestadistico() usa ",
      anteriores[[buscado]], "."
    )
  }
  conocidos <- c(mapas$mapa, paste0("zonas_", c(
    "entidades", "municipios", "ageb_rural", "localidades_urbanas", "localidades_rurales"
  )))
  if (!buscado %in% conocidos) {
    detener(
      "`mapa = ", comillas(mapa), "` no es una opci\u00f3n v\u00e1lida. Mapas de la edici\u00f3n ",
      marcogeo_nombre_edicion(edicion), ": ", enumerar(disponibles), "."
    )
  }
  if (!buscado %in% disponibles) {
    detener(
      "La edici\u00f3n ", marcogeo_nombre_edicion(edicion), " del Marco Geoestad\u00edstico no tiene el mapa ",
      comillas(buscado), ". Mapas de esa edici\u00f3n: ", enumerar(disponibles),
      ". bases_importinegi(\"sig_marcogeoestadistico\") lista los mapas de todas las ediciones."
    )
  }
  buscado
}

# Entidades ---------------------------------------------------------------------------

# Claves de las 32 entidades. Es una funcion para poder sustituirla en las pruebas.
marcogeo_todas <- function() {
  sprintf("%02d", 1:32)
}

marcogeo_nombres_entidad <- function(claves) {
  tabla <- inegi_estados()
  tabla$nombre[match(claves, tabla$clave)]
}

# Nombres de las entidades para los mensajes: "Colima, Nayarit".
marcogeo_lista_entidades <- function(claves) {
  paste(marcogeo_nombres_entidad(claves), collapse = ", ")
}

# Entidad por omision. Sigue la regla comun del paquete: todo el pais cuando
# hay un archivo nacional, y Aguascalientes, con aviso, cuando el INEGI publica
# el mapa solo por entidad. Tambien se toma Aguascalientes en las manzanas, los
# frentes de manzana y las vialidades, cuyo archivo nacional es muy grande.
marcogeo_estado_omision <- function(edicion, mapa) {
  if (edicion$via == "A") {
    return("00")
  }
  if (edicion$via == "B") {
    return(if (mapa %in% c("manzanas", "frentes", "vialidades")) "01" else "00")
  }
  if (marcogeo_tiene_nacional(edicion) && mapa %in% marcogeo_mapas_nacional()) "00" else "01"
}

# Los productos por entidad traen un zip nacional desde diciembre de 2018.
marcogeo_tiene_nacional <- function(edicion) {
  edicion$via == "P" && edicion$year >= 2019
}

marcogeo_avisar_omision <- function(edicion, mapa) {
  mapas <- marcogeo_mapas()
  detalle <- if (edicion$via == "B" && mapa != "frentes") {
    capa <- mg_capas[mg_capas$year == edicion$year & mg_capas$capa == mapas$capa[mapas$mapa == mapa], ]
    paste0("archivo nacional de ", tamano_texto(capa$bytes))
  } else if (edicion$via == "B") {
    "32 archivos, con m\u00e1s de 11 millones de l\u00edneas en total"
  } else {
    paste0("32 archivos, ", tamano_texto(marcogeo_producto(edicion)$bytes), " en total")
  }
  message(
    "No indicaste `estado`: se descarga el mapa de Aguascalientes. Para todo el pa\u00eds usa ",
    "estado = \"nacional\" (", detalle, ")."
  )
}

# Claves de las entidades pedidas: "00" para todo el pais o una o varias claves.
marcogeo_claves <- function(estado, edicion, mapa) {
  if (startsWith(mapa, "zonas_")) {
    if (!is.null(estado)) {
      message("Las zonas en disputa no pertenecen a una entidad: se devuelve el mapa completo y `estado` no se usa.")
    }
    return("00")
  }
  if (is.null(estado)) {
    omision <- marcogeo_estado_omision(edicion, mapa)
    if (omision != "00") {
      marcogeo_avisar_omision(edicion, mapa)
    }
    return(omision)
  }
  if (length(estado) == 0 || anyNA(estado)) {
    detener("`estado` debe ser el nombre o la clave de una o varias entidades, o \"nacional\".")
  }
  claves <- unique(vapply(as.list(estado), estado_clave, character(1)))
  if ("00" %in% claves) "00" else sort(claves)
}

# Descarga y lectura -----------------------------------------------------------------

# Carpeta donde se guardan las descargas grandes del Marco Geoestadistico: la
# copia local del usuario si la activo (opcion importinegi.cache) o, si no, una
# carpeta temporal que dura la sesion de R. Asi, pedir un segundo mapa de la
# misma edicion o de la misma entidad no repite la descarga.
marcogeo_ruta_local <- function(nombre) {
  carpeta <- if (isTRUE(getOption("importinegi.cache", FALSE))) {
    file.path(inegi_cache_dir(), "marcogeo")
  } else {
    file.path(tempdir(), "importinegi", "marcogeo")
  }
  file.path(carpeta, nombre)
}

marcogeo_descargar <- function(url, tamano = NULL) {
  destino <- marcogeo_ruta_local(basename(url))
  if (file.exists(destino) && es_zip(destino)) {
    message("Usando la copia local de ", basename(url), ".")
    return(destino)
  }
  inegi_descargar(url, destino = destino, tamano = tamano, cache = FALSE)
}

# Extrae de un zip los archivos de una capa (shp, shx, dbf, prj, cpg ...) en una
# carpeta temporal y devuelve la ruta del .shp. La capa se elige por su `ruta`
# dentro del zip (sin extension), por el nombre de su archivo (`base`, una o
# varias opciones) o, sin ninguno de los dos, por ser la unica del zip. Las
# comparaciones no distinguen mayusculas de minusculas. Con exigir = FALSE
# devuelve NULL si la capa no esta.
marcogeo_extraer_shp <- function(zip, ruta = NULL, base = NULL, exigir = TRUE) {
  indice <- zip_indice_local(zip)
  nombres <- tolower(indice$nombre)
  sin_extension <- sub("[.][a-z0-9]+$", "", nombres)
  candidatas <- which(grepl("[.]shp$", nombres))
  if (!is.null(ruta)) {
    buscada <- tolower(ruta)
    candidatas <- candidatas[
      sin_extension[candidatas] == buscada | endsWith(sin_extension[candidatas], paste0("/", buscada))
    ]
  } else if (!is.null(base)) {
    candidatas <- candidatas[basename(sin_extension[candidatas]) %in% tolower(base)]
  }
  if (length(candidatas) > 1) {
    # Se prefiere la capa menos anidada
    profundidad <- lengths(strsplit(nombres[candidatas], "/", fixed = TRUE))
    candidatas <- candidatas[profundidad == min(profundidad)]
  }
  if (length(candidatas) != 1) {
    if (length(candidatas) == 0 && !exigir) {
      return(NULL)
    }
    capas <- indice$nombre[grepl("[.]shp$", nombres)]
    detener(
      "El archivo ", basename(zip), " no tiene la estructura esperada: ",
      if (length(candidatas) == 0) "no contiene la capa" else "contiene varias veces la capa",
      if (!is.null(ruta)) paste0(" \"", ruta, "\""), if (!is.null(base)) paste0(" \"", base[1], "\""),
      ". Capas que contiene: ", if (length(capas) > 0) enumerar(capas, maximo = 20) else "ninguna",
      ". Puedes reportarlo en https://github.com/crenteriam/importinegi/issues"
    )
  }
  # De la capa solo se extraen los archivos que lee GDAL; el INEGI incluye a
  # veces indices espaciales y metadatos (.sbn, .sbx, .xml) que no hacen falta.
  partes <- indice[
    sin_extension == sin_extension[candidatas] & grepl("[.](shp|shx|dbf|prj|cpg)$", nombres),
  ]
  if (anyNA(partes$original)) {
    detener("Los archivos de la capa en ", basename(zip), " tienen nombres que R no puede leer.")
  }
  zip_exigir_metodos(partes, zip)

  carpeta <- tempfile("importinegi_")
  dir.create(carpeta)
  resultado <- tryCatch(
    utils::unzip(zip, files = partes$original, exdir = carpeta, junkpaths = TRUE),
    warning = function(w) w,
    error = function(e) e
  )
  if (inherits(resultado, "condition") || length(resultado) < nrow(partes)) {
    detalle <- if (inherits(resultado, "condition")) conditionMessage(resultado) else "faltan archivos"
    detener("No se pudo descomprimir ", basename(zip), ": ", detalle, ".")
  }
  # Algunos zip del INEGI separan las carpetas con "\": en los sistemas que no
  # son Windows el archivo queda con la ruta completa como nombre.
  for (archivo in list.files(carpeta)) {
    if (grepl("\\", archivo, fixed = TRUE)) {
      file.rename(file.path(carpeta, archivo), file.path(carpeta, sub("^.*\\\\", "", archivo)))
    }
  }
  file.path(carpeta, basename(indice$nombre[candidatas]))
}

# Lee una capa. No cambia la proyeccion ni los nombres de las columnas. El texto
# se lee con la codificacion que declara la capa (en su archivo .cpg o en el
# encabezado del DBF). Si no declara ninguna (la mayoria de las capas de 1995 a
# 2013), GDAL entregaria los bytes tal cual, marcados como UTF-8, y el texto
# con acentos quedaria invalido en R; esas capas estan en Latin-1, y eso se le
# indica al lector. La opcion no se usa en las capas que si declaran su
# codificacion, porque la sustituiria.
marcogeo_leer <- function(shp) {
  carpeta <- dirname(shp)
  base <- tolower(tools::file_path_sans_ext(basename(shp)))
  hermanos <- list.files(carpeta)
  hermanos <- hermanos[tolower(tools::file_path_sans_ext(hermanos)) == base]
  extensiones <- tolower(tools::file_ext(hermanos))
  opciones <- NULL
  if (!"cpg" %in% extensiones && "dbf" %in% extensiones) {
    encabezado <- readBin(file.path(carpeta, hermanos[extensiones == "dbf"][1]), what = "raw", n = 30)
    if (length(encabezado) == 30 && encabezado[30] == as.raw(0)) {
      opciones <- "ENCODING=ISO-8859-1"
    }
  }
  datos <- sf::st_read(shp, quiet = TRUE, stringsAsFactors = FALSE, options = opciones)
  nulos_a_na(datos)
}

# Conserva las filas de las entidades pedidas en un mapa de todo el pais. La
# clave de la entidad se toma de la columna que la contiene, que cambia con la
# edicion: una columna propia o los dos primeros caracteres de una clave
# concatenada. Las columnas no se renombran.
marcogeo_filtrar <- function(datos, claves) {
  columnas <- c("CVE_ENT", "CVE_EDO", "CVEGEO", "CVE_GEO", "CVEMUNI", "CVE_CONCA", "CLVAGB", "CLAVE")
  posicion <- match(columnas, toupper(names(datos)))
  posicion <- posicion[!is.na(posicion)][1]
  if (is.na(posicion)) {
    detener(
      "El mapa no tiene una columna con la clave de la entidad, as\u00ed que no se puede elegir una. ",
      "Usa estado = \"nacional\"."
    )
  }
  entidad <- substr(trimws(as.character(datos[[posicion]])), 1, 2)
  message(
    "El INEGI publica esta edici\u00f3n solo para todo el pa\u00eds: se conservan las filas de ",
    marcogeo_lista_entidades(claves), "."
  )
  datos <- datos[entidad %in% claves, ]
  rownames(datos) <- NULL
  datos
}

# Indica si dos proyecciones son la misma aunque esten escritas de distinta
# manera. El INEGI escribe una misma proyeccion con nombres distintos de un
# archivo a otro ("MEXICO_ITRF_2008_LCC", "Mexico_ITRF2008_LCC"), y GDAL solo
# reconoce algunos de esos nombres: sf considera distintas las dos escrituras.
# Se comparan sus parametros (proyeccion, meridiano, paralelos, origen,
# elipsoide y unidades), sin el termino +towgs84, que GDAL agrega solo a las
# escrituras que reconoce.
marcogeo_misma_proyeccion <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    return(is.na(a) && is.na(b))
  }
  if (isTRUE(a == b)) {
    return(TRUE)
  }
  parametros <- function(crs) {
    texto <- gsub("[+]towgs84=[^ ]*", "", crs$proj4string)
    sort(strsplit(trimws(texto), " +")[[1]])
  }
  !is.na(a$proj4string) && !is.na(b$proj4string) && identical(parametros(a), parametros(b))
}

# Une los mapas de varias entidades en uno solo. No transforma coordenadas. Si
# los archivos declaran la misma proyeccion escrita de distinta manera, el mapa
# combinado conserva la declaracion del primero, y se avisa; si las
# proyecciones son de verdad distintas, se detiene.
marcogeo_combinar <- function(partes) {
  partes <- partes[!vapply(partes, is.null, logical(1))]
  if (length(partes) == 1) {
    return(partes[[1]])
  }
  referencia <- sf::st_crs(partes[[1]])
  columnas <- unique(unlist(lapply(partes, names)))
  geometria <- attr(partes[[1]], "sf_column")
  columnas <- c(setdiff(columnas, geometria), geometria)
  reescritas <- 0
  for (i in seq_along(partes)) {
    parte <- partes[[i]]
    actual <- sf::st_crs(parte)
    if (!isTRUE(actual == referencia)) {
      if (!marcogeo_misma_proyeccion(actual, referencia)) {
        detener(
          "Las entidades pedidas no se pueden combinar: el INEGI declara proyecciones distintas en sus archivos. ",
          "Desc\u00e1rgalas por separado."
        )
      }
      # Mismos parametros: se cambia la declaracion, no las coordenadas
      parte <- suppressWarnings(sf::st_set_crs(parte, referencia))
      reescritas <- reescritas + 1
    }
    if (!identical(attr(parte, "sf_column"), geometria)) {
      parte <- sf::st_set_geometry(parte, geometria)
    }
    for (columna in setdiff(columnas, names(parte))) {
      parte[[columna]] <- NA
    }
    partes[[i]] <- parte[, columnas]
  }
  if (reescritas > 0) {
    message(
      "Los archivos de las entidades declaran la misma proyecci\u00f3n escrita de distinta manera: ",
      "el mapa combinado conserva la declaraci\u00f3n del primer archivo. Las coordenadas no cambian."
    )
  }
  combinado <- do.call(rbind, partes)
  rownames(combinado) <- NULL
  combinado
}

# Borra las carpetas temporales de una capa ya leida.
marcogeo_limpiar <- function(shp) {
  unlink(dirname(shp), recursive = TRUE)
}

# Via A: producto completo de todo el pais.
marcogeo_via_a <- function(edicion, mapa, claves) {
  producto <- marcogeo_producto(edicion)
  url <- marcogeo_url_producto(edicion)
  zip <- marcogeo_descargar(url, tamano_texto(producto$bytes))
  ruta <- strsplit(marcogeo_capas_producto(edicion$clave)[[mapa]], "!/", fixed = TRUE)[[1]]
  if (length(ruta) == 2) {
    # La capa esta en un zip interno: se extrae solo ese zip
    indice <- zip_indice_local(zip)
    interno <- indice[tolower(basename(indice$nombre)) == tolower(ruta[1]), ]
    if (nrow(interno) != 1 || is.na(interno$original)) {
      detener(
        "El archivo ", basename(zip), " no tiene la estructura esperada: no contiene ", ruta[1],
        ". Contiene: ", enumerar(indice$nombre, maximo = 15),
        ". Puedes reportarlo en https://github.com/crenteriam/importinegi/issues"
      )
    }
    zip_exigir_metodos(interno, zip)
    carpeta <- tempfile("importinegi_")
    dir.create(carpeta)
    on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
    extraer_zip(zip, carpeta, archivos = interno$original)
    zip <- list.files(carpeta, recursive = TRUE, full.names = TRUE)[1]
    ruta <- ruta[2]
  }
  shp <- marcogeo_extraer_shp(zip, ruta = ruta)
  datos <- marcogeo_leer(shp)
  marcogeo_limpiar(shp)
  if (!identical(claves, "00")) {
    datos <- marcogeo_filtrar(datos, claves)
  }
  attr(datos, "fuente") <- url
  datos
}

# Via B: un archivo por edicion, capa y area.
marcogeo_via_b <- function(edicion, mapa, claves) {
  mapas <- marcogeo_mapas()
  capa <- mapas$capa[mapas$mapa == mapa]
  registro <- mg_capas[mg_capas$year == edicion$year & mg_capas$capa == capa, ]
  faltan <- strsplit(registro$faltan, " ", fixed = TRUE)[[1]]
  if (identical(claves, "00") && capa == "FM") {
    # El archivo nacional de frentes de manzana no se usa: el de 2025 esta
    # comprimido con un metodo que R no abre y, en los demas, la tabla mide de
    # 7 a 9 GB. Se combinan los archivos de las entidades.
    claves <- setdiff(marcogeo_todas(), faltan)
    message(
      "Los frentes de manzana de todo el pa\u00eds se arman con los archivos de las ", length(claves),
      " entidades. Son m\u00e1s de 11 millones de l\u00edneas: la descarga tarda y necesita mucha memoria."
    )
  } else if (identical(claves, "00") && is.na(registro$bytes)) {
    # Capa sin archivo de todo el pais
    claves <- setdiff(marcogeo_todas(), faltan)
    message(
      "El INEGI publica este mapa solo por entidad: se descargan y se combinan los archivos de las ",
      length(claves), " entidades."
    )
  }
  sin_archivo <- intersect(claves, faltan)
  if (length(sin_archivo) > 0) {
    detener(
      "El INEGI no publica el mapa ", comillas(mapa), " de ", marcogeo_lista_entidades(sin_archivo),
      " en la edici\u00f3n ", marcogeo_nombre_edicion(edicion), " del Marco Geoestad\u00edstico. ",
      "Lo publica para ", length(marcogeo_todas()) - length(faltan), " entidades: con estado = \"nacional\" ",
      "se obtiene el mapa de todo el pa\u00eds."
    )
  }
  urls <- vapply(claves, function(clave) marcogeo_url_capa(edicion$year, clave, capa), character(1))
  partes <- lapply(seq_along(claves), function(i) {
    tamano <- if (claves[i] == "00") tamano_texto(registro$bytes) else NULL
    zip <- inegi_descargar(urls[[i]], tamano = tamano)
    shp <- marcogeo_extraer_shp(zip)
    datos <- marcogeo_leer(shp)
    marcogeo_limpiar(shp)
    if (!isTRUE(getOption("importinegi.cache", FALSE))) {
      unlink(zip)
    }
    datos
  })
  datos <- marcogeo_combinar(partes)
  attr(datos, "fuente") <- unname(urls)
  datos
}

# Via P: producto de 2.4 a 2.9 GB con un zip por entidad guardado sin
# comprimir. Se descarga solo el zip de cada entidad pedida (o el nacional).
marcogeo_via_p <- function(edicion, mapa, claves) {
  mapas <- marcogeo_mapas()
  sufijos <- strsplit(mapas$sufijo[mapas$mapa == mapa], "|", fixed = TRUE)[[1]]
  url <- marcogeo_url_producto(edicion)
  indice <- zip_remoto_indice(url)
  todas <- identical(claves, "00")
  nacional <- indice[grepl("integrad[^/]*[.]zip$", indice$nombre, ignore.case = TRUE), ]
  if (todas && nrow(nacional) == 1 && mapa %in% marcogeo_mapas_nacional()) {
    entradas <- list(nacional)
    claves <- "00"
  } else {
    if (todas) {
      claves <- marcogeo_todas()
    }
    entradas <- lapply(claves, function(clave) {
      indice[grepl(paste0("^", clave, "_[^/]*[.]zip$"), indice$nombre, ignore.case = TRUE), ]
    })
    halladas <- vapply(entradas, nrow, integer(1)) == 1
    if (!all(halladas)) {
      detener(
        "El producto ", basename(url), " no tiene la estructura esperada: no contiene el archivo de ",
        marcogeo_lista_entidades(claves[!halladas]),
        ". Puedes reportarlo en https://github.com/crenteriam/importinegi/issues"
      )
    }
    if (todas) {
      total <- sum(vapply(entradas, function(entrada) entrada$comprimido, numeric(1)))
      message(
        "El INEGI publica esta edici\u00f3n solo por entidad: se descargan y se combinan los archivos de las ",
        length(claves), " entidades (", tamano_texto(total), " en total)."
      )
    }
  }
  partes <- lapply(seq_along(claves), function(i) {
    entrada <- entradas[[i]]
    destino <- marcogeo_ruta_local(paste0(edicion$clave, "_", basename(entrada$nombre)))
    if (file.exists(destino) && es_zip(destino)) {
      message("Usando la copia local de ", basename(entrada$nombre), ".")
    } else {
      zip_remoto_entrada(url, entrada, destino)
    }
    shp <- marcogeo_extraer_shp(destino, base = paste0(claves[i], sufijos), exigir = FALSE)
    if (is.null(shp)) {
      if (todas) {
        return(NULL)
      }
      detener(
        "El INEGI no publica el mapa ", comillas(mapa), " de ", marcogeo_nombres_entidad(claves[i]),
        " en la edici\u00f3n ", marcogeo_nombre_edicion(edicion), " del Marco Geoestad\u00edstico. ",
        "Con estado = \"nacional\" se combinan las entidades que s\u00ed lo tienen."
      )
    }
    datos <- marcogeo_leer(shp)
    marcogeo_limpiar(shp)
    datos
  })
  datos <- marcogeo_combinar(partes)
  attr(datos, "fuente") <- url
  datos
}

# Motor de sig_marcogeoestadistico(). Devuelve siempre un solo mapa: un objeto
# de clase sf.
marcogeo_obtener <- function(year, mapa, estado = NULL, version = NULL) {
  edicion <- marcogeo_edicion(year, version)
  mapa <- marcogeo_mapa(mapa, edicion)
  claves <- marcogeo_claves(estado, edicion, mapa)
  requiere_paquete("sf")
  datos <- switch(
    edicion$via,
    A = marcogeo_via_a(edicion, mapa, claves),
    B = marcogeo_via_b(edicion, mapa, claves),
    P = marcogeo_via_p(edicion, mapa, claves)
  )
  if (is.na(sf::st_crs(datos))) {
    message(
      "El INEGI no publica el archivo de proyecci\u00f3n de este mapa: se devuelve sin proyecci\u00f3n. ",
      "La secci\u00f3n \"Proyeccion\" de ?sig_marcogeoestadistico explica c\u00f3mo asignarla."
    )
  }
  datos
}

# Consulta -----------------------------------------------------------------------------

# Ediciones del Marco Geoestadistico para bases_importinegi(): una fila por
# edicion. `base` es la version, como se escribe en el argumento `version`; en
# los años con una sola edicion, el texto con que el INEGI la identifica, que
# el argumento tambien acepta.
marcogeo_bases <- function() {
  ediciones <- marcogeo_ediciones()
  filas <- lapply(seq_len(nrow(ediciones)), function(i) {
    edicion <- ediciones[i, ]
    producto <- marcogeo_producto(edicion)
    bytes <- if (edicion$via == "B") {
      sumar_bytes(mg_capas$bytes[mg_capas$year == edicion$year])
    } else {
      producto$bytes
    }
    cobertura <- if (edicion$via == "A") {
      "nacional"
    } else if (edicion$via == "B" || marcogeo_tiene_nacional(edicion)) {
      "nacional y por entidad"
    } else {
      "por entidad"
    }
    data.frame(
      proyecto = "sig_marcogeoestadistico",
      year = edicion$year,
      modulo = NA_character_,
      base = if (nzchar(edicion$version)) edicion$version else edicion$etiqueta,
      descripcion = paste0(producto$titulo, if (isTRUE(nzchar(producto$escala))) paste0(", escala ", producto$escala)),
      tablas = paste(marcogeo_mapas_edicion(edicion), collapse = ", "),
      cobertura = cobertura,
      formato = "shp",
      tamano_mb = round(bytes / 1024^2, 1),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, filas)
}
