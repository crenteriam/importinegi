# Las muestras de fixtures/sig las genera data-raw/muestras_sig.R. Imitan la
# forma de los archivos del Marco Geoestadistico del INEGI, con datos
# inventados.
#
# sig_marcogeoestadistico() devuelve siempre un solo mapa (un objeto de clase
# sf): cada prueba que descarga un mapa lo comprueba con esperar_un_mapa().

# Columnas del mapa sin la geometria.
atributos <- function(mapa) {
  sf::st_drop_geometry(mapa)
}

# Dos entidades en lugar de 32, para las pruebas que combinan todo el pais.
dos_entidades <- function(entorno = parent.frame()) {
  local_mocked_bindings(marcogeo_todas = function() c("01", "06"), .env = entorno)
}

# Ediciones ---------------------------------------------------------------------------

test_that("marcogeo_ediciones() tiene las 26 ediciones, cada una con su producto", {
  ediciones <- marcogeo_ediciones()
  expect_identical(nrow(ediciones), 26L)
  expect_identical(anyDuplicated(ediciones[, c("year", "version")]), 0L)
  expect_identical(anyDuplicated(ediciones$clave), 0L)
  expect_true(all(ediciones$via %in% c("A", "B", "P")))
  expect_identical(as.vector(table(ediciones$via)[c("A", "B", "P")]), c(13L, 10L, 3L))
  # La version distingue las ediciones de un mismo año; las ediciones unicas no
  # la necesitan y tienen una etiqueta
  por_year <- table(ediciones$year)
  varias <- ediciones$year %in% names(por_year)[por_year > 1]
  expect_identical(sort(unique(ediciones$year[varias])), c(2010L, 2017L, 2018L, 2023L))
  expect_true(all(nzchar(ediciones$version[varias])))
  expect_true(all(!nzchar(ediciones$version[!varias]) & nzchar(ediciones$etiqueta[!varias])))
  # Cada edicion tiene su producto, del mismo año
  productos <- mg_productos[match(ediciones$clave, mg_productos$clave), ]
  expect_false(anyNA(productos$clave))
  expect_identical(productos$edicion, ediciones$year)
  # De 2017 en adelante se usa la descarga por capa, salvo en las tres
  # ediciones que solo existen como producto completo
  expect_identical(sort(ediciones$year[ediciones$via == "B"]), 2017:2026)
  expect_true(all(ediciones$year[ediciones$via == "B"] %in% mg_capas$year))
  expect_identical(
    marcogeo_nombre_edicion(ediciones[ediciones$via == "P", ][1, ]), "2018 (febrero)"
  )
})

test_that("marcogeo_edicion() identifica la edicion con el a\u00f1o y la version", {
  expect_identical(marcogeo_edicion(2025)$clave, "794551163061")
  expect_identical(marcogeo_edicion("2025")$clave, "794551163061")
  expect_identical(marcogeo_edicion(2010, "5.0")$clave, "702825292812")
  expect_identical(marcogeo_edicion(2010, "5.0.A")$clave, "702825292805")
  expect_identical(marcogeo_edicion(2010, "5.0.A 1:20 000")$clave, "702825296629")
  expect_identical(marcogeo_edicion(2010, 4.3)$clave, "702825296520")
  # Sin distinguir mayusculas, acentos, puntuacion ni espacios
  expect_identical(marcogeo_edicion(2010, "5.0.a")$clave, "702825292805")
  expect_identical(marcogeo_edicion(2010, "5.0.a 1:20000")$clave, "702825296629")
  expect_identical(marcogeo_edicion(2018, "DICIEMBRE")$clave, "889463674658")
  expect_identical(marcogeo_edicion(2023, "Censo Agropecuario 2022")$clave, "794551067284")
  # Una parte de la version basta si identifica una sola edicion
  expect_identical(marcogeo_edicion(2018, "feb")$clave, "889463526636")
  expect_identical(marcogeo_edicion(2023, "agropecuario")$clave, "794551067284")
  # En los años con una edicion se acepta el texto con que el INEGI la identifica
  expect_identical(marcogeo_edicion(2014, "6.2")$clave, "702825004386")
  expect_identical(marcogeo_edicion(2026, "Encuesta Intercensal 2025")$clave, "794551196649")
  expect_identical(marcogeo_edicion(2020, "censo")$clave, "889463807469")
})

test_that("marcogeo_edicion() se detiene y lista las versiones si el a\u00f1o tiene varias", {
  expect_error(
    marcogeo_edicion(2010),
    "`year = 2010` tiene 4 ediciones.*Indica una en `version`: \"4.3\", \"5.0\", \"5.0.A\", \"5.0.A 1:20 000\""
  )
  expect_error(marcogeo_edicion(2017), "tiene 2 ediciones.*\"junio\", \"diciembre\"")
  expect_error(marcogeo_edicion(2018), "tiene 3 ediciones.*\"febrero\", \"junio\", \"diciembre\"")
  expect_error(marcogeo_edicion(2023), "tiene 2 ediciones.*\"censo agropecuario 2022\", \"diciembre\"")
})

test_that("marcogeo_edicion() explica los a\u00f1os y las versiones que no existen", {
  expect_error(marcogeo_edicion(), "Debes indicar el a\u00f1o de edici\u00f3n.*A\u00f1os disponibles: 1995, 2000")
  expect_error(marcogeo_edicion(NULL), "Debes indicar el a\u00f1o")
  expect_error(marcogeo_edicion(c(2020, 2021)), "`year`")
  expect_error(marcogeo_edicion("dos mil"), "`year` debe ser un a\u00f1o en formato num\u00e9rico")
  expect_error(marcogeo_edicion(2012), "No hay una edici\u00f3n .* con `year = 2012`.*A\u00f1os disponibles")
  expect_error(marcogeo_edicion(2015, "6.2"), "No hay una edici\u00f3n .*`year = 2015`.*usa `year = 2014`")
  expect_error(marcogeo_edicion(2018, "marzo"), "no es una edici\u00f3n de `year = 2018`.*Versiones de 2018")
  # Una version que coincide con varias ediciones
  expect_error(marcogeo_edicion(2010, "5"), "coincide con varias ediciones de `year = 2010`")
  expect_error(marcogeo_edicion(2010, c("4.3", "5.0")), "`version`")
  # La edicion existe, pero el INEGI la publico con otro año
  expect_error(
    marcogeo_edicion(2025, "encuesta intercensal 2025"),
    "usa `year = 2026`.*`year = 2025` tiene una sola edici\u00f3n y no necesita `version`"
  )
  expect_error(marcogeo_edicion(2022, "censo agropecuario 2022"), "usa `year = 2023` y `version = \"censo agropecuario 2022\"`")
})

# Mapas -------------------------------------------------------------------------------

test_that("cada edicion ofrece los mapas que publica el INEGI", {
  ediciones <- marcogeo_ediciones()
  mapas <- lapply(seq_len(nrow(ediciones)), function(i) marcogeo_mapas_edicion(ediciones[i, ]))
  names(mapas) <- vapply(seq_len(nrow(ediciones)), function(i) marcogeo_nombre_edicion(ediciones[i, ]), character(1))
  # Entidades y municipios estan en todas
  expect_true(all(vapply(mapas, function(x) all(c("entidades", "municipios") %in% x), logical(1))))
  expect_identical(mapas[["1995"]], c("entidades", "municipios"))
  expect_identical(mapas[["2000"]], c("entidades", "municipios", "ageb_urbana"))
  expect_identical(mapas[["2007"]], c("entidades", "municipios", "ageb_rural", "localidades_urbanas", "localidades_rurales"))
  expect_identical(
    mapas[["2010 (4.3)"]],
    c("entidades", "municipios", "ageb_rural", "localidades_urbanas", "localidades_rurales",
      "zonas_entidades", "zonas_municipios", "zonas_ageb_rural", "zonas_localidades_urbanas",
      "zonas_localidades_rurales")
  )
  expect_true("territorio_insular" %in% mapas[["2010 (5.0.A 1:20 000)"]])
  expect_false("zonas_ageb_rural" %in% mapas[["2010 (5.0.A 1:20 000)"]])
  expect_identical(mapas[["2014"]], c("entidades", "municipios", "ageb_rural", "integracion_territorial", "territorio_insular"))
  # Descarga por capa: 14 mapas en 2017 y 16 desde 2018
  expect_length(mapas[["2017 (diciembre)"]], 14L)
  expect_false(any(c("poligono_externo", "poligono_externo_manzana") %in% mapas[["2017 (diciembre)"]]))
  for (nombre in c("2018 (diciembre)", "2019", "2020", "2021", "2022", "2023 (diciembre)", "2024", "2025", "2026")) {
    expect_length(mapas[[nombre]], 16L)
  }
  # Productos por entidad: sin poligonos externos antes de diciembre de 2018
  expect_length(mapas[["2018 (febrero)"]], 14L)
  expect_length(mapas[["2018 (junio)"]], 14L)
  expect_length(mapas[["2023 (censo agropecuario 2022)"]], 16L)
  # Todos los mapas son del vocabulario
  vocabulario <- c(marcogeo_mapas()$mapa, paste0("zonas_", c(
    "entidades", "municipios", "ageb_rural", "localidades_urbanas", "localidades_rurales"
  )))
  expect_identical(setdiff(unlist(mapas), vocabulario), character())
  # Las capas de los productos completos tienen su ubicacion dentro del zip
  for (clave in ediciones$clave[ediciones$via == "A"]) {
    capas <- marcogeo_capas_producto(clave)
    expect_type(capas, "character")
    expect_false(anyNA(capas), info = clave)
    expect_identical(anyDuplicated(names(capas)), 0L, info = clave)
  }
  expect_identical(marcogeo_capas_producto("702825292836")[["entidades"]], "mge1995.zip!/Entidades_1995")
  expect_identical(marcogeo_capas_producto("702825296520")[["zonas_entidades"]], "Zonas_Interestatales/ESTADOS")
  expect_error(marcogeo_capas_producto("000"), "no tiene una lista de capas")
})

test_that("marcogeo_mapa() acepta el nombre del mapa o el codigo del INEGI", {
  edicion <- marcogeo_edicion(2025)
  expect_identical(marcogeo_mapa("municipios", edicion), "municipios")
  expect_identical(marcogeo_mapa("Municipios", edicion), "municipios")
  expect_identical(marcogeo_mapa("Localidades Rurales", edicion), "localidades_rurales")
  expect_identical(marcogeo_mapa("caser\u00edo_disperso", edicion), "caserio_disperso")
  codigos <- c(
    ENT = "entidades", MUN = "municipios", AR = "ageb_rural", A = "ageb_urbana", LPR = "localidades_rurales",
    L = "localidades_amanzanadas", M = "manzanas", FM = "frentes", E = "vialidades", TI = "territorio_insular",
    SIA = "servicios_area", SIL = "servicios_linea", SIP = "servicios_punto", PE = "poligono_externo",
    PEM = "poligono_externo_manzana", CD = "caserio_disperso"
  )
  for (codigo in names(codigos)) {
    expect_identical(marcogeo_mapa(codigo, edicion), codigos[[codigo]], info = codigo)
    expect_identical(marcogeo_mapa(tolower(codigo), edicion), codigos[[codigo]], info = codigo)
  }
  expect_identical(marcogeo_mapa("zonas_entidades", marcogeo_edicion(2010, "4.3")), "zonas_entidades")
})

test_that("marcogeo_mapa() explica los mapas que no existen", {
  edicion <- marcogeo_edicion(2025)
  expect_error(marcogeo_mapa(NULL, edicion), "Debes indicar el mapa en `mapa`. Mapas de la edici\u00f3n 2025: \"entidades\"")
  expect_error(marcogeo_mapa(c("municipios", "entidades"), edicion), "`mapa`")
  expect_error(marcogeo_mapa(3, edicion), "`mapa` debe ser el nombre de un mapa")
  expect_error(marcogeo_mapa("colonias", edicion), "`mapa = \"colonias\"` no es una opci\u00f3n v\u00e1lida. Mapas de la edici\u00f3n 2025")
  # Mapas que existen en otras ediciones
  expect_error(
    marcogeo_mapa("manzanas", marcogeo_edicion(1995)),
    "La edici\u00f3n 1995 .* no tiene el mapa \"manzanas\". Mapas de esa edici\u00f3n: \"entidades\", \"municipios\""
  )
  expect_error(marcogeo_mapa("localidades_urbanas", edicion), "no tiene el mapa \"localidades_urbanas\"")
  expect_error(marcogeo_mapa("PE", marcogeo_edicion(2017, "diciembre")), "no tiene el mapa \"poligono_externo\"")
  expect_error(marcogeo_mapa("zonas_ageb_rural", marcogeo_edicion(2010, "5.0.A 1:20 000")), "no tiene el mapa")
  # Opciones de sig_marcogeo()
  antes <- marcogeo_edicion(2009)
  expect_error(marcogeo_mapa("ageb", antes), "era una opci\u00f3n de sig_marcogeo\\(\\).*\"ageb_urbana\" o \"ageb_rural\"")
  expect_error(marcogeo_mapa("urbano", antes), "usa \"localidades_urbanas\"")
  expect_error(marcogeo_mapa("rural", antes), "usa \"localidades_rurales\"")
})

# Entidades ---------------------------------------------------------------------------

test_that("marcogeo_claves() sigue la regla comun de `estado`", {
  edicion <- marcogeo_edicion(2025)
  expect_identical(marcogeo_claves("Colima", edicion, "municipios"), "06")
  expect_identical(marcogeo_claves(c("nay", "06", 1), edicion, "municipios"), c("01", "06", "18"))
  expect_identical(marcogeo_claves(c("Colima", "colima", "col"), edicion, "municipios"), "06")
  expect_identical(marcogeo_claves("nacional", edicion, "manzanas"), "00")
  expect_identical(marcogeo_claves(c("Colima", "Nacional"), edicion, "municipios"), "00")
  expect_error(marcogeo_claves("Narnia", edicion, "municipios"), "No se reconoce la entidad `estado = \"Narnia\"`")
  expect_error(marcogeo_claves(character(), edicion, "municipios"), "`estado` debe ser")
  expect_error(marcogeo_claves(NA, edicion, "municipios"), "`estado` debe ser")
})

test_that("sin `estado` se descarga todo el pais o, si es muy pesado, Aguascalientes con un aviso", {
  # Descarga por capa: todo el pais, salvo en los tres mapas mas pesados
  edicion <- marcogeo_edicion(2025)
  expect_silent(expect_identical(marcogeo_claves(NULL, edicion, "municipios"), "00"))
  expect_silent(expect_identical(marcogeo_claves(NULL, edicion, "territorio_insular"), "00"))
  expect_message(
    expect_identical(marcogeo_claves(NULL, edicion, "manzanas"), "01"),
    "No indicaste `estado`: se descarga el mapa de Aguascalientes.*estado = \"nacional\" \\(archivo nacional de 412 MB\\)"
  )
  expect_message(expect_identical(marcogeo_claves(NULL, edicion, "vialidades"), "01"), "archivo nacional de 375 MB")
  expect_message(expect_identical(marcogeo_claves(NULL, edicion, "frentes"), "01"), "32 archivos, con m\u00e1s de 11 millones")
  # Productos completos de todo el pais
  expect_silent(expect_identical(marcogeo_claves(NULL, marcogeo_edicion(1995), "municipios"), "00"))
  # Productos por entidad: Aguascalientes, salvo en los mapas del zip nacional
  febrero <- marcogeo_edicion(2018, "febrero")
  expect_message(
    expect_identical(marcogeo_claves(NULL, febrero, "municipios"), "01"),
    "se descarga el mapa de Aguascalientes.*\\(32 archivos, 2.4 GB en total\\)"
  )
  agropecuario <- marcogeo_edicion(2023, "censo agropecuario 2022")
  expect_silent(expect_identical(marcogeo_claves(NULL, agropecuario, "municipios"), "00"))
  expect_silent(expect_identical(marcogeo_claves(NULL, agropecuario, "localidades_rurales"), "00"))
  expect_message(expect_identical(marcogeo_claves(NULL, agropecuario, "manzanas"), "01"), "2.9 GB en total")
  # Las zonas en disputa no son de una entidad
  zonas <- marcogeo_edicion(2010, "4.3")
  expect_silent(expect_identical(marcogeo_claves(NULL, zonas, "zonas_entidades"), "00"))
  expect_message(expect_identical(marcogeo_claves("Colima", zonas, "zonas_entidades"), "00"), "`estado` no se usa")
})

test_that("las direcciones se arman como en el sitio del INEGI", {
  withr::local_options(importinegi.base = NULL)
  expect_identical(
    marcogeo_url_capa(2025, "00", "MUN"),
    "https://www.inegi.org.mx/contenidos/descargadenue/MGdescarga/MGN2025/2025_1_00_MUN.zip"
  )
  expect_identical(
    marcogeo_url_capa(2017, "06", "FM"),
    "https://www.inegi.org.mx/contenidos/descargadenue/MGdescarga/MGN2017/06/2017_1_06_FM.zip"
  )
  expect_identical(
    marcogeo_url_producto(marcogeo_edicion(2020)),
    paste0(
      "https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/",
      "marcogeo/889463807469/889463807469_s.zip"
    )
  )
  # El archivo del producto de 2014 no se llama como su clave
  expect_match(marcogeo_url_producto(marcogeo_edicion(2014)), "/geografia/marc_geo/889463084105_s[.]zip$")
  sin_producto <- marcogeo_edicion(2025)
  sin_producto$clave <- "000000000000"
  expect_error(marcogeo_producto(sin_producto), "no tiene el producto 000000000000")
})

# Lectura -----------------------------------------------------------------------------

test_that("marcogeo_extraer_shp() extrae del zip solo los archivos de la capa", {
  skip_if_not_installed("sf")
  # La unica capa del zip; los archivos que GDAL no lee se quedan en el zip
  shp <- marcogeo_extraer_shp(muestra("sig/capa_2025_1_00_MUN.zip"))
  expect_identical(basename(shp), "2025_1_00_MUN.shp")
  expect_setequal(list.files(dirname(shp)), paste0("2025_1_00_MUN.", c("cpg", "dbf", "prj", "shp", "shx")))
  marcogeo_limpiar(shp)
  expect_false(dir.exists(dirname(shp)))

  # Por su ruta dentro del zip, sin distinguir mayusculas
  zip <- muestra("sig/producto_702825296520_s.zip")
  shp <- marcogeo_extraer_shp(zip, ruta = "zonas_interestatales/estados")
  expect_identical(basename(shp), "ESTADOS.shp")
  expect_setequal(list.files(dirname(shp)), paste0("ESTADOS.", c("dbf", "prj", "shp", "shx")))
  marcogeo_limpiar(shp)
  # Sin ruta, el zip tiene varias capas
  expect_error(marcogeo_extraer_shp(zip), "no tiene la estructura esperada: contiene varias veces la capa")
  expect_error(
    marcogeo_extraer_shp(zip, ruta = "32_Entidades_Federativas/AGEB"),
    "no contiene la capa \"32_Entidades_Federativas/AGEB\". Capas que contiene: .*MUNICIPIOS[.]shp"
  )
  expect_null(marcogeo_extraer_shp(zip, base = "06ti", exigir = FALSE))
})

test_that("marcogeo_extraer_shp() avisa con claridad si la capa esta en Deflate64", {
  expect_error(marcogeo_extraer_shp(muestra("sig/capa_deflate64.zip")), "comprimi\u00f3 con Deflate64")
})

test_that("marcogeo_leer() lee el texto con la codificacion que corresponde a cada capa", {
  skip_if_not_installed("sf")
  leer <- function(zip, ...) {
    shp <- marcogeo_extraer_shp(muestra(zip), ...)
    withr::defer(marcogeo_limpiar(shp))
    marcogeo_leer(shp)
  }
  # Con .cpg: UTF-8 y pagina de codigos 1252
  expect_identical(leer("sig/capa_2025_1_00_MUN.zip")$NOMGEO[3], "Armer\u00eda")
  expect_identical(leer("sig/capa_2025_1_06_MUN.zip")$NOMGEO[1], "Armer\u00eda")
  # Sin .cpg y con la pagina de codigos declarada en el DBF (850 de DOS y ANSI)
  zonas <- leer("sig/producto_702825296520_s.zip", ruta = "Zonas_Interestatales/ESTADOS")
  expect_identical(zonas$NOM_ENT[1], "Campeche - Yucat\u00e1n")
  expect_identical(leer("sig/producto_889463084105_s.zip", ruta = "CONTINUO_NACIONAL/MUNICIPIOS")$NOM_MUN[2], "Armer\u00eda")
  # Sin .cpg y sin pagina de codigos: Latin-1
  municipios <- leer("sig/producto_702825296520_s.zip", ruta = "32_Entidades_Federativas/MUNICIPIOS")
  expect_identical(municipios$NOM_MUN[2], "Armer\u00eda")
  expect_true(all(validUTF8(municipios$NOM_MUN)))
  # Sin indicarle la codificacion, sf entrega ese texto como bytes no validos
  shp <- marcogeo_extraer_shp(muestra("sig/producto_702825296520_s.zip"), ruta = "32_Entidades_Federativas/MUNICIPIOS")
  withr::defer(marcogeo_limpiar(shp))
  directo <- sf::st_read(shp, quiet = TRUE)
  expect_false(validUTF8(directo$NOM_MUN[2]))
})

test_that("marcogeo_filtrar() encuentra la clave de la entidad en la columna que la tenga", {
  skip_if_not_installed("sf")
  puntos <- function(...) {
    datos <- data.frame(..., stringsAsFactors = FALSE)
    sf::st_sf(datos, geometry = sf::st_sfc(lapply(seq_len(nrow(datos)), function(i) sf::st_point(c(i, i)))))
  }
  casos <- list(
    puntos(CVE_ENT = c("01", "06", "18"), NOM = c("a", "b", "c")),
    puntos(CVE_EDO = c("01", "06", "18")),
    puntos(CVEGEO = c("01001", "06002", "18001")),
    puntos(CVE_GEO = c("01", "06", "18")),
    puntos(CVEMUNI = c("01001", "06002", "18001")),
    puntos(CVE_CONCA = c("01001", "06002", "18001")),
    puntos(CLVAGB = c("010010001293-5", "060020001015-3", "180010001001-1")),
    puntos(CLAVE = c("010010001287A", "0600200010153", "1800100010011")),
    puntos(cve_ent = c("01", "06", "18")),
    puntos(CVE_ENT = c("01 ", "06 ", "18 "))
  )
  for (caso in casos) {
    expect_message(filtrado <- marcogeo_filtrar(caso, "06"), "se conservan las filas de Colima[.]")
    esperar_un_mapa(filtrado)
    expect_identical(nrow(filtrado), 1L, info = names(caso)[1])
    expect_identical(names(filtrado), names(caso))
    expect_identical(rownames(filtrado), "1")
  }
  # La columna de la entidad tiene preferencia sobre la clave concatenada
  mixto <- puntos(CVEGEO = c("99001", "99002"), CVE_ENT = c("06", "18"))
  expect_identical(suppressMessages(marcogeo_filtrar(mixto, "18"))$CVEGEO, "99002")
  expect_message(varias <- marcogeo_filtrar(casos[[1]], c("01", "18")), "Aguascalientes, Nayarit")
  expect_identical(varias$CVE_ENT, c("01", "18"))
  expect_identical(nrow(suppressMessages(marcogeo_filtrar(casos[[1]], "32"))), 0L)
  expect_error(marcogeo_filtrar(puntos(NOMBRE = c("a", "b")), "06"), "no tiene una columna con la clave de la entidad")
})

test_that("marcogeo_combinar() une varias entidades en un solo mapa sin transformar coordenadas", {
  skip_if_not_installed("sf")
  lambert <- "+proj=lcc +lat_0=12 +lon_0=-102 +lat_1=17.5 +lat_2=29.5 +x_0=2500000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
  parte <- function(claves, crs, ...) {
    datos <- data.frame(CVE_ENT = claves, ..., stringsAsFactors = FALSE)
    puntos <- lapply(seq_along(claves), function(i) sf::st_point(c(2400000 + i, 1100000)))
    sf::st_sf(datos, geometry = sf::st_sfc(puntos, crs = if (is.na(crs)) sf::NA_crs_ else crs))
  }
  a <- parte(c("01", "01"), lambert)
  b <- parte("06", lambert)
  expect_silent(combinado <- marcogeo_combinar(list(a, b)))
  esperar_un_mapa(combinado)
  expect_identical(combinado$CVE_ENT, c("01", "01", "06"))
  expect_identical(rownames(combinado), c("1", "2", "3"))
  expect_identical(unname(sf::st_coordinates(combinado)[, 1]), 2400000 + c(1, 2, 1))
  # Una sola parte se devuelve tal cual; las que no existen se omiten
  expect_identical(marcogeo_combinar(list(a)), a)
  expect_identical(marcogeo_combinar(list(NULL, b, NULL)), b)
  # Las columnas que faltan en una entidad quedan con NA
  con_extra <- marcogeo_combinar(list(a, parte("06", lambert, AMBITO = "Urbana")))
  expect_identical(names(con_extra), c("CVE_ENT", "AMBITO", "geometry"))
  expect_identical(con_extra$AMBITO, c(NA, NA, "Urbana"))
  # Sin proyeccion en ninguna de las dos
  esperar_un_mapa(marcogeo_combinar(list(parte("01", NA), parte("06", NA))))
  # Proyecciones distintas: no se combinan
  geograficas <- parte("06", 4326)
  expect_error(marcogeo_combinar(list(a, geograficas)), "el INEGI declara proyecciones distintas")
  expect_error(marcogeo_combinar(list(a, parte("06", NA))), "proyecciones distintas")
})

test_that("la misma proyeccion escrita de dos maneras se combina y conserva la primera declaracion", {
  skip_if_not_installed("sf")
  leer <- function(zip) {
    shp <- marcogeo_extraer_shp(muestra(zip))
    withr::defer(marcogeo_limpiar(shp))
    marcogeo_leer(shp)
  }
  # Los .prj de las dos muestras son de archivos del INEGI: la misma proyeccion
  # con nombres distintos
  aguascalientes <- leer("sig/capa_2025_1_01_MUN.zip")
  colima <- leer("sig/capa_2025_1_06_MUN.zip")
  expect_true(marcogeo_misma_proyeccion(sf::st_crs(aguascalientes), sf::st_crs(colima)))
  expect_false(marcogeo_misma_proyeccion(sf::st_crs(aguascalientes), sf::st_crs(4326)))
  expect_false(marcogeo_misma_proyeccion(sf::st_crs(aguascalientes), sf::st_crs(NA)))
  expect_true(marcogeo_misma_proyeccion(sf::st_crs(NA), sf::st_crs(NA)))

  mensajes <- testthat::capture_messages(combinado <- marcogeo_combinar(list(aguascalientes, colima)))
  esperar_un_mapa(combinado)
  expect_identical(nrow(combinado), 4L)
  expect_true(sf::st_crs(combinado) == sf::st_crs(aguascalientes))
  # Las coordenadas no cambian
  propias <- sf::st_coordinates(colima)[, 1:2]
  expect_identical(unname(utils::tail(sf::st_coordinates(combinado)[, 1:2], nrow(propias))), unname(propias))
  # sf solo considera distintas las dos escrituras con algunas versiones de GDAL
  if (!isTRUE(sf::st_crs(aguascalientes) == sf::st_crs(colima))) {
    expect_match(mensajes, "misma proyecci\u00f3n escrita de distinta manera", all = FALSE)
  }
})

# Descarga por capa (de diciembre de 2017 en adelante) -----------------------------------

test_that("descarga por capa: el archivo de todo el pais", {
  servidor <- marcogeo_local()
  expect_message(
    municipios <- sig_marcogeoestadistico(2025, "municipios"),
    "Descargando 2025_1_00_MUN[.]zip \\(42 MB\\)"
  )
  esperar_un_mapa(municipios)
  expect_identical(names(municipios), c("CVEGEO", "CVE_ENT", "CVE_MUN", "NOMGEO", "geometry"))
  expect_identical(nrow(municipios), 5L)
  # Las claves conservan sus ceros y el texto, sus acentos
  expect_identical(municipios$CVEGEO, c("01001", "01002", "06001", "06002", "18001"))
  expect_identical(municipios$NOMGEO[3], "Armer\u00eda")
  expect_s3_class(sf::st_geometry(municipios), "sfc_POLYGON")
  # La proyeccion es la que declara el archivo: no se transforma
  expect_match(sf::st_crs(municipios)$proj4string, "+proj=lcc", fixed = TRUE)
  expect_match(sf::st_crs(municipios)$proj4string, "+lat_1=17.5", fixed = TRUE)
  expect_equal(unname(sf::st_bbox(municipios)[c("xmin", "ymin")]), c(2400000, 1100000))
  expect_match(attr(municipios, "fuente"), "/MGdescarga/MGN2025/2025_1_00_MUN[.]zip$")
  # "nacional" y el codigo del INEGI dan lo mismo
  expect_identical(suppressMessages(sig_marcogeoestadistico(2025, "MUN", estado = "nacional")), municipios)
})

test_that("descarga por capa: una entidad y varias entidades en un solo mapa", {
  servidor <- marcogeo_local()
  antes <- descargas_sig(servidor)$solicitudes
  colima <- suppressMessages(sig_marcogeoestadistico(2025, "municipios", estado = "Colima"))
  esperar_un_mapa(colima)
  expect_identical(colima$CVEGEO, c("06001", "06002"))
  # El archivo de Colima declara la pagina de codigos 1252
  expect_identical(colima$NOMGEO, c("Armer\u00eda", "Colima"))
  expect_match(attr(colima, "fuente"), "/MGN2025/06/2025_1_06_MUN[.]zip$")
  expect_identical(descargas_sig(servidor)$solicitudes - antes, 1)

  dos <- suppressMessages(sig_marcogeoestadistico(2025, "municipios", estado = c("col", "Aguascalientes")))
  esperar_un_mapa(dos)
  expect_identical(dos$CVEGEO, c("01001", "01002", "06001", "06002"))
  expect_identical(names(dos), names(colima))
  expect_length(attr(dos, "fuente"), 2L)
  expect_match(attr(dos, "fuente")[1], "/MGN2025/01/2025_1_01_MUN[.]zip$")
  # Conserva la proyeccion del primer archivo, sin transformar las coordenadas
  expect_match(sf::st_crs(dos)$proj4string, "+proj=lcc", fixed = TRUE)
  expect_equal(unname(sf::st_bbox(dos)[c("xmin", "ymin")]), c(2400000, 1100000))
})

test_that("descarga por capa: manzanas por omision y el texto NULL como NA", {
  servidor <- marcogeo_local()
  mensajes <- testthat::capture_messages(manzanas <- sig_marcogeoestadistico(2025, "manzanas"))
  expect_match(mensajes, "No indicaste `estado`: se descarga el mapa de Aguascalientes", all = FALSE)
  esperar_un_mapa(manzanas)
  expect_identical(unique(manzanas$CVE_ENT), "01")
  expect_identical(manzanas$CVEGEO[1], "0100100010229001")
  expect_identical(manzanas$AMBITO, c("Urbana", "Urbana", NA))
  expect_identical(manzanas$TIPOMZA[1], "T\u00edpica")
})

test_that("descarga por capa: los frentes de manzana de todo el pais se arman con las entidades", {
  servidor <- marcogeo_local()
  dos_entidades()
  mensajes <- testthat::capture_messages(frentes <- sig_marcogeoestadistico(2025, "frentes", estado = "nacional"))
  expect_match(mensajes, "se arman con los archivos de las 2 entidades", all = FALSE)
  expect_match(mensajes, "Descargando 2025_1_01_FM[.]zip", all = FALSE)
  expect_match(mensajes, "Descargando 2025_1_06_FM[.]zip", all = FALSE)
  esperar_un_mapa(frentes)
  expect_identical(nrow(frentes), 5L)
  expect_identical(frentes$CVE_ENT, c("01", "01", "06", "06", "06"))
  expect_s3_class(sf::st_geometry(frentes), "sfc_LINESTRING")
  expect_identical(frentes$NOMVIAL[4], "Ni\u00f1os H\u00e9roes")
})

test_that("descarga por capa: entidades sin archivo y archivos que el INEGI no tiene", {
  servidor <- marcogeo_local()
  antes <- descargas_sig(servidor)$solicitudes
  # Aguascalientes no tiene territorio insular: se sabe sin descargar nada
  expect_error(
    sig_marcogeoestadistico(2025, "territorio_insular", estado = c("Colima", "Aguascalientes")),
    "El INEGI no publica el mapa \"territorio_insular\" de Aguascalientes en la edici\u00f3n 2025.*Lo publica para 13 entidades"
  )
  expect_error(
    sig_marcogeoestadistico(2018, "PEM", estado = "Ciudad de M\u00e9xico", version = "diciembre"),
    "no publica el mapa \"poligono_externo_manzana\" de Ciudad de M\u00e9xico en la edici\u00f3n 2018 \\(diciembre\\).*29 entidades"
  )
  expect_identical(descargas_sig(servidor)$solicitudes, antes)
  islas <- suppressMessages(sig_marcogeoestadistico(2020, "territorio_insular", estado = "Colima"))
  esperar_un_mapa(islas)
  expect_identical(islas$NOMGEO, c("Isla Socorro", "Isla San Benedicto"))
  # Archivo que no esta en el servidor: responde como el INEGI, con una pagina HTML
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2024, "municipios", estado = "Colima")),
    "El INEGI no tiene publicado el archivo .*/MGN2024/06/2024_1_06_MUN[.]zip"
  )
})

test_that("descarga por capa: sin archivo de todo el pais se combinan las entidades", {
  servidor <- marcogeo_local()
  dos_entidades()
  capas <- mg_capas
  capas$bytes[capas$year == 2025 & capas$capa == "MUN"] <- NA
  local_mocked_bindings(mg_capas = capas)
  mensajes <- testthat::capture_messages(municipios <- sig_marcogeoestadistico(2025, "municipios", estado = "nacional"))
  expect_match(mensajes, "publica este mapa solo por entidad.*las 2 entidades", all = FALSE)
  esperar_un_mapa(municipios)
  expect_identical(municipios$CVE_ENT, c("01", "01", "06", "06"))
})

# Productos completos (de 1995 a junio de 2017) ---------------------------------------

test_that("producto completo: capas en zip internos, sin codificacion declarada", {
  servidor <- marcogeo_local()
  expect_message(
    municipios <- sig_marcogeoestadistico(1995, "municipios"),
    "Descargando 702825292836_s[.]zip \\(35 MB\\)"
  )
  esperar_un_mapa(municipios)
  expect_identical(names(municipios), c("CVE_ENT", "CVE_MUN", "NOM_MUN", "geometry"))
  expect_identical(municipios$CVE_MUN, c("001", "001", "007", "053"))
  # El archivo no declara su codificacion: se lee como Latin-1
  expect_identical(municipios$NOM_MUN, c("Aguascalientes", "Armer\u00eda", "Manzanillo", "Morelia"))
  expect_match(sf::st_crs(municipios)$proj4string, "+proj=lcc", fixed = TRUE)
  expect_match(attr(municipios, "fuente"), "/geografia/marc_geo/702825292836_s[.]zip$")

  # Otro mapa de la misma edicion no repite la descarga
  antes <- descargas_sig(servidor)$solicitudes
  expect_message(entidades <- sig_marcogeoestadistico(1995, "entidades"), "Usando la copia local de 702825292836_s[.]zip")
  esperar_un_mapa(entidades)
  expect_identical(entidades$NOM_ENT, c("Aguascalientes", "Colima", "Michoac\u00e1n de Ocampo"))
  expect_identical(descargas_sig(servidor)$solicitudes, antes)
})

test_that("producto completo: `estado` conserva las filas de las entidades pedidas", {
  servidor <- marcogeo_local()
  mensajes <- testthat::capture_messages(colima <- sig_marcogeoestadistico(1995, "municipios", estado = "Colima"))
  expect_match(mensajes, "solo para todo el pa\u00eds: se conservan las filas de Colima", all = FALSE)
  esperar_un_mapa(colima)
  expect_identical(colima$NOM_MUN, c("Armer\u00eda", "Manzanillo"))
  expect_identical(rownames(colima), c("1", "2"))
  varias <- suppressMessages(sig_marcogeoestadistico(1995, "entidades", estado = c(16, 1)))
  esperar_un_mapa(varias)
  expect_identical(varias$CVE_ENT, c("01", "16"))
})

test_that("producto completo: 2005 no trae proyeccion en entidades ni en municipios", {
  servidor <- marcogeo_local()
  mensajes <- testthat::capture_messages(municipios <- sig_marcogeoestadistico(2005, "municipios", estado = "Colima"))
  expect_match(mensajes, "no publica el archivo de proyecci\u00f3n de este mapa: se devuelve sin proyecci\u00f3n", all = FALSE)
  esperar_un_mapa(municipios)
  expect_true(is.na(sf::st_crs(municipios)))
  # Los nombres de las columnas son los de esa edicion
  expect_identical(names(municipios), c("CVE_EDO", "CVE_MUN", "NOMBRE_MUN", "CVE_CONCA", "geometry"))
  expect_identical(municipios$NOMBRE_MUN, c("Armer\u00eda", "Manzanillo"))
  # Las AGEB urbanas si traen proyeccion, y solo la clave concatenada
  mensajes <- testthat::capture_messages(ageb <- sig_marcogeoestadistico(2005, "ageb_urbana", estado = "Aguascalientes"))
  expect_false(any(grepl("sin proyecci\u00f3n", mensajes)))
  esperar_un_mapa(ageb)
  expect_false(is.na(sf::st_crs(ageb)))
  expect_identical(ageb$CLAVE, "010010001287A")
})

test_that("producto completo: capas en carpetas y zonas en disputa (2010, version 4.3)", {
  servidor <- marcogeo_local()
  municipios <- suppressMessages(sig_marcogeoestadistico(2010, "municipios", version = "4.3"))
  esperar_un_mapa(municipios)
  expect_identical(municipios$NOM_MUN, c("Aguascalientes", "Armer\u00eda", "Manzanillo"))
  # Las zonas se devuelven completas: no pertenecen a una entidad
  mensajes <- testthat::capture_messages(
    zonas <- sig_marcogeoestadistico(2010, "zonas_entidades", estado = "Colima", version = "4.3")
  )
  expect_match(mensajes, "`estado` no se usa", all = FALSE)
  esperar_un_mapa(zonas)
  expect_identical(zonas$CVE_ENT, c("CY", "CQ"))
  expect_identical(zonas$NOM_ENT[1], "Campeche - Yucat\u00e1n")
  # Mapa que el producto deberia tener y no tiene
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2010, "ageb_rural", version = "4.3")),
    "702825296520_s[.]zip no tiene la estructura esperada: no contiene la capa \"32_Entidades_Federativas/AGEB\""
  )
})

test_that("producto completo: 2014 viene en coordenadas geograficas", {
  servidor <- marcogeo_local()
  # El archivo del producto no se llama como su clave
  mensajes <- testthat::capture_messages(municipios <- sig_marcogeoestadistico(2014, "municipios", estado = "06"))
  expect_match(mensajes, "Descargando 889463084105_s[.]zip \\(136 MB\\)", all = FALSE)
  esperar_un_mapa(municipios)
  expect_true(sf::st_is_longlat(municipios))
  expect_identical(municipios$NOM_MUN, c("Armer\u00eda", "Manzanillo"))
  expect_equal(unname(sf::st_bbox(municipios)["xmin"]), -104)
})

test_that("producto completo: errores del servidor y del contenido", {
  servidor <- marcogeo_local()
  # Producto que no esta en el servidor
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2000, "municipios")),
    "El INEGI no tiene publicado el archivo .*702825292843_s[.]zip"
  )
  # Producto sin el zip interno de la capa
  capas <- function(clave) c(entidades = "mge1995.zip!/Entidades_1995", municipios = "mgx1995.zip!/Municipios_1995")
  local_mocked_bindings(marcogeo_capas_producto = capas)
  expect_error(
    suppressMessages(sig_marcogeoestadistico(1995, "municipios")),
    "no tiene la estructura esperada: no contiene mgx1995[.]zip. Contiene: \"mge1995[.]zip\", \"mgm1995[.]zip\""
  )
})

# Productos por entidad (febrero y junio de 2018, Censo Agropecuario 2022) -------------

test_that("producto por entidad: descarga solo la parte de la entidad pedida", {
  servidor <- marcogeo_local()
  local_mocked_bindings(zip_tamano_cola = function() 400)
  producto <- muestra("sig/producto_889463526636_s.zip")
  colima <- zip_indice_local(producto)
  colima <- colima[colima$nombre == "06_colima.zip", ]
  antes <- descargas_sig(servidor)
  expect_message(
    municipios <- sig_marcogeoestadistico(2018, "municipios", estado = "Colima", version = "febrero"),
    "Descargando 06_colima[.]zip \\(.*es una parte de 889463526636_s[.]zip\\)"
  )
  despues <- descargas_sig(servidor)
  esperar_un_mapa(municipios)
  expect_identical(municipios$NOMGEO, c("Armer\u00eda", "Colima"))
  expect_match(attr(municipios, "fuente"), "/geografia/marcogeo/889463526636_s[.]zip$")
  # Tres solicitudes parciales: el indice del producto, el encabezado del zip
  # de la entidad y sus datos. Nunca el producto completo.
  expect_identical(despues$solicitudes - antes$solicitudes, 3)
  expect_identical(despues$bytes - antes$bytes, 400 + 30 + colima$comprimido)
  expect_lt(despues$bytes - antes$bytes, file.size(producto))

  # Otro mapa de la misma entidad no descarga nada; el .CPG en mayusculas se usa
  expect_message(
    ageb <- sig_marcogeoestadistico(2018, "ageb_urbana", estado = "Colima", version = "febrero"),
    "Usando la copia local de 06_colima[.]zip"
  )
  esperar_un_mapa(ageb)
  expect_identical(ageb$CVE_AGEB, c("0229", "0233"))
  expect_identical(descargas_sig(servidor)$solicitudes, despues$solicitudes)
  # La capa del territorio insular tiene otro nombre en esta edicion
  islas <- suppressMessages(sig_marcogeoestadistico(2018, "TI", estado = "Colima", version = "febrero"))
  esperar_un_mapa(islas)
  expect_identical(islas$NOMGEO, "Isla Socorro")
  entidad <- suppressMessages(sig_marcogeoestadistico(2018, "entidades", estado = "Colima", version = "febrero"))
  esperar_un_mapa(entidad)
  expect_identical(entidad$CVE_GEO, "06")
})

test_that("producto por entidad: Aguascalientes por omision y todo el pais combinando entidades", {
  servidor <- marcogeo_local()
  dos_entidades()
  mensajes <- testthat::capture_messages(omision <- sig_marcogeoestadistico(2018, "municipios", version = "febrero"))
  expect_match(mensajes, "No indicaste `estado`: se descarga el mapa de Aguascalientes", all = FALSE)
  esperar_un_mapa(omision)
  expect_identical(omision$CVEGEO, c("01001", "01002"))

  mensajes <- testthat::capture_messages(
    pais <- sig_marcogeoestadistico(2018, "municipios", estado = "nacional", version = "febrero")
  )
  expect_match(mensajes, "solo por entidad: se descargan y se combinan los archivos de las 2 entidades", all = FALSE)
  esperar_un_mapa(pais)
  expect_identical(pais$CVEGEO, c("01001", "01002", "06001", "06002"))
  # Una entidad sin el mapa: error si se pide; se omite al combinar todo el pais
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2018, "territorio_insular", estado = "Aguascalientes", version = "febrero")),
    "no publica el mapa \"territorio_insular\" de Aguascalientes en la edici\u00f3n 2018 \\(febrero\\).*estado = \"nacional\""
  )
  islas <- suppressMessages(sig_marcogeoestadistico(2018, "territorio_insular", estado = "nacional", version = "febrero"))
  esperar_un_mapa(islas)
  expect_identical(islas$CVE_ENT, "06")
})

test_that("producto por entidad: el zip de todo el pais del Censo Agropecuario 2022", {
  servidor <- marcogeo_local()
  expect_message(
    municipios <- sig_marcogeoestadistico(2023, "municipios", version = "censo agropecuario 2022"),
    "Descargando mgagro2022_integrado[.]zip"
  )
  esperar_un_mapa(municipios)
  expect_identical(municipios$CVEGEO, c("01001", "01002", "06001", "06002", "18001"))
  entidades <- suppressMessages(sig_marcogeoestadistico(2023, "entidades", estado = "nacional", version = "censo agropecuario 2022"))
  esperar_un_mapa(entidades)
  expect_identical(entidades$NOMGEO, c("Aguascalientes", "Colima", "Nayarit"))
  # Por entidad, con la proyeccion escrita de otra manera en uno de los archivos
  dos <- suppressMessages(
    sig_marcogeoestadistico(2023, "municipios", estado = c("Colima", "Aguascalientes"), version = "censo agropecuario 2022")
  )
  esperar_un_mapa(dos)
  expect_identical(dos$CVEGEO, c("01001", "01002", "06001", "06002"))
  # Los mapas que no estan en el zip de todo el pais se arman con las entidades
  dos_entidades()
  islas <- suppressMessages(
    sig_marcogeoestadistico(2023, "territorio_insular", estado = "nacional", version = "censo agropecuario 2022")
  )
  esperar_un_mapa(islas)
  expect_identical(islas$CVEGEO, "060070199")
})

test_that("producto por entidad: errores del servidor y del contenido", {
  servidor <- marcogeo_local()
  # Producto que no esta en el servidor: no hay respuesta parcial
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2018, "municipios", estado = "Colima", version = "junio")),
    "c\u00f3digo 200 a la descarga parcial de .*889463592587_s[.]zip"
  )
  # Producto sin el zip de una entidad
  expect_error(
    suppressMessages(sig_marcogeoestadistico(2018, "municipios", estado = "Nayarit", version = "febrero")),
    "889463526636_s[.]zip no tiene la estructura esperada: no contiene el archivo de Nayarit"
  )
})

# Funcion exportada -------------------------------------------------------------------

test_that("sig_marcogeoestadistico() valida los argumentos antes de descargar", {
  servidor <- marcogeo_local()
  antes <- descargas_sig(servidor)$solicitudes
  expect_error(sig_marcogeoestadistico(), "Debes indicar el a\u00f1o de edici\u00f3n")
  expect_error(sig_marcogeoestadistico(mapa = "municipios"), "Debes indicar el a\u00f1o de edici\u00f3n")
  expect_error(sig_marcogeoestadistico(2025), "Debes indicar el mapa en `mapa`")
  expect_error(sig_marcogeoestadistico(2010, "municipios"), "Indica una en `version`")
  # Con el orden de argumentos de sig_marcogeo(), la version cae en `estado`
  expect_error(sig_marcogeoestadistico(2010, "municipios", "5.0"), "Indica una en `version`")
  expect_error(sig_marcogeoestadistico(2009, "ageb"), "era una opci\u00f3n de sig_marcogeo")
  expect_error(sig_marcogeoestadistico(2025, "municipios", estado = "Narnia"), "No se reconoce la entidad")
  expect_error(sig_marcogeoestadistico(1995, "manzanas"), "no tiene el mapa \"manzanas\"")
  expect_identical(descargas_sig(servidor)$solicitudes, antes)
})

test_that("sig_marcogeoestadistico() avisa si falta el paquete sf, despues de validar", {
  local_mocked_bindings(requiere_paquete = function(paquete) detener("falta el paquete ", paquete))
  expect_error(sig_marcogeoestadistico(2025, "municipios"), "falta el paquete sf")
  expect_error(sig_marcogeoestadistico(2025, "colonias"), "no es una opci\u00f3n v\u00e1lida")
})

test_that("con la copia local activada, las descargas se conservan en la carpeta del usuario", {
  servidor <- marcogeo_local()
  carpeta <- withr::local_tempdir()
  withr::local_options(importinegi.cache = TRUE, importinegi.cache_dir = carpeta)
  primero <- suppressMessages(sig_marcogeoestadistico(1995, "entidades"))
  esperar_un_mapa(primero)
  expect_true(file.exists(file.path(carpeta, "marcogeo", "702825292836_s.zip")))
  colima <- suppressMessages(sig_marcogeoestadistico(2018, "municipios", estado = "Colima", version = "febrero"))
  expect_true(file.exists(file.path(carpeta, "marcogeo", "889463526636_06_colima.zip")))
  capa <- suppressMessages(sig_marcogeoestadistico(2025, "municipios", estado = "Colima"))
  expect_length(list.files(carpeta, pattern = "2025_1_06_MUN[.]zip$", recursive = TRUE), 1L)
  # La segunda vez no se pide nada al servidor
  antes <- descargas_sig(servidor)$solicitudes
  memoria_borrar()
  expect_message(sig_marcogeoestadistico(1995, "entidades"), "Usando la copia local")
  expect_message(sig_marcogeoestadistico(2025, "municipios", estado = "Colima"), "Usando la copia local")
  expect_identical(descargas_sig(servidor)$solicitudes, antes)
})

# Consulta ----------------------------------------------------------------------------

test_that("bases_importinegi() lista las 26 ediciones del Marco Geoestadistico y sus mapas", {
  withr::local_options(importinegi.base = "http://127.0.0.1:9")
  bases <- bases_importinegi("sig_marcogeoestadistico")
  expect_identical(nrow(bases), 26L)
  expect_identical(unique(bases$proyecto), "sig_marcogeoestadistico")
  expect_identical(unique(bases$formato), "shp")
  expect_true(all(is.na(bases$modulo)))
  expect_false(anyNA(bases[c("year", "base", "descripcion", "tablas", "cobertura", "tamano_mb")]))
  expect_identical(anyDuplicated(bases[, c("year", "base")]), 0L)
  # `base` es la version, como se escribe en el argumento `version`
  expect_identical(bases$base[bases$year == 2010], c("4.3", "5.0", "5.0.A", "5.0.A 1:20 000"))
  expect_identical(bases$base[bases$year == 2018], c("febrero", "junio", "diciembre"))
  for (i in seq_len(nrow(bases))) {
    edicion <- marcogeo_edicion(bases$year[i], bases$base[i])
    # Cada mapa anunciado es un mapa que la funcion acepta
    for (mapa in strsplit(bases$tablas[i], ", ", fixed = TRUE)[[1]]) {
      expect_identical(marcogeo_mapa(mapa, edicion), mapa)
    }
  }
  fila <- function(year, base) bases[bases$year == year & bases$base == base, ]
  expect_identical(fila(1995, "conteo 1995")$descripcion, "Marco Geoestad\u00edstico municipal 1995. Conteo de Poblaci\u00f3n y Vivienda 1995")
  expect_match(fila(2010, "5.0.A 1:20 000")$descripcion, ", escala 1:20 000$")
  expect_identical(fila(1995, "conteo 1995")$cobertura, "nacional")
  expect_identical(fila(2018, "febrero")$cobertura, "por entidad")
  expect_identical(fila(2023, "censo agropecuario 2022")$cobertura, "nacional y por entidad")
  expect_identical(fila(2025, "2025")$cobertura, "nacional y por entidad")
  expect_equal(fila(1995, "conteo 1995")$tamano_mb, 35.4)
  expect_equal(fila(2018, "febrero")$tamano_mb, 2501.9)
  # Descarga por capa: la suma de los archivos de todo el pais
  expect_equal(fila(2025, "2025")$tamano_mb, round(sum(mg_capas$bytes[mg_capas$year == 2025]) / 1024^2, 1))
  # Filtros de bases_importinegi()
  expect_identical(nrow(bases_importinegi("sig_marcogeoestadistico", year = 2018)), 3L)
  expect_true("sig_marcogeoestadistico" %in% bases_importinegi(year = 2010)$proyecto)
})
