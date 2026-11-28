# La muestra sig/api_productos.json es una respuesta real del servicio de
# productos del INEGI (27 de noviembre de 2026), reducida a 5 de sus 29
# productos: tres ediciones del Marco Geoestadistico, una carta en PDF y la capa
# de areas de control del Marco Censal Agropecuario.

productos_muestra <- function() {
  jsonlite::fromJSON(leer_muestra("sig/api_productos.json"), simplifyVector = FALSE)$list$mapas
}

test_that("la consulta del servicio de productos es la de la coleccion del Marco Geoestadistico", {
  withr::local_options(importinegi.base = NULL)
  expect_identical(
    marcogeo_api_url(),
    "https://www.inegi.org.mx/app/api/productos/interna_v2/componente/mapas/lista/resultados/"
  )
  consulta <- marcogeo_api_consulta()
  expect_identical(consulta$tema, "213")
  expect_identical(consulta$titg, "4328")
  expect_identical(
    as.character(jsonlite::toJSON(consulta, auto_unbox = TRUE)),
    "{\"tema\":\"213\",\"titg\":\"4328\",\"orden\":4,\"desc\":true,\"pag\":0,\"tam\":\"100\"}"
  )
})

test_that("marcogeo_api_a_tabla() conserva solo las ediciones del Marco Geoestadistico en SHP", {
  tabla <- marcogeo_api_a_tabla(productos_muestra())
  expect_named(tabla, c("clave", "titulo", "edicion", "escala", "ruta"))
  # Quedan fuera la carta en PDF y la capa del Marco Censal Agropecuario
  expect_identical(tabla$clave, c("702825296629", "702825004386", "794551196649"))
  expect_identical(tabla$edicion, c(2010L, 2014L, 2026L))
  expect_identical(tabla$escala, c("1:20 000", "", ""))
  expect_identical(tabla$titulo[2], "Marco geoestad\u00edstico 2014 versi\u00f3n 6.2 (DENUE)")
  base <- "/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/"
  expect_identical(
    tabla$ruta,
    paste0(base, c("marc_geo/702825296629_s.zip", "marc_geo/889463084105_s.zip", "marcogeo/794551196649_s.zip"))
  )
  # Los productos de la muestra estan en el registro del paquete, con los mismos datos
  registro <- mg_productos[match(tabla$clave, mg_productos$clave), names(tabla)]
  rownames(registro) <- NULL
  expect_identical(registro, tabla)
})

test_that("marcogeo_api_a_tabla() tolera productos incompletos y listas vacias", {
  vacia <- marcogeo_api_a_tabla(list())
  expect_named(vacia, c("clave", "titulo", "edicion", "escala", "ruta"))
  expect_identical(nrow(vacia), 0L)
  productos <- productos_muestra()
  # Un producto del Marco Geoestadistico sin archivo SHP, y otro sin titulo
  productos[[1]]$formatos[[1]]$extension <- "PDF"
  productos[[4]]$titulo <- NULL
  expect_identical(marcogeo_api_a_tabla(productos)$clave, "702825296629")
})

test_that("marcogeo_api_productos() consulta el servicio y valida su respuesta", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0)
  expect_identical(marcogeo_api_productos(), marcogeo_api_a_tabla(productos_muestra()))
  # Respuesta del servicio cuando la consulta no es valida
  local_mocked_bindings(marcogeo_api_consulta = function() list(tema = "", titg = "", tam = "100"))
  expect_error(marcogeo_api_productos(), "no devolvi\u00f3 una lista v\u00e1lida: Par\u00e1metros de consulta nulos")
})

test_that("marcogeo_api_productos() se detiene si la respuesta puede estar incompleta", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0)
  # La respuesta trae 5 productos: con un maximo de 5 puede haber mas
  consulta <- marcogeo_api_consulta()
  consulta$tam <- "5"
  local_mocked_bindings(marcogeo_api_consulta = function() consulta)
  expect_error(marcogeo_api_productos(), "devolvi\u00f3 5 productos, el m\u00e1ximo de la consulta")
})

test_that("marcogeo_sondear() da el tama\u00f1o de un zip sin descargarlo, o NA si no existe", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0)
  capa <- "/contenidos/descargadenue/MGdescarga/MGN2025/2025_1_00_MUN.zip"
  antes <- descargas_sig(servidor)
  expect_identical(marcogeo_sondear(capa), as.numeric(file.size(muestra("sig/capa_2025_1_00_MUN.zip"))))
  despues <- descargas_sig(servidor)
  # Una solicitud de cuatro bytes
  expect_identical(despues$solicitudes - antes$solicitudes, 1)
  expect_identical(despues$bytes - antes$bytes, 4)
  # Archivo inexistente: el INEGI responde 200 con una pagina HTML
  expect_identical(marcogeo_sondear("/contenidos/descargadenue/MGdescarga/MGN2027/2027_1_00_ENT.zip"), NA_real_)
  expect_identical(marcogeo_sondear("/datos/404.zip"), NA_real_)
  # Servidor que no atiende la cabecera Range: se usa el tamaño de lo recibido
  expect_identical(marcogeo_sondear("/datos/muestra_dbf.zip"), as.numeric(file.size(muestra("muestra_dbf.zip"))))
  expect_error(marcogeo_sondear("/datos/403.zip"), "respondi\u00f3 con el c\u00f3digo 403")
})

test_that("marcogeo_registro_productos() agrega el tama\u00f1o exacto de cada producto", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0)
  expect_message(tabla <- marcogeo_registro_productos(), "Productos del Marco Geoestad\u00edstico en el servicio del INEGI: 3")
  expect_named(tabla, c("clave", "titulo", "edicion", "escala", "ruta", "bytes"))
  # Solo el producto de 2014 esta en el servidor local
  expect_identical(
    tabla$bytes, c(NA, as.numeric(file.size(muestra("sig/producto_889463084105_s.zip"))), NA)
  )
  expect_silent(marcogeo_registro_productos(silencioso = TRUE))
})

test_that("marcogeo_registro_capas() sondea el archivo nacional y el de cada entidad", {
  servidor <- servidor_prueba()
  withr::local_options(importinegi.base = sub("/$", "", servidor$url("/")), importinegi.espera = 0)
  local_mocked_bindings(
    marcogeo_todas = function() c("01", "06", "18"),
    marcogeo_capas_descarga = function() c("MUN", "M", "FM", "PE")
  )
  expect_message(capas <- marcogeo_registro_capas(2025), "Sondeando la descarga por capa de 2025")
  expect_named(capas, c("year", "capa", "bytes", "faltan"))
  expect_identical(capas$year, rep(2025L, 4))
  expect_identical(capas$capa, c("MUN", "M", "FM", "PE"))
  # Municipios: archivo nacional y de dos entidades; manzanas y frentes, solo
  # por entidad; poligono externo, ninguno
  expect_identical(
    capas$bytes, c(as.numeric(file.size(muestra("sig/capa_2025_1_00_MUN.zip"))), NA, NA, NA)
  )
  expect_identical(capas$faltan, c("18", "06 18", "18", "todas"))
  varios <- marcogeo_registro_capas(c(2020, 2025), silencioso = TRUE)
  expect_identical(varios$year, rep(c(2020L, 2025L), each = 4))
})

test_that("los datos del Marco Geoestadistico que se distribuyen con el paquete son consistentes", {
  expect_named(mg_productos, c("clave", "titulo", "edicion", "escala", "ruta", "bytes"))
  # 26 productos el 27 de noviembre de 2026; el INEGI agrega uno o dos por año
  expect_gte(nrow(mg_productos), 26L)
  expect_true(all(marcogeo_ediciones()$clave %in% mg_productos$clave))
  expect_identical(anyDuplicated(mg_productos$clave), 0L)
  expect_type(mg_productos$edicion, "integer")
  expect_type(mg_productos$bytes, "double")
  expect_false(anyNA(mg_productos))
  expect_true(all(mg_productos$bytes > 30e6 & mg_productos$bytes < 4e9))
  expect_true(all(grepl(
    "^/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/geografia/marc(_|o)geo/.*_s[.]zip$",
    mg_productos$ruta
  )))
  expect_true(all(validUTF8(mg_productos$titulo)))
  expect_true(all(startsWith(normalizar_texto(mg_productos$titulo), "marco geoestadistico")))
  expect_true("1:20 000" %in% mg_productos$escala)

  expect_named(mg_capas, c("year", "capa", "bytes", "faltan"))
  years <- sort(unique(mg_capas$year))
  expect_identical(years[1:10], 2017:2026)
  expect_identical(years, seq(2017L, max(years)))
  capas <- marcogeo_capas_descarga()
  expect_length(capas, 16L)
  expect_identical(nrow(mg_capas), 16L * length(years))
  expect_identical(mg_capas$capa, rep(capas, times = length(years)))
  expect_identical(mg_capas$year, rep(years, each = 16L))
  # Las capas que no existen no tienen archivo nacional, y al reves
  expect_identical(is.na(mg_capas$bytes), mg_capas$faltan == "todas")
  expect_identical(
    paste(mg_capas$year, mg_capas$capa)[mg_capas$faltan == "todas"], c("2017 PE", "2017 PEM")
  )
  # `faltan`: claves de entidad separadas por espacios
  con_faltantes <- mg_capas$faltan[!mg_capas$faltan %in% c("", "todas")]
  claves <- unlist(strsplit(con_faltantes, " ", fixed = TRUE))
  expect_true(all(claves %in% marcogeo_todas()))
  # El territorio insular existe en 13 entidades en todos los años
  islas <- mg_capas$faltan[mg_capas$capa == "TI"]
  expect_identical(unique(lengths(strsplit(islas, " ", fixed = TRUE))), 19L)
  expect_identical(mg_capas$faltan[mg_capas$year == 2018 & mg_capas$capa == "PEM"], "02 09 10")
})

test_that("los CSV de data-raw coinciden con los datos internos", {
  # data-raw no se incluye en el paquete construido
  productos <- test_path("..", "..", "data-raw", "marcogeo_productos.csv")
  capas <- test_path("..", "..", "data-raw", "marcogeo_capas.csv")
  skip_if_not(file.exists(productos) && file.exists(capas), "data-raw no esta disponible")
  leidos <- utils::read.csv(
    productos, encoding = "UTF-8", na.strings = character(), stringsAsFactors = FALSE,
    colClasses = c(clave = "character", titulo = "character", edicion = "integer", escala = "character",
                   ruta = "character", bytes = "numeric")
  )
  expect_identical(leidos, mg_productos, ignore_attr = TRUE)
  leidas <- utils::read.csv(
    capas, encoding = "UTF-8", na.strings = character(), stringsAsFactors = FALSE,
    colClasses = c(year = "integer", capa = "character", bytes = "numeric", faltan = "character")
  )
  expect_identical(leidas, mg_capas, ignore_attr = TRUE)
})
