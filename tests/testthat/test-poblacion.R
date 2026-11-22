test_that("rellenar_clave() completa las claves con ceros a la izquierda", {
  expect_identical(rellenar_clave(c(6L, 18L, NA), 2L), c("06", "18", NA))
  expect_identical(rellenar_clave(c(1, 10, 100), 3L), c("001", "010", "100"))
  expect_identical(rellenar_clave(c("6", "06", " 18 ", NA, ""), 2L), c("06", "06", "18", NA, ""))
  expect_identical(rellenar_clave(c("12", "003A", "1A"), 4L), c("0012", "003A", "001A"))
})

test_that("poblacion_normalizar_claves() reconoce las claves por nombre", {
  datos <- data.frame(ent = c(6, 18), CVE_MUN = c("1", "20"), LOC = c("1", "9998"),
                      POBTOT = c(5L, 6L), stringsAsFactors = FALSE)
  normal <- poblacion_normalizar_claves(datos)
  expect_identical(normal$ent, c("06", "18"))
  expect_identical(normal$CVE_MUN, c("001", "020"))
  expect_identical(normal$LOC, c("0001", "9998"))
  expect_identical(normal$POBTOT, c(5L, 6L))
  expect_identical(columna_clave(datos, "ent"), "ent")
  expect_identical(columna_clave(datos, "mun"), "CVE_MUN")
  expect_identical(columna_clave(datos, "ageb"), NA_character_)
})

test_that("poblacion_clasificar_nivel() identifica los totales del ITER", {
  iter <- data.frame(
    ENTIDAD = c("00", "00", "06", "06", "06", "06", "06", "06"),
    MUN     = c("000", "000", "000", "000", "001", "001", "001", "001"),
    LOC     = c("0000", "9998", "0000", "9999", "0000", "0001", "9998", "0350"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    poblacion_clasificar_nivel(iter, "iter"),
    c("nacional", "agregado", "entidad", "agregado", "municipio", "localidad", "agregado", "localidad")
  )
  expect_null(poblacion_clasificar_nivel(iter[, c("ENTIDAD", "MUN")], "iter"))
})

test_that("poblacion_clasificar_nivel() identifica los totales por AGEB y manzana", {
  ageb <- data.frame(
    ENTIDAD = "06",
    MUN  = c("000", "002", "002", "002", "002"),
    LOC  = c("0000", "0000", "0001", "0001", "0001"),
    AGEB = c("0000", "0000", "0000", "003A", "003A"),
    MZA  = c("000", "000", "000", "000", "010"),
    stringsAsFactors = FALSE
  )
  expect_identical(poblacion_clasificar_nivel(ageb, "ageb"),
                   c("entidad", "municipio", "localidad", "ageb", "manzana"))
  expect_null(poblacion_clasificar_nivel(ageb[, c("ENTIDAD", "MUN", "LOC")], "ageb"))
})

test_that("poblacion_filtrar_nivel() avisa si la tabla no tiene claves geograficas", {
  datos <- data.frame(A = 1:2, B = c("x", "y"), stringsAsFactors = FALSE)
  expect_identical(poblacion_filtrar_nivel(datos, "iter", "todos", TRUE), datos)
  expect_error(poblacion_filtrar_nivel(datos, "iter", "municipio", TRUE),
               "No se puede filtrar por `nivel`.*Usa nivel = \"todos\"")
  expect_warning(resultado <- poblacion_filtrar_nivel(datos, "iter", "localidad", FALSE),
                 "se devuelven todas las filas")
  expect_identical(resultado, datos)
})

test_that("combinar_tablas() une entidades con columnas o tipos distintos", {
  a <- data.frame(ENT = "06", X = 1:2, stringsAsFactors = FALSE)
  b <- data.frame(ENT = "18", X = 3L, stringsAsFactors = FALSE)
  expect_identical(combinar_tablas(list(a)), a)
  expect_identical(combinar_tablas(list(a, b)),
                   data.frame(ENT = c("06", "06", "18"), X = 1:3, stringsAsFactors = FALSE))

  # Una columna numerica en una entidad y de texto en otra queda como texto
  texto <- data.frame(ENT = "18", X = "003", stringsAsFactors = FALSE)
  expect_identical(combinar_tablas(list(a, texto))$X, c("1", "2", "003"))

  extra <- data.frame(ENT = "18", Y = "z", stringsAsFactors = FALSE)
  expect_warning(unida <- combinar_tablas(list(a, extra)), "no tienen las mismas columnas")
  expect_named(unida, c("ENT", "X", "Y"))
  expect_identical(unida$X, c(1L, 2L, NA))
  expect_identical(unida$Y, c(NA, NA, "z"))
})

# Conversion de tipos tal como la hacia el motor antes de convertir cada
# entidad por separado: unir primero y convertir despues. Sirve de referencia
# para comprobar que el resultado no cambio.
convertir_original <- function(datos, texto = NULL, ceros = FALSE) {
  protegidas <- tolower(texto %||% character())
  for (columna in names(datos)) {
    valores <- datos[[columna]]
    if (!is.character(valores) || tolower(columna) %in% protegidas) {
      next
    }
    presentes <- trimws(valores[!is.na(valores)])
    if (length(presentes) == 0) {
      next
    }
    es_numero <- grepl("^-?[0-9]+(\\.[0-9]+)?$", presentes)
    es_clave <- grepl("^-?0[0-9]", presentes)
    if (all(es_numero) && (ceros || !any(es_clave))) {
      numeros <- as.numeric(valores)
      enteros <- !any(grepl(".", presentes, fixed = TRUE)) &&
        all(abs(numeros) <= .Machine$integer.max, na.rm = TRUE)
      datos[[columna]] <- if (enteros) as.integer(numeros) else numeros
    }
  }
  datos
}

preparar_original <- function(tablas, base) {
  datos <- combinar_tablas(tablas)
  datos <- poblacion_normalizar_claves(datos)
  faltantes <- poblacion_faltantes(base)
  if (!is.null(faltantes)) {
    datos <- limpiar_faltantes(datos, faltantes)
  }
  datos <- convertir_original(datos, texto = columnas_texto(datos), ceros = base %in% c("muestra", "asistencia"))
  altitud <- grep("^ALTITUD$", names(datos), ignore.case = TRUE, value = TRUE)
  if (length(altitud) > 0) {
    datos[altitud] <- convertir_original(datos[altitud], ceros = TRUE)
  }
  datos
}

test_that("poblacion_preparar() convierte cada entidad y da el mismo resultado que unir primero", {
  a <- data.frame(
    ENT = "06", MUN = c("1", "2"), EDAD = c("034", "008"), OCUPA = c("0123", "0201"),
    PESO = c("1.5", "2"), GRANDE = c("2147483648", "1"), SALDO = c("-5", "3"), VACIA = NA_character_,
    ALTITUD = c("0020", "1500"), NUMVIV = c("00001", "00002"), CODIGO = c("05", "12"), POBMAS = c("*", "7"),
    stringsAsFactors = FALSE
  )
  b <- data.frame(
    ENT = "18", MUN = "3", EDAD = "45", OCUPA = "A10", PESO = "3", GRANDE = "7", SALDO = "1",
    VACIA = NA_character_, ALTITUD = "30", NUMVIV = "00009", CODIGO = "7", POBMAS = "N/D",
    stringsAsFactors = FALSE
  )
  # Como la entrega un DBF: claves y edad numericas, y sin la columna CODIGO
  c <- data.frame(
    ENT = 1L, MUN = 4L, EDAD = 70L, OCUPA = "0300", PESO = "4", GRANDE = "8", SALDO = "0",
    VACIA = NA_character_, ALTITUD = "0040", NUMVIV = "00010", POBMAS = "2", stringsAsFactors = FALSE
  )
  for (base in c("muestra", "iter", "asistencia")) {
    for (tablas in list(list(a), list(b), list(a, b), list(b, a))) {
      expect_identical(poblacion_preparar(function() tablas, base), preparar_original(tablas, base), info = base)
    }
    expect_warning(nuevo <- poblacion_preparar(function() list(a, b, c), base), "no tienen las mismas columnas")
    expect_warning(original <- preparar_original(list(a, b, c), base), "no tienen las mismas columnas")
    expect_identical(nuevo, original, info = base)
  }

  dos <- poblacion_preparar(function() list(a, b), "muestra")
  expect_identical(dos$ENT, c("06", "06", "18"))
  expect_identical(dos$MUN, c("001", "002", "003"))
  expect_identical(dos$EDAD, c(34L, 8L, 45L))
  # Una columna con un valor que no es numero en una entidad queda como texto
  # en todas, con sus ceros
  expect_identical(dos$OCUPA, c("0123", "0201", "A10"))
  expect_identical(dos$PESO, c(1.5, 2, 3))
  # Un valor que no cabe en un entero hace real la columna
  expect_identical(dos$GRANDE, c(2147483648, 1, 7))
  expect_identical(dos$SALDO, c(-5L, 3L, 1L))
  expect_identical(dos$ALTITUD, c(20L, 1500L, 30L))
  expect_identical(dos$NUMVIV, c("00001", "00002", "00009"))
  expect_identical(dos$CODIGO, c(5L, 12L, 7L))
  expect_identical(dos$VACIA, rep(NA_character_, 3))
  iter <- poblacion_preparar(function() list(a, b), "iter")
  expect_identical(iter$CODIGO, c("05", "12", "7"))
  expect_identical(iter$POBMAS, c(NA, 7L, NA))
})

test_that("decidir_conversion() decide con los valores de todas las tablas", {
  a <- data.frame(X = c("1", "2"), Y = c("01", "2"), Z = c("1.5", NA), stringsAsFactors = FALSE)
  b <- data.frame(X = "3", Y = "4", Z = "x", stringsAsFactors = FALSE)
  expect_identical(decidir_conversion(list(a, b)), c(X = "entero"))
  expect_identical(decidir_conversion(list(a, b), ceros = TRUE), c(X = "entero", Y = "entero"))
  expect_identical(decidir_conversion(list(a)), c(X = "entero", Z = "real"))
  expect_identical(decidir_conversion(list(a, b), texto = "x", medidas = "Y"), c(Y = "entero"))
  expect_identical(aplicar_conversion(a, c(X = "entero", Z = "real")),
                   data.frame(X = 1:2, Y = c("01", "2"), Z = c(1.5, NA), stringsAsFactors = FALSE))
})

test_that("la entidad por omision es Aguascalientes en la muestra y en las bases sin archivo nacional", {
  bases <- poblacion_bases()
  for (i in seq_len(nrow(bases))) {
    archivos <- poblacion_archivos(bases$programa[i], bases$year[i], bases$base[i])
    esperada <- if (bases$base[i] == "muestra" || !"00" %in% archivos$clave) "01" else "00"
    expect_identical(poblacion_estado_omision(bases$base[i], archivos), esperada,
                     info = paste(bases$programa[i], bases$year[i], bases$base[i]))
  }
  expect_identical(poblacion_estado_omision("muestra", poblacion_archivos("censo", 2020L, "muestra")), "01")
  expect_identical(poblacion_estado_omision("entorno", poblacion_archivos("censo", 2020L, "entorno")), "00")
  expect_identical(poblacion_estado_omision("entorno", poblacion_archivos("censo", 2010L, "entorno")), "01")
  expect_message(
    poblacion_avisar_omision("muestra", poblacion_archivos("censo", 2020L, "muestra")),
    "se descarga la muestra de Aguascalientes. Para todo el país usa estado = \"nacional\" \\(archivo nacional de 464 MB\\)\\."
  )
})

test_that("poblacion_elegir_tabla() encuentra la tabla dentro del archivo descargado", {
  carpeta <- inegi_descomprimir(muestra("censo/muestra2000_06.zip"))
  withr::defer(unlink(carpeta, recursive = TRUE))
  expect_identical(basename(poblacion_elegir_tabla(carpeta, "dbf", "^per", "personas")), "PER_F06.DBF")
  expect_identical(basename(poblacion_elegir_tabla(carpeta, "dbf", "^viv|^vho", "viviendas")), "VHO_F06.DBF")
  expect_error(poblacion_elegir_tabla(carpeta, "dbf", "^hog", "hogares"),
               "No se encontr\u00f3 la tabla \"hogares\".*MIN_F06.DBF.*PER_F06.DBF.*VHO_F06.DBF")
  expect_error(poblacion_elegir_tabla(carpeta, "dbf"), "m\u00e1s de un archivo para la tabla de datos")
  expect_error(poblacion_elegir_tabla(carpeta, "csv"), "no contiene lo que se esperaba")

  # Una tabla repetida: se prefiere la menos anidada (la de un zip interno de
  # otra entidad queda en una carpeta); entre copias identicas, la primera; si
  # son distintas, el error dice el nombre del zip y la ruta de cada archivo
  repetida <- withr::local_tempdir()
  dir.create(file.path(repetida, "cpv95_12_dbf"))
  writeLines("a", file.path(repetida, "datgen95.dbf"))
  writeLines("b", file.path(repetida, "cpv95_12_dbf", "DATGEN95.DBF"))
  writeLines("c", file.path(repetida, "migint95.dbf"))
  expect_identical(basename(poblacion_elegir_tabla(repetida, "dbf", "^datgen", "personas")), "datgen95.dbf")
  copias <- withr::local_tempdir()
  for (sub in c("x", "y")) {
    dir.create(file.path(copias, sub))
    writeLines("a", file.path(copias, sub, "trpmue03.DBF"))
  }
  expect_identical(basename(poblacion_elegir_tabla(copias, "dbf", "^trpmue", "personas")), "trpmue03.DBF")
  writeLines("distinta", file.path(copias, "y", "trpmue03.DBF"))
  expect_error(
    poblacion_elegir_tabla(copias, "dbf", "^trpmue", "personas", archivo = "cpv2005_03_dbf.zip"),
    "más de un archivo para la tabla \"personas\" en el archivo descargado cpv2005_03_dbf.zip, que contiene: \"x/trpmue03.DBF\", \"y/trpmue03.DBF\""
  )

  # En los zip de datos abiertos se descartan diccionario, catalogos y metadatos
  abiertos <- inegi_descomprimir(muestra("censo/iter2020_06.zip"))
  withr::defer(unlink(abiertos, recursive = TRUE))
  expect_identical(basename(poblacion_elegir_tabla(abiertos, "csv")), "conjunto_de_datos_iter_06CSV20.csv")
})

test_that("las tablas de bases y programas son coherentes", {
  bases <- poblacion_bases()
  programas <- poblacion_programas()
  # Cada base se reconoce por el titulo y la descripcion, o por la ruta
  expect_true(all(!is.na(bases$reconoce) | !is.na(bases$ruta)))
  expect_identical(anyDuplicated(bases[, c("programa", "year", "base")]), 0L)
  for (programa in unique(bases$programa)) {
    expect_true(all(bases$year[bases$programa == programa] %in% programas[[programa]]$years), info = programa)
  }
  expect_false(anyNA(poblacion_nombre_base(unique(bases$base))))
  titulos <- vapply(seq_len(nrow(bases)), function(i) {
    poblacion_titulo_base(bases$programa[i], bases$year[i], bases$base[i])
  }, character(1))
  expect_false(anyNA(titulos))
  expect_identical(poblacion_titulo_base("censo", 1990L, "muestra"), "Muestra (extracto del 10 % de las viviendas)")
  expect_identical(poblacion_titulo_base("censo", 2000L, "muestra"), "Muestra (cuestionario ampliado)")
  expect_identical(poblacion_titulo_base("conteo", 1995L, "muestra"), "Muestra (encuesta del Conteo, cuestionario ampliado)")
  expect_identical(poblacion_titulo_base("conteo", 2005L, "muestra"), "Muestra (extracto del 10 % de las viviendas)")
  expect_identical(poblacion_titulo_base("intercensal", 2015L, "muestra"), "Muestra (microdatos de la encuesta)")
  # Ningun año pertenece a dos programas
  expect_identical(anyDuplicated(unlist(lapply(programas, function(p) p$years))), 0L)
})

test_that("poblacion_archivos() devuelve una tabla vacia si la base no existe o no esta en el registro", {
  columnas <- c("clave", "formato", "tamano", "bytes", "url")
  for (caso in list(c(1990, "ageb"), c(2000, "localidades"), c(2010, "viviendas"))) {
    archivos <- poblacion_archivos("censo", as.integer(caso[1]), caso[2])
    expect_identical(nrow(archivos), 0L, info = paste(caso, collapse = " "))
    expect_named(archivos, columnas)
  }
  expect_named(poblacion_archivos("censo", 2010L, "iter"), columnas)
})

test_that("registro_url() arma la direccion con la base del sitio", {
  filas <- data.frame(
    ruta = c("/programas/ccpv/2010/microdatos/iter/00_nacional_2010_iter", ""),
    sufijo = c("_dbf.zip", ""),
    url = c("https://www.inegi.org.mx/otra", "http://www.inegi.org.mx/app/cuadro"),
    stringsAsFactors = FALSE
  )
  withr::local_options(importinegi.base = "http://127.0.0.1:9999")
  expect_identical(registro_url(filas), c(
    "http://127.0.0.1:9999/contenidos/programas/ccpv/2010/microdatos/iter/00_nacional_2010_iter_dbf.zip",
    "http://www.inegi.org.mx/app/cuadro"
  ))
})

test_that("el registro del paquete tiene las columnas que usan las funciones", {
  registro <- registro_inegi()
  expect_true(all(c("proyecto", "edicion", "titulo", "descripcion", "formato", "sufijo",
                    "tamano", "bytes", "ruta", "url") %in% names(registro)))
  expect_gt(nrow(registro), 1000)
  expect_match(registro_generado(), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  withr::local_options(importinegi.registro = registro[1:3, ])
  expect_identical(nrow(registro_inegi()), 3L)
})

test_that("tamano_texto() resume los bytes de una descarga", {
  expect_identical(tamano_texto(500), "1 KB")
  expect_identical(tamano_texto(300 * 1024), "300 KB")
  expect_identical(tamano_texto(2.5 * 1024^2), "2.5 MB")
  expect_identical(tamano_texto(c(200, 145.4) * 1024^2), "345 MB")
  expect_identical(tamano_texto(c(1.5 * 1024^3, NA)), "1.5 GB")
})

test_that("sumar_bytes() suma bytes enteros, para que el total no dependa de la plataforma", {
  expect_identical(sumar_bytes(c(1.4, 1.4, NA)), 2)
  expect_identical(sumar_bytes(numeric()), 0)
  # La muestra de la Encuesta Intercensal 2015 suma justo 813.95 MB: con los
  # bytes con decimales, el redondeo a un decimal cambiaba entre plataformas
  bytes <- poblacion_archivos("intercensal", 2015L, "muestra")$bytes
  expect_identical(sumar_bytes(bytes) %% 1, 0)
  expect_identical(round(sumar_bytes(bytes) / 1024^2, 1), 813.9)
  expect_identical(tamano_texto(bytes), "814 MB")
})

test_that("leer_tabla() entrega en UTF-8 el texto de los DBF", {
  # iter_06: texto en Latin-1 sin pagina de codigos declarada
  carpeta <- inegi_descomprimir(muestra("censo/iter_06.zip"))
  withr::defer(unlink(carpeta, recursive = TRUE))
  archivo <- buscar_archivos(carpeta, extension = "dbf")
  expect_identical(dbf_codificacion(archivo), NA_character_)
  crudo <- foreign::read.dbf(archivo, as.is = TRUE)
  expect_false(all(validUTF8(crudo$NOM_LOC)))
  leido <- leer_tabla(archivo)
  expect_true(all(validUTF8(leido$NOM_LOC)))
  expect_identical(leido$NOM_LOC[5], "Ciudad de Armer\u00eda")
  expect_identical(leido$POBTOT, crudo$POBTOT)

  # localidades_06: el encabezado declara la pagina de codigos 850
  carpeta850 <- inegi_descomprimir(muestra("censo/localidades_06.zip"))
  withr::defer(unlink(carpeta850, recursive = TRUE))
  archivo850 <- buscar_archivos(carpeta850, extension = "dbf")
  expect_identical(dbf_codificacion(archivo850), "CP850")
  expect_identical(leer_tabla(archivo850)$NOM_LOC, c("El Para\u00edso", "Rinc\u00f3n de L\u00f3pez"))
})

test_that("poblacion_descomprimir() extrae solo la tabla pedida cuando puede decidir cual es", {
  zip <- muestra("intercensal/muestra2025_00.zip")
  carpeta <- poblacion_descomprimir(zip, "csv", poblacion_tablas("intercensal", 2025L, "muestra")[["migrantes"]])
  withr::defer(unlink(carpeta, recursive = TRUE))
  expect_identical(list.files(carpeta, recursive = TRUE), "Migrantes00.CSV")
  # El diccionario del zip no cuenta como candidato
  expect_identical(poblacion_tabla_en_lista(c("Personas01.CSV", "diccionario_personas01.csv"), "csv", "persona"),
                   "Personas01.CSV")
  # Con zip dentro del zip, o sin un candidato unico, se descomprime todo
  expect_identical(poblacion_tabla_en_lista(c("datgen95.dbf", "cpv95_12_dbf.zip"), "dbf", "^datgen"), NA_character_)
  expect_identical(poblacion_tabla_en_lista(c("Personas01.CSV", "Personas01_b.csv"), "csv", "persona"), NA_character_)
  expect_identical(poblacion_tabla_en_lista(character(), "csv"), NA_character_)
  # Se prefiere la tabla menos anidada y la de conjunto_de_datos
  expect_identical(poblacion_tabla_en_lista(c("x/TR_PERSONA01.CSV", "TR_PERSONA01.CSV"), "csv", "persona"),
                   "TR_PERSONA01.CSV")
  expect_identical(poblacion_tabla_en_lista(c("iter/conjunto_de_datos/a.csv", "iter/diccionario_de_datos/b.csv",
                                              "iter/metadatos/c.csv"), "csv"),
                   "iter/conjunto_de_datos/a.csv")
  todo <- poblacion_descomprimir(muestra("censo/muestra2000_06.zip"), "dbf", NULL)
  withr::defer(unlink(todo, recursive = TRUE))
  expect_gt(length(list.files(todo, recursive = TRUE)), 1L)
})

test_that("poblacion_archivos() reconoce las cuatro formas de describir los archivos del registro", {
  base <- registro_archivos[0, ]
  fila <- function(proyecto, edicion, titulo, ruta, formato = "csv", descripcion = "Descarga") {
    data.frame(proyecto = proyecto, edicion = edicion, id_api = 1L, id_archivo = "1", titulo = titulo,
               descripcion = descripcion, anio = edicion, formato = formato,
               sufijo = if (formato %in% c("xls", "xlsx")) paste0(".", formato) else paste0("_", formato, ".zip"),
               tamano = "1 MB", bytes = 1024^2, ruta = ruta, url = NA_character_, stringsAsFactors = FALSE)
  }
  registro <- rbind(
    base,
    fila("eic", 2015L, " México    ", "/programas/intercensal/2015/microdatos/eic2015_15"),
    fila("eic", 2015L, "Colima", "/programas/intercensal/2015/microdatos/eic2015_06"),
    fila("eic", 2015L, "Descriptor de archivos (FD)", "/programas/intercensal/2015/doc/eic2015_fd", "xls"),
    fila("eic", 2025L, "Encuesta Intercensal|Bases de datos|Colima", "/programas/eic/2025/microdatos/eic2025_micro_06"),
    fila("eic", 2025L, "Encuesta Intercensal|Documentación de la base de datos|Clasificaciones (CSV)",
         "/programas/eic/2025/microdatos/889463931966"),
    fila("eic", 2025L, "Principales resultados por localidad de 50 000 y más habitantes|Base de datos",
         "/programas/eic/2025/microdatos/eic2025_micro_105", "xlsx")
  )
  attr(registro, "generado") <- "2026-11-22"
  withr::local_options(importinegi.registro = registro)
  expect_identical(poblacion_archivos("intercensal", 2015L, "muestra")$clave, c("06", "15"))
  expect_identical(poblacion_archivos("intercensal", 2025L, "muestra")$clave, "06")
  resultados <- poblacion_archivos("intercensal", 2025L, "resultados")
  expect_identical(resultados$clave, "00")
  expect_identical(resultados$formato, "xlsx")
})
