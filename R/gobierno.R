# Censos nacionales de gobierno: motor compartido --------------------------------
#
# El INEGI publica cada censo nacional de gobierno por edicion, modulo y tema.
# Cada tema es un zip con varias tablas, un catalogo por variable codificada y
# un diccionario de datos. Este archivo reune lo que comparten las funciones de
# esos censos: cngmd() y cndhf().
#
# Los temas salen del registro de archivos del INEGI: el titulo de cada archivo
# es "Modulo|Tema". Se leen los archivos de datos abiertos (CSV); los DBF solo
# se usan en los modulos que el INEGI no publica como datos abiertos (agua
# potable y residuos solidos del censo municipal de 2011).
#
# Lo que cambia de un censo a otro esta en tres funciones: gobierno_programas()
# (siglas de los archivos y primera edicion con indice y diccionario en forma
# de tabla), gobierno_modulos() y gobierno_claves() (claves geograficas). Las
# demas reciben el censo en `programa`.

# Programas, modulos y temas ----------------------------------------------------

# Censos de gobierno que descarga el paquete: la funcion y su proyecto en el
# registro. `sufijo` reconoce las siglas del censo con la edicion al final del
# nombre de un archivo ("_cngmd2025", "_cndhf2021"). `documentado` es la primera
# edicion en que el INEGI publica el indice de tablas y el diccionario de datos
# como tablas; antes, el diccionario es un documento.
gobierno_programas <- function() {
  data.frame(
    programa = c("cngmd", "cndhf"),
    proyecto = c("cngmd", "cndhf"),
    sufijo = c("_cng[a-z]+[0-9]{3,4}$", "_cndhf[0-9]{4}$"),
    documentado = c(2021L, 2020L),
    stringsAsFactors = FALSE
  )
}

# Dato de un censo en gobierno_programas().
gobierno_dato <- function(programa, dato) {
  programas <- gobierno_programas()
  fila <- match(programa, programas$programa)
  if (is.na(fila)) {
    detener("No se reconoce el censo de gobierno \"", programa, "\".")
  }
  programas[[dato]][fila]
}

# Modulos de un censo. `patron` reconoce el modulo en el titulo que le da el
# INEGI, ya normalizado (minusculas, sin acentos y sin la numeracion inicial):
# los titulos cambian de redaccion entre ediciones ("1. Ayuntamiento",
# "Ayuntamientos y alcaldias").
gobierno_modulos <- function(programa) {
  switch(
    programa,
    cngmd = data.frame(
      modulo = c("ayuntamiento", "administracion", "catastro", "territorio", "seguridad",
                 "proteccion_civil", "justicia", "agua", "residuos"),
      patron = c("^ayuntamiento", "^administracion publica", "^catastro",
                 "^planeacion y gestion territorial", "^seguridad publica", "^proteccion civil",
                 "^justicia", "^agua potable", "^residuos solidos"),
      stringsAsFactors = FALSE
    ),
    # El segundo modulo cambio de titulo: "Ejercicio de la funcion en materia
    # de proteccion de derechos humanos" (2017 a 2019), "Proteccion de derechos
    # humanos" (2020) y "Capacitacion, difusion, defensa y proteccion de los
    # derechos humanos" (desde 2021).
    cndhf = data.frame(
      modulo = c("estructura", "proteccion"),
      patron = c("^estructura organizacional y recursos",
                 "^(ejercicio de la funcion|proteccion de derechos humanos|capacitacion)"),
      stringsAsFactors = FALSE
    ),
    detener("No se reconoce el censo de gobierno \"", programa, "\".")
  )
}

# Nombre del archivo de un tema sin lo que no lo distingue: el prefijo
# "conjunto_de_datos_" y las siglas del censo con la edicion. Es el nombre corto
# que muestra bases_importinegi().
gobierno_identificador <- function(archivo, programa = "cngmd") {
  nombre <- tolower(archivo)
  nombre <- sub("^conjunto_de_datos_", "", nombre)
  sub(gobierno_dato(programa, "sufijo"), "", nombre)
}

# Temas de un censo, uno por fila, con su modulo, su titulo, el nombre de su
# archivo y su direccion. En cada edicion y modulo se usan los archivos CSV; si
# el modulo no los tiene, los DBF.
gobierno_temas <- function(programa, year = NULL) {
  programas <- gobierno_programas()
  proyecto <- programas$proyecto[match(programa, programas$programa)]
  modulos <- gobierno_modulos(programa)
  registro <- registro_inegi()
  filas <- registro[registro$proyecto == proyecto & registro$formato %in% c("csv", "dbf"), ]
  if (!is.null(year)) {
    filas <- filas[filas$edicion %in% year, ]
  }

  partes <- strsplit(filas$titulo, "|", fixed = TRUE)
  primero <- vapply(partes, function(p) p[1], character(1))
  tema <- vapply(partes, function(p) {
    if (length(p) > 1) paste(p[-1], collapse = "|") else sub("^[0-9]+\\.\\s*", "", p[1])
  }, character(1))
  clave <- sub("^[0-9]+ ", "", normalizar_texto(primero))
  modulo <- rep(NA_character_, nrow(filas))
  for (i in seq_len(nrow(modulos))) {
    modulo[is.na(modulo) & grepl(modulos$patron[i], clave)] <- modulos$modulo[i]
  }

  archivo <- basename(filas$ruta)
  temas <- data.frame(
    year = filas$edicion,
    modulo = modulo,
    tema = trimws(tema),
    base = gobierno_identificador(archivo, programa),
    archivo = archivo,
    formato = filas$formato,
    bytes = filas$bytes,
    tamano = filas$tamano,
    url = registro_url(filas),
    stringsAsFactors = FALSE
  )
  temas <- temas[!is.na(temas$modulo), ]
  grupo <- paste(temas$year, temas$modulo)
  temas <- temas[temas$formato == "csv" | !grupo %in% grupo[temas$formato == "csv"], ]
  temas <- temas[order(temas$year, match(temas$modulo, modulos$modulo), temas$base, method = "radix"), ]
  rownames(temas) <- NULL
  temas
}

# Temas de todos los censos de gobierno con la forma de bases_importinegi().
gobierno_bases <- function() {
  programas <- gobierno_programas()
  tablas <- lapply(programas$programa, function(programa) {
    temas <- gobierno_temas(programa)
    data.frame(
      proyecto = rep(programa, nrow(temas)),
      year = temas$year,
      modulo = temas$modulo,
      base = temas$base,
      descripcion = temas$tema,
      tablas = rep(NA_character_, nrow(temas)),
      cobertura = rep("nacional", nrow(temas)),
      formato = temas$formato,
      tamano_mb = round(round(temas$bytes) / 1024^2, 1),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, tablas)
}

# Validacion de los argumentos ---------------------------------------------------

gobierno_validar_year <- function(programa, year) {
  propios <- sort(unique(gobierno_temas(programa)$year))
  disponibles <- paste0("A\u00f1os disponibles en ", programa, "(): ", enumerar(propios), ".")
  if (is.null(year) || length(year) != 1 || is.na(year)) {
    detener("Falta indicar el a\u00f1o en `year` (un solo valor). ", disponibles)
  }
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero) || !grepl("^[0-9]{4}$", trimws(as.character(year)))) {
    detener("`year = ", comillas(year), "` no es un a\u00f1o. ", disponibles)
  }
  if (!numero %in% propios) {
    detener("`year = ", numero, "` no est\u00e1 disponible. ", disponibles)
  }
  numero
}

# El modulo se acepta con mayusculas, acentos o espacios ("Proteccion civil").
gobierno_validar_modulo <- function(programa, year, modulo) {
  todos <- gobierno_modulos(programa)$modulo
  temas <- gobierno_temas(programa, year)
  propios <- intersect(todos, temas$modulo)
  disponibles <- paste0("M\u00f3dulos de ", programa, "(", year, "): ", enumerar(propios), ".")
  if (is.null(modulo) || length(modulo) != 1 || is.na(modulo) || !is.character(modulo)) {
    detener("Falta indicar el m\u00f3dulo en `modulo` (un solo valor). ", disponibles)
  }
  buscado <- gsub(" ", "_", normalizar_texto(modulo), fixed = TRUE)
  if (!buscado %in% todos) {
    detener("`modulo = ", comillas(modulo), "` no es una opci\u00f3n v\u00e1lida. ", disponibles)
  }
  if (!buscado %in% propios) {
    detener(
      "El m\u00f3dulo \"", buscado, "\" no existe en la edici\u00f3n ", year, ". ", disponibles,
      " Consulta los temas de cada edici\u00f3n con bases_importinegi(\"", programa, "\")."
    )
  }
  buscado
}

# Temas de un modulo como texto para los mensajes: nombre del archivo y titulo.
gobierno_listar_temas <- function(temas) {
  paste0("\n", paste0("  ", temas$base, ": ", temas$tema, collapse = "\n"))
}

# Elige el tema de un modulo. `tema` es una parte del titulo que le da el INEGI
# o del nombre de su archivo, sin distinguir mayusculas ni acentos. Si el texto
# es igual al titulo o al nombre del archivo de un tema, se elige ese tema; si
# no, debe coincidir con un solo tema del modulo.
gobierno_elegir_tema <- function(programa, year, modulo, tema) {
  temas <- gobierno_temas(programa, year)
  temas <- temas[temas$modulo == modulo, ]
  lugar <- paste0("del m\u00f3dulo \"", modulo, "\" de ", year)
  if (is.null(tema)) {
    if (nrow(temas) == 1) {
      return(temas)
    }
    detener(
      "Falta indicar el tema en `tema`: una parte de su t\u00edtulo o del nombre de su archivo. ",
      "Temas ", lugar, " (nombre del archivo y t\u00edtulo):", gobierno_listar_temas(temas)
    )
  }
  if (length(tema) != 1 || is.na(tema) || !is.character(tema) || !nzchar(normalizar_texto(tema))) {
    detener("`tema` debe ser un texto: una parte del t\u00edtulo del tema o del nombre de su archivo.")
  }
  buscado <- normalizar_texto(tema)
  titulo <- normalizar_texto(temas$tema)
  corto <- normalizar_texto(temas$base)
  largo <- normalizar_texto(temas$archivo)

  exactos <- which(titulo == buscado | corto == buscado | largo == buscado)
  if (length(exactos) == 1) {
    return(temas[exactos, ])
  }
  parciales <- if (length(exactos) > 1) {
    exactos
  } else {
    which(grepl(buscado, titulo, fixed = TRUE) | grepl(buscado, corto, fixed = TRUE) |
            grepl(buscado, largo, fixed = TRUE))
  }
  if (length(parciales) == 1) {
    return(temas[parciales, ])
  }
  if (length(parciales) > 1) {
    detener(
      "`tema = ", comillas(tema), "` coincide con ", length(parciales), " temas ", lugar,
      ". Indica uno con m\u00e1s texto o con el nombre de su archivo:",
      gobierno_listar_temas(temas[parciales, ])
    )
  }
  detener(
    "`tema = ", comillas(tema), "` no coincide con ning\u00fan tema ", lugar,
    ". Temas disponibles (nombre del archivo y t\u00edtulo):", gobierno_listar_temas(temas)
  )
}

# `tabla` puede ser NULL (todas), el nombre de una o varias tablas o uno de los
# valores que devuelven la documentacion del tema.
gobierno_especiales <- function() {
  c("indice", "catalogos", "diccionario")
}

gobierno_validar_tabla <- function(tabla, programa = "cngmd") {
  if (is.null(tabla)) {
    return(NULL)
  }
  if (!is.character(tabla) || length(tabla) == 0 || anyNA(tabla) || !all(nzchar(trimws(tabla)))) {
    detener(
      "`tabla` debe ser el nombre de una o varias tablas del tema, o bien ",
      enumerar(gobierno_especiales()), "."
    )
  }
  tabla <- unique(tolower(trimws(tabla)))
  especial <- gsub(" ", "_", normalizar_texto(tabla), fixed = TRUE) %in% gobierno_especiales()
  if (any(especial)) {
    if (length(tabla) > 1) {
      detener(
        "`tabla = ", comillas(tabla[especial][1]), "` se pide sola: devuelve la documentaci\u00f3n ",
        "del tema y no se combina con otras tablas."
      )
    }
    return(normalizar_texto(tabla))
  }
  unique(gobierno_nombre_tabla(tabla, programa))
}

# Claves de las entidades pedidas en `estado`, o NULL para todo el pais.
gobierno_entidades <- function(estado) {
  if (is.null(estado)) {
    return(NULL)
  }
  if (length(estado) == 0 || anyNA(estado)) {
    detener("`estado` debe ser una o varias entidades (nombre, abreviatura o clave) o \"nacional\".")
  }
  claves <- unique(vapply(as.list(estado), estado_clave, character(1)))
  if ("00" %in% claves) NULL else claves
}

# Contenido del zip de un tema -----------------------------------------------------

# Nombre de una tabla: el del archivo, en minusculas, sin extension y sin las
# siglas del censo con la edicion ("m1s1p3_cngmd2025.csv" pasa a ser "m1s1p3").
gobierno_nombre_tabla <- function(archivo, programa = "cngmd") {
  nombre <- tolower(sub("\\.(csv|dbf)$", "", archivo, ignore.case = TRUE))
  sub(gobierno_dato(programa, "sufijo"), "", nombre)
}

# Clasifica los archivos del zip de un tema. Los zip del INEGI no tienen todos
# la misma forma: la carpeta de las tablas puede estar dentro de otra, llamarse
# "conjunto_de datos" o "Bases_Datos", y 45 zip de datos abiertos usan "\" como
# separador de carpetas. `original` es el nombre con que el zip guarda el
# archivo y `ruta`, el mismo nombre con "/" como separador. Las bitacoras de
# cambios que algunos temas traen en la carpeta de las tablas no son tablas del
# censo ("m2s8p1_cndhf2021_bitacora_de_cambios.csv").
gobierno_contenido <- function(zip, formato, programa = "cngmd") {
  lista <- tryCatch(utils::unzip(zip, list = TRUE), error = function(e) NULL)
  if (is.null(lista) || nrow(lista) == 0) {
    detener("No se pudo leer el contenido de ", basename(zip), ".")
  }
  original <- lista$Name
  ruta <- gobierno_ruta(original)
  carpeta <- tolower(dirname(ruta))
  archivo <- basename(ruta)
  extension <- tolower(tools::file_ext(archivo))
  en <- function(patron) grepl(paste0("(^|/)", patron, "$"), carpeta)

  catalogo <- en("cat.{1,2}logos") & extension == formato
  diccionario <- en("diccionario[^/]*") & extension == "csv"
  datos <- (en("conjunto_de[_ ]datos") | en("bases?[ _](de[ _])?datos")) & extension == formato
  if (!any(datos)) {
    ajenas <- en("(metadatos|modelo[^/]*|descripcion[^/]*)")
    datos <- extension == formato & !catalogo & !diccionario & !ajenas
  }
  datos <- datos & !grepl("bitacora_de_cambios", tolower(archivo), fixed = TRUE)
  tipo <- rep("otro", length(ruta))
  tipo[diccionario] <- "diccionario"
  tipo[catalogo] <- "catalogo"
  tipo[datos] <- "datos"
  tipo[datos & grepl("^0_indice", tolower(archivo))] <- "indice"
  tipo[grepl("/$", ruta)] <- "carpeta"

  tabla <- gobierno_nombre_tabla(archivo, programa)
  for (clase in c("datos", "catalogo")) {
    tabla[tipo == clase] <- make.unique(tabla[tipo == clase], sep = "_")
  }
  data.frame(original = original, ruta = ruta, tipo = tipo, tabla = tabla, stringsAsFactors = FALSE)
}

# Orden de los nombres de las tablas con los numeros en su orden natural
# ("m1s1p3" antes que "m1s1p12a") y sin depender del idioma del equipo.
gobierno_orden_natural <- function(nombres) {
  clave <- nombres
  numeros <- gregexpr("[0-9]+", clave)
  regmatches(clave, numeros) <- lapply(regmatches(clave, numeros), function(n) {
    formatC(as.numeric(n), width = 8, flag = "0", format = "d")
  })
  order(clave, method = "radix")
}

# Nombre de un archivo dentro de un zip con "/" como separador y en UTF-8.
gobierno_ruta <- function(nombres) {
  malos <- !validUTF8(nombres)
  nombres[malos] <- iconv(nombres[malos], from = "latin1", to = "UTF-8")
  gsub("\\", "/", nombres, fixed = TRUE)
}

# Extrae del zip los archivos indicados (filas de gobierno_contenido()) y
# devuelve la ruta de cada uno. En Linux y macOS, los archivos que el zip guarda
# con "\" quedan con ese caracter en el nombre, sin carpeta; en Windows quedan
# dentro de su carpeta. Por eso se buscan por su nombre con "/".
gobierno_extraer <- function(zip, filas) {
  carpeta <- inegi_descomprimir(zip, anidados = FALSE, archivos = filas$original)
  extraidos <- list.files(carpeta, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  posicion <- match(filas$ruta, gobierno_ruta(extraidos))
  if (anyNA(posicion)) {
    unlink(carpeta, recursive = TRUE)
    detener(
      "No se pudo extraer de ", basename(zip), ": ",
      enumerar(basename(filas$ruta[is.na(posicion)]), maximo = 10), "."
    )
  }
  list(carpeta = carpeta, archivos = file.path(carpeta, extraidos[posicion]))
}

# Lectura y preparacion de las tablas -----------------------------------------------

# Claves geograficas de los censos de gobierno, con el ancho que usa el INEGI.
# El nombre de la clave del municipio cambia con la edicion y con la tabla:
# ubic_geo, ubi_geo, ub_geo, ubicageo_c, ubicageo_f o cvegeo.
#
# El censo de derechos humanos federal no tiene clave del informante (es uno
# solo, la CNDH), pero algunas de sus tablas desglosan por municipio. De 2020 a
# 2022 traen la clave del municipio en una columna (ubicageo_b, ubicageo_c,
# ubicageo_e), que se deja en 5 digitos. La clave de la entidad viene aparte
# desde 2022 (enticve1) y la del municipio, desde 2023 (municve1); se dejan en
# 2 y en 3 digitos. Las columnas de entidad con catalogo (entidad_c, entfede_c)
# no son claves: se conservan como numero, igual que en su catalogo.
gobierno_claves <- function(programa = "cngmd") {
  switch(
    programa,
    cngmd = list(
      municipio = list(patron = "^(ub(ic?a?)?_?geo[a-z_]*|cvegeo)$", ancho = 5),
      entidad = list(patron = "^(cve_ent|cve_entidad|entidad)$", ancho = 2),
      mun = list(patron = "^cve_mun$", ancho = 3)
    ),
    cndhf = list(
      municipio = list(patron = "^ubicageo_[a-z]+$", ancho = 5),
      entidad = list(patron = "^enticve[0-9]*$", ancho = 2),
      mun = list(patron = "^municve[0-9]*$", ancho = 3)
    ),
    detener("No se reconoce el censo de gobierno \"", programa, "\".")
  )
}

# Columnas de una tabla que son claves geograficas, por tipo.
gobierno_columnas_clave <- function(datos, programa = "cngmd") {
  lapply(gobierno_claves(programa), function(clave) {
    names(datos)[grepl(clave$patron, tolower(names(datos)))]
  })
}

# Completa con ceros a la izquierda los valores que son numeros ("1001" o 1001
# pasan a ser "01001"); los demas valores no cambian.
gobierno_rellenar <- function(valores, ancho) {
  texto <- if (is.numeric(valores)) {
    as.character(as.integer(round(valores)))
  } else {
    trimws(as.character(valores))
  }
  cortos <- !is.na(texto) & grepl("^[0-9]+$", texto) & nchar(texto) < ancho
  texto[cortos] <- paste0(strrep("0", ancho - nchar(texto[cortos])), texto[cortos])
  texto
}

# Prepara una tabla recien leida: la sigla "NA" (no aplica) pasa a ser NA; las
# claves geograficas quedan como texto con ceros a la izquierda, con el nombre
# que les da el INEGI, y las columnas que solo contienen numeros se convierten
# a numero. Las siglas "NSS", "ND" y "NP" se conservan, asi que las columnas
# que las contienen quedan como texto.
gobierno_preparar <- function(datos, programa = "cngmd") {
  datos <- limpiar_faltantes(datos, "NA")
  columnas <- gobierno_columnas_clave(datos, programa)
  claves <- gobierno_claves(programa)
  for (tipo in names(columnas)) {
    for (columna in columnas[[tipo]]) {
      datos[[columna]] <- gobierno_rellenar(datos[[columna]], claves[[tipo]]$ancho)
    }
  }
  convertir_numericas(datos, texto = unlist(columnas, use.names = FALSE))
}

# Lee una tabla y la prepara. El formato se indica en lugar de deducirlo del
# nombre del archivo: algunos zip guardan sus carpetas con nombres que no son
# texto valido en UTF-8.
gobierno_leer_tabla <- function(archivo, formato, programa = "cngmd") {
  datos <- if (identical(formato, "dbf")) {
    reparado <- gobierno_reparar_dbf(archivo)
    if (!identical(reparado, archivo)) {
      on.exit(unlink(reparado), add = TRUE)
    }
    leer_dbf(reparado)
  } else {
    leer_csv(archivo)
  }
  gobierno_preparar(datos, programa)
}

# Algunos DBF del censo municipal de 2011 declaran en su encabezado mas
# registros de los que contienen (50,122 en lugar de unos 2,450); al leerlos
# tal cual, las filas que faltan llegan vacias. Si el archivo es mas corto de lo
# que declara, devuelve la ruta de una copia con el numero real de registros;
# si no, la misma ruta.
gobierno_reparar_dbf <- function(archivo) {
  encabezado <- readBin(archivo, "raw", n = 12)
  if (length(encabezado) < 12) {
    return(archivo)
  }
  entero <- function(bytes) sum(as.numeric(bytes) * 256^(seq_along(bytes) - 1))
  declarados <- entero(encabezado[5:8])
  largo_encabezado <- entero(encabezado[9:10])
  largo_registro <- entero(encabezado[11:12])
  if (largo_registro == 0) {
    return(archivo)
  }
  reales <- floor((file.size(archivo) - largo_encabezado) / largo_registro)
  if (reales < 0 || reales >= declarados) {
    return(archivo)
  }
  copia <- tempfile("importinegi_", fileext = ".dbf")
  file.copy(archivo, copia)
  conexion <- file(copia, open = "r+b")
  on.exit(close(conexion))
  seek(conexion, where = 4, rw = "write")
  writeBin(as.integer(reales), conexion, size = 4, endian = "little")
  copia
}

# Clave de la entidad de cada fila: los dos primeros digitos de la clave del
# municipio o, si la tabla no la tiene, la clave de la entidad. NULL si la
# tabla no tiene ninguna de las dos.
gobierno_entidad_fila <- function(datos, programa = "cngmd") {
  columnas <- gobierno_columnas_clave(datos, programa)
  if (length(columnas$municipio) > 0) {
    return(substr(datos[[columnas$municipio[1]]], 1, 2))
  }
  if (length(columnas$entidad) > 0) {
    return(datos[[columnas$entidad[1]]])
  }
  NULL
}

# Conserva las filas de las entidades pedidas. Devuelve NULL si la tabla no
# tiene clave geografica.
gobierno_filtrar_entidades <- function(datos, entidades, programa = "cngmd") {
  entidad <- gobierno_entidad_fila(datos, programa)
  if (is.null(entidad)) {
    return(NULL)
  }
  datos <- datos[!is.na(entidad) & entidad %in% entidades, , drop = FALSE]
  rownames(datos) <- NULL
  datos
}

# Indice de las tablas de un tema: el nombre de cada tabla y su contenido. El
# INEGI incluye el indice en los zip desde la edicion 2021 (2020 en el censo de
# derechos humanos federal); en las anteriores, el contenido queda como NA.
gobierno_indice <- function(tablas, archivo = NA_character_, programa = "cngmd") {
  indice <- data.frame(tabla = tablas, contenido = rep(NA_character_, length(tablas)),
                       stringsAsFactors = FALSE)
  if (is.na(archivo)) {
    return(indice)
  }
  crudo <- tryCatch(leer_csv(archivo), error = function(e) NULL)
  nombres <- tolower(names(crudo))
  if (is.null(crudo) || !all(c("archivo", "contenido") %in% nombres)) {
    return(indice)
  }
  descritas <- gobierno_nombre_tabla(crudo[[match("archivo", nombres)]], programa)
  indice$contenido <- crudo[[match("contenido", nombres)]][match(indice$tabla, descritas)]
  indice
}

# Tabla a la que corresponde un archivo del diccionario de datos: el nombre de
# la tabla forma parte del nombre del archivo.
gobierno_tabla_diccionario <- function(archivo, tablas, programa = "cngmd") {
  nombre <- sub("^(diccionario(_de)?_datos|dd|fd)_", "", gobierno_nombre_tabla(archivo, programa))
  if (nombre %in% tablas) {
    return(nombre)
  }
  contenidas <- tablas[vapply(tablas, function(tabla) {
    grepl(paste0("(^|[^a-z0-9])", tabla, "($|[^a-z0-9])"), nombre)
  }, logical(1))]
  if (length(contenidas) == 0) nombre else contenidas[which.max(nchar(contenidas))]
}

# Diccionario de datos de un tema en una sola tabla. Desde la edicion 2021 el
# INEGI publica un CSV por tabla, con una fila por variable; antes, el
# diccionario era un documento.
gobierno_diccionario <- function(archivos, tablas, programa = "cngmd") {
  partes <- lapply(archivos, function(archivo) {
    crudo <- tryCatch(leer_csv(archivo), error = function(e) NULL)
    if (is.null(crudo) || nrow(crudo) == 0) {
      return(NULL)
    }
    # Los encabezados cambian entre temas (con o sin acentos, con "_" o con
    # espacios): se dejan en mayusculas, sin acentos ni espacios.
    # En algunos archivos de 2025 los acentos del encabezado llegan mal
    # codificados; esas columnas se reconocen por el resto de su nombre.
    nombres <- toupper(gsub(" ", "_", normalizar_texto(names(crudo)), fixed = TRUE))
    nombres[grepl("^DESCRIP", nombres)] <- "DESCRIPCION"
    nombres[grepl("^C.*DIGO.*V.*LIDO$", nombres)] <- "CODIGO_VALIDO"
    names(crudo) <- nombres
    if (!all(c("COLUMNA", "DESCRIPCION") %in% names(crudo))) {
      return(NULL)
    }
    data.frame(tabla = gobierno_tabla_diccionario(basename(gobierno_ruta(archivo)), tablas, programa), crudo,
               stringsAsFactors = FALSE, check.names = FALSE)
  })
  partes <- partes[!vapply(partes, is.null, logical(1))]
  if (length(partes) == 0) {
    return(NULL)
  }
  columnas <- unique(unlist(lapply(partes, names)))
  partes <- lapply(partes, function(parte) {
    for (columna in setdiff(columnas, names(parte))) {
      parte[[columna]] <- NA_character_
    }
    parte[, columnas, drop = FALSE]
  })
  diccionario <- do.call(rbind, partes)
  rownames(diccionario) <- NULL
  diccionario
}

# Descarga y lectura -----------------------------------------------------------------

# Descarga el archivo de un tema y devuelve lo que pide `tabla`. `estado` solo
# lo usan los censos con un informante por municipio o por entidad.
gobierno_obtener <- function(programa, year, modulo, tema, tabla, estado = NULL) {
  year <- gobierno_validar_year(programa, year)
  modulo <- gobierno_validar_modulo(programa, year, modulo)
  elegido <- gobierno_elegir_tema(programa, year, modulo, tema)
  tabla <- gobierno_validar_tabla(tabla, programa)
  entidades <- gobierno_entidades(estado)
  etiqueta <- paste0("\"", elegido$tema, "\" (", year, ")")

  zip <- inegi_descargar(elegido$url, tamano = elegido$tamano)
  if (!isTRUE(getOption("importinegi.cache", FALSE))) {
    on.exit(unlink(zip), add = TRUE)
  }
  contenido <- gobierno_contenido(zip, elegido$formato, programa)
  datos <- contenido[contenido$tipo == "datos", ]
  datos <- datos[gobierno_orden_natural(datos$tabla), ]
  if (nrow(datos) == 0) {
    detener(
      "El archivo del tema ", etiqueta, " no contiene tablas en formato ", elegido$formato,
      ". Contiene: ", enumerar(basename(contenido$ruta[contenido$tipo != "carpeta"]), maximo = 15), "."
    )
  }
  carpetas <- character()
  on.exit(unlink(carpetas, recursive = TRUE), add = TRUE)
  extraer <- function(filas) {
    extraido <- gobierno_extraer(zip, filas)
    carpetas <<- c(carpetas, extraido$carpeta)
    extraido$archivos
  }
  con_fuente <- function(resultado) {
    attr(resultado, "fuente") <- elegido$url
    resultado
  }
  indice_del_tema <- function() {
    fila <- contenido[contenido$tipo == "indice", ]
    gobierno_indice(datos$tabla, if (nrow(fila) > 0) extraer(fila[1, ]) else NA_character_, programa)
  }

  if (identical(tabla, "indice")) {
    return(con_fuente(indice_del_tema()))
  }
  if (identical(tabla, "catalogos")) {
    filas <- contenido[contenido$tipo == "catalogo", ]
    filas <- filas[gobierno_orden_natural(filas$tabla), ]
    if (nrow(filas) == 0) {
      detener("El archivo del tema ", etiqueta, " no incluye cat\u00e1logos.")
    }
    catalogos <- lapply(extraer(filas), gobierno_leer_tabla, formato = elegido$formato, programa = programa)
    names(catalogos) <- filas$tabla
    return(con_fuente(catalogos))
  }
  if (identical(tabla, "diccionario")) {
    filas <- contenido[contenido$tipo == "diccionario", ]
    filas <- filas[gobierno_orden_natural(basename(filas$ruta)), ]
    diccionario <- if (nrow(filas) > 0) gobierno_diccionario(extraer(filas), datos$tabla, programa) else NULL
    if (is.null(diccionario)) {
      detener(
        "El diccionario de datos del tema ", etiqueta, " no es una tabla: el INEGI lo publica ",
        "como tabla desde la edici\u00f3n ", gobierno_dato(programa, "documentado"),
        ". En las ediciones anteriores es un documento que ",
        "viene dentro del archivo del tema: ", elegido$url
      )
    }
    return(con_fuente(diccionario))
  }

  if (!is.null(tabla)) {
    ajenas <- setdiff(tabla, datos$tabla)
    if (length(ajenas) > 0) {
      detener(
        "`tabla = ", enumerar(ajenas), "` no es una tabla del tema ", etiqueta, ". Tablas: ",
        enumerar(datos$tabla, maximo = 60), ". Con tabla = \"indice\" se obtiene el contenido de cada una."
      )
    }
    datos <- datos[match(tabla, datos$tabla), ]
  }
  archivos <- extraer(datos)
  sin_clave <- character()
  tablas <- lapply(seq_len(nrow(datos)), function(i) {
    leida <- gobierno_leer_tabla(archivos[i], elegido$formato, programa)
    if (!is.null(entidades)) {
      filtrada <- gobierno_filtrar_entidades(leida, entidades, programa)
      if (is.null(filtrada)) {
        sin_clave <<- c(sin_clave, datos$tabla[i])
      } else {
        leida <- filtrada
      }
    }
    con_fuente(leida)
  })
  names(tablas) <- datos$tabla
  if (length(sin_clave) > 0) {
    message(
      "Sin clave geogr\u00e1fica, se devuelve completa: ", enumerar(sin_clave, maximo = 20),
      ". `estado` no puede elegir sus filas."
    )
  }
  if (!is.null(tabla) && length(tabla) == 1) {
    return(tablas[[1]])
  }
  attr(tablas, "indice") <- indice_del_tema()
  con_fuente(tablas)
}
