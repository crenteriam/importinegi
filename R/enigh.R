# Encuesta Nacional de Ingresos y Gastos de los Hogares: motor compartido ----------
#
# El INEGI publica la ENIGH en tres series: la tradicional (1984 a 2014), la
# Nueva construccion (2008 a 2014) y la Nueva serie (desde 2016). Cada edicion
# se publica en varias tablas, todas nacionales. Este archivo reune lo que
# comparten las funciones de la ENIGH: enigh_tradicional() y enigh_nc().
#
# Las direcciones salen del registro de archivos del INEGI. En la serie
# tradicional no hay una tabla por archivo: varias tablas pueden venir juntas
# en un zip (las erogaciones dentro del zip de gastos, los trabajos dentro del
# de poblacion) y el nombre del DBF dentro del zip cambia con la edicion
# ("hogares.dbf", "POBLA84.dbf", "Tra_Hogares_2006_concil_2010_DBF/Hogares.dbf").
# En la Nueva construccion cada tabla viene en su propio zip. Por eso cada
# tabla se ubica con dos expresiones: una para el archivo del registro y otra
# para la tabla dentro del zip.
#
# Las tablas se leen en DBF, con los tipos que declara el INEGI. La excepcion
# es la tabla de negocios no agropecuarios de la Nueva construccion de 2010,
# que se lee en CSV (ver enigh_tablas() y enigh_tipos_csv()).

# Series y tablas -------------------------------------------------------------------

# Series de la ENIGH que descarga el paquete: la funcion, su proyecto en el
# registro y el nombre de la serie para los mensajes.
enigh_programas <- function() {
  list(
    enigh_tradicional = list(
      proyecto = "enigh_trad",
      nombre = "Encuesta Nacional de Ingresos y Gastos de los Hogares",
      etiqueta = "serie tradicional",
      serie = "la serie tradicional de la ENIGH"
    ),
    enigh_nc = list(
      proyecto = "enigh_nc",
      nombre = "Encuesta Nacional de Ingresos y Gastos de los Hogares",
      etiqueta = "Nueva construcci\u00f3n",
      serie = "la Nueva construcci\u00f3n de la ENIGH"
    )
  )
}

# Tablas de una serie, una fila por tabla y edicion.
#   archivo  Expresion regular que reconoce el archivo del INEGI en el que viene
#            la tabla, por su nombre corto (ver enigh_nombre_archivo()).
#   patron   Expresion regular que reconoce la tabla dentro del zip, por el
#            nombre de su archivo en minusculas y sin extension.
#   formato  Formato en el que se lee la tabla: "dbf" o "csv".
# Los nombres de las tablas son los mismos en todas las series. Siguen los que
# el INEGI usa en la Nueva construccion, salvo "agro" y "noagro".
enigh_tablas <- function(programa) {
  fila <- function(tabla, years, archivo, patron, formato = "dbf") {
    data.frame(tabla = tabla, year = as.integer(years), archivo = archivo, patron = patron,
               formato = formato, stringsAsFactors = FALSE)
  }
  switch(
    programa,
    enigh_tradicional = {
      todas <- c(1984, 1989, 1992, 1994, 1996, 1998, 2000, 2002, 2004, 2005, 2006, 2008, 2010, 2012, 2014)
      hasta_2010 <- todas[todas <= 2010]
      rbind(
        fila("concentrado", todas, "^concen(trado)?$", "conce"),
        fila("viviendas", c(2012, 2014), "^vivi(endas)?$", "vivi"),
        fila("hogares", todas, "^hogares$", "hogares"),
        fila("poblacion", todas, "^poblacion$", "pobla"),
        # Hasta 2010 los trabajos vienen en el zip de poblacion
        fila("trabajos", c(2008, 2010), "^poblacion$", "trabajos"),
        fila("trabajos", c(2012, 2014), "^trabajos$", "trabajos"),
        fila("ingresos", todas, "^ingresos$", "ingresos"),
        # El zip de gastos trae una tabla hasta 1992, dos desde 1994 (gastos y
        # erogaciones), cuatro en 2008 y seis en 2010
        fila("gastos", hasta_2010, "^gastos$", "^gastos[0-9]*$"),
        fila("gastohogar", c(2012, 2014), "^gastohogar$", "gastohogar"),
        fila("gastopersona", c(2012, 2014), "^gastopersona$", "gastopersona"),
        fila("gastodiario", c(2008, 2010), "^gastos$", "^g_diario$"),
        fila("gastoeducacion", c(2008, 2010), "^gastos$", "^g_educa$"),
        fila("gastorecibo", 2010, "^gastos$", "^g_recibo$"),
        fila("gastocosto", 2010, "^gastos$", "^g_costo$"),
        fila("gastotarjetas", c(2008, 2010), "^tarjetas$", "tarje"),
        fila("gastotarjetas", c(2012, 2014), "^gastotarjetas$", "gastotarjetas"),
        fila("erogaciones", hasta_2010[hasta_2010 >= 1994], "^gastos$", "^eroga"),
        fila("erogaciones", c(2012, 2014), "^erogaciones$", "eroga"),
        fila("nomonetario", hasta_2010, "^nomon(etario)?$", "nomon"),
        # En 2008 y 2010 los negocios del hogar vienen juntos en un zip
        fila("agro", c(2008, 2010), "^negocios$", "^agro$"),
        fila("agro", c(2012, 2014), "^agropecuario$", "agropecuario"),
        fila("noagro", c(2008, 2010), "^negocios$", "^noagro$"),
        fila("noagro", c(2012, 2014), "^noagropecuario$", "noagropecuario")
      )
    },
    enigh_nc = {
      # Una tabla por zip. Las tablas quedan en el mismo orden que en la serie
      # tradicional.
      todas <- c(2008, 2010, 2012, 2014)
      hasta_2010 <- c(2008, 2010)
      desde_2012 <- c(2012, 2014)
      rbind(
        fila("concentrado", todas, "^concentrado$", "_concentrado_"),
        fila("viviendas", desde_2012, "^vivi(endas)?$", "_vivi"),
        fila("hogares", todas, "^hogares$", "_hogares_"),
        fila("poblacion", todas, "^poblacion$", "_poblacion_"),
        fila("trabajos", todas, "^trabajos$", "_trabajos_"),
        fila("ingresos", todas, "^ingresos$", "_ingresos_"),
        fila("gastos", hasta_2010, "^gastos$", "_gastos_"),
        fila("gastohogar", desde_2012, "^gastohogar$", "_gastohogar_"),
        fila("gastopersona", desde_2012, "^gastopersona$", "_gastopersona_"),
        fila("gastodiario", hasta_2010, "^gastodiario$", "_gastodiario_"),
        fila("gastoeducacion", hasta_2010, "^gastoeducacion$", "_gastoeducacion_"),
        fila("gastorecibo", 2010, "^gastorecibo$", "_gastorecibo_"),
        fila("gastocosto", 2010, "^gastocosto$", "_gastocosto_"),
        fila("gastotarjetas", todas, "^gastotarjetas$", "_gastotarjetas_"),
        fila("erogaciones", todas, "^erogaciones$", "_erogaciones_"),
        fila("nomonetario", hasta_2010, "^nomonetario$", "_nomonetario_"),
        fila("agro", todas, "^agropecuario$", "_agropecuario_"),
        fila("noagro", c(2008, 2012, 2014), "^noagropecuario$", "_noagropecuario_"),
        # El DBF que el INEGI publica para esta tabla en 2010 lleva su nombre,
        # pero su contenido es una copia de la tabla agropecuaria (los mismos
        # 4,067 registros y 47 columnas, byte por byte; revisado el 23 de
        # noviembre de 2026). La tabla correcta, de 7,600 filas y 97 columnas,
        # esta en el CSV, que trae un solo archivo: "noagro.csv".
        fila("noagro", 2010, "^noagropecuario$", "^noagro$", formato = "csv")
      )
    },
    detener("No se reconoce la serie de la ENIGH \"", programa, "\".")
  )
}

# Nombre corto de un archivo de la ENIGH: el de su ruta en el registro, en
# minusculas, sin el prefijo de la serie ("Tra_", "NCV_") ni el sufijo que
# llevan las ediciones de 2006 a 2014 ("_2006_concil_2010").
enigh_nombre_archivo <- function(ruta) {
  nombre <- tolower(basename(ruta))
  nombre <- sub("^(tra|ncv)_", "", nombre)
  sub("_[0-9]{4}_concil_[0-9]{4}$", "", nombre)
}

# Tablas que se pueden descargar, una fila por tabla y edicion, con el nombre
# corto, la direccion y el tamano del archivo del INEGI en el que viene cada
# una, en el formato en el que se lee. Solo incluye las tablas cuyo archivo
# esta en el registro. Las tablas de cada edicion quedan en el orden de
# enigh_tablas().
enigh_archivos <- function(programa, year = NULL) {
  tablas <- enigh_tablas(programa)
  registro <- registro_inegi()
  propio <- registro$proyecto == enigh_programas()[[programa]]$proyecto & registro$formato %in% tablas$formato
  registro <- registro[propio, , drop = FALSE]
  if (!is.null(year)) {
    tablas <- tablas[tablas$year %in% year, , drop = FALSE]
    registro <- registro[registro$edicion %in% year, , drop = FALSE]
  }
  nombre <- enigh_nombre_archivo(registro$ruta)
  posicion <- vapply(seq_len(nrow(tablas)), function(i) {
    candidatos <- which(
      registro$edicion == tablas$year[i] & registro$formato == tablas$formato[i] & grepl(tablas$archivo[i], nombre)
    )
    if (length(candidatos) == 1) candidatos else NA_integer_
  }, integer(1))
  tablas$nombre <- nombre[posicion]
  tablas$url <- registro_url(registro)[posicion]
  tablas$tamano <- registro$tamano[posicion]
  tablas$bytes <- registro$bytes[posicion]
  tablas <- tablas[!is.na(posicion), , drop = FALSE]
  orden <- order(tablas$year, match(tablas$tabla, unique(enigh_tablas(programa)$tabla)))
  tablas <- tablas[orden, , drop = FALSE]
  rownames(tablas) <- NULL
  tablas
}

# Direccion del descriptor de archivos de una edicion (el documento del INEGI
# que describe las tablas y sus variables), o NA si el registro no lo tiene.
enigh_descriptor <- function(programa, year) {
  registro <- registro_inegi()
  propio <- registro$proyecto == enigh_programas()[[programa]]$proyecto & registro$edicion %in% year
  registro <- registro[propio, , drop = FALSE]
  es_descriptor <- grepl("descriptor de archivos", normalizar_texto(registro$titulo), fixed = TRUE)
  direcciones <- registro_url(registro[es_descriptor, , drop = FALSE])
  if (length(direcciones) == 0) NA_character_ else direcciones[1]
}

# Ediciones de todas las series con la forma de bases_importinegi(): una fila
# por serie y edicion, con las tablas que se pueden pedir y el peso de todos
# sus archivos.
enigh_bases <- function() {
  programas <- enigh_programas()
  filas <- lapply(names(programas), function(programa) {
    archivos <- enigh_archivos(programa)
    lapply(split(archivos, archivos$year), function(edicion) {
      distintos <- !duplicated(edicion$url)
      data.frame(
        proyecto = programa,
        year = edicion$year[1],
        modulo = NA_character_,
        base = "microdatos",
        descripcion = paste0(programas[[programa]]$nombre, " ", edicion$year[1], ", ", programas[[programa]]$etiqueta),
        tablas = paste(edicion$tabla, collapse = ", "),
        cobertura = "nacional",
        # El de la mayoria de las tablas: en la Nueva construccion de 2010 una
        # tabla se lee en CSV y las otras catorce, en DBF
        formato = names(which.max(table(edicion$formato))),
        tamano_mb = round(sumar_bytes(edicion$bytes[distintos]) / 1024^2, 1),
        stringsAsFactors = FALSE
      )
    })
  })
  filas <- unlist(filas, recursive = FALSE, use.names = FALSE)
  if (length(filas) == 0) {
    return(data.frame(
      proyecto = character(), year = integer(), modulo = character(), base = character(),
      descripcion = character(), tablas = character(), cobertura = character(),
      formato = character(), tamano_mb = numeric(), stringsAsFactors = FALSE
    ))
  }
  bases <- do.call(rbind, filas)
  rownames(bases) <- NULL
  bases
}

# Validacion de los argumentos -------------------------------------------------------

enigh_validar_year <- function(programa, year) {
  propios <- sort(unique(enigh_archivos(programa)$year))
  disponibles <- paste0("A\u00f1os disponibles en ", programa, "(): ", enumerar(propios), ".")
  if (is.null(year) || length(year) != 1 || is.na(year)) {
    detener("Falta indicar el a\u00f1o en `year` (un solo valor). ", disponibles)
  }
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero) || !grepl("^[0-9]{4}$", trimws(as.character(year)))) {
    detener("`year = ", comillas(year), "` no es un a\u00f1o. ", disponibles)
  }
  if (!numero %in% propios) {
    detener("`year = ", numero, "` no est\u00e1 disponible. ", disponibles, enigh_nota_year(programa, numero))
  }
  numero
}

# Aclaracion para un año que pertenece a otra serie de la ENIGH.
enigh_nota_year <- function(programa, year) {
  if (year >= 2016) {
    return(" Desde 2016 el INEGI publica la ENIGH como Nueva serie, que esta funci\u00f3n no descarga.")
  }
  if (identical(programa, "enigh_nc") && year %in% enigh_archivos("enigh_tradicional")$year) {
    return(paste0(
      " La Nueva construcci\u00f3n empieza en 2008: la edici\u00f3n ", year,
      " est\u00e1 en la serie tradicional, que se descarga con enigh_tradicional()."
    ))
  }
  ""
}

# Pista para una tabla que no existe en la edicion pedida, cuando su contenido
# esta en otra tabla de esa edicion.
enigh_pista_tabla <- function(tabla, year) {
  if (tabla == "viviendas" && year <= 2010) {
    return(" Hasta 2010 las caracter\u00edsticas de la vivienda est\u00e1n en la tabla \"hogares\".")
  }
  if (tabla %in% c("gastohogar", "gastopersona") && year <= 2010) {
    return(" Hasta 2010 los gastos est\u00e1n en la tabla \"gastos\".")
  }
  if (tabla == "gastos" && year >= 2012) {
    return(" Desde 2012 los gastos est\u00e1n en las tablas \"gastohogar\" y \"gastopersona\".")
  }
  ""
}

# Las tablas se aceptan con mayusculas, acentos o espacios ("Poblacion",
# "gasto hogar"). Devuelve los nombres de las tablas, sin repetir.
enigh_validar_tabla <- function(programa, year, tabla) {
  archivos <- enigh_archivos(programa)
  propias <- archivos$tabla[archivos$year == year]
  disponibles <- paste0("Tablas de ", programa, "(", year, "): ", enumerar(propias), ".")
  if (is.null(tabla) || length(tabla) == 0 || !is.character(tabla) || anyNA(tabla)) {
    detener("`tabla` debe ser el nombre de una o varias tablas. ", disponibles)
  }
  buscadas <- gsub(" ", "", normalizar_texto(tabla), fixed = TRUE)
  ajenas <- !buscadas %in% propias
  if (any(ajenas)) {
    detalle <- vapply(which(ajenas), function(i) {
      ediciones <- archivos$year[archivos$tabla == buscadas[i]]
      if (length(ediciones) == 0) {
        return(paste0("`tabla = ", comillas(tabla[i]), "` no es una tabla de ", enigh_programas()[[programa]]$serie, "."))
      }
      paste0(
        "La tabla ", comillas(buscadas[i]), " no existe en la edici\u00f3n ", year, ": existe en ",
        enumerar_y(ediciones), ".", enigh_pista_tabla(buscadas[i], year)
      )
    }, character(1))
    detener(paste(unique(detalle), collapse = " "), " ", disponibles)
  }
  unique(buscadas)
}

# Aviso de la tabla que se descarga cuando no se indica `tabla`.
enigh_avisar_omision <- function(programa, year, tabla) {
  otras <- setdiff(enigh_archivos(programa, year)$tabla, tabla)
  message(
    "No indicaste `tabla`: se descarga ", comillas(tabla), ", la tabla con una fila por hogar. ",
    "Otras tablas de ", programa, "(", year, "): ", enumerar(otras), "."
  )
}

# Descarga y lectura -----------------------------------------------------------------

# Tablas de un zip de la ENIGH en el formato indicado: el nombre con que el zip
# guarda cada archivo (`original`), el mismo nombre con "/" como separador
# (`ruta`) y el nombre del archivo en minusculas y sin extension (`nombre`).
enigh_contenido <- function(zip, formato = "dbf") {
  lista <- tryCatch(utils::unzip(zip, list = TRUE), error = function(e) NULL)
  if (is.null(lista) || nrow(lista) == 0) {
    detener("No se pudo leer el contenido de ", basename(zip), ".")
  }
  ruta <- gobierno_ruta(lista$Name)
  extension <- paste0("\\.", formato, "$")
  propio <- grepl(extension, ruta, ignore.case = TRUE)
  data.frame(
    original = lista$Name[propio],
    ruta = ruta[propio],
    nombre = tolower(sub(extension, "", basename(ruta[propio]), ignore.case = TRUE)),
    stringsAsFactors = FALSE
  )
}

# Lee una tabla de la ENIGH. Los tipos de las columnas son los que declara el
# INEGI en el DBF: el texto queda como texto (los folios, las claves y los
# codigos conservan sus ceros, y hay campos con varios codigos pegados que no
# se pueden tratar como numeros) y los numeros, como numero. Los nombres de las
# columnas se pasan a mayusculas: el INEGI los publica en mayusculas en unas
# tablas y en minusculas en otras, a veces dentro de la misma edicion, y asi
# las tablas no se podrian unir por sus folios.
#
# Un CSV no declara tipos: las tablas que se leen en ese formato reciben los
# de `tipos` (ver enigh_tipos_csv() y enigh_leer_csv()).
enigh_leer_tabla <- function(archivo, formato = "dbf", tipos = NULL) {
  if (identical(formato, "csv")) {
    return(enigh_leer_csv(archivo, tipos))
  }
  datos <- leer_dbf(archivo)
  names(datos) <- toupper(names(datos))
  attr(datos, "data_types") <- NULL
  datos
}

# Tipos de las columnas de las tablas que se leen en CSV, con los nombres en
# mayusculas: `texto` son las columnas que se dejan como texto y `entero`, las
# numericas sin decimales; las demas son numeros con decimales. Devuelve NULL
# para las tablas que se leen en DBF.
#
# Inferir los tipos del contenido da resultados incoherentes: en esta tabla,
# MESACT_1 a MESACT_9 quedarian como texto ("01") y MESACT_10 a MESACT_12 como
# numero, y FOLIOHOG, que es parte de la llave, como numero. Por eso se fijan
# aqui. Son los que el INEGI declara para la misma tabla en su archivo de
# Stata (las columnas de texto) y en el DBF de la edicion 2012, que tiene 95 de
# sus 97 columnas (los anchos y los decimales de las numericas).
enigh_tipos_csv <- function(programa, year, tabla) {
  if (identical(programa, "enigh_nc") && year == 2010 && identical(tabla, "noagro")) {
    personal <- c("EMP", "CPAGO", "ISPAGO", "NISPAGO")
    return(list(
      texto = c(
        "FOLIOVIV", "FOLIOHOG", "NUMREN", "NUMTRAB", "TIPOACT", "NUMESTA", "LUGACT", "SOCIOS", "MISMOP",
        paste0("MES_", 2:6), "AUTOCONS", "MES", "PERACT", paste0("MESACT_", 1:12),
        "REG_NOT", "REG_CONT", "COM_FIS"
      ),
      entero = c(
        "TOTESTA", "NUMSOCIO", paste0("PHOGAR", 1:6), "ENPRODUC", "NOVEND", "CONSINTER",
        paste0("T_", personal), paste0("H_", personal), paste0("M_", personal)
      )
    ))
  }
  NULL
}

# Lee una tabla de la ENIGH de un CSV. El INEGI escribe un espacio en los datos
# que faltan; leer_csv() los devuelve como NA. Las columnas que no estan en
# `tipos$texto` se convierten a numero, y a entero las de `tipos$entero`. Una
# columna con un valor que no es un numero se deja como texto, para no perder
# el dato. El CSV escribe algunos decimales sin el cero inicial (".831765").
enigh_leer_csv <- function(archivo, tipos = NULL) {
  datos <- leer_csv(archivo)
  names(datos) <- toupper(names(datos))
  numero <- "^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$"
  for (columna in setdiff(names(datos), tipos$texto)) {
    valores <- datos[[columna]]
    presentes <- !is.na(valores)
    if (!all(grepl(numero, valores[presentes]))) {
      next
    }
    numeros <- as.numeric(valores)
    enteros <- columna %in% tipos$entero &&
      all(numeros[presentes] == trunc(numeros[presentes]) & abs(numeros[presentes]) <= .Machine$integer.max)
    datos[[columna]] <- if (enteros) as.integer(numeros) else numeros
  }
  datos
}

# Descarga un archivo del INEGI y lee las tablas indicadas. `tablas` son sus
# nombres y `patrones`, las expresiones que reconocen su archivo dentro del
# zip; `formato` es el formato del zip y `tipos`, una lista con los tipos de
# las tablas que se leen en CSV (ver enigh_tipos_csv()). Con `destino`, el zip
# se guarda en esa ruta y se reutiliza si ya existe (ver enigh_obtener()).
enigh_leer_archivo <- function(url, tamano, tablas, patrones, destino = NA_character_,
                               formato = "dbf", tipos = list()) {
  if (is.na(destino)) {
    zip <- inegi_descargar(url, tamano = tamano)
    conservar <- isTRUE(getOption("importinegi.cache", FALSE))
  } else {
    zip <- inegi_descargar(url, destino = destino, tamano = tamano, cache = TRUE)
    conservar <- TRUE
  }
  if (!conservar) {
    on.exit(unlink(zip), add = TRUE)
  }
  contenido <- enigh_contenido(zip, formato)
  posicion <- vapply(patrones, function(patron) {
    hallados <- grep(patron, contenido$nombre)
    if (length(hallados) == 1) hallados else NA_integer_
  }, integer(1), USE.NAMES = FALSE)
  if (anyNA(posicion)) {
    faltan <- tablas[is.na(posicion)]
    detener(
      "El archivo ", basename(url), " no contiene ",
      if (length(faltan) == 1) "la tabla " else "las tablas ", enumerar_y(faltan),
      " con el nombre que se esperaba. Tablas que contiene: ",
      if (nrow(contenido) > 0) enumerar(basename(contenido$ruta), maximo = 15) else "ninguna",
      ". Puedes reportarlo en https://github.com/crenteriam/importinegi/issues"
    )
  }
  extraido <- gobierno_extraer(zip, contenido[posicion, , drop = FALSE])
  on.exit(unlink(extraido$carpeta, recursive = TRUE), add = TRUE)
  leidas <- lapply(seq_along(tablas), function(i) {
    datos <- enigh_leer_tabla(extraido$archivos[i], formato, tipos[[tablas[i]]])
    attr(datos, "fuente") <- url
    datos
  })
  names(leidas) <- tablas
  leidas
}

# Descarga las tablas pedidas de una edicion. Cada archivo del INEGI se
# descarga una sola vez, aunque se pidan varias de sus tablas. Si son varios
# archivos y uno falla, los que ya se descargaron se conservan en la carpeta
# temporal de la sesion, para que al repetir la llamada solo se descarguen los
# que faltan; si todos se leen bien, se borran. Con la copia local activada
# (importinegi.cache) se usa esa copia.
#
# `omision` indica que quien llamo a la funcion no escribio `tabla`: en ese
# caso se avisa que tabla se descarga.
enigh_obtener <- function(programa, year, tabla, omision = FALSE) {
  year <- enigh_validar_year(programa, year)
  tablas <- enigh_validar_tabla(programa, year, tabla)
  if (isTRUE(omision)) {
    enigh_avisar_omision(programa, year, tablas)
  }
  archivos <- enigh_archivos(programa, year)
  plan <- archivos[match(tablas, archivos$tabla), , drop = FALSE]
  descriptor <- enigh_descriptor(programa, year)
  tipos <- stats::setNames(lapply(plan$tabla, function(nombre) enigh_tipos_csv(programa, year, nombre)), plan$tabla)

  direcciones <- unique(plan$url)
  varios <- length(direcciones) > 1 && !isTRUE(getOption("importinegi.cache", FALSE))
  destinos <- if (varios) ruta_reintento(direcciones) else rep(NA_character_, length(direcciones))
  leidas <- stats::setNames(vector("list", nrow(plan)), plan$tabla)
  for (i in seq_along(direcciones)) {
    filas <- which(plan$url == direcciones[i])
    del_archivo <- tryCatch(
      enigh_leer_archivo(
        direcciones[i], plan$tamano[filas[1]], plan$tabla[filas], plan$patron[filas],
        destino = destinos[i], formato = plan$formato[filas[1]], tipos = tipos
      ),
      error = function(e) {
        guardados <- if (varios) sum(file.exists(destinos)) else 0L
        if (guardados == 0) {
          stop(e)
        }
        detener(conditionMessage(e), " ", aviso_reintento(guardados))
      }
    )
    leidas[names(del_archivo)] <- del_archivo
  }
  if (varios) {
    unlink(destinos)
  }
  for (nombre in names(leidas)) {
    attr(leidas[[nombre]], "descriptor") <- descriptor
  }
  if (length(leidas) == 1) {
    return(leidas[[1]])
  }
  attr(leidas, "fuente") <- unique(plan$url)
  attr(leidas, "descriptor") <- descriptor
  leidas
}
