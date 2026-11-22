# Censos, conteos y encuestas de poblacion: motor compartido --------------------
#
# censo(), conteo() e intercensal() usan las funciones de este archivo. El
# trabajo se reparte en tres piezas:
#
#   1. El registro de archivos del INEGI (registro_inegi()) dice que archivos
#      existen, en que formato, cuanto pesan y cual es su direccion.
#   2. poblacion_bases() dice como reconocer en el registro los archivos de cada
#      base de datos ("iter", "ageb", ...). Es la unica tabla que se edita a mano
#      cuando el INEGI publica una base nueva o cambia sus descripciones.
#   3. poblacion_obtener() valida los argumentos, descarga, lee, limpia, filtra
#      el nivel de agregacion y combina las entidades.

# Programas de poblacion del INEGI y sus años. `funcion` es la funcion del
# paquete que los descarga (NA mientras no exista).
poblacion_programas <- function() {
  list(
    censo = list(
      proyecto = "ccpv",
      nombre = "Censo de Poblaci\u00f3n y Vivienda",
      con_articulo = "al Censo de Poblaci\u00f3n y Vivienda",
      years = c(1990L, 2000L, 2010L, 2020L),
      funcion = "censo"
    ),
    conteo = list(
      proyecto = "ccpv",
      nombre = "Conteo de Poblaci\u00f3n y Vivienda",
      con_articulo = "al Conteo de Poblaci\u00f3n y Vivienda",
      years = c(1995L, 2005L),
      funcion = "conteo"
    ),
    intercensal = list(
      proyecto = "eic",
      nombre = "Encuesta Intercensal",
      con_articulo = "a la Encuesta Intercensal",
      years = c(2015L, 2025L),
      funcion = "intercensal"
    )
  )
}

# Bases de datos de cada programa y año. `reconoce` es una expresion regular
# que se compara con la descripcion de cada archivo en el registro y con el
# primer segmento de su titulo, ya normalizados (minusculas y sin acentos).
# `ruta`, si no es NA, es una expresion regular que se compara con la ruta del
# archivo: sirve para la Encuesta Intercensal 2015, cuyos titulos solo tienen
# el nombre de la entidad.
poblacion_bases <- function() {
  reglas <- c(
    iter = "\\biter\\b",
    ageb = "\\bageb\\b",
    muestra = "\\bmuestra\\b|cuestionario ampliado",
    localidades = "localidades con menos|de las localidades",
    entorno = "entorno urbano",
    asistencia = "asistencia social",
    viviendas = "viviendas habitadas y deshabitadas",
    resultados = "principales resultados por localidad de 50"
  )
  fila <- function(programa, year, bases, reconoce = unname(reglas[bases]), ruta = NA_character_) {
    data.frame(
      programa = programa, year = year, base = bases,
      reconoce = reconoce, ruta = ruta, stringsAsFactors = FALSE
    )
  }
  rbind(
    fila("censo", 1990L, c("iter", "muestra")),
    fila("censo", 2000L, c("iter", "muestra")),
    fila("censo", 2010L, c("iter", "ageb", "muestra", "localidades", "entorno")),
    fila("censo", 2020L, c("iter", "ageb", "muestra", "localidades", "entorno", "asistencia")),
    fila("conteo", 1995L, c("iter", "muestra")),
    fila("conteo", 2005L, c("iter", "muestra", "viviendas")),
    fila("intercensal", 2015L, "muestra", reconoce = NA_character_, ruta = "/microdatos/eic2015_[0-9]{2}$"),
    fila("intercensal", 2025L, c("muestra", "resultados"),
         reconoce = c("\\bencuesta intercensal\\b", reglas[["resultados"]]))
  )
}

# Nombre de cada base para los mensajes.
poblacion_nombre_base <- function(base) {
  nombres <- c(
    iter = "los principales resultados por localidad (ITER)",
    ageb = "los resultados por AGEB y manzana urbana",
    muestra = "la muestra",
    localidades = "los resultados sobre localidades",
    entorno = "los resultados sobre el entorno urbano",
    asistencia = "los resultados sobre alojamientos de asistencia social",
    viviendas = "las viviendas habitadas y deshabitadas por localidad",
    resultados = "los principales resultados por localidad de 50 000 y m\u00e1s habitantes"
  )
  unname(nombres[base])
}

# Titulo de cada base, como lo muestra bases_importinegi(). El de la muestra
# depende del levantamiento: en el Censo de 1990 y en el Conteo de 2005 es un
# extracto del 10 % de las viviendas del archivo completo; en los censos desde
# 2000, el cuestionario ampliado; en el Conteo de 1995, la encuesta del Conteo;
# en la Encuesta Intercensal, los microdatos de la encuesta.
poblacion_titulo_base <- function(programa, year, base) {
  if (base == "muestra") {
    extracto <- "Muestra (extracto del 10 % de las viviendas)"
    if (programa == "censo") {
      return(if (year == 1990L) extracto else "Muestra (cuestionario ampliado)")
    }
    if (programa == "conteo") {
      return(if (year == 1995L) "Muestra (encuesta del Conteo, cuestionario ampliado)" else extracto)
    }
    if (programa == "intercensal") {
      return("Muestra (microdatos de la encuesta)")
    }
    return("Muestra")
  }
  titulos <- c(
    iter = "Principales resultados por localidad (ITER)",
    ageb = "Principales resultados por AGEB y manzana urbana",
    localidades = "Caracter\u00edsticas de las localidades",
    entorno = "Caracter\u00edsticas del entorno urbano",
    asistencia = "Alojamientos de asistencia social",
    viviendas = "Viviendas habitadas y deshabitadas por localidad",
    resultados = "Principales resultados por localidad de 50 000 y m\u00e1s habitantes"
  )
  unname(titulos[base])
}

# Tablas de las bases que se publican en varias tablas, con el patron con el
# que se reconoce el archivo de cada una dentro del zip. La primera es la que
# se descarga por omision. NULL significa que la base tiene una sola tabla.
poblacion_tablas <- function(programa, year, base) {
  if (base == "muestra" && programa == "conteo") {
    if (year == 1995L) {
      # datgen95 y migint95: datos generales de las personas y migrantes
      # internacionales de la Encuesta del Conteo
      return(c(personas = "^datgen", migrantes = "^migint"))
    }
    # trpmueNN, trvmueNN y trhmueNN, dentro de la carpeta cpv2005_NN_dbf
    return(c(personas = "^trpmue", viviendas = "^trvmue", hogares = "^trhmue"))
  }
  if (base == "muestra" && programa == "intercensal") {
    # TR_PERSONA y TR_VIVIENDA en 2015; personas, viviendas y migrantes en
    # 2025. Los patrones reconocen los nombres con o sin el prefijo TR_.
    tablas <- c(personas = "persona", viviendas = "vivienda")
    return(if (year >= 2025L) c(tablas, migrantes = "migrante") else tablas)
  }
  if (base == "muestra" && !(programa == "censo" && year == 1990L)) {
    return(c(personas = "^per", viviendas = "^viv|^vho", migrantes = "^mig|^min"))
  }
  if (base == "entorno" && programa == "censo" && year >= 2020L) {
    return(c(manzanas = "manzana", vialidades = "vialidad"))
  }
  if (base == "asistencia") {
    # TR_ALO_CAAS, TI_USU_CAAS y TI_TRA_CAAS: alojamientos, su poblacion
    # usuaria residente y su poblacion trabajadora
    return(c(alojamientos = "(^|_)alo(_|\\.)", usuarios = "(^|_)usu(_|\\.)", trabajadores = "(^|_)tra(_|\\.)"))
  }
  NULL
}

# Bases de un programa que tienen varias tablas en algun año.
poblacion_bases_con_tablas <- function(programa) {
  bases <- poblacion_bases()
  bases <- bases[bases$programa == programa, ]
  varias <- vapply(seq_len(nrow(bases)), function(i) {
    !is.null(poblacion_tablas(programa, bases$year[i], bases$base[i]))
  }, logical(1))
  unique(bases$base[varias])
}

# Bases de un programa que mezclan filas de totales con filas de detalle.
poblacion_bases_con_niveles <- function(programa) {
  bases <- unique(poblacion_bases()$base[poblacion_bases()$programa == programa])
  bases[vapply(bases, function(base) !is.null(poblacion_niveles(base)), logical(1))]
}

# "la base "x"" o "las bases "x" y "y"", para los mensajes.
nombrar_bases <- function(bases) {
  if (length(bases) == 1) paste0("la base ", comillas(bases)) else paste0("las bases ", enumerar_y(bases))
}

# Niveles de agregacion de las bases que mezclan filas de totales con filas de
# detalle, del mas agregado al mas detallado. NULL si la base no tiene totales.
poblacion_niveles <- function(base) {
  switch(
    base,
    iter = ,
    viviendas = ,
    resultados = c("nacional", "entidad", "municipio", "localidad"),
    ageb = c("entidad", "municipio", "localidad", "ageb", "manzana"),
    NULL
  )
}

# Bases en las que el INEGI reserva datos con asterisco o los marca como no
# disponibles ("N/D").
poblacion_faltantes <- function(base) {
  if (base %in% c("iter", "ageb", "localidades", "viviendas", "resultados")) c("*", "N/D") else NULL
}

# Formatos de archivo que se leen en cada base. Los principales resultados por
# localidad de la Encuesta Intercensal 2025 solo se publican en XLSX.
poblacion_formatos <- function(base) {
  if (base == "resultados") "xlsx" else c("csv", "dbf")
}

# Claves geograficas: nombres con los que aparecen en las tablas y su ancho.
claves_geograficas <- function() {
  list(
    ent = list(nombres = c("ENTIDAD", "ENT", "CVE_ENT"), ancho = 2L),
    mun = list(nombres = c("MUN", "CVE_MUN"), ancho = 3L),
    loc = list(nombres = c("LOC", "CVE_LOC"), ancho = 4L),
    ageb = list(nombres = c("AGEB", "CVE_AGEB"), ancho = 4L),
    mza = list(nombres = c("MZA", "CVE_MZA"), ancho = 3L)
  )
}

# Validacion de argumentos ------------------------------------------------------

poblacion_validar_year <- function(programa, year) {
  programas <- poblacion_programas()
  propios <- programas[[programa]]$years
  disponibles <- paste0("A\u00f1os disponibles en ", programa, "(): ", enumerar(propios), ".")
  if (is.null(year) || length(year) != 1 || is.na(year)) {
    detener("Falta indicar el a\u00f1o en `year` (un solo valor). ", disponibles)
  }
  numero <- suppressWarnings(as.integer(as.character(year)))
  if (is.na(numero) || !grepl("^[0-9]{4}$", trimws(as.character(year)))) {
    detener("`year = ", comillas(year), "` no es un a\u00f1o. ", disponibles)
  }
  if (!numero %in% propios) {
    otro <- names(programas)[vapply(programas, function(p) numero %in% p$years, logical(1))]
    if (length(otro) == 1) {
      funcion <- programas[[otro]]$funcion
      detener(
        "`year = ", numero, "` corresponde ", programas[[otro]]$con_articulo, ", no ",
        programas[[programa]]$con_articulo,
        if (!is.na(funcion)) paste0(": usa ", funcion, "(", numero, ")"), ". ", disponibles
      )
    }
    detener("`year = ", numero, "` no est\u00e1 disponible. ", disponibles)
  }
  numero
}

poblacion_validar_base <- function(programa, year, base) {
  bases <- poblacion_bases()
  bases <- bases[bases$programa == programa, ]
  if (!is.character(base) || length(base) != 1 || is.na(base)) {
    detener("`base` debe ser el nombre de una base de datos. Opciones: ", enumerar(unique(bases$base)), ".")
  }
  base <- tolower(trimws(base))
  del_year <- bases$base[bases$year == year]
  if (!base %in% del_year) {
    otros <- bases$year[bases$base == base]
    if (length(otros) > 0) {
      detener(
        "La base ", comillas(base), " no existe para ", year, ": el INEGI la publica para ",
        enumerar(otros), ". Bases de ", year, ": ", enumerar(del_year),
        ". Consulta todas con bases_importinegi(\"", programa, "\")."
      )
    }
    detener(
      "`base = ", comillas(base), "` no es una opci\u00f3n v\u00e1lida. Bases de ", year, ": ",
      enumerar(del_year), ". Consulta todas con bases_importinegi(\"", programa, "\")."
    )
  }
  base
}

# Devuelve el nombre de la tabla de la muestra (NA si la muestra tiene una sola).
poblacion_validar_tabla <- function(programa, year, base, tabla) {
  tablas <- poblacion_tablas(programa, year, base)
  if (is.null(tablas)) {
    if (!is.null(tabla)) {
      if (base %in% poblacion_bases_con_tablas(programa)) {
        detener("La base ", comillas(base), " de ", year, " se publica en una sola tabla: omite el argumento `tabla`.")
      }
      detener(
        "`tabla` solo se usa con ", nombrar_bases(poblacion_bases_con_tablas(programa)),
        "; la base ", comillas(base), " tiene una sola tabla."
      )
    }
    return(NA_character_)
  }
  if (is.null(tabla)) {
    return(names(tablas)[1])
  }
  if (!is.character(tabla) || length(tabla) != 1 || is.na(tabla)) {
    detener("`tabla` debe ser una sola de estas opciones: ", enumerar(names(tablas)), ".")
  }
  validar_opcion(tolower(trimws(tabla)), names(tablas), "tabla")
}

# Lista entre comillas unida con "y": "a", "b" y "c".
enumerar_y <- function(opciones) {
  texto <- vapply(as.list(opciones), comillas, character(1))
  if (length(texto) < 2) {
    return(paste(texto, collapse = ""))
  }
  paste(paste(texto[-length(texto)], collapse = ", "), "y", texto[length(texto)])
}

# Devuelve los niveles pedidos, "todos", o NULL si la base no tiene niveles.
poblacion_validar_nivel <- function(programa, base, nivel) {
  niveles <- poblacion_niveles(base)
  if (is.null(niveles)) {
    if (!is.null(nivel)) {
      detener(
        "`nivel` solo se usa con ", nombrar_bases(poblacion_bases_con_niveles(programa)), "; la base ",
        comillas(base), " no mezcla totales con filas de detalle."
      )
    }
    return(NULL)
  }
  if (is.null(nivel)) {
    return(niveles[length(niveles)])
  }
  if (!is.character(nivel) || length(nivel) == 0 || anyNA(nivel)) {
    detener("`nivel` debe ser \"todos\" o uno o varios de: ", enumerar(niveles), ".")
  }
  nivel <- unique(tolower(trimws(nivel)))
  if (identical(nivel, "todos")) {
    return("todos")
  }
  ajenos <- setdiff(nivel, niveles)
  if (length(ajenos) > 0) {
    detener(
      "`nivel = ", enumerar(ajenos), "` no es una opci\u00f3n v\u00e1lida para la base ", comillas(base),
      ". Opciones: ", enumerar(niveles), ", o \"todos\"."
    )
  }
  nivel
}

# Archivos en el registro -------------------------------------------------------

# Archivos de una base en el registro: una fila por entidad, con su clave
# ("00" para el archivo nacional), formato, tamano y direccion. Si la base se
# publica en CSV y en DBF se prefiere el CSV.
poblacion_archivos <- function(programa, year, base) {
  bases <- poblacion_bases()
  regla <- bases[bases$programa == programa & bases$year == year & bases$base == base, ]
  vacio <- data.frame(clave = character(), formato = character(), tamano = character(),
                      bytes = numeric(), url = character(), stringsAsFactors = FALSE)
  if (nrow(regla) != 1) {
    return(vacio)
  }
  proyecto <- poblacion_programas()[[programa]]$proyecto
  registro <- registro_inegi()
  filas <- registro[
    registro$proyecto == proyecto & !is.na(registro$edicion) & registro$edicion == year &
      registro$formato %in% poblacion_formatos(base),
  ]

  # El registro describe los archivos de cuatro formas:
  #   censos y conteos hasta 2010: titulo "Bases de datos|<entidad>" y la base
  #     en la descripcion;
  #   Censo 2020 y Encuesta Intercensal 2025: titulo
  #     "<base>|...|Bases de datos|<entidad>";
  #   Encuesta Intercensal 2015: titulo con solo el nombre de la entidad (a
  #     veces con espacios de no separacion); la base se reconoce por la ruta;
  #   bases con un solo archivo, nacional: titulo "<base>|Base de datos".
  # La base se reconoce en la descripcion o en el primer segmento del titulo.
  # Solo se conservan los archivos cuyo penultimo segmento es "Bases de datos",
  # o cuyo ultimo segmento lo es (archivo nacional): asi se descartan la
  # documentacion y las bases de ejemplo, que no contienen datos reales.
  partes <- lapply(strsplit(filas$titulo, "|", fixed = TRUE), trimws, whitespace = "[\\h\\v]")
  primero <- vapply(partes, function(p) p[1], character(1))
  ultimo <- vapply(partes, function(p) p[length(p)], character(1))
  grupo <- normalizar_texto(vapply(partes, function(p) if (length(p) >= 2) p[length(p) - 1] else "", character(1)))
  de_datos <- "^bases? de datos$|^microdatos$|^datos abiertos$"
  sin_entidad <- grepl(de_datos, normalizar_texto(ultimo))
  entidad <- ifelse(sin_entidad, "nacional", ultimo)
  por_texto <- if (is.na(regla$reconoce)) {
    rep(FALSE, nrow(filas))
  } else {
    grepl(regla$reconoce, normalizar_texto(paste(filas$descripcion, primero))) &
      (grepl(de_datos, grupo) | sin_entidad)
  }
  por_ruta <- if (is.na(regla$ruta)) rep(FALSE, nrow(filas)) else grepl(regla$ruta, filas$ruta)
  es_datos <- por_texto | por_ruta
  filas <- filas[es_datos, ]
  entidad <- entidad[es_datos]

  nombres <- unique(entidad)
  claves <- vapply(nombres, function(nombre) {
    tryCatch(estado_clave(nombre), error = function(e) NA_character_)
  }, character(1))
  filas$clave <- unname(claves[match(entidad, nombres)])
  filas <- filas[!is.na(filas$clave), ]
  if (nrow(filas) == 0) {
    return(vacio)
  }

  cobertura <- tapply(filas$clave, filas$formato, function(x) length(unique(x)))
  formato <- if ("csv" %in% names(cobertura) && cobertura[["csv"]] >= max(cobertura)) "csv" else names(which.max(cobertura))
  filas <- filas[filas$formato == formato, ]
  filas <- filas[!duplicated(filas$clave), ]
  filas <- filas[order(filas$clave), ]
  filas$url <- registro_url(filas)
  rownames(filas) <- NULL
  filas[, c("clave", "formato", "tamano", "bytes", "url")]
}

# Claves de las entidades pedidas en `estado`, ya verificadas contra los
# archivos que existen para la base. "nacional" es todo el pais: el archivo
# nacional o, si el INEGI publica la base solo por entidad, todas las
# entidades, que despues se combinan en una sola tabla. Si la base solo tiene
# archivo nacional, se aceptan las entidades: poblacion_obtener() descarga ese
# archivo y conserva sus filas.
poblacion_claves <- function(estado, archivos, descripcion) {
  if (is.null(estado) || length(estado) == 0 || anyNA(estado)) {
    detener("Falta indicar `estado`: una o varias entidades, o \"nacional\".")
  }
  entidades <- setdiff(archivos$clave, "00")
  claves <- unique(vapply(as.list(estado), function(e) estado_clave(e), character(1)))

  if ("00" %in% claves) {
    if (length(claves) > 1) {
      detener("`estado = \"nacional\"` no se combina con otras entidades: pide todo el pa\u00eds o una lista de entidades.")
    }
    if (!"00" %in% archivos$clave) {
      return(entidades)
    }
  }
  faltan <- setdiff(claves, archivos$clave)
  if (length(faltan) > 0 && !identical(archivos$clave, "00")) {
    tabla <- inegi_estados()
    detener(
      "El registro del paquete no tiene el archivo de ", descripcion, " para: ",
      enumerar(tabla$nombre[match(faltan, tabla$clave)]), "."
    )
  }
  claves
}

# Entidad que se descarga cuando no se indica `estado`: todo el pais, salvo en
# la muestra y en las bases sin archivo nacional, donde se descarga solo
# Aguascalientes para no hacer sin pedirlo una descarga grande.
poblacion_estado_omision <- function(base, archivos) {
  if (base == "muestra" || !"00" %in% archivos$clave) "01" else "00"
}

# Aviso de que se descarga Aguascalientes por omision, con el tamaño de la
# descarga de todo el pais.
poblacion_avisar_omision <- function(base, archivos) {
  nacional <- archivos$clave == "00"
  detalle <- if (any(nacional)) {
    paste0("archivo nacional de ", tamano_texto(archivos$bytes[nacional]))
  } else {
    paste0(sum(!nacional), " archivos, ", tamano_texto(archivos$bytes[!nacional]), " en total")
  }
  descarga <- if (base == "muestra") {
    "se descarga la muestra de Aguascalientes"
  } else {
    "se descargan los datos de Aguascalientes"
  }
  message("No indicaste `estado`: ", descarga, ". Para todo el pa\u00eds usa estado = \"nacional\" (", detalle, ").")
}

# Lectura -----------------------------------------------------------------------

# Elige, dentro de la carpeta descomprimida, el archivo que contiene la tabla.
# Los zip de datos abiertos traen ademas el diccionario, los catalogos y los
# metadatos; esos archivos se descartan. Si la tabla aparece varias veces, se
# prefiere la que esta menos anidada: algunos zip traen dentro, por error, el
# zip de otra entidad (en la muestra del Conteo 1995, el de Chiapas trae el de
# Guerrero y el de Chihuahua, el de Guanajuato), que se descomprime en una
# carpeta. Si aun quedan varias copias identicas, se usa la primera.
# `archivo` es el nombre del zip, para los mensajes.
poblacion_elegir_tabla <- function(carpeta, formato, patron = NULL, tabla = NA_character_, archivo = NULL) {
  candidatos <- buscar_archivos(carpeta, extension = formato, exigir = TRUE)
  relativa <- tolower(substring(candidatos, nchar(carpeta) + 2))
  en_conjunto <- grepl("conjunto_de_datos", relativa, fixed = TRUE)
  if (any(en_conjunto)) {
    candidatos <- candidatos[en_conjunto]
  } else {
    de_apoyo <- grepl("diccionario|catalogo|metadato", relativa)
    if (!all(de_apoyo)) {
      candidatos <- candidatos[!de_apoyo]
    }
  }
  encontrados <- candidatos
  if (!is.null(patron)) {
    encontrados <- candidatos[grepl(patron, basename(candidatos), ignore.case = TRUE)]
  }
  if (length(encontrados) > 1) {
    relativas <- substring(encontrados, nchar(carpeta) + 2)
    profundidad <- lengths(strsplit(relativas, "/", fixed = TRUE))
    encontrados <- encontrados[profundidad == min(profundidad)]
  }
  if (length(encontrados) > 1 && length(unique(tolower(basename(encontrados)))) == 1 &&
      length(unique(unname(tools::md5sum(encontrados)))) == 1) {
    encontrados <- encontrados[1]
  }
  if (length(encontrados) != 1) {
    detener(
      if (length(encontrados) == 0) "No se encontr\u00f3" else "Se encontr\u00f3 m\u00e1s de un archivo para",
      if (!is.na(tabla)) paste0(" la tabla ", comillas(tabla)) else " la tabla de datos",
      " en el archivo descargado", if (!is.null(archivo)) paste0(" ", archivo), ", que contiene: ",
      enumerar(substring(candidatos, nchar(carpeta) + 2), maximo = 15), ". ",
      "Es posible que el INEGI haya cambiado el contenido del archivo; puedes reportarlo en ",
      "https://github.com/crenteriam/importinegi/issues"
    )
  }
  encontrados
}

# Descarga un archivo del registro y devuelve su tabla, sin limpiar. Con
# `destino`, el archivo se guarda en esa ruta y se reutiliza si ya existe (ver
# poblacion_leer_entidades()). Los zip se descomprimen y se busca la tabla
# dentro; los XLSX se leen directamente.
poblacion_leer <- function(url, formato, tamano = NULL, patron = NULL, tabla = NA_character_,
                           destino = NA_character_) {
  if (is.na(destino)) {
    archivo <- inegi_descargar(url, tamano = tamano)
    conservar <- isTRUE(getOption("importinegi.cache", FALSE))
  } else {
    archivo <- inegi_descargar(url, destino = destino, tamano = tamano, cache = TRUE)
    conservar <- TRUE
  }
  if (!conservar) {
    on.exit(unlink(archivo), add = TRUE)
  }
  if (identical(formato, "xlsx")) {
    return(leer_tabla(archivo))
  }
  carpeta <- poblacion_descomprimir(archivo, formato, patron)
  on.exit(unlink(carpeta, recursive = TRUE), add = TRUE)
  leer_tabla(poblacion_elegir_tabla(carpeta, formato, patron, tabla, archivo = basename(url)))
}

# Descomprime solo el archivo de la tabla pedida, cuando la lista del contenido
# del zip basta para saber cual es: algunos zip traen varias tablas grandes (el
# nacional de la muestra de la Encuesta Intercensal 2025 trae tres) y
# descomprimirlas todas ocuparia varios GB de disco. La eleccion sigue las mismas
# reglas que poblacion_elegir_tabla(). Si el zip trae otros zip o si hay mas de
# un candidato, descomprime todo y poblacion_elegir_tabla() decide.
poblacion_descomprimir <- function(zip, formato, patron = NULL) {
  contenido <- tryCatch(utils::unzip(zip, list = TRUE)$Name, error = function(e) character())
  elegido <- poblacion_tabla_en_lista(contenido, formato, patron)
  if (is.na(elegido)) {
    return(inegi_descomprimir(zip))
  }
  inegi_descomprimir(zip, anidados = FALSE, archivos = elegido)
}

# Ruta, dentro de la lista del contenido de un zip, del unico archivo que puede
# tener la tabla, o NA si no se puede decidir sin descomprimir.
poblacion_tabla_en_lista <- function(contenido, formato, patron = NULL) {
  if (length(contenido) == 0 || any(grepl("\\.zip$", contenido, ignore.case = TRUE))) {
    return(NA_character_)
  }
  candidatos <- contenido[tolower(tools::file_ext(contenido)) %in% tolower(formato)]
  relativa <- tolower(candidatos)
  en_conjunto <- grepl("conjunto_de_datos", relativa, fixed = TRUE)
  if (any(en_conjunto)) {
    candidatos <- candidatos[en_conjunto]
  } else {
    de_apoyo <- grepl("diccionario|catalogo|metadato", relativa)
    if (!all(de_apoyo)) {
      candidatos <- candidatos[!de_apoyo]
    }
  }
  if (!is.null(patron)) {
    candidatos <- candidatos[grepl(patron, basename(candidatos), ignore.case = TRUE)]
  }
  if (length(candidatos) > 1) {
    profundidad <- lengths(strsplit(candidatos, "/", fixed = TRUE))
    candidatos <- candidatos[profundidad == min(profundidad)]
  }
  if (length(candidatos) == 1) candidatos else NA_character_
}

# Ruta en la carpeta temporal de la sesion donde se guarda cada archivo de una
# descarga de varios archivos.
ruta_reintento <- function(url) {
  relativa <- gsub("[^A-Za-z0-9._/-]", "_", sub("^https?://[^/]+/", "", url))
  file.path(tempdir(), "importinegi", "descargas", relativa)
}

# Descarga y lee los archivos seleccionados. Si son varios y uno falla, los que
# ya se descargaron se conservan en la carpeta temporal de la sesion, para que
# al repetir la llamada solo se descarguen los que faltan; si todos se leen
# bien, se borran. Con la copia local activada (importinegi.cache) se usa esa
# copia.
poblacion_leer_entidades <- function(seleccion, patron, tabla) {
  varios <- nrow(seleccion) > 1 && !isTRUE(getOption("importinegi.cache", FALSE))
  destinos <- if (varios) ruta_reintento(seleccion$url) else rep(NA_character_, nrow(seleccion))
  tablas <- vector("list", nrow(seleccion))
  for (i in seq_len(nrow(seleccion))) {
    tablas[[i]] <- tryCatch(
      poblacion_leer(
        seleccion$url[i], seleccion$formato[i], tamano = seleccion$tamano[i],
        patron = patron, tabla = tabla, destino = destinos[i]
      ),
      error = function(e) {
        guardados <- if (varios) sum(file.exists(destinos)) else 0L
        if (guardados == 0) {
          stop(e)
        }
        detener(
          conditionMessage(e), " ",
          if (guardados == 1) {
            "El archivo que ya se descarg\u00f3 se conserva"
          } else {
            paste0("Los ", guardados, " archivos que ya se descargaron se conservan")
          },
          " durante esta sesi\u00f3n de R: si repites la llamada, solo se descargar\u00e1n los que faltan."
        )
      }
    )
  }
  if (varios) {
    unlink(destinos)
  }
  tablas
}

# Deja las tablas de varias entidades con las mismas columnas, en el mismo
# orden y del mismo tipo, para poder unirlas. Las columnas que faltan en alguna
# entidad se llenan con NA; una columna que llega como numero en una entidad y
# como texto en otra queda como texto en todas.
armonizar_tablas <- function(tablas) {
  if (length(tablas) < 2) {
    return(tablas)
  }
  columnas <- unique(unlist(lapply(tablas, names)))
  distintas <- !all(vapply(tablas, function(t) identical(names(t), columnas), logical(1)))
  if (distintas) {
    warning(
      "Las tablas de las entidades no tienen las mismas columnas; ",
      "las que faltan en alguna entidad se llenaron con NA.",
      call. = FALSE
    )
    tablas <- lapply(tablas, function(t) {
      for (columna in setdiff(columnas, names(t))) {
        t[[columna]] <- NA
      }
      t[, columnas, drop = FALSE]
    })
  }
  for (columna in columnas) {
    es_texto <- vapply(tablas, function(t) is.character(t[[columna]]), logical(1))
    if (any(es_texto) && !all(es_texto)) {
      for (i in which(!es_texto)) {
        tablas[[i]][[columna]] <- as.character(tablas[[i]][[columna]])
      }
    }
  }
  tablas
}

# Une las tablas, ya armonizadas, de varias entidades.
unir_tablas <- function(tablas) {
  if (length(tablas) == 1) {
    return(tablas[[1]])
  }
  unida <- do.call(rbind, tablas)
  rownames(unida) <- NULL
  unida
}

# Une las tablas de varias entidades. Si no tienen las mismas columnas, las que
# falten se llenan con NA.
combinar_tablas <- function(tablas) {
  unir_tablas(armonizar_tablas(tablas))
}

# Claves geograficas y niveles --------------------------------------------------

# Nombre de la columna que contiene una clave geografica, o NA si no existe.
columna_clave <- function(datos, tipo) {
  nombres <- claves_geograficas()[[tipo]]$nombres
  posicion <- match(nombres, toupper(names(datos)))
  posicion <- posicion[!is.na(posicion)]
  if (length(posicion) == 0) NA_character_ else names(datos)[posicion[1]]
}

# Completa una clave con ceros a la izquierda ("9" o 9 pasan a ser "09").
rellenar_clave <- function(valores, ancho) {
  if (is.numeric(valores)) {
    texto <- rep(NA_character_, length(valores))
    presentes <- !is.na(valores)
    texto[presentes] <- formatC(as.integer(valores[presentes]), width = ancho, flag = "0", format = "d")
    return(texto)
  }
  valores <- trimws(as.character(valores))
  cortos <- !is.na(valores) & nzchar(valores) & nchar(valores) < ancho
  valores[cortos] <- paste0(strrep("0", ancho - nchar(valores[cortos])), valores[cortos])
  valores
}

# Deja las claves geograficas como texto del ancho que usa el INEGI. En los
# principales resultados por localidad de la Encuesta Intercensal, la clave
# integrada CVEGEO tiene siempre 9 digitos (entidad, municipio y localidad).
poblacion_normalizar_claves <- function(datos, base = NULL) {
  claves <- claves_geograficas()
  for (tipo in names(claves)) {
    columna <- columna_clave(datos, tipo)
    if (!is.na(columna)) {
      datos[[columna]] <- rellenar_clave(datos[[columna]], claves[[tipo]]$ancho)
    }
  }
  integrada <- match("CVEGEO", toupper(names(datos)))
  if (identical(base, "resultados") && !is.na(integrada)) {
    datos[[integrada]] <- rellenar_clave(datos[[integrada]], 9L)
  }
  datos
}

# Nivel de agregacion de cada fila. El INEGI identifica los totales con claves
# en ceros: municipio "000" para el total de la entidad, localidad "0000" para
# el del municipio, AGEB "0000" para el de la localidad y manzana "000" para el
# del AGEB. Se marcan como "agregado" las filas que reunen varias localidades:
# en el ITER, las localidades "9998" y "9999" (localidades de una y de dos
# viviendas); en los principales resultados por localidad de la Encuesta
# Intercensal, el municipio "997" con la localidad "9997" (localidades de
# menos de 50 000 habitantes de cada entidad).
#
# Devuelve NULL si la tabla no tiene las columnas de claves esperadas.
poblacion_clasificar_nivel <- function(datos, base) {
  tipos <- if (base == "ageb") c("ent", "mun", "loc", "ageb", "mza") else c("ent", "mun", "loc")
  columnas <- vapply(tipos, function(tipo) columna_clave(datos, tipo), character(1))
  if (anyNA(columnas)) {
    return(NULL)
  }
  cero <- function(tipo) {
    valores <- datos[[columnas[[tipo]]]]
    !is.na(valores) & grepl("^0+$", valores)
  }
  if (base == "ageb") {
    nivel <- rep("manzana", nrow(datos))
    nivel[cero("mza")] <- "ageb"
    nivel[cero("mza") & cero("ageb")] <- "localidad"
    nivel[cero("mza") & cero("ageb") & cero("loc")] <- "municipio"
    nivel[cero("mza") & cero("ageb") & cero("loc") & cero("mun")] <- "entidad"
  } else {
    nivel <- rep("localidad", nrow(datos))
    nivel[cero("loc")] <- "municipio"
    nivel[cero("loc") & cero("mun")] <- "entidad"
    nivel[cero("loc") & cero("mun") & cero("ent")] <- "nacional"
    nivel[datos[[columnas[["loc"]]]] %in% c("9998", "9999")] <- "agregado"
    nivel[datos[[columnas[["mun"]]]] %in% "997" & datos[[columnas[["loc"]]]] %in% "9997"] <- "agregado"
  }
  nivel
}

poblacion_filtrar_nivel <- function(datos, base, nivel, explicito) {
  if (identical(nivel, "todos")) {
    return(datos)
  }
  clasificacion <- poblacion_clasificar_nivel(datos, base)
  if (is.null(clasificacion)) {
    if (explicito) {
      detener(
        "No se puede filtrar por `nivel`: la tabla no tiene las columnas de claves geogr\u00e1ficas ",
        "esperadas. Columnas: ", enumerar(names(datos), maximo = 12), ". Usa nivel = \"todos\"."
      )
    }
    warning(
      "No se identificaron las claves geogr\u00e1ficas de la tabla, as\u00ed que se devuelven todas ",
      "las filas, incluidas las de totales.",
      call. = FALSE
    )
    return(datos)
  }
  datos[clasificacion %in% nivel, , drop = FALSE]
}

# Conserva las filas de las entidades indicadas (claves de dos digitos). Se usa
# en las bases que solo tienen archivo nacional.
poblacion_filtrar_entidades <- function(datos, entidades) {
  columna <- columna_clave(datos, "ent")
  if (is.na(columna)) {
    detener(
      "No se pueden conservar las filas de las entidades pedidas: la tabla no tiene la clave ",
      "de la entidad. Columnas: ", enumerar(names(datos), maximo = 12), ". Usa estado = \"nacional\"."
    )
  }
  datos[datos[[columna]] %in% entidades, , drop = FALSE]
}

# Tipos de las columnas -----------------------------------------------------------

# Columnas que se conservan como texto aunque solo contengan numeros: las claves
# geograficas, los identificadores de vivienda, hogar y persona, las variables
# del diseño de la muestra y las coordenadas, que el INEGI escribe en grados,
# minutos y segundos sin separadores ("1023456").
columnas_texto <- function(datos) {
  geograficas <- unlist(lapply(claves_geograficas(), function(clave) clave$nombres), use.names = FALSE)
  otras <- "^ID_|^FOLIO|^NUMVIV$|^LOC50K$|^UPM$|^ESTRATO$|^LONGITUD$|^LATITUD$|^CVEGEO$"
  nombres <- names(datos)
  nombres[toupper(nombres) %in% geograficas | grepl(otras, nombres, ignore.case = TRUE)]
}

# Decide que columnas se convierten a numero, con los valores de todas las
# entidades. En la muestra y en la base de asistencia social se convierten
# tambien las que tienen ceros a la izquierda ("008" en la edad, "0012" en el
# factor de expansion): son respuestas codificadas, no claves. La altitud es
# una medida: es numerica en todos los censos, aunque algunos la escriban con
# ceros a la izquierda ("0020").
poblacion_decidir_conversion <- function(tablas, base) {
  decidir_conversion(
    tablas,
    texto = columnas_texto(tablas[[1]]),
    ceros = base %in% c("muestra", "asistencia"),
    medidas = "ALTITUD"
  )
}

# Convierte a numero las columnas numericas de una tabla.
poblacion_convertir <- function(datos, base) {
  aplicar_conversion(datos, poblacion_decidir_conversion(list(datos), base))
}

# Lee las tablas de las entidades con `leer()` y las prepara: claves
# geograficas, datos faltantes y tipos de las columnas; despues las une. Cada
# entidad se convierte a numero antes de unirlas, para que no esten en memoria
# dos copias completas de los datos (un entero ocupa la mitad que un texto).
# Que columnas se convierten se decide con todas las entidades, de modo que el
# resultado es el mismo que si se unieran primero.
#
# `leer` es una funcion que devuelve la lista de tablas: asi la lista solo
# existe dentro de esta funcion y la memoria de cada tabla original se libera
# en cuanto se convierte.
poblacion_preparar <- function(leer, base) {
  tablas <- armonizar_tablas(leer())
  faltantes <- poblacion_faltantes(base)
  for (i in seq_along(tablas)) {
    tablas[[i]] <- poblacion_normalizar_claves(tablas[[i]], base)
    if (!is.null(faltantes)) {
      tablas[[i]] <- limpiar_faltantes(tablas[[i]], faltantes)
    }
  }
  conversion <- poblacion_decidir_conversion(tablas, base)
  for (i in seq_along(tablas)) {
    tablas[[i]] <- aplicar_conversion(tablas[[i]], conversion)
  }
  unir_tablas(tablas)
}

# Motor -------------------------------------------------------------------------

poblacion_obtener <- function(programa, year, base, estado, tabla, nivel) {
  nombre_programa <- poblacion_programas()[[programa]]$nombre
  year <- poblacion_validar_year(programa, year)
  base <- poblacion_validar_base(programa, year, base)
  tabla <- poblacion_validar_tabla(programa, year, base, tabla)
  nivel_explicito <- !is.null(nivel)
  nivel <- poblacion_validar_nivel(programa, base, nivel)
  descripcion <- paste0(poblacion_nombre_base(base), " de ", year)

  archivos <- poblacion_archivos(programa, year, base)
  if (nrow(archivos) == 0) {
    detener(
      "El registro de archivos de esta versi\u00f3n del paquete (generado el ", registro_generado(),
      ") no incluye ", descripcion, " (", nombre_programa, "). ",
      "Actualiza importinegi o rep\u00f3rtalo en https://github.com/crenteriam/importinegi/issues"
    )
  }
  if (is.null(estado)) {
    estado <- poblacion_estado_omision(base, archivos)
    if (estado != "00") {
      poblacion_avisar_omision(base, archivos)
    }
  }
  claves <- poblacion_claves(estado, archivos, descripcion)
  if ("nacional" %in% nivel && !identical(claves, "00")) {
    detener("`nivel = \"nacional\"` solo est\u00e1 en el archivo nacional: usa estado = \"nacional\".")
  }
  # Si la base solo tiene archivo nacional, se descarga ese archivo y despues
  # se conservan las filas de las entidades pedidas.
  entidades <- NULL
  if (identical(archivos$clave, "00") && !identical(claves, "00")) {
    entidades <- claves
    claves <- "00"
  }
  seleccion <- archivos[match(claves, archivos$clave), ]
  if ("xlsx" %in% seleccion$formato) {
    requiere_paquete("readxl")
  }
  if (!is.null(entidades)) {
    nombres <- inegi_estados()$nombre[match(entidades, inegi_estados()$clave)]
    message(
      "El INEGI publica ", descripcion, " en un solo archivo nacional: se descarga ese archivo (",
      tamano_texto(seleccion$bytes), ") y se conservan las filas de ", paste(nombres, collapse = ", "), "."
    )
  }
  if (nrow(seleccion) > 1) {
    message(
      "Se descargar\u00e1n ", nrow(seleccion), " archivos del INEGI (",
      tamano_texto(seleccion$bytes), " en total)."
    )
  }
  if (base == "muestra" && sum(seleccion$bytes, na.rm = TRUE) > 500 * 1024^2) {
    message(
      "Leer esta muestra (", tamano_texto(seleccion$bytes), " comprimidos) puede requerir m\u00e1s memoria ",
      "de la que tienen muchas computadoras. Si solo necesitas algunas entidades, ind\u00edcalas en `estado`."
    )
  }

  patron <- if (is.na(tabla)) NULL else poblacion_tablas(programa, year, base)[[tabla]]
  datos <- poblacion_preparar(function() poblacion_leer_entidades(seleccion, patron, tabla), base)
  if (!is.null(entidades)) {
    datos <- poblacion_filtrar_entidades(datos, entidades)
  }
  if (!is.null(nivel)) {
    datos <- poblacion_filtrar_nivel(datos, base, nivel, nivel_explicito)
  }
  rownames(datos) <- NULL
  attr(datos, "fuente") <- seleccion$url
  datos
}
