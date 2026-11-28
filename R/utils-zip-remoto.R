# Utilidades internas: indice de archivos zip, locales y remotos ---------------
#
# Un zip termina con un indice (el "directorio central") que lista sus archivos:
# nombre, metodo de compresion, tamano y posicion dentro del zip. Leerlo
# directamente sirve para tres cosas que utils::unzip() no resuelve:
#
#   * conocer el contenido de un zip sin que importe la codificacion de los
#     nombres de sus archivos (algunos zip del INEGI no usan UTF-8);
#   * avisar con claridad cuando un archivo usa un metodo de compresion que R
#     no puede abrir (unzip() solo dice "zip file is corrupt");
#   * descargar una sola parte de un zip remoto. Varios productos del INEGI son
#     un zip de 2 a 3 GB que contiene, sin comprimir, un zip por entidad: con
#     el indice se sabe en que bytes esta cada uno y se pide solo ese tramo.

# Bytes del final de un zip que se leen para buscar su indice. El registro que
# cierra un zip mide 22 bytes mas un comentario de hasta 65 535; en los zip del
# INEGI, el indice completo suele caber tambien en ese tramo. Es una funcion
# para poder sustituirla en las pruebas.
zip_tamano_cola <- function() {
  66000
}

# Numero entero sin signo guardado con el byte menos significativo primero.
zip_numero <- function(bytes) {
  sum(as.numeric(bytes) * 256^(seq_along(bytes) - 1))
}

# Campo de `n` bytes que empieza `desplazamiento` bytes despues de la posicion
# `inicio` (la del primer byte del registro, contando desde 1).
zip_campo <- function(bytes, inicio, desplazamiento, n) {
  zip_numero(bytes[(inicio + desplazamiento):(inicio + desplazamiento + n - 1)])
}

zip_firma <- function(bytes, inicio, firma) {
  inicio >= 1 && inicio + 3 <= length(bytes) &&
    identical(bytes[inicio:(inicio + 3)], as.raw(firma))
}

# Nombre del metodo de compresion de un archivo del zip, para los mensajes.
zip_metodo <- function(metodo) {
  nombres <- c("0" = "sin comprimir", "8" = "Deflate", "9" = "Deflate64", "12" = "BZIP2", "14" = "LZMA")
  nombre <- nombres[as.character(metodo)]
  ifelse(is.na(nombre), paste("m\u00e9todo", metodo), nombre)
}

# Interpreta el indice de un zip. `cola` son los ultimos bytes del archivo,
# `tamano` es el tamano total del archivo y `leer(desde, cuantos)` devuelve
# `cuantos` bytes a partir de la posicion `desde` (contando desde 0), para
# cuando el indice no cabe en la cola.
#
# Devuelve una tabla con una fila por archivo: `nombre` (con "/" como separador
# de carpetas y "_" en lugar de cada byte que no es ASCII), `original` (el
# nombre tal como esta en el zip, o NA si no es ASCII), `metodo`, `comprimido`,
# `tamano` y `posicion` (la de su encabezado dentro del zip, desde 0).
zip_indice_interpretar <- function(cola, tamano, leer) {
  inicio_cola <- tamano - length(cola)
  fin <- NA
  candidatos <- which(cola == as.raw(0x50))
  for (candidato in rev(candidatos[candidatos <= length(cola) - 21])) {
    if (zip_firma(cola, candidato, c(0x50, 0x4b, 0x05, 0x06))) {
      fin <- candidato
      break
    }
  }
  if (is.na(fin)) {
    detener("El archivo no es un zip v\u00e1lido: no se encontr\u00f3 su \u00edndice.")
  }
  tamano_indice <- zip_campo(cola, fin, 12, 4)
  posicion_indice <- zip_campo(cola, fin, 16, 4)
  if (zip_campo(cola, fin, 10, 2) == 65535 || tamano_indice == 4294967295 ||
      posicion_indice == 4294967295) {
    # Zip de mas de 4 GB o de mas de 65 535 archivos: los datos del indice
    # estan en un registro aparte.
    localizador <- fin - 20
    if (!zip_firma(cola, localizador, c(0x50, 0x4b, 0x06, 0x07))) {
      detener("El archivo no es un zip v\u00e1lido: su \u00edndice est\u00e1 incompleto.")
    }
    registro <- leer(zip_campo(cola, localizador, 8, 8), 56)
    tamano_indice <- zip_campo(registro, 1, 40, 8)
    posicion_indice <- zip_campo(registro, 1, 48, 8)
  }
  indice <- if (posicion_indice >= inicio_cola) {
    cola[(posicion_indice - inicio_cola + 1):(posicion_indice - inicio_cola + tamano_indice)]
  } else {
    leer(posicion_indice, tamano_indice)
  }

  filas <- list()
  inicio <- 1
  while (inicio + 45 <= length(indice) && zip_firma(indice, inicio, c(0x50, 0x4b, 0x01, 0x02))) {
    metodo <- zip_campo(indice, inicio, 10, 2)
    comprimido <- zip_campo(indice, inicio, 20, 4)
    descomprimido <- zip_campo(indice, inicio, 24, 4)
    largo_nombre <- zip_campo(indice, inicio, 28, 2)
    largo_extra <- zip_campo(indice, inicio, 30, 2)
    largo_comentario <- zip_campo(indice, inicio, 32, 2)
    posicion <- zip_campo(indice, inicio, 42, 4)
    bytes <- indice[seq_len(largo_nombre) + inicio + 45]

    # Los tamanos y posiciones de 4 GB o mas van en un campo adicional.
    campo <- inicio + 46 + largo_nombre
    fin_extra <- campo + largo_extra
    while (campo + 4 <= fin_extra) {
      largo <- zip_campo(indice, campo, 2, 2)
      if (zip_campo(indice, campo, 0, 2) == 1) {
        valor <- campo + 4
        if (descomprimido == 4294967295) {
          descomprimido <- zip_campo(indice, valor, 0, 8)
          valor <- valor + 8
        }
        if (comprimido == 4294967295) {
          comprimido <- zip_campo(indice, valor, 0, 8)
          valor <- valor + 8
        }
        if (posicion == 4294967295) {
          posicion <- zip_campo(indice, valor, 0, 8)
        }
      }
      campo <- campo + 4 + largo
    }

    ascii <- all(bytes < as.raw(0x80))
    legible <- bytes
    legible[legible >= as.raw(0x80)] <- as.raw(0x5f)
    legible[legible == as.raw(0x5c)] <- as.raw(0x2f)
    filas[[length(filas) + 1]] <- data.frame(
      nombre = rawToChar(legible),
      original = if (ascii) rawToChar(bytes) else NA_character_,
      metodo = metodo,
      comprimido = comprimido,
      tamano = descomprimido,
      posicion = posicion,
      stringsAsFactors = FALSE
    )
    inicio <- inicio + 46 + largo_nombre + largo_extra + largo_comentario
  }
  if (length(filas) == 0) {
    return(data.frame(
      nombre = character(), original = character(), metodo = numeric(), comprimido = numeric(),
      tamano = numeric(), posicion = numeric(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, filas)
}

# Indice de un zip guardado en disco.
zip_indice_local <- function(zip) {
  if (!es_zip(zip)) {
    detener("El archivo ", basename(zip), " no es un zip v\u00e1lido.")
  }
  tamano <- file.size(zip)
  conexion <- file(zip, open = "rb")
  on.exit(close(conexion), add = TRUE)
  leer <- function(desde, cuantos) {
    seek(conexion, where = desde, origin = "start")
    readBin(conexion, what = "raw", n = cuantos)
  }
  cuantos <- min(tamano, zip_tamano_cola())
  zip_indice_interpretar(leer(tamano - cuantos, cuantos), tamano, leer)
}

# Se detiene si alguno de los archivos del zip usa un metodo de compresion que
# utils::unzip() no puede abrir (solo abre los metodos 0 y 8).
zip_exigir_metodos <- function(indice, zip) {
  ajenos <- indice[!indice$metodo %in% c(0, 8), ]
  if (nrow(ajenos) > 0) {
    detener(
      "R no puede descomprimir ", enumerar(basename(ajenos$nombre), maximo = 6), " de ", basename(zip),
      ": el INEGI lo comprimi\u00f3 con ", zip_metodo(ajenos$metodo[1]), ", un m\u00e9todo que ",
      "utils::unzip() no maneja. Puedes descomprimir el archivo con otro programa y leerlo con sf::st_read()."
    )
  }
  invisible(TRUE)
}

# Zip remotos ---------------------------------------------------------------------

# Posicion en bytes como texto, sin notacion cientifica.
zip_posicion <- function(valor) {
  sprintf("%.0f", valor)
}

# Pide al servidor un tramo de un archivo (cabecera Range). `rango` es el texto
# que sigue a "bytes=": "0-99", o "-66000" para los ultimos 66 000 bytes.
zip_remoto_solicitar <- function(url, rango, destino = NULL, progreso = FALSE) {
  solicitud <- httr2::req_headers(inegi_solicitud(url), Range = paste0("bytes=", rango))
  if (progreso && interactive()) {
    solicitud <- httr2::req_progress(solicitud)
  }
  respuesta <- inegi_ejecutar(solicitud, destino = destino)
  estado <- httr2::resp_status(respuesta)
  if (estado != 206) {
    detener(
      "El INEGI respondi\u00f3 con el c\u00f3digo ", estado, " a la descarga parcial de ", url, ". ",
      "Es posible que el archivo ya no exista o que el servidor haya dejado de aceptar descargas ",
      "parciales; en ese caso har\u00eda falta descargar el archivo completo. Puedes reportarlo en ",
      "https://github.com/crenteriam/importinegi/issues"
    )
  }
  respuesta
}

# Indice de un zip remoto. Solo descarga el final del archivo. Se guarda en la
# memoria de la sesion, para no pedirlo en cada llamada.
zip_remoto_indice <- function(url) {
  url <- inegi_url(url)
  clave <- paste0("zip_indice:", url)
  guardado <- memoria_obtener(clave)
  if (!is.null(guardado)) {
    return(guardado)
  }
  respuesta <- zip_remoto_solicitar(url, paste0("-", zip_posicion(zip_tamano_cola())))
  cola <- httr2::resp_body_raw(respuesta)
  rango <- httr2::resp_header(respuesta, "Content-Range")
  tamano <- suppressWarnings(as.numeric(sub("^.*/", "", if (is.null(rango)) "" else rango)))
  if (is.na(tamano)) {
    detener("El INEGI no indic\u00f3 el tama\u00f1o de ", url, "; no se puede leer su \u00edndice.")
  }
  leer <- function(desde, cuantos) {
    tramo <- paste0(zip_posicion(desde), "-", zip_posicion(desde + cuantos - 1))
    httr2::resp_body_raw(zip_remoto_solicitar(url, tramo))
  }
  memoria_guardar(clave, zip_indice_interpretar(cola, tamano, leer))
}

# Descarga uno de los archivos de un zip remoto, que debe estar guardado sin
# comprimir. `entrada` es su fila en el indice. Devuelve la ruta local.
zip_remoto_entrada <- function(url, entrada, destino, silencioso = FALSE) {
  url <- inegi_url(url)
  if (entrada$metodo != 0) {
    detener(
      basename(entrada$nombre), " est\u00e1 comprimido dentro de ", basename(url),
      " y no se puede descargar por separado."
    )
  }
  encabezado <- httr2::resp_body_raw(zip_remoto_solicitar(
    url, paste0(zip_posicion(entrada$posicion), "-", zip_posicion(entrada$posicion + 29))
  ))
  if (!zip_firma(encabezado, 1, c(0x50, 0x4b, 0x03, 0x04))) {
    detener("El \u00edndice de ", basename(url), " no corresponde a su contenido.")
  }
  inicio <- entrada$posicion + 30 + zip_campo(encabezado, 1, 26, 2) + zip_campo(encabezado, 1, 28, 2)

  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  parcial <- paste0(destino, ".parcial")
  on.exit(unlink(parcial), add = TRUE)
  if (!silencioso) {
    message(
      "Descargando ", basename(entrada$nombre), " (", tamano_texto(entrada$comprimido),
      "; es una parte de ", basename(url), ") ..."
    )
  }
  zip_remoto_solicitar(
    url, paste0(zip_posicion(inicio), "-", zip_posicion(inicio + entrada$comprimido - 1)),
    destino = parcial, progreso = !silencioso
  )
  if (!file.exists(parcial) || file.size(parcial) != entrada$comprimido) {
    detener("La descarga de ", basename(entrada$nombre), " qued\u00f3 incompleta. Intenta de nuevo.")
  }
  unlink(destino)
  if (!file.rename(parcial, destino)) {
    file.copy(parcial, destino, overwrite = TRUE)
  }
  invisible(destino)
}
