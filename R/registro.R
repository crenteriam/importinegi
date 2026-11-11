# Registro de archivos del INEGI ------------------------------------------------
#
# El sitio del INEGI tiene un servicio (el que usa su propia pagina de
# descargas) que lista los archivos de microdatos de cada proyecto, con su ruta,
# sus formatos y su tamano. Estas funciones lo consultan y arman el registro de
# direcciones de descarga del paquete. El registro se genera con el script
# data-raw/registro.R; las funciones del paquete no dependen de este servicio
# para descargar.
#
# El servicio no esta documentado por el INEGI y puede cambiar sin aviso.

# Direccion del listado de archivos de un proyecto (tipo 4 = microdatos).
api_url_archivos <- function(id, tipo = 4) {
  paste0(
    inegi_base(),
    "/app/api/descarga/componente/descargamasiva/lista/archivoscompaginacion",
    "?tema=0&subtema=0&areaGeografica=0&proyecto=0&anio=0",
    "&tipodocto=", tipo,
    "&agrupacion=VG9kYXM=",
    "&idBiinegi=", id,
    "&desde=1&hasta=5000&textoBuscar=&ordenar=titulo&ingles=0&datosAbiertos=0&orden="
  )
}

# Identificador de un proyecto en el servicio de descargas. Se lee de la pagina
# del programa (por ejemplo "enoe/15ymas" o "ccpv/2010").
inegi_api_id <- function(pagina) {
  pagina <- gsub("^/+|/+$", "", pagina)
  html <- inegi_texto(paste0("/programas/", pagina, "/"))
  etiqueta <- regmatches(html, regexpr("<menu-gen[^>]*idm=\"[0-9]+\"", html))
  if (length(etiqueta) == 0) {
    detener(
      "No se encontr\u00f3 el identificador del proyecto en la p\u00e1gina ",
      inegi_url(paste0("/programas/", pagina, "/")), "."
    )
  }
  as.integer(sub(".*idm=\"([0-9]+)\"$", "\\1", etiqueta))
}

# Listado de archivos de microdatos de un proyecto, una fila por archivo y formato.
inegi_api_archivos <- function(id) {
  json <- inegi_texto(api_url_archivos(id))
  registros <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (!is.list(registros)) {
    detener("El INEGI no devolvi\u00f3 un listado de archivos v\u00e1lido para el proyecto ", id, ".")
  }
  api_a_tabla(registros)
}

# Tamano en texto ("26.1 MB", "1,021 KB") a bytes aproximados.
tamano_a_bytes <- function(tamano) {
  partes <- regmatches(tamano, regexec("^([0-9.,]+) ?(B|KB|MB|GB)$", tamano))
  vapply(partes, function(p) {
    if (length(p) != 3) {
      return(NA_real_)
    }
    factor <- c(B = 1, KB = 1024, MB = 1024^2, GB = 1024^3)[[p[3]]]
    as.numeric(gsub(",", "", p[2], fixed = TRUE)) * factor
  }, numeric(1))
}

# Convierte la respuesta del servicio en una tabla. Cada registro del servicio
# describe un archivo con uno o varios formatos, por ejemplo
#   formato   = "_csv.zip&384 KB&SIN_ESTANDAR|_dbf.zip&355 KB&SIN_ESTANDAR|"
#   extension = "csv|dbf|"
# y la direccion de descarga es /contenidos + pathLogico + el sufijo del formato.
api_a_tabla <- function(registros) {
  filas <- lapply(registros, function(registro) {
    campo <- function(nombre) {
      valor <- registro[[nombre]]
      if (is.null(valor)) "" else as.character(valor)
    }
    partes <- strsplit(strsplit(campo("formato"), "|", fixed = TRUE)[[1]], "&", fixed = TRUE)
    partes <- partes[lengths(partes) > 0]
    if (length(partes) == 0) {
      return(NULL)
    }
    sufijo <- vapply(partes, function(p) p[1], character(1))
    tamano <- vapply(partes, function(p) if (length(p) >= 2) p[2] else NA_character_, character(1))
    tamano <- gsub(" +", " ", trimws(tamano))

    extension <- strsplit(campo("extension"), "|", fixed = TRUE)[[1]]
    formato <- if (length(extension) == length(sufijo)) {
      extension
    } else {
      sub("^[._]", "", sub("\\.zip$", "", sufijo))
    }

    ruta <- campo("pathLogico")
    enlace <- campo("url")
    url <- if (nzchar(ruta)) {
      paste0(inegi_base(), "/contenidos", ruta, sufijo)
    } else if (nzchar(enlace)) {
      rep(inegi_url(enlace), length(sufijo))
    } else {
      rep(NA_character_, length(sufijo))
    }

    anio <- regmatches(campo("anioInformacion"), regexpr("[0-9]{4}", campo("anioInformacion")))
    data.frame(
      id_archivo = campo("idArchivo"),
      titulo = campo("titulo"),
      descripcion = campo("descripcion"),
      anio = if (length(anio) == 1) as.integer(anio) else NA_integer_,
      formato = formato,
      sufijo = sufijo,
      tamano = tamano,
      bytes = tamano_a_bytes(tamano),
      ruta = ruta,
      url = url,
      stringsAsFactors = FALSE
    )
  })
  filas <- filas[!vapply(filas, is.null, logical(1))]
  if (length(filas) == 0) {
    return(data.frame(
      id_archivo = character(), titulo = character(), descripcion = character(),
      anio = integer(), formato = character(), sufijo = character(),
      tamano = character(), bytes = numeric(), ruta = character(),
      url = character(), stringsAsFactors = FALSE
    ))
  }
  tabla <- do.call(rbind, filas)
  rownames(tabla) <- NULL
  tabla
}

# Construye el registro para una lista de proyectos. `proyectos` es una tabla
# con las columnas proyecto, edicion y pagina (ver data-raw/proyectos.csv).
# Si un proyecto falla, continua con los demas y lo reporta en el atributo
# "fallas" del resultado.
registro_construir <- function(proyectos, silencioso = FALSE) {
  necesarias <- c("proyecto", "edicion", "pagina")
  if (!is.data.frame(proyectos) || !all(necesarias %in% names(proyectos))) {
    detener("`proyectos` debe ser una tabla con las columnas: ", enumerar(necesarias), ".")
  }
  pausa <- getOption("importinegi.pausa", 0.5)
  tablas <- list()
  fallas <- character()

  for (i in seq_len(nrow(proyectos))) {
    proyecto <- as.character(proyectos$proyecto[i])
    edicion <- suppressWarnings(as.integer(as.character(proyectos$edicion[i])))
    pagina <- as.character(proyectos$pagina[i])
    etiqueta <- if (is.na(edicion)) proyecto else paste(proyecto, edicion)

    resultado <- tryCatch({
      id <- inegi_api_id(pagina)
      tabla <- inegi_api_archivos(id)
      if (nrow(tabla) == 0) {
        detener("el INEGI no lista archivos de microdatos para este proyecto")
      }
      data.frame(proyecto = proyecto, edicion = edicion, id_api = id, tabla,
                 stringsAsFactors = FALSE)
    }, error = function(e) e)

    if (inherits(resultado, "error")) {
      fallas[etiqueta] <- conditionMessage(resultado)
      if (!silencioso) {
        message("  ", etiqueta, ": FALLA. ", conditionMessage(resultado))
      }
    } else {
      tablas[[length(tablas) + 1]] <- resultado
      if (!silencioso) {
        message("  ", etiqueta, ": ", nrow(resultado), " archivos")
      }
    }
    if (i < nrow(proyectos)) {
      Sys.sleep(pausa)
    }
  }

  registro <- if (length(tablas) > 0) {
    do.call(rbind, tablas)
  } else {
    data.frame(proyecto = character(), edicion = integer(), id_api = integer(),
               api_a_tabla(list()), stringsAsFactors = FALSE)
  }
  rownames(registro) <- NULL
  attr(registro, "fallas") <- fallas
  registro
}
