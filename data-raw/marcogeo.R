# Genera los datos del Marco Geoestadistico que usa sig_marcogeoestadistico().
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/marcogeo.R           Consulta al INEGI y actualiza todo.
#   Rscript data-raw/marcogeo.R --local   Solo pasa los CSV a R/sysdata.rda,
#                                         sin conexion a internet.
#
# Sin --local requiere conexion a internet con acceso a
# https://www.inegi.org.mx y tarda varios minutos: sondea mas de cinco mil
# direcciones. Escribe tres archivos:
#
#   data-raw/marcogeo_productos.csv  Productos completos del Marco
#       Geoestadistico: clave, titulo, edicion, escala, ruta y bytes. Salen del
#       servicio de productos del INEGI (tema 213, coleccion 4328).
#   data-raw/marcogeo_capas.csv      Descarga por capa: una fila por año y
#       capa, con el tamaño del archivo de todo el pais (bytes) y las entidades
#       que no tienen archivo (faltan). Sale de sondear cada direccion.
#   R/sysdata.rda                    Las dos tablas, como `mg_productos` y
#       `mg_capas`. Los demas objetos del archivo (el registro de microdatos,
#       que genera data-raw/registro.R) se conservan.
#
# Una edicion nueva del Marco Geoestadistico aparece sola en los CSV, pero
# sig_marcogeoestadistico() no la ofrece hasta que se agrega a
# marcogeo_ediciones(), en R/marcogeo.R: el script avisa cuando encuentra una.

if (!file.exists("DESCRIPTION") || !file.exists("data-raw/marcogeo_productos.csv")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}
solo_local <- "--local" %in% commandArgs(trailingOnly = TRUE)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  loadNamespace("importinegi")
}
interna <- function(nombre) utils::getFromNamespace(nombre, "importinegi")

archivo_productos <- "data-raw/marcogeo_productos.csv"
archivo_capas <- "data-raw/marcogeo_capas.csv"

# Todo o nada: si algo falla no se escribe nada, para no sustituir los datos
# del paquete por unos incompletos.
detener_si_hay <- function(problemas) {
  if (length(problemas) > 0) {
    message("\nNo se modificaron los datos del Marco Geoestadistico:")
    message(paste0("  ", problemas, collapse = "\n"))
    quit(status = 1)
  }
}

if (!solo_local) {
  ediciones <- interna("marcogeo_ediciones")()
  problemas <- character()

  message("Consultando el servicio de productos del INEGI ...")
  productos <- interna("marcogeo_registro_productos")()
  sin_archivo <- productos$clave[is.na(productos$bytes)]
  if (length(sin_archivo) > 0) {
    problemas <- c(problemas, paste0(
      "La direccion de estos productos no responde con un zip: ", paste(sin_archivo, collapse = ", "), "."
    ))
  }
  ausentes <- setdiff(ediciones$clave, productos$clave)
  if (length(ausentes) > 0) {
    problemas <- c(problemas, paste0(
      "El servicio ya no lista estos productos de marcogeo_ediciones(): ", paste(ausentes, collapse = ", "), "."
    ))
  }
  # Antes del sondeo, que es la parte lenta
  detener_si_hay(problemas)

  # Años de la descarga por capa: los de las ediciones que la usan y, si el
  # INEGI ya lo publico, el siguiente.
  years <- sort(unique(ediciones$year[ediciones$via == "B"]))
  siguiente <- max(ediciones$year) + 1
  hay_siguiente <- !is.na(interna("marcogeo_sondear")(interna("marcogeo_url_capa")(siguiente, "00", "ENT")))
  if (hay_siguiente) {
    years <- c(years, siguiente)
  }
  capas <- interna("marcogeo_registro_capas")(years)
  sin_entidades <- capas$year[capas$capa == "ENT" & is.na(capas$bytes)]
  if (length(sin_entidades) > 0) {
    problemas <- c(problemas, paste0(
      "La descarga por capa ya no tiene estos a\u00f1os: ", paste(sin_entidades, collapse = ", "), "."
    ))
  }

  detener_si_hay(problemas)

  utils::write.csv(productos, archivo_productos, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  utils::write.csv(capas, archivo_capas, row.names = FALSE, fileEncoding = "UTF-8", na = "")

  nuevos <- productos[!productos$clave %in% ediciones$clave, ]
  if (nrow(nuevos) > 0) {
    message(
      "\nProductos que no estan en marcogeo_ediciones() (R/marcogeo.R). Para ofrecerlos hay que agregar ",
      "ahi su fila y, si se descargan como producto completo, sus capas en marcogeo_capas_producto():"
    )
    message(paste0("  ", nuevos$clave, "  ", nuevos$edicion, "  ", nuevos$titulo, collapse = "\n"))
  }
  if (hay_siguiente) {
    message("\nLa descarga por capa ya tiene el a\u00f1o ", siguiente, ", que no esta en marcogeo_ediciones().")
  }
}

# Datos internos ----------------------------------------------------------------

mg_productos <- utils::read.csv(
  archivo_productos, encoding = "UTF-8", na.strings = character(), stringsAsFactors = FALSE,
  colClasses = c(clave = "character", titulo = "character", edicion = "integer", escala = "character",
                 ruta = "character", bytes = "numeric")
)
mg_capas <- utils::read.csv(
  archivo_capas, encoding = "UTF-8", na.strings = character(), stringsAsFactors = FALSE,
  colClasses = c(year = "integer", capa = "character", bytes = "numeric", faltan = "character")
)

internos <- new.env()
if (file.exists("R/sysdata.rda")) {
  load("R/sysdata.rda", envir = internos)
}
internos$mg_productos <- mg_productos
internos$mg_capas <- mg_capas
save(list = sort(ls(internos)), envir = internos, file = "R/sysdata.rda", compress = "xz", version = 2)

message(
  "Datos del Marco Geoestadistico: ", nrow(mg_productos), " productos y ", nrow(mg_capas),
  " capas de ", length(unique(mg_capas$year)), " a\u00f1os en la descarga por capa."
)
