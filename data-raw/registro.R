# Genera el registro de archivos de microdatos del INEGI que usa el paquete.
#
# Uso, desde la carpeta raiz del paquete:
#
#   Rscript data-raw/registro.R
#
# Requiere conexion a internet con acceso a https://www.inegi.org.mx.
#
# Lee la lista de proyectos de data-raw/proyectos.csv, consulta el servicio de
# descargas del INEGI y escribe dos archivos:
#
#   data-raw/registro_inegi.csv  El registro en texto, para revisar los cambios.
#   R/sysdata.rda                El registro que se distribuye con el paquete.
#
# Para agregar un proyecto o una edicion, anade una fila a proyectos.csv
# (pagina es la ruta de https://www.inegi.org.mx/programas/<pagina>/) y vuelve
# a ejecutar el script.

if (!file.exists("DESCRIPTION") || !file.exists("data-raw/proyectos.csv")) {
  stop("Ejecuta este script desde la carpeta raiz del paquete importinegi.")
}

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  loadNamespace("importinegi")
}
registro_construir <- utils::getFromNamespace("registro_construir", "importinegi")

proyectos <- utils::read.csv("data-raw/proyectos.csv", colClasses = "character",
                             stringsAsFactors = FALSE)
message("Consultando ", nrow(proyectos), " proyectos en el servicio de descargas del INEGI ...")
registro <- registro_construir(proyectos)
fallas <- attr(registro, "fallas")

# Todo o nada: si algun proyecto falla no se escribe nada, para no sustituir
# el registro del paquete por uno incompleto.
if (length(fallas) > 0 || nrow(registro) == 0) {
  message("\nNo se modifico el registro: fallaron ", length(fallas), " de ",
          nrow(proyectos), " proyectos.")
  message(paste0("  ", names(fallas), ": ", fallas, collapse = "\n"))
  quit(status = 1)
}

orden <- order(registro$proyecto, registro$edicion, registro$ruta, registro$url,
               registro$formato, na.last = TRUE)
registro <- registro[orden, ]
rownames(registro) <- NULL
attr(registro, "fallas") <- NULL

utils::write.csv(registro, "data-raw/registro_inegi.csv", row.names = FALSE,
                 fileEncoding = "UTF-8", na = "")

registro_archivos <- registro
attr(registro_archivos, "generado") <- as.character(Sys.Date())
save(registro_archivos, file = "R/sysdata.rda", compress = "xz", version = 2)

message(
  "Registro generado: ", nrow(registro), " archivos de ",
  length(unique(paste(registro$proyecto, registro$edicion))), " proyectos."
)
resumen <- as.data.frame(table(proyecto = registro$proyecto, formato = registro$formato),
                         stringsAsFactors = FALSE)
resumen <- resumen[resumen$Freq > 0, ]
print(stats::xtabs(Freq ~ proyecto + formato, data = resumen))
