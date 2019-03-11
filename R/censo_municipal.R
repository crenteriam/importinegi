CensoMunicipal = function(fuente = NA, year = NA, datos = NA){
library(foreign)
  library(rio)

# Open Metadata
if (is.na(fuente) & is.na(year) & is.na(datos)) {shell.exec("http://www3.inegi.org.mx/rnm/index.php/catalog/208")}
else {}

# Renombrar carpetas y URL  ---------------------------------------------------------------------------
## URL
if      (year == 2011) {
    url.datos.comis = "ayu_comisiones_cngmd"
    url.datos.estru = "ayu_estructura_cngmd"
    url.datos.apest = "apmd_estructura_cngmd"
    url.datos.apfun = "apmd_funciones_cngmd"
    url.datos.apmar = "apmd_marco_cngmd"
    url.datos.appar = "apmd_participacion_cngmd"
    url.datos.aprec = "apmd_recursos_cngmd"
    url.datos.aptra = "apmd_tramites_cngmd"
    url.datos.aptrn = "apmd_transparencia_cngmd"
    folder.comis = "/Comisiones e iniciativas de los Ayuntamientos/Base de datos/"
    folder.estru = "/Estructura de los Ayuntamientos/Base de datos/"
    folder.apest = "/Estructura organizacional/Base de datos/"
    folder.apfun = "/Funciones Especificas/Base de datos/"
    folder.apmar = "/Marco Regulatorio/Base de datos/"
    folder.appar = "/Participacion Ciudadana/Base de datos/"
    folder.aprec = "/Recursos de las Administraciones Publicas Municipales/Base de datos/"
    folder.aptra = "/Tramites y Servicios y Gobierno Electronico/Base de datos/"
    folder.aptrn = "/Transparencia y Anticorrupcion/Base de datos/"
}
else if (year == 2013) {
    url.datos.comis = "Com_ini_aytto_cngmd"
    url.datos.estru = "Estruct_aytto_cngmd"
}
else if (year == 2015) {
  url.datos.comis = "m1/Com_ini_aytto_cngmd"
  url.datos.estru = "m1/Estruct_aytto_cngmd"
}
else if (year == 2017) {
  url.datos.comis = "m1/Actividad_ayuntami_cngmd"
  url.datos.estru = "m1/Integrantes_ayuntami_cngmd"
}
else {}
if      (fuente == "ayuntamiento" & datos == "comision")   {url.complemento = url.datos.comis}
else if (fuente == "ayuntamiento" & datos == "estructura") {url.complemento = url.datos.estru}

## Folders  ------------------------------------------------------------------------------------------
if (year == 2013) {
  folder.comis = "/Com_ini_aytto/Bases_de_datos/"
  folder.estru = "/Estruct_aytto/Bases_de_datos/"
}
else if (year == 2015) {
  folder.comis = paste0("/Com_ini_aytto_cngmd", year, "_dbf/Bases_de_datos/")
  folder.estru = paste0("/Estruct_aytto_cngmd", year, "_dbf/Bases_de_datos/")
}
else if (year == 2017) {
  folder.comis = "/Bases_Datos/CABILDOS."
  folder.estru = "/Bases_Datos/AYUNTAMI."
}
else {}

# Combine into folder.complement
if      (fuente == "ayuntamiento" & datos == "comision")   {folder.complemento = folder.comis}
else if (fuente == "ayuntamiento" & datos == "estructura") {folder.complemento = folder.estru}
else {}

### Descargar  -----------------------------------------------------------------------------------------
#else {stop("Nombre de fuente no reconocida en la base de datos. Revisa el Diccionario de Datos utilizando la función CatalogoInegi()")}

  # Temp files
  fformat    = "dbf"
  temp.cngmd = tempfile()
  zipdir     = tempdir()

  # Descargar
  url.base = paste0("http://www.beta.inegi.org.mx/contenidos/programas/cngmd/", year, "/microdatos/", url.complemento, year, "_", fformat, ".zip")
  utils::download.file(url.base, temp.cngmd)
  utils::unzip(temp.cngmd, exdir = zipdir)
  list_dataraw = as.list(list.files(paste0(zipdir, folder.complemento)))

  # Read all .dbf files in the folder
  for (i in list_dataraw) {
    Object = read.dbf(paste0(zipdir, folder.complemento, i))
    Object$UBIC_GEO = paste0(formatC(as.character(Object$UBIC_GEO), width=5, flag="0"))
    assign(paste0("dt.", tools::file_path_sans_ext(i)), Object)
  }
  list_dataclean = mget(ls(pattern = "dt."))

# Return Object
return(list_dataclean)
} # End of Function
