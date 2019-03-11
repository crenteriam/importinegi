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

# Ayuntamiento
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2011/microdatos/ayu_comisiones_cngmd2011_dbf.zip"
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2011/microdatos/ayu_estructura_cngmd2011_dbf.zip"
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2013/microdatos/Estruct_aytto_cngmd2013_dbf.zip"
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2013/microdatos/Com_ini_aytto_cngmd2013_dbf.zip"
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2017/microdatos/m1/Integrantes_ayuntami_cngmd2017_dbf.zip"
#"http://www.beta.inegi.org.mx/contenidos/programas/cngmd/2017/microdatos/m1/Actividad_ayuntami_cngmd2017_dbf.zip"

## Folders  ------------------------------------------------------------------------------------------
if      (year == 2011) {
  folder.comis = "\\Comisiones e iniciativas de los Ayuntamientos\\Base de datos\\ACT_AYUN."
  folder.estru = "\\Estructura de los Ayuntamientos\\Base de datos\\EST_AYUN."
}
else if (year == 2013) {
  folder.comis = "\\Com_ini_aytto\\Bases_de_datos\\ACT_AYUN."
  folder.estru = "\\Estruct_aytto\\Bases_de_datos\\EST_AYUN."
}
else if (year == 2015) {
  folder.comis = paste0("\\Com_ini_aytto_cngmd", year, "_dbf\\Bases_de_datos\\ACT_AYUN.")
  folder.estru = paste0("\\Estruct_aytto_cngmd", year, "_dbf\\Bases_de_datos\\EST_AYUN.")
}
else if (year == 2017) {
  folder.comis = "\\Bases_Datos\\CABILDOS."
  folder.estru = "\\Bases_Datos\\AYUNTAMI."
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
  raw  = paste0(zipdir, folder.complemento, fformat)
  data = foreign::read.dbf(raw, as.is = TRUE)

### Final Editions ------------------------------------------------------------------------------------
# Fix UBIC_GEO
data$UBIC_GEO = paste0(formatC(as.character(data$UBIC_GEO), width=5, flag="0"))
return(data)
} # End of Function
