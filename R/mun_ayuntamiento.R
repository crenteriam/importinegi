mun_ayuntamiento = function(fuente = NA, year = NA){
  library(foreign)
  library(rio)

  # Abrir metadatos
  if (fuente == Na & year == NA) {shell.exec("http://www3.inegi.org.mx/rnm/index.php/catalog/208")}

  # Create Temporary Files
  fformat = "dbf"
  temp.cngmd = tempfile()
  zipdir = tempfile()



  # 2011


}
# Fin de la funcion


"""
### ADMINISTRACION PUBLICA - ESTRUCTURA ---------------------------------------------------------------
else if (fuente == "INSTITUC" | fuente == "INST_FSE" | fuente == "TITULARE") {
url.estructura = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Estructura_organizacional_cngmd2015_",fformat, ".zip")
utils::download.file(url.estructura, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "INSTITUC") {
raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\INSTITUC.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "INST_FSE") {
raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\INST_FSE.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "TITULARE") {
raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\TITULARE.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Estructura

### ADMINISTRACION PUBLICA - RECURSOS DE LAS ADMINISTRACIONES ---------------------------------------------------------------
else if (fuente == "REC_HUMA" | fuente == "REC_PRES" | fuente == "REC_MATE_1" | fuente == "REC_MATE_2"  | fuente == "REC_MATE_3") {
url.recursos   = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Recursos_cngmd2015_", fformat, ".zip")
utils::download.file(url.recursos, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "REC_HUMA") {
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_HUMA.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "REC_PRES") {
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_PRES.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "REC_MATE_1") {
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "REC_MATE_2") {
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "REC_MATE_3") {
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Recuros de las Administraciones

### ADMINISTRACION PUBLICA - GOBIERNO ELECTRÓNICO ---------------------------------------------------------------
else if (fuente == "GOB_ELEC") {
url.gobiernoelectronico = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Gob_elec_cngmd2015_",fformat, ".zip")
utils::download.file(url.gobiernoelectronico, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Gob_elec_cngmd2015_dbf\\Bases_de_datos\\GOB_ELEC.", fformat)
data = foreign::read.dbf(raw)
} # End of Administracion Publica - Gobierno Electronico

### ADMINISTRACION PUBLICA - PLANEACIÓN Y EVALUACIÓN ---------------------------------------------------------------
else if (fuente == "PLAN_EVA") {
url.planeacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Plan_evalua_cngmd2015_",fformat, ".zip")
utils::download.file(url.planeacion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Plan_evalua_cngmd2015_dbf\\Bases_datos\\PLAN_EVA.", fformat)
data = foreign::read.dbf(raw)
} # End of Administracion Publica - Planeacion y Evaluacion

### ADMINISTRACION PUBLICA - ACTIVIDADES ESTADÍSTICAS ---------------------------------------------------------------
else if (fuente == "ESTA_GEO") {
url.actividadesestadisticas = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Act_estadis_geograficas_cngmd2015_", fformat, ".zip")
utils::download.file(url.actividadesestadisticas, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Bases_datos\\ESTA_GEO.", fformat)
data = foreign::read.dbf(raw)
} # End of Administracion Publica - Actividades y Evaluacion

### ADMINISTRACION PUBLICA - TRÁMITES Y SERVICIOS ---------------------------------------------------------------
else if (fuente == "TRAM_SER") {
url.tramites = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Tramites_serv_cngmd2015_", fformat, ".zip")
utils::download.file(url.tramites, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Bases_datos\\TRAM_SER.", fformat)
data = foreign::read.dbf(raw)
} # End of Administracion Publica - Tramites y Servicios

### ADMINISTRACION PUBLICA - PROTECCIÓN CIVIL ---------------------------------------------------------------
else if (fuente == "CAPA_PER" | fuente == "ACC_PREV" | fuente == "CAM_CLIM" | fuente == "FEN_PERT_1" | fuente == "FEN_PERT_2" | fuente == "FEN_PERT_3") {
url.proteccioncivil = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Proteccion_civil_cngmd2015_",fformat, ".zip")
utils::download.file(url.proteccioncivil, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "CAPA_PER") {
raw  = paste0(zipdir, "\\Bases_datos\\CAPA_PER.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ACC_PREV") {
raw  = paste0(zipdir, "\\Bases_datos\\ACC_PREV.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "CAM_CLIM") {
raw  = paste0(zipdir, "\\Bases_datos\\CAM_CLIM.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "FEN_PERT_1") {
raw  = paste0(zipdir, "\\Bases_datos\\FEN_PERT_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "FEN_PERT_2") {
raw  = paste0(zipdir, "\\Bases_datos\\FEN_PERT_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "FEN_PERT_3") {
raw  = paste0(zipdir, "\\Bases_datos\\FEN_PERT_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Proteccion Civil

### ADMINISTRACION PUBLICA - SERVICIOS PÚBLICOS ---------------------------------------------------------------
else if (fuente == "ALUM_PUB" | fuente == "MER_CATM" | fuente == "PANT_CEM" | fuente == "CAPARJAR" | fuente == "RASTROS") {
url.serviciospublicos = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Serv_publicos_cngmd2015_",fformat, ".zip")
utils::download.file(url.serviciospublicos, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "ALUM_PUB") {
raw  = paste0(zipdir, "\\Bases_datos\\ALUM_PUB.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "MER_CATM") {
raw  = paste0(zipdir, "\\Bases_datos\\MER_CATM.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "PANT_CEM") {
raw  = paste0(zipdir, "\\Bases_datos\\PANT_CEM.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "CAPARJAR") {
raw  = paste0(zipdir, "\\Bases_datos\\CAPARJAR.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "RASTROS") {
raw  = paste0(zipdir, "\\Bases_datos\\RASTROS.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Servicios Publicos

### ADMINISTRACION PUBLICA - TRANSPARENCIA ---------------------------------------------------------------
else if (fuente == "TRAM_TRA" | fuente == "CAPA_TRA" | fuente == "INFO_PUB" | fuente == "ARCHIVO") {
url.transparencia = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Transparencia_cngmd2015_",fformat, ".zip")
utils::download.file(url.transparencia, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "TRAM_TRA") {
raw  = paste0(zipdir, "\\Transparencia_cngmd2015_dbf\\Bases_de_datos\\TRAM_TRA.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "CAPA_TRA") {
raw  = paste0(zipdir, "\\Transparencia_cngmd2015_dbf\\Bases_de_datos\\CAPA_TRA.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "INFO_PUB") {
raw  = paste0(zipdir, "\\Transparencia_cngmd2015_dbf\\Bases_de_datos\\INFO_PUB.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ARCHIVO") {
raw  = paste0(zipdir, "\\Transparencia_cngmd2015_dbf\\Bases_de_datos\\ARCHIVO.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Transparencia

### ADMINISTRACION PUBLICA - CONTROL INTERNO Y ANTICORRUPCIÓN ---------------------------------------------------------------
else if (fuente == "CAPA_ANT" | fuente == "CONTINT" | fuente == "ANTICORR") {
url.controlinterno = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Ctrl_inter_anticor_cngmd2015_",fformat, ".zip")
utils::download.file(url.controlinterno, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "CAPA_ANT") {
raw  = paste0(zipdir, "\\Ctrl_inter_anticor_cngmd2015_dbf\\Bases_de_datos\\CAPA_ANT.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "CONTINT") {
raw  = paste0(zipdir, "\\Ctrl_inter_anticor_cngmd2015_dbf\\Bases_de_datos\\CONTINT.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ANTICORR") {
raw  = paste0(zipdir, "\\Ctrl_inter_anticor_cngmd2015_dbf\\Bases_de_datos\\ANTICORR.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Control Interno y Anticorrupcion

### ADMINISTRACION PUBLICA - PARTICIPACIÓN CIUDADANA ---------------------------------------------------------------
else if (fuente == "PAR_CIUD" | fuente == "AUTO_AUX") {
url.participacionciudadana = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Participacion_cuidadana_cngmd2015_",fformat, ".zip")
utils::download.file(url.participacionciudadana, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "PAR_CIUD") {
raw  = paste0(zipdir, "\\Participacion_cuidadana_cngmd2015_dbf\\Bases_datos\\PAR_CIUD.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "AUTO_AUX") {
raw  = paste0(zipdir, "\\Participacion_cuidadana_cngmd2015_dbf\\Bases_datos\\AUTO_AUX.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Participacion Ciudadana

### ADMINISTRACION PUBLICA - MARCO REGULATORIO ---------------------------------------------------------------
else if (fuente == "M_REGULA_1" | fuente == "M_REGULA_2" | fuente == "M_REGULA_3" | fuente == "ASOC_INT" | fuente == "ASO_INMA" | fuente == "ASO_INCF" | fuente == "ASO_INMC") {
url.marcoregulatorio = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Marco_regulatorio_cngmd2015_",fformat, ".zip")
utils::download.file(url.marcoregulatorio, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "M_REGULA_1") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\M_REGULA_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "M_REGULA_2") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\M_REGULA_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "M_REGULA_3") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\M_REGULA_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ASOC_INT") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\ASOC_INT.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ASO_INMA") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\ASO_INMA.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ASO_INCF") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\ASO_INCF.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "ASO_INMC") {
raw  = paste0(zipdir, "\\Marco_regulatorio_cngmd2015_dbf\\Bases_datos\\ASO_INMC.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Administracion Publica - Marco Regulatorio

### CATASTRO - ESTRUCTURA ORGANIZACIONAL ---------------------------------------------------------------
else if (fuente == "EST_ORG") {
url.estructura = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Estructura_organizacional_catastro_cngmd2015_",fformat, ".zip")
utils::download.file(url.estructura, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Estructura_organizacional_catastro_cngmd2015_dbf\\Estructura_organizacional_catastro_cngmd2015_dbf\\Bases_de_datos\\EST_ORG.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Estructura Organizacional

### CATASTRO - CAPACITACIÓN PERSONAL -------------------------------------------------------------------|||||||||||||||||||||||||||||
else if (fuente == "CAPAPERS") {
url.capacitacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Capacitacion_catastro_cngmd2015_",fformat, ".zip")
utils::download.file(url.capacitacion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\CapacitaciÓn_catastro_cngmd2015_dbf\\Capacitación_catastro_cngmd2015_dbf\\Bases_de_datos\\CAPAPERS.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Capacitacion Personal

### CATASTRO - MODERNIZACIÓN CATASTRAL -----------------------------------------------------------------|||||||||||||||||||||||||||||
else if (fuente == "MODR_CAT") {
url.modernizacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Programa_modernizacion_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.modernizacion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Programa_modernizaciÓn_catastral_cngmd2015_dbf\\Programa_modernizaciÓn_catastral_cngmd2015_dbf\\Bases_de_datos\\MODR_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Modernizacion Catastral

### CATASTRO - TIC CATASTRAL --------------------------------------------------------------------||||||||||||||||||
else if (fuente == "TEC_INF") {
url.tic = "http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Tecnologias_informacion_catastral_cngmd2015_dbf.zip"
utils::download.file(url.tic, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = `Encoding<-`(raw,"UTF-8")
raw  = paste0(zipdir, "\\Tecnologías_informacion_catastral_cngmd2015_dbf\\Tecnologías_informacion_catastral_cngmd2015_dbf\\Bases_de_datos\\TEC_INF.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro TIC Catastral

### CATASTRO - RESGUARDO INFORMACIÓN -------------------------------------------------------------------
else if (fuente == "RESG_INF") {
url.resguardo = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Resguardo_informacion_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.resguardo, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Resguardo_informacion_catastral_cngmd2015_dbf\\Resguardo_informacion_catastral_cngmd2015_dbf\\Bases_de_datos\\RESG_INF.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Resguardo Informacion

### CATASTRO - PROCESOS CATASTRALES --------------------------------------------------------------------
else if (fuente == "PROC_CAT") {
url.procesos = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Procesos_catastrales_cngmd2015_",fformat, ".zip")
utils::download.file(url.procesos, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Procesos_catastrales_cngmd2015_dbf\\Procesos_catastrales_cngmd2015_dbf\\Bases_de_datos\\PROC_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Procesos Catastrales

### CATASTRO - PADRON CATASTRAL ------------------------------------------------------------------------
else if (fuente == "PADR_CAT") {
url.padron = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Padron_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.padron, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Padron_catastral_cngmd2015_dbf\\Bases_de_datos\\PADR_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Padron Catastral

### CATASTRO - CARTOGRAFIA CATASTRAL -------------------------------------------------------------------
else if (fuente == "CART_CAT") {
url.cartografia = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Cartografia_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.cartografia, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Cartografia_catastral_cngmd2015_dbf\\Cartografia_catastral_cngmd2015_dbf\\Bases_de_datos\\CART_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Cartografia Catastral

### CATASTRO - INSPECCIÓN DE CAMPO ---------------------------------------------------------------------
else if (fuente == "INSP_CAM") {
url.inspeccion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Inspecciones_de_campo_cngmd2015_",fformat, ".zip")
utils::download.file(url.inspeccion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Inspecciones_de_campo_cngmd2015_dbf\\Inspecciones_de_campo_cngmd2015_dbf\\Bases_de_datos\\INSP_CAM.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Inspección de Campo

### CATASTRO - IMPUESTO PREDIAL ------------------------------------------------------------------------
else if (fuente == "IMP_PRED") {
url.predial = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Impuesto_predial_cngmd2015_",fformat, ".zip")
utils::download.file(url.predial, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Impuesto_predial_cngmd2015_dbf\\Impuesto_predial_cngmd2015_dbf\\Bases_de_datos\\IMP_PRED.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Impuesto Predial

### CATASTRO - VALUACIÓN CATASTRAL ---------------------------------------------------------------------
else if (fuente == "VAL_CAT") {
url.valuacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Valuacion_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.valuacion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Valuacion_catastral_cngmd2015_dbf\\Valuacion_catastral_cngmd2015_dbf\\Bases_de_datos\\VAL_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Valuación Catastral

### CATASTRO - VINCULACIÓN CATASTRAL -------------------------------------------------------------------
else if (fuente == "VINC_CAT") {
url.vinculacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Vinculacion_catastral_cngmd2015_",fformat, ".zip")
utils::download.file(url.vinculacion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\Vinculacion_catastral_cngmd2015_dbf\\Vinculacion_catastral_cngmd2015_dbf\\Bases_de_datos\\VINC_CAT.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Vinculación Catastral

### SEGURIDAD PÚBLICA - SEGURIDAD PÚBLICA --------------------------------------------------------------
else if (fuente == "SPEJFUES" | fuente == "SPINFEST" | fuente == "SPINFRAE") {
url.seguridadpublica = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/sp_Seguridad_publica_cngmd2015_",fformat, ".zip")
utils::download.file(url.seguridadpublica, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "SPEJFUES") {
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPEJFUES.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPINFEST") {
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPINFEST.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPINFRAE") {
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPINFRAE.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Catastro Seguridad Pública

### SEGURIDAD PÚBLICA - RECURSOS HUMANOS ---------------------------------------------------------------
else if (fuente == "SPRE_HUM_1" | fuente == "SPRE_HUM_2" | fuente == "SPRE_HUM_3" | fuente == "SPRE_HUM_4" | fuente == "SPRE_HUM_5" | fuente == "SPRE_HUM_6") {
url.rh = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/sp_Recursos_cngmd2015_",fformat, ".zip")
utils::download.file(url.rh, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "SPRE_HUM_1") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPRE_HUM_2") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPRE_HUM_3") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPRE_HUM_4") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_4.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPRE_HUM_5") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_5.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "SPRE_HUM_6") {
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_6.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Catastro Recursos Humanos

### SEGURIDAD PÚBLICA - EJERCICIO DE LA FUNCIÓN --------------------------------------------------------
else if (fuente == "EJER_FNC_1" | fuente == "EJER_FNC_2" | fuente == "EJER_FNC_3" | fuente == "EJER_FNC_4" | fuente == "EJER_FNC_5" | fuente == "EJER_FNC_6") {
url.funcion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/SP_Ejercicio_funcion_cngmd2015_",fformat, ".zip")
utils::download.file(url.funcion, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "EJER_FNC_1") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJER_FNC_2") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJER_FNC_3") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJER_FNC_4") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_4.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJER_FNC_5") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_5.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJER_FNC_6") {
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_6.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Catastro Ejercicio de la Función

### JUSTICIA MUNICIPAL - RECURSOS HUMANOS --------------------------------------------------------------
else if (fuente == "RE_HU_JM_1" | fuente == "RE_HU_JM_2" | fuente == "RE_HU_JM_3") {
url.rh = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m4/JM_Recursos_cngmd2015_",fformat, ".zip")
utils::download.file(url.rh, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "RE_HU_JM_1") {
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "RE_HU_JM_2") {
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "RE_HU_JM_3") {
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Catastro Recursos Humanos

### JUSTICIA MUNICIPAL - INFRAESTRUCTURA ---------------------------------------------------------------
else if (fuente == "JC_INFRA") {
url.infraestructura = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m4/JM_Infraestuctura_cngmd2015_",fformat, ".zip")
utils::download.file(url.infraestructura, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)
raw  = paste0(zipdir, "\\JM_Infraestuctura_cngmd2015_dbf\\Bases_de_datos\\JC_INFRA.", fformat)
data = foreign::read.dbf(raw)
} # End of Catastro Infraestructura

### JUSTICIA MUNICIPAL - EJERCICIO DE LA FUNCIÓN -------------------------------------------------------
else if (fuente == "EJ_FJMU_1" | fuente == "EJ_FJMU_2" | fuente == "EJ_FJMU_3" | fuente == "EJ_FJMU_4" | fuente == "EJ_FJMU_5" | fuente == "EJ_FJMU_6") {
url.ejercicio = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m4/JM_Ejercicio_funcion_cngmd2015_",fformat, ".zip")
utils::download.file(url.ejercicio, temp.cngmd)
utils::unzip(temp.cngmd, exdir = zipdir)

if (fuente == "EJ_FJMU_1") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_1.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJ_FJMU_2") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_2.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJ_FJMU_3") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_3.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJ_FJMU_4") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_4.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJ_FJMU_5") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_5.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
if (fuente == "EJ_FJMU_6") {
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_6.", fformat)
data = foreign::read.dbf(raw)
} # End Nest-Unzip
} # End of Catastro Ejercicio de la Funcion
"""
