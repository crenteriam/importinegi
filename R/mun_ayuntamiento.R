# Administracion



### ADMINISTRACION PUBLICA - ESTRUCTURA ---------------------------------------------------------------

raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\INSTITUC.", fformat)
raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\INST_FSE.", fformat)
raw  = paste0(zipdir, "\\Estructura_organizacional_cngmd2015_dbf\\Bases_de_datos\\TITULARE.", fformat)

### ADMINISTRACION PUBLICA - RECURSOS DE LAS ADMINISTRACIONES ---------------------------------------------------------------
url.recursos   = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Recursos_cngmd2015_", fformat, ".zip")
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_HUMA.", fformat)
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_PRES.", fformat)
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_1.", fformat)
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_2.", fformat)
raw  = paste0(zipdir, "\\Recursos_cngmd2015_dbf\\Bases_datos\\REC_MATE_3.", fformat)

### ADMINISTRACION PUBLICA - GOBIERNO ELECTRÓNICO ---------------------------------------------------------------
url.gobiernoelectronico = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Gob_elec_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\Gob_elec_cngmd2015_dbf\\Bases_de_datos\\GOB_ELEC.", fformat)

### ADMINISTRACION PUBLICA - PLANEACIÓN Y EVALUACIÓN ---------------------------------------------------------------
url.planeacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Plan_evalua_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\Plan_evalua_cngmd2015_dbf\\Bases_datos\\PLAN_EVA.", fformat)

### ADMINISTRACION PUBLICA - ACTIVIDADES ESTADÍSTICAS ---------------------------------------------------------------
url.actividadesestadisticas = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Act_estadis_geograficas_cngmd2015_", fformat, ".zip")
raw  = paste0(zipdir, "\\Bases_datos\\ESTA_GEO.", fformat)

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
url.predial = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Impuesto_predial_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\Impuesto_predial_cngmd2015_dbf\\Impuesto_predial_cngmd2015_dbf\\Bases_de_datos\\IMP_PRED.", fformat)

### CATASTRO - VALUACIÓN CATASTRAL ---------------------------------------------------------------------
url.valuacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Valuacion_catastral_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\Valuacion_catastral_cngmd2015_dbf\\Valuacion_catastral_cngmd2015_dbf\\Bases_de_datos\\VAL_CAT.", fformat)

### CATASTRO - VINCULACIÓN CATASTRAL -------------------------------------------------------------------
url.vinculacion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m2/Vinculacion_catastral_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\Vinculacion_catastral_cngmd2015_dbf\\Vinculacion_catastral_cngmd2015_dbf\\Bases_de_datos\\VINC_CAT.", fformat)

### SEGURIDAD PÚBLICA - SEGURIDAD PÚBLICA --------------------------------------------------------------
url.seguridadpublica = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/sp_Seguridad_publica_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPEJFUES.", fformat)
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPINFEST.", fformat)
raw  = paste0(zipdir, "\\SP_Seguridad_publica_cngmd2015_dbf\\Bases_datos\\SPINFRAE.", fformat)


### SEGURIDAD PÚBLICA - RECURSOS HUMANOS ---------------------------------------------------------------
url.rh = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/sp_Recursos_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_1.", fformat)
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_2.", fformat)
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_3.", fformat)
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_4.", fformat)
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_5.", fformat)
raw  = paste0(zipdir, "\\SP_Recursos_cngmd2015_dbf\\Bases_datos\\SPRE_HUM_6.", fformat)


### SEGURIDAD PÚBLICA - EJERCICIO DE LA FUNCIÓN --------------------------------------------------------
url.funcion = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m3/SP_Ejercicio_funcion_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_1.", fformat)
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_2.", fformat)
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_3.", fformat)
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_4.", fformat)
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_5.", fformat)
raw  = paste0(zipdir, "\\SP_Ejercicio_funcion_cngmd2015_dbf\\Bases_datos\\EJER_FNC_6.", fformat)

### JUSTICIA MUNICIPAL - RECURSOS HUMANOS --------------------------------------------------------------
url.rh = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m4/JM_Recursos_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_1.", fformat)
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_2.", fformat)
raw  = paste0(zipdir, "\\JM_Recursos_cngmd2015_dbf\\Bases_de_datos\\RE_HU_JM_3.", fformat)


### JUSTICIA MUNICIPAL - INFRAESTRUCTURA ---------------------------------------------------------------
raw  = paste0(zipdir, "\\JM_Infraestuctura_cngmd2015_dbf\\Bases_de_datos\\JC_INFRA.", fformat)

### JUSTICIA MUNICIPAL - EJERCICIO DE LA FUNCIÓN -------------------------------------------------------
url.ejercicio = paste0("http://www.beta.inegi.org.mx/contenidos/proyectos/censosgobierno/municipal/cngmd/2015/microdatos/m4/JM_Ejercicio_funcion_cngmd2015_",fformat, ".zip")
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_1.", fformat)
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_2.", fformat)
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_3.", fformat)
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_4.", fformat)
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_5.", fformat)
raw  = paste0(zipdir, "\\JM_Ejercicio_funcion_cngmd2015_dbf\\Bases_de_datos\\EJ_FJMU_6.", fformat)
"""
