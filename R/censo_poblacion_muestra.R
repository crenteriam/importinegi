#' Censos y Conteos de Población y Vivienda.
#' MUESTRA
#'
#' @param year Año requerido del censo o conteo. En esta versión están disponibles 2000, 2005, 2010 y 2015.
#' @param estado Descarga datos por estado. Utilizar el nombre del estado, con espacios. Ejemplos: "Aguascalientes", "CDMX", "San Luis Potosi".
#' @param muestra Utilizar "Migrantes", "Personas", o "Viviendas" según el tipo de base de datos requerida.

censo_poblacion_muestra <- function(year = "2010", estado = NA, muestra = NA){
  library(foreign) # Importar archivos dbf.

# Debug.
if ((muestra!="Personas" | muestra!= "Migrantes" | muestra!="Viviendas") & year!="1990"){
  stop("La funcion no reconoce la muestra requerida.
       \rLas muestras validas son: \"Personas\", \"Viviendas\" y \"Migrantes\".
       \rEl Censo 1990 no requiere tipo de muestra, ya que existe una sola base de datos disponible.")} else{}

# Informacion de la version
  message("censo_poblacion_rural() Versión 1.0.
          \rResultados sobre localidades con menos de 5 mil habitantes
          \rAño disponible: 2010.
          \r\n")

  # Objetos generales
  formato_archivo = "dbf"
  inegi.base      = "http://www.beta.inegi.org.mx/contenidos/programas/ccpv/"

  # Estado  -------------------------------------------------------------
  if      (estado == "Aguascalientes"){  censo.state = "01" }
  else if (estado == "Baja California"){ censo.state = "02" }
  else if (estado == "Baja California Sur"){ censo.state = "03"}
  else if (estado == "Campeche"){ censo.state = "04"}
  else if (estado == "Coahuila"){ censo.state = "05"}
  else if (estado == "Colima")  { censo.state = "06"}
  else if (estado == "Chiapas") { censo.state = "07"}
  else if (estado == "Chihuahua"){censo.state = "08"}
  else if (estado == "CDMX")    { censo.state = "09"}
  else if (estado == "Durango") { censo.state = "10"}
  else if (estado == "Guanajuato"){censo.state = "11"}
  else if (estado == "Guerrero"){ censo.state = "12"}
  else if (estado == "Hidalgo") { censo.state = "13"}
  else if (estado == "Jalisco") { censo.state = "14"}
  else if (estado == "Mexico")  { censo.state = "15"}
  else if (estado == "Michoacan"){censo.state = "16"}
  else if (estado == "Morelos") { censo.state = "17"}
  else if (estado == "Nayarit") { censo.state = "18"}
  else if (estado == "Nuevo Leon"){censo.state = "19"}
  else if (estado == "Oaxaca")  { censo.state = "20"}
  else if (estado == "Puebla")  { censo.state = "21"}
  else if (estado == "Queretaro"){censo.state = "22"}
  else if (estado == "Quintana Roo"){censo.state = "23"}
  else if (estado == "San Luis Potosi"){censo.state = "24"}
  else if (estado == "Sinaloa") { censo.state = "25"}
  else if (estado == "Sonora")  { censo.state = "26"}
  else if (estado == "Tabasco") { censo.state = "27"}
  else if (estado == "Tamaulipas"){censo.state = "28"}
  else if (estado == "Tlaxcala"){ censo.state = "29"}
  else if (estado == "Veracruz"){ censo.state = "30"}
  else if (estado == "Yucatan") { censo.state = "31"}
  else if (estado == "Zacatecas"){censo.state = "32"}
  else if (estado == "Nacional") {censo.state = "00"}
  else {stop("La funcion no reconoce el nombre del estado.
             \rUsa letras capitales y espacios. Por ejemplo, Quintana Roo.")}


#1 Obtener URL  ----------------------------------------------------------------
if (year == "2010") {
censo.url.muestra =  paste0(inegi.base, year, "/microdatos/mpv/MC", year, "_", censo.state, "_", formato_archivo, ".zip")
}
else if (year == "2000") {
censo.url.muestra =  paste0(inegi.base, year, "/microdatos/muestra/cgpv", year, "_", censo.state, "_", formato_archivo, ".zip")
}
else if (year == "1990"){
censo.url.muestra =  paste0(inegi.base, year, "/microdatos/cgpv90p_", censo.state, "_", formato_archivo, ".zip")
}  else {}

#2 DOWNLOAD TEMPORARY FILE
censo.temp.muestra = base::tempfile()
download.file(censo.url.muestra, censo.temp.muestra)

#3 Unzip, read file and fix missing. Unlink temporary file
zipdir = tempfile()
utils::unzip(censo.temp.muestra, exdir = zipdir)
zipped_file = dir(zipdir)

if (year == "2010"){
raw.muestra = paste0(zipdir, "\\", muestra, "_", censo.state, ".", formato_archivo)
} # End of Unzip for year 2010
if (year == "2000"){
if      (muestra == "Migrantes"){muestra_2000 = "MIN_F" }
else if (muestra == "Personas") {muestra_2000 = "PER_F" }
else if (muestra == "Viviendas"){muestra_2000 = "VHO_F" }
else {}
raw.muestra = paste0(zipdir, "\\", muestra_2000, censo.state, ".", formato_archivo)
} # End of Unzip for year 2000
if (year == "1990"){
raw.muestra = paste0(zipdir, "\\", zipped_file)
} else {} # End of Unzip for year 1990

data.output.muestra = foreign::read.dbf(raw.muestra, as.is = TRUE)
data.output.muestra[data.output.muestra=="*"]<-NA
return(data.output.muestra)
} # End of function


