#' ENOE
#'
#' Encuesta Nacional de Ocupación y Empleo (ENOE), población de 15 años y más de edad.
#'
#' La Encuesta Nacional de Ocupación y Empleo (ENOE) es la principal fuente de información sobre el mercado laboral mexicano al ofrecer datos mensuales y trimestrales de la fuerza de trabajo, la ocupación, la informalidad laboral, la subocupación y la desocupación. Constituye también el proyecto estadístico continuo más grande del país al proporcionar cifras nacionales y de cuatro tamaños de localidad, de cada una de las 32 entidades federativas y para un total de 36 ciudades.
#'
#' @param year Año de levantamiento de la encuesta en formato numerico.
#' @param trimestre Trimestre de levantamiento de la encuesta en formato alfanumerico. Las opciones son: "trim1", "trim2", "trim3" y "trim4".
#' @param integrar \code{FALSE}: descarga en una lista las cinco bases de datos, por separado, que componen la ENOE. \code{TRUE}: integra las cinco bases de datos en una sola, emparejando por el identificador unico del entrevistado.
#'
#' @examples
#'
#' # Descarga las bases de datos de la ENOE 2009, Trimestre 1, sin integrar.
#' \dontrun{enoe(year = 2009, trimestre = "trim1")}
#'
#' # Descarga las bases de datos de la ENOE 2009, Trimestre 1, integradas
#' \dontrun{enoe(year = 2009, trimestre = "trim1", integrar = TRUE)}

enoe = function(year = NA, trimestre = NA, integrar = FALSE){
if (is.na(year) & is.na(trimestre)) {shell.exec("https://www.inegi.org.mx/programas/enoe/15ymas/")}
# Temp files
fformat = "dbf"
temp.enoe = tempfile()
zipdir     = tempdir()

# Descargar
url.base = paste0("http://www.beta.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/", year, trimestre,  "_", fformat, ".zip")
utils::download.file(url.base, temp.enoe)
utils::unzip(temp.enoe, exdir = zipdir)
list_dataraw = list.files(zipdir, pattern = ".dbf")

# Read all .dbf files in the folder
for (i in list_dataraw) {
  Object = foreign::read.dbf(paste0(zipdir,"\\", i), as.is = TRUE)
  assign(paste0("dt.", tools::file_path_sans_ext(i)), Object)
}
output = mget(ls(pattern = "dt."))

# INTEGRAR - TRUE  FALSE
if (integrar == TRUE) {
    data.vivienda = get(ls(pattern = "dt\\.viv"))
    data.hogar    = get(ls(pattern = "dt\\.hog"))
    data.sdem     = get(ls(pattern = "dt\\.sdem"))
    data.coe1     = get(ls(pattern = "dt\\.coe1"))
    data.coe2     = get(ls(pattern = "dt\\.coe2"))

    # Merge viviendas vivienda 1:m hogares
    #CD_A, ENT, CON, V_SEL
    data.compiled = merge(data.vivienda, data.hogar)

    # Merge data.compiled (vivienda + hogares) 1:m sociodemograficos
    # CD_A, ENT, CON, V_SEL, N_HOG, H_MUD
    data.compiled = merge(data.vivienda, data.sdem)

    # Merge data.compiled (viv + hog + sociodem) 1:1 coe1
    # CD_A, ENT, CON, V_SEL, N_HOG, H_MUD, N_REN
    data.compiled = merge(data.vivienda, data.coe1)

    # Merge data.compiled (viv + hog + sociodem + coe1) 1:1 coe2
    # CD_A, ENT, CON, V_SEL, N_HOG, H_MUD, N_REN
    data.compiled = merge(data.vivienda, data.coe2)
    output = data.compiled
}

# Return Object
return(output)
}
# Fin de la funcion
