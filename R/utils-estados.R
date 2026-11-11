# Utilidades internas: catalogo de entidades federativas ----------------------

# Catalogo unico de entidades: clave del INEGI, nombre y abreviatura.
# La clave "00" corresponde al total nacional.
inegi_estados <- function() {
  data.frame(
    clave = sprintf("%02d", 0:32),
    nombre = c(
      "Nacional", "Aguascalientes", "Baja California", "Baja California Sur",
      "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
      "Ciudad de M\u00e9xico", "Durango", "Guanajuato", "Guerrero", "Hidalgo",
      "Jalisco", "M\u00e9xico", "Michoac\u00e1n", "Morelos", "Nayarit", "Nuevo Le\u00f3n",
      "Oaxaca", "Puebla", "Quer\u00e9taro", "Quintana Roo", "San Luis Potos\u00ed",
      "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
      "Yucat\u00e1n", "Zacatecas"
    ),
    abreviatura = c(
      "nal", "ags", "bc", "bcs", "camp", "coah", "col", "chis", "chih",
      "cdmx", "dgo", "gto", "gro", "hgo", "jal", "mex", "mich", "mor", "nay",
      "nl", "oax", "pue", "qro", "qroo", "slp", "sin", "son", "tab", "tamps",
      "tlax", "ver", "yuc", "zac"
    ),
    stringsAsFactors = FALSE
  )
}

# Otras formas de escribir cada entidad (ya normalizadas), por clave.
estados_alias <- function() {
  list(
    "00" = c("estados unidos mexicanos", "eum", "pais", "republica mexicana", "total nacional"),
    "05" = c("coahuila de zaragoza"),
    "09" = c("ciudad de mexico", "distrito federal", "df", "d f"),
    "15" = c("estado de mexico", "edomex", "edo mex", "edo de mexico"),
    "16" = c("michoacan de ocampo"),
    "22" = c("queretaro de arteaga", "queretaro arteaga"),
    "23" = c("q roo"),
    "30" = c("veracruz de ignacio de la llave", "veracruz llave")
  )
}

# Minusculas, sin acentos ni puntuacion y con espacios simples.
normalizar_texto <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1", "aeiouunAEIOUUN", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub(" +", " ", x))
}

# Convierte el nombre, la abreviatura o la clave de una entidad en su clave de
# dos digitos. Acepta mayusculas, minusculas, acentos y nombres alternativos.
estado_clave <- function(estado, nacional = TRUE, argumento = "estado") {
  validar_escalar(estado, argumento)
  tabla <- inegi_estados()

  if (is.numeric(estado) || grepl("^[0-9]{1,2}$", estado)) {
    clave <- sprintf("%02d", as.integer(estado))
    if (!clave %in% tabla$clave) {
      detener(
        "`", argumento, " = ", comillas(estado), "` no es una clave de entidad v\u00e1lida. ",
        "Las claves van de 1 a 32 (0 para el total nacional)."
      )
    }
  } else {
    alias <- estados_alias()
    nombres <- c(
      stats::setNames(tabla$clave, normalizar_texto(tabla$nombre)),
      stats::setNames(tabla$clave, tabla$abreviatura),
      stats::setNames(rep(names(alias), lengths(alias)), unlist(alias, use.names = FALSE))
    )
    buscado <- normalizar_texto(estado)
    if (!buscado %in% names(nombres)) {
      parecidos <- agrep(buscado, normalizar_texto(tabla$nombre), max.distance = 0.25)
      sugerencia <- if (length(parecidos) > 0) {
        paste0(" \u00bfQuisiste decir ", enumerar(tabla$nombre[parecidos]), "?")
      } else {
        ""
      }
      detener(
        "No se reconoce la entidad `", argumento, " = ", comillas(estado), "`.", sugerencia,
        " Usa el nombre de la entidad, su clave (1 a 32) o \"Nacional\"."
      )
    }
    clave <- nombres[[buscado]]
  }

  if (clave == "00" && !nacional) {
    detener(
      "Esta informaci\u00f3n solo se publica por entidad federativa. ",
      "Indica una entidad en `", argumento, "` en lugar del total nacional."
    )
  }
  clave
}
