




#3 EntornoUrbano -----------------------------------------------------------------------------------
else if (type == "EntornoUrbano"){
  #1 GET URL
  censo.url.eu =  paste0(inegi.base,
                         year, "/microdatos/urbano/MANZANA_EU_", censo.state, "_", fformat, ".zip")
  #2 DOWNLOAD TEMPORARY FILE
  censo.temp.eu = base::tempfile()
  download.file(censo.url.eu, censo.temp.eu)
  #3 Unzip, read file and fix missing. Unlink temporary file
  data.output.eu = foreign::read.dbf((utils::unzip(censo.temp.eu)))
  return(data.output.eu)
} # End of type->EntornoUrbano nest
