#4 Rural --------------------------------------------------------------------------------------------
else if (type == "Rural"){
  #1 GET URL
  censo.url.rural =  paste0(inegi.base, year,
                            "/microdatos/cinco_mil_menos/resloc_", censo.state, "_", year, "_", fformat, ".zip")
  #2 DOWNLOAD TEMPORARY FILE
  censo.temp.rural = base::tempfile()
  download.file(censo.url.rural, censo.temp.rural)
  #3 Unzip, read file and fix missing. Unlink temporary file
  data.output.rural = foreign::read.dbf((utils::unzip(censo.temp.rural)))
  data.output.rural[data.output.rural=="*"]<-NA
  return(data.output.rural)

} # End of type->Rural 2-nest
