test_that("abrir_pagina() muestra la direccion fuera de una sesion interactiva", {
  skip_if(interactive(), "Solo aplica a sesiones no interactivas")
  expect_message(
    url <- abrir_pagina("/programas/enoe/15ymas/"),
    "Consulta la informaci\u00f3n en: https://www.inegi.org.mx/programas/enoe/15ymas/"
  )
  expect_identical(url, "https://www.inegi.org.mx/programas/enoe/15ymas/")
})

test_that("abrir_pagina() pasa a https las direcciones antiguas", {
  skip_if(interactive(), "Solo aplica a sesiones no interactivas")
  expect_identical(
    suppressMessages(abrir_pagina("http://www.inegi.org.mx/programas/cngmd/2011/")),
    "https://www.inegi.org.mx/programas/cngmd/2011/"
  )
})
