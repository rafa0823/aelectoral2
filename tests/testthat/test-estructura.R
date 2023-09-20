test_that("bdos completas",{
  nac <- paste0("nacional/",c("df_12", "df_21", "df_18", "df_15", "pr_18", "cp_22", "sen_18") %>% sort(), ".rda")
  mex <- paste0("mex/",c("pm_21", "pm_18", "gb_17", "dl_21", "dl_18", "dl_15", "pm_15") %>% sort(),".rda")
  chis <- paste0("chis/",c("pm_21", "pm_18", "pm_15", "gb_18", "dl_21") %>% sort(),".rda")
  dgo <- paste0("dgo/",c("dl_21", "pm_19", "pm_16", "gb_16", "dl_16", "dl_18") %>% sort(),".rda")
  coah <- paste0("coah/",c("pm_21", "dl_20", "pm_18", "gb_17", "pm_17", "dl_17") %>% sort(),".rda")
  ags <- paste0("ags/", sort(c("dl_16", "dl_18", "dl_21", "gb_16", "pm_16", "pm_19", "pm_21")), ".rda")
  cdmx <- paste0("cdmx/", sort(c("dl_18", "dl_21", "gb_18", "pm_18", "pm_21")), ".rda")
  gto <- paste0("gto/", sort(c("dl_18", "dl_21", "gb_18", "pm_18", "pm_21")), ".rda")
  hgo <- paste0("hgo/", sort(c("dl_16", "dl_18", "gb_16", "pm_16", "pm_20")), ".rda")
  mor <- paste0("mor/", sort(c("dl_18", "dl_21", "gb_18", "pm_18", "pm_21")), ".rda")
  oaxaca <- paste0("oaxaca/", sort(c("dl_16", "dl_18", "gb_16", "pm_16", "pm_18")), ".rda")
  pue <- paste0("pue/", sort(c("dl_18", "dl_21", "gb_18", "pm_18", "pm_21")), ".rda")
  q_roo <- paste0("q_roo/", sort(c("dl_16", "dl_19", "gb_16", "pm_16", "pm_18")), ".rda")
  tamps <- paste0("tamps/", sort(c("dl_16", "dl_19", "gb_16", "pm_16", "pm_18", "pm_21")), ".rda")
  ver <- paste0("ver/", sort(c("dl_18", "dl_21", "gb_18", "pm_17", "pm_21")), ".rda")

  expect_identical(
    list.files("inst/electoral", full.names = T) %>%
      map2(list.files("inst/electoral"), ~{
      paste(.y, list.files(.x), sep = "/")})%>% reduce(c) %>% sort() ,
    c(nac,mex,chis,dgo, coah, ags, cdmx, gto, hgo, mor, oaxaca, pue, q_roo, tamps, ver) %>% sort)
})

test_that("shp completo",{

  expect_equal(
    list.files("inst/shp", full.names = T) %>% map_int(~list.files(.x) %>% length),
    rep(32, length(list.files("inst/shp")))
  )

})

test_that("alianzas completas",{
  nac <- paste0(c("df_21", "df_18", "df_15", "pr_18") %>% sort(), ".rda")
  mex <- paste0(c("pm_21", "pm_18", "gb_17", "dl_21", "dl_18") %>% sort(), ".rda")
  chis <- paste0(c("pm_21", "pm_18", "gb_18", "dl_21") %>% sort(), ".rda")
  dgo <- paste0(c("dl_21", "pm_19", "pm_16", "gb_16", "dl_16", "dl_18") %>% sort(), ".rda")
  coah <- paste0(c("pm_21", "dl_20", "pm_18", "gb_17", "pm_17", "dl_17") %>% sort(), ".rda")

  expect_identical(
    list.files("inst/alianzas/nacional") %>% sort,
    nac %>% sort
  )
  expect_identical(
    list.files("inst/alianzas/mex") %>% sort,
    mex %>% sort
  )

  expect_identical(
    list.files("inst/alianzas/chis") %>% sort,
    chis %>% sort
  )
  expect_identical(
    list.files("inst/alianzas/dgo") %>% sort,
    dgo %>% sort
  )

  expect_identical(
    list.files("inst/alianzas/nacional") %>% sort,
    nac %>% sort
  )
})

test_that("catalogo completo", {
  carpetas <- c("shp", "electoral", "auxiliares", "alianzas")
  cat <- catalogo %>%
    transmute(archivo = paste(carpeta1, capeta,bd, sep = "/"))

  expect_identical(cat %>% pull(archivo) %>% sort,
                   list.files(paste0("inst/",carpetas), full.names = T) %>%
                     map(~{
                       prefijo <- stringr::str_split(.x,pattern = "/") %>% pluck(1,2)
                       prefijo2 <- stringr::str_split(.x,pattern = "/") %>% pluck(1,3)
                       paste(prefijo, prefijo2, list.files(.x),sep = "/")
                     }) %>% do.call(base::c,.) %>% sort()
  )

})
