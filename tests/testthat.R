library(testthat)
library(aelectoral2)

test_check("aelectoral2")

test_that("bdos completas",{
  nac <- paste0("nac_", c("df_21", "df_18", "df_15", "pr_18", "cp_22") %>% sort(), ".rda")
  mex <- paste0("mex_", c("pmext_21", "pm_21", "pm_18", "gb_17", "dl_21", "dl_18") %>% sort(),".rda")
  chis <- paste0("chis_", c("pm_21", "pm_18", "gb_18", "dl_21", "pmext_22") %>% sort(),".rda")
  dgo <- paste0("dgo_", c("dl_21", "pm_19", "pm_16", "gb_16", "dl_16", "dl_18") %>% sort(),".rda")
  coah <- paste0("coah_", c("pm_21", "dl_20", "pm_18", "gb_17", "pm_17", "dl_17") %>% sort(),".rda")

  expect_identical(list.files("inst/electoral",pattern = "nac_") %>% sort(),nac)
  expect_identical(list.files("inst/electoral",pattern = "mex_") %>% sort(), mex)
  expect_identical(list.files("inst/electoral",pattern = "chis_") %>% sort(), chis)
  expect_identical(list.files("inst/electoral",pattern = "dgo_") %>% sort(), dgo)
  expect_identical(list.files("inst/electoral",pattern = "coah_") %>% sort(), coah)

  expect_identical(list.files("inst/electoral") %>% sort(), c(nac,mex,chis,dgo, coah) %>% sort)
})
