ja <- readr::read_delim("data-raw/df_21.csv",delim = "|",skip = 1,
                        locale = readr::locale(encoding = "CP1252"))
al_df_21 <- ja %>%
  filter(PARTIDO_CI %in% c("PAN_PRI_PRD", "PVEM_PT_MORENA")) %>%
  transmute(distritof_21 = paste(formatC(ESTADO, width = 2, flag = 0),
                                 formatC(DISTRITO, width = 2, flag = 0),sep = "_"),
            coalicion = tolower(PARTIDO_CI))

usethis::use_data(al_df_21, overwrite = T)
