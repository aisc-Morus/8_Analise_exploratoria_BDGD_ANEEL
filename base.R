
# 1. Prepara ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, sf, purr)

# 2. Monta df ----

bdgd <- list.files(pattern = ".gdb") %>% 
        as.list
camadas <- map(bdgd, st_layers)
camadas

df <- map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                      lay <- st_layers(f)$name
                      lay_filtrado <- lay %>%
                                      grep("tab", ., value = TRUE, ignore.case = TRUE) %>%
                                      grep("UC", ., value = TRUE, ignore.case = TRUE)
                      map(lay_filtrado, \(l) {st_read(f, layer = l) %>%
                                              mutate(ano = ano, 
                                                     camada = l) %>% 
                                              rename_with(tolower)
                                             })
                     }) %>%
      flatten() %>% 
      bind_rows %>% 
           select(distribuidora = dist, 
                  id            = cod_id, 
                  livre         = liv, 
                  tarifa        = gru_tar, 
                  cnae,
                  classe_bdgd   = clas_sub,
                  cep, 
                  municipio     = mun,
                  ano,
                  camada,
                  starts_with("ene"))

df_shape <-  map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                             lay <- st_layers(f)$name
                             lay_filtrado <- "PONNOT"
                             map(lay_filtrado, \(l) {st_read(f, layer = l) %>%
                                                     mutate(ano = ano, 
                                                     camada = l) %>% 
                                                     rename_with(tolower)
                                                     })
                            }) %>% 
             flatten() %>% 
             bind_rows() %>% 
             select(cod_id, shape) %>%  distinct()
        
                                      


