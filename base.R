
# 1. Prepara ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, sf, purr)

# 2. Camadas BDGD ----

bdgd <- list.files(pattern = ".gdb") %>% 
        as.list
camadas <- map(bdgd, st_layers)
camadas

# 3. Monta shapefile ----
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
             select(cod_id, shape) %>%  
             distinct() %>% 
             mutate(lon = st_coordinates(shape)[, 1],
                    lat = st_coordinates(shape)[, 2])
df_shape %>% st_crs

# 4. Projeta shapefile usando mapa interativo TMP

tm_shape(df_shape) +
  tm_dots(col = "red", size = 0.2) +
  tm_basemap("OpenStreetMap") +
  tm_layout(title = "PONNOT/BDGD - EFLUL/SC ")


# 4. Monta df ----
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
             starts_with("ene")) %>% 
      left_join(df_sha)


        














                                      


