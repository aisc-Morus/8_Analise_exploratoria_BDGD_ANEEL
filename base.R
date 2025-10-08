
# 1. Prepara ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, sf, tmap)

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
             select(id = cod_id, shape) %>%  
             distinct() %>% 
             mutate(lon = st_coordinates(shape)[, 1],
                    lat = st_coordinates(shape)[, 2])
df_shape %>% st_crs
df_shape %>% str

# 4. Projeta shapefile usando mapa interativo TMAP

tmap_mode("view")
tm_shape(df_shape) +
  tm_dots(col = "blue", size = 0.15) +
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
             id            = pn_con, 
             livre         = liv, 
             gru_tar, 
             cnae,
             classe_bdgd   = clas_sub,
             cep, 
             municipio     = mun,
             ano,
             camada,
             starts_with("ene")) %>% 
       left_join(df_shape, 
                 by ="id", 
                 multiple = "first") %>% 
       select(-matches("^ene(_p|_f)")) %>%
       pivot_longer(cols = starts_with("ene"),
                    names_to = "mes",
                    values_to = "kwh" ) %>%
       mutate(referencia = as.Date(paste0(ano, 
                                          "-", 
                                          str_extract(mes, "\\d{2}"), "-01")),
              cnae       = str_remove_all(cnae, "[^0-9]")) %>% 
       distinct() %>% 
       pivot_wider(id_cols     = c(distribuidora, id, livre, gru_tar, cnae, classe_bdgd, cep, municipio, camada, shape, lon, lat),
                   names_from  = referencia,         
                   values_from = kwh)


df %>% str

      


df %>%
  summarise(n = n(), .by = c(distribuidora, id, livre, gru_tar, cnae, classe_bdgd, cep, municipio, camada, shape, lon, lat, referencia)) %>%
  filter(n > 1)








df <- map(bdgd, \(f) {
  ano <- str_extract(f, "20[0-9]{2}")
  lay <- st_layers(f)$name
  lay_filtrado <- lay %>%
    grep("tab", ., value = TRUE, ignore.case = TRUE) %>%
    grep("UC", ., value = TRUE, ignore.case = TRUE)
  
  map(lay_filtrado, \(l) {
    st_read(f, layer = l) %>%
      mutate(ano = ano, camada = l) %>%
      rename_with(tolower)
  })
}) %>%
  flatten() %>%
  bind_rows() %>%
  select(distribuidora = dist, 
         id            = pn_con, 
         livre         = liv, 
         gru_tar, 
         cnae,
         classe_bdgd   = clas_sub,
         cep, 
         municipio     = mun,
         ano,
         camada,
         starts_with("ene")) %>%
  left_join(df_shape, by = "id", multiple = "first") %>%
  select(-matches("^ene(_p|_f)")) %>%  # remove colunas ene_p e ene_f
  pivot_longer(
    cols = starts_with("ene"),
    names_to = "mes",
    values_to = "kwh"
  ) %>%
  mutate(
    referencia = as.Date(paste0(ano, "-", str_extract(mes, "\\d{2}"), "-01")),
    cnae = str_remove_all(cnae, "[^0-9]")
  ) %>%
  pivot_wider(
    id_cols     = c(distribuidora, id, livre, gru_tar, cnae, classe_bdgd, cep, municipio, camada, shape, lon, lat),
    names_from  = referencia,
    values_from = kwh
  )







                                      


