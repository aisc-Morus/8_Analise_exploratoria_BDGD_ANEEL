
# 1. Prepara ambiente ----
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, readxl, sf, tmap)

# 2. Camadas BDGD ----
bdgd <- list.files(pattern = ".gdb") %>% 
        as.list
camadas <- map(bdgd, st_layers)
camadas

# 3. Monta shapefile ----
# 3.1 Ponto Notável (PONOOT) ----
df_shape_ponnot <-  map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                                    camada <- st_layers(f)$name
                                    camada_filtrado <- "PONNOT"
                                    map(camada_filtrado, \(l) {st_read(f, layer = l) %>%
                                                               mutate(ano = ano, 
                                                                      camada = l) %>% 
                                                               rename_with(tolower)
                                                              })
                                   }) %>% 
                    flatten() %>% 
                    bind_rows() %>% 
                    distinct() 

df_shape_ponnot$shape %>% class
df_shape_ponnot %>% st_crs
df_shape_ponnot %>% str

# 3.2 Área de Concessão (ARAT) ----
df_shape_arat <- map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                             camada <- st_layers(f)$name
                             camada_filtrado <- "ARAT"
                             map(camada_filtrado, \(l) {st_read(f, layer = l) %>%
                                                     mutate(ano = ano, 
                                                            camada = l) %>% 
                                                     rename_with(tolower)
                                                    })
                                 }) %>% 
                 flatten() %>% 
                 bind_rows()

# 3.3 Segmento do Sistema de Distribuição de Baixa Tensão (SSDBT) ----
df_shape_ssdbt <- map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                                  camada <- st_layers(f)$name
                                  camada_filtrado <- "SSDBT"
                                  map(camada_filtrado, \(l) {st_read(f, layer = l) %>%
                                                             mutate(ano = ano, 
                                                             camada = l) %>% 
                                      rename_with(tolower)
                                  })
                                  }) %>% 
                  flatten() %>% 
                  bind_rows() %>% 
                  distinct()

# 3.4 Segmento do Sistema de Distribuição de Média Tensão (SSDMT) ----
df_shape_ssdmt <- map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                                  camada <- st_layers(f)$name
                                  camada_filtrado <- "SSDMT"
                                  map(camada_filtrado, \(l) {st_read(f, layer = l) %>%
                                                             mutate(ano = ano, 
                                                                    camada = l) %>% 
                                                             rename_with(tolower)
                                                            })
                                 }) %>% 
                  flatten() %>% 
                  bind_rows() %>% 
                  distinct() 

# 3.5 Segmento do Sistema de Distribuição de Alta Tensão (SSDAT) ----
df_shape_ssdat <- map(bdgd, \(f) {ano <- stringr::str_extract(f, "20[0-9]{2}")
                                  camada <- st_layers(f)$name
                                  camada_filtrado <- "SSDAT"
                                  map(camada_filtrado, \(l) {st_read(f, layer = l) %>%
                                                             mutate(ano = ano, 
                                                                    camada = l) %>% 
                                                             rename_with(tolower)
                                                            })
                                 }) %>% 
                  flatten() %>% 
                  bind_rows() %>% 
                  distinct() 

# 4. Projeta shapefiles usando mapa interativo TMAP ----
tmap_mode("view")
tm_basemap("OpenStreetMap") +
  tm_shape(df_shape_arat) +                  
    tm_polygons(col = "gray80", 
                alpha = 0.1,    
                border.col = "red") +
  tm_shape(df_shape_ssdbt) +
    tm_lines(col = "blue")+
  tm_shape(df_shape_ssdmt) +
    tm_lines(col = "green")+
  tm_shape(df_shape_ssdat) +
    tm_lines(col = "red")+
  tm_shape(df_shape_ponnot) +                 
    tm_dots(col = "gray60", size = 0.15) +
    tm_layout(title = "EFLUL/SC")

# 5. Monta df ----
classe_bdgd <- read_xlsx("Tipo de Classe e Subclasse.xlsx") %>% 
               rename_with(tolower)

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
      left_join(df_shape_ponnot %>% select(id = cod_id, shape), 
                by ="id", 
                multiple = "first") %>% 
      select(-matches("^ene(_p|_f)")) %>%
      pivot_longer(cols      = starts_with("ene"),
                   names_to  = "mes",
                   values_to = "kwh" ) %>%
      mutate(referencia = as.Date(paste0(ano, 
                                         "-", 
                                         str_extract(mes, "\\d{2}"), "-01")),
             cnae       = str_remove_all(cnae, "[^0-9]") %>% na_if("")) %>% 
      select(-ano, -mes) %>%
      group_by(id, distribuidora, camada, shape, referencia) %>% 
      summarise(n = n(), 
                kwh = sum(kwh),
                livre = first(livre),
                cnae = first(cnae),
                cep = first(cep),
                municipio = first(municipio),
                gru_tar = first(gru_tar), 
                classe_bdgd = first(classe_bdgd)) %>% 
      ungroup()

df_largo <- df %>% pivot_wider(id_cols     = c(distribuidora, id, shape, livre, gru_tar, cnae, classe_bdgd, cep, municipio),
                               names_from  = referencia,         
                               values_from = kwh)   

#5.1 Consumo por grupo tarifário ----  
df %>% group_by(gru_tar, referencia) %>% 
       summarise(mwh = (sum(kwh)/1000)) %>% 
       ggplot(., aes(x = referencia, y = mwh, colour = gru_tar))+
        geom_line(size = 1) +
        geom_point(size = 2) +
       labs(title = "Evolução mensal do consumo por grupo tarifário",
            x = "Mês",
            y = "Consumo total (MWh)",
            color = "Grupo Tarifário") +
        theme_minimal()

#5.2 Número de consumidores por grupo tarifário ----  
df %>% group_by(gru_tar, referencia) %>% 
       summarise(n_consumidores = n_distinct(id)) %>% 
       ggplot(., aes(x = referencia, y = n_consumidores, colour = gru_tar))+
       geom_line() +
       geom_point() +
       labs(title = "Evolução mensal do número de consumidores por grupo tarifário",
            x = "Mês",
            y = "Número de consumidores",
            color = "Grupo Tarifário",) +
       theme_minimal()

