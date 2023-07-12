

## Origen - Destino

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, sf, fs, tidyverse, scatterpie, rmapshaper, rgeos, stringr, glue, readxl, openxlsx, xlsx)

g <- gc(reset = T)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------

# Tabular data
excel_sheets('tbl/BASE DATOS FINAL - copia.xlsx')
tble <- read_excel('tbl/BASE DATOS FINAL - copia.xlsx')
rgns <- read.table('tbl/regiones.csv', sep = ';', header = T)

# Spatial data
mpio <- vect('D:/data/spatial/igac/mpios.gpkg')
aqui <- mpio[mpio$DPTO_CNMBR == 'ANTIOQUIA',]
rgns <- mutate(rgns, Municipio = toupper(Municipio))
east <- aqui[aqui$MPIO_CNMBR %in% rgns$Municipio,]
trbl <- anti_join(rgns, as.data.frame(east)[,c('MPIO_CNMBR', 'DPTO_CNMBR')], by = c('Municipio' = 'MPIO_CNMBR'))

plot(east, col = 'red')

east <- east[,c('MPIO_CCNCT', 'MPIO_CCDGO', 'MPIO_CNMBR')]

# Summarise ---------------------------------------------------------------

smmr <- tble %>% 
  dplyr::select(municip, don_trab) %>% 
  group_by(municip, don_trab) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  setNames(c('origen', 'destino', 'conteo')) %>% 
  mutate(origen = toupper(origen), 
         destino = toupper(destino))

write.xlsx(smmr, 'tbl/smmr_mpio.xlsx')

smmr <- as_tibble(xlsx::read.xlsx('tbl/smmr_mpio.xlsx', 'Sheet1'))

# To make the join --------------------------------------------------------
smmr.mpio <- inner_join(smmr, rgns, by = c('origen' = 'Municipio')) %>% 
  rename(zona_origen = Zona) %>% 
  inner_join(., rgns, by = c('destino' = 'Municipio')) %>% 
  rename(zona_destino = Zona)

anti_join(rgns, smmr.mpio, by = c('Municipio' = 'origen'))

write.xlsx(smmr.mpio, 'tbl/smmr_mpio_v2.xlsx')

smmr.rgns <- smmr.mpio %>% 
  group_by(zona_origen, zona_destino) %>% 
  summarise(conteo = sum(conteo)) %>% 
  ungroup()

write.xlsx(smmr.rgns, 'tbl/smmr_rgns.xlsx')

smmr.rgns <- read.xlsx('tbl/smmr_rgns.xlsx', 'Sheet1')[,-1]

# Spatial data - aggregate ------------------------------------------------

east <- aqui %>% 
  st_as_sf() %>% 
  dplyr::select(MPIO_CCNCT, MPIO_CNMBR) %>% 
  inner_join(., rgns, by = c('MPIO_CNMBR' = 'Municipio'))

dplyr::select(east, Zona) %>% plot()

dir.create('gpkg')
st_write(east, 'gpkg/mpios_zonas_east.gpkg')

# To dissolve -------------------------------------------------------------

east.znes <- ms_dissolve(east, field = 'Zona')

smmr.rgns <- smmr.rgns %>% 
  group_by(zona_origen) %>% 
  mutate(porc = conteo / sum(conteo) * 100) %>% 
  rename(conteo_origen = conteo, 
         porc_origen = porc) %>% 
  ungroup()

east.znes

smmr.rgns <- smmr.rgns %>% 
  dplyr::select(-conteo_origen) %>% 
  spread(zona_destino, porc_origen) 

smmr.rgns[is.na(smmr.rgns)] <- 0

east.znes.o <- inner_join(east.znes, smmr.rgns, by = c('Zona' = 'zona_origen'))

crds <- east.znes %>% 
  as(., 'Spatial') %>% 
  coordinates() %>%
  as.data.frame() %>% 
  mutate(zone = east.znes$Zona) %>% 
  setNames(c('lon', 'lat', 'zone')) %>% 
  inner_join(., st_drop_geometry(east.znes.o), by = c('zone' = 'Zona'))

g.orig <- ggplot() +
  geom_sf(data = east.znes, fill = 'grey60') + 
  geom_scatterpie(data = crds, aes(x = lon, y = lat, group = zone), col = c('Altiplano', 'Bosques', 'Embalses', 'Páramos'), pie_scale = 8) + 
  geom_sf_text(data = east.znes, aes(label = Zona)) +
  scale_fill_viridis_d() +
  labs(fill = 'Zona', x = 'Lon', y = 'Lat') +
  theme_minimal() + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5), 
        legend.position = 'bottom') +
  guides(fill = guide_legend( 
    direction = 'horizontal',
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))

ggsave(plot = g.orig, filename = 'png/mapa_orig-destino.png', units = 'in', width = 9, height = 7, dpi = 300)



