library(terra)
library(purrr)
library(NLMR)
library(tmap)
library(landscapetools)
library(landscapemetrics)
library(ggplot2)
library(motif)
library(tidyr)
library(comat)
library(philentropy)
library(tidyr)

# # simulate ----------------------------------------------------------------
# #fract_dim = konfiguracja czyli tak naprawdę autokorelacja
# #mala wartosc = szum
# #wysoka wartość = łagodny gradient
# 
# simulate_composition = function(fract_dim){
#   sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
#   sim1
# }
# 
# param_df = expand.grid(fract_dim = seq(0.1, 1.8, by = 0.3))
# 
# set.seed(2022-06-13)
# my_sims = map(param_df$fract_dim, simulate_composition)
# 
# 
# #weighting = kompozycja, czyli udział jednej z kategorii
# param_df2 = expand.grid(weighting = seq(0.01, 0.5, length.out = 6),
#                         fract_dim = seq(0.1, 1.8, by = 0.3))
# param_df2$my_sims = rep(my_sims, each = 6)
# 
# 
# simulate_configuration = function(x, weighting){
#   sim1 = x %>% 
#     landscapetools::util_classify(3, weighting) %>%  
#     rast()
#   crs(sim1) = "EPSG:2180"
#   sim1
# }
# 
# param_df2$my_sims2 = map2(param_df2$my_sims, param_df2$weighting, simulate_configuration)
# 
# 
# my_sims2 = do.call(c, param_df2$my_sims2)
# 
# # viz ---------------------------------------------------------------------
# writeRaster(my_sims2, "my_sims2.tif", overwrite = TRUE)
# my_sims3 = raster::stack("my_sims2.tif")
# tm_shape(my_sims3) +
#   tm_raster(legend.show = FALSE) +
#   tm_facets(ncol = 6)



# -------------------------------------------------------------------------
param_df2 = expand.grid(weighting = I(list(c(0.33, 0.33, 0.33), lapply(seq(0.98, 0.5, by = -0.08), fun1))),
                        fract_dim = seq(0.1, 1.8, by = 0.3))


# Mode 2
util_classify(fractal_landscape,
              weighting = c(0.33, 0.33, 0.33),
              level_names = c("Land Use 1", "Land Use 2", "Land Use 3")) %>% 
  plot()


#opcja 1: 1/3 1/3 1/3
#opcja 2: jedna dominuje, a reszta prawie 0
#opcja 3: 0.4, 0.4, 0.1

# 1: 1/3, 1/3, 1/3
# 2: 0.97, 0.02, 0.01
# 3: 0.88, 0.08, 0.04

x = 0.98
fun1 = function(x){
  w = 1-x
  w2 = w * 2/3
  w3 = w * 1/3
  c(x, w2, w3)
}

my_list = c(lapply(seq(0.98, 0.4, by = -0.08), fun1),list(c(1/3, 1/3, 1/3)))
test = expand_grid(aa = my_list,
                   fract_dim = seq(0.1, 1.8, by = 0.3))


simulate_composition = function(fract_dim){
  sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
  sim1
}

param_df = expand.grid(fract_dim = seq(0.1, 1.8, by = 0.3))

set.seed(2022-06-13)
my_sims = map(param_df$fract_dim, simulate_composition)


param_df2 = expand.grid(weighting = my_list,
                        fract_dim = seq(0.1, 1.8, by = 0.3))
param_df2$my_sims = rep(my_sims, each = 9)


simulate_configuration = function(x, weighting){
  sim1 = x %>% 
    landscapetools::util_classify(3, weighting) %>%  
    rast()
  crs(sim1) = "EPSG:2180"
  sim1
}

param_df2$my_sims2 = map2(param_df2$my_sims, param_df2$weighting, simulate_configuration)


my_sims2 = do.call(c, param_df2$my_sims2)

# viz ---------------------------------------------------------------------
writeRaster(my_sims2, "my_sims2_3classes.tif", overwrite = TRUE)
my_sims3 = raster::stack("my_sims2_3classes.tif")

wykres1_3classes = tm_shape(my_sims3) +
  tm_raster(legend.show = FALSE) +
  tm_facets(ncol = 9) +
  tm_layout(scale = 0.6,
            panel.labels = paste0("FD = ", param_df2$fract_dim,
                                  " | W = ", lapply(param_df2$weighting, round, 2))) + #jak usunąć c() z wykresu?
  tm_xlab("kompozycja", size = 2) +
  tm_ylab("konfiguracja", size = 2)

tmap_save(tm = wykres1_3classes, filename = "plots/wykres1_3classes.png",
          width = 1000, height = 1000)


# #zrobić tabele z nazwą pliku i każdym jego parametrem fract dim i weighting
# 
# #entropia brzegowa
# #im niższa wartość tym bardziej jedna kategoria dominuje
# #wysoka entropia znaczy że rozkład jest bardziej jednorodny
# #entropia odnosi sie tylko do kompozycji
# lsm_l_ent2 = function(x,...){
#   lsm_l_ent(landscape = x,...)$value
# }
# 
# lsm_l_ent2(param_df2$my_sims2[[1]], base = "log")
# 
# x = map_dbl(param_df2$my_sims2, lsm_l_ent2)
# 
# #konfiguracja - rmi/mi
# lsm_l_relmutinf()
# lsm_l_mutinf()
# 
# 
# #tabela 2 - porównawcza
# #id1 vs id2
# #każdy obraz porównać z każdym każdą miarą
# 
# #pierwsze obrazki powinny różnić się tylko kompozycją
# #potem tylko konfiguracją


