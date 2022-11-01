# library(terra)
# library(purrr)
# library(NLMR)
# library(tmap)
# library(landscapetools)
# library(landscapemetrics)
# library(ggplot2)
# library(motif)
# library(tidyr)
# library(comat)
# library(philentropy)
# library(tidyr)

# simulate ----------------------------------------------------------------
distribute_weights = function(x){
  w = 1-x
  w2 = w * 2/3
  w3 = w * 1/3
  c(x, w2, w3)
}

fract_dim = c(0.1, 0.4, 0.55, 0.8, 1.4, 1.6) #0.1, 0.4, 0.55, 0.7, 1.0, 1.6
weights = c(0.98, 0.94, 0.90, 0.82, 0.74, 0.66, 0.58, 0.42)

my_list = c(lapply(weights, distribute_weights), list(c(1/3, 1/3, 1/3)))
test = expand_grid(aa = my_list,
                   fract_dim = fract_dim)

simulate_composition = function(fract_dim){
  sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
  sim1
}

param_df = expand.grid(fract_dim = fract_dim)

set.seed(2022-06-25)
my_sims = map(param_df$fract_dim, simulate_composition)

param_df2 = expand.grid(weighting = my_list,
                        fract_dim = fract_dim)
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

# calculate metrics -------------------------------------------------------
param_df2$ent = lsm_l_ent(my_sims3)$value
param_df2$mutinf = lsm_l_mutinf(my_sims3)$value
param_df2$relmutinf = lsm_l_relmutinf(my_sims3)$value

ggplot(param_df2, aes(ent, relmutinf)) +
  geom_point()

# distances ---------------------------------------------------------------
compare_dist = function(r1, r2, method = "jensen-shannon", unit = "log2"){
  r1m = as.matrix(r1, wide = TRUE)
  r2m = as.matrix(r2, wide = TRUE)

  r1mc = get_cove(get_coma(r1m), normalization = "pdf")
  r2mc = get_cove(get_coma(r2m), normalization = "pdf")

  dist_one_one(r1mc, r2mc, method = method, unit = unit)
}

calc_dist = function(method = "jensen-shannon"){
  param_df2$id = seq_along(param_df2$weighting)
  dist_mat = matrix(nrow = nrow(param_df2), ncol = nrow(param_df2))
  for (i in param_df2$id){
    for (j in param_df2$id){
      dist_mat[i, j] = compare_dist(my_sims2[[i]], my_sims2[[j]], method)
    }
  }

  dist_df = as.data.frame(dist_mat)
  dist_df$id = seq_along(dist_df$V1)
  dist_df_long = pivot_longer(dist_df, 1:nrow(dist_df))
  dist_df_long$name = as.numeric(gsub("V", "", dist_df_long$name))
  colnames(dist_df_long)[colnames(dist_df_long) == "value"] = method

  return(dist_df_long[3])
}

dist_mat = matrix(nrow = nrow(param_df2), ncol = nrow(param_df2))
dist_df = as.data.frame(dist_mat)
dist_df$id = seq_along(dist_df$V1)
dist_df_long = pivot_longer(dist_df, 1:nrow(dist_df))[1:2]
dist_df_long$name = as.numeric(gsub("V", "", dist_df_long$name))

all_methods = philentropy::getDistMethods()[1:4] %>%
  lapply(calc_dist) %>%
  do.call(what = cbind)

all_methods = cbind(dist_df_long, all_methods)

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
