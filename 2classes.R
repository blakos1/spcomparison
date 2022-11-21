# devtools::install_github("cran/RandomFields")
# devtools::install_github("ropensci/NLMR")
# devtools::install_github("ropensci/landscapetools")
# devtools::install_github("drostlab/philentropy")
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

# simulate ----------------------------------------------------------------
simulate_composition = function(fract_dim){
  sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
  sim1
}

fract_dim = c(0.1, 0.4, 0.55, 0.7, 1.0, 1.6)
param_df = expand.grid(fract_dim = fract_dim)

set.seed(2022-06-14)
my_sims = map(param_df$fract_dim, simulate_composition)

param_df2 = expand.grid(breaks = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.5),
                        fract_dim = fract_dim)
param_df2$my_sims = rep(my_sims, each = 6)

simulate_configuration = function(x, breaks){
  sim1 = x %>% 
    landscapetools::util_binarize(breaks) %>%  
    rast()
  crs(sim1) = "EPSG:2180"
  sim1
}

param_df2$my_sims2 = map2(param_df2$my_sims, param_df2$breaks, simulate_configuration)

my_sims2 = do.call(c, param_df2$my_sims2)

# viz ---------------------------------------------------------------------
writeRaster(my_sims2, "my_sims2_2classes.tif", overwrite = TRUE)
my_sims3 = raster::stack("my_sims2_2classes.tif")

wykres1_2classes = tm_shape(my_sims3) +
  tm_raster(legend.show = FALSE) +
  tm_facets(ncol = 6) +
  tm_layout(scale = 0.6,
    panel.labels = paste0("FD = ", param_df2$fract_dim,
                          " | B = ", round(param_df2$breaks,2))) +
  tm_xlab("kompozycja", size = 2) +
  tm_ylab("konfiguracja", size = 2)

dir.create("plots")
tmap_save(tm = wykres1_2classes, filename = "plots/wykres1_2classes.png",
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
  param_df2$id = seq_along(param_df2$breaks)
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

#benchmark szybkosc liczenia vs trafnosc obliczenia
