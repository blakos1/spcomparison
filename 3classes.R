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
writeRaster(my_sims2, "data/my_sims2_3classes.tif", overwrite = TRUE)
my_sims3 = raster::stack("data/my_sims2_3classes.tif")

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

colnums = rep(seq(1:9), 6)
rownums = rep(1:6, each = 9)
colnames(dist_df) = paste0("img_3classes_", "row", rownums, "_col", colnums)
dist_df$id = paste0("img_3classes_", "row", rownums, "_col", colnums)

dist_df_long = pivot_longer(dist_df, 1:nrow(dist_df))[1:2]

all_methods = philentropy::getDistMethods() %>%
  lapply(calc_dist) %>%
  do.call(what = cbind)

all_methods = cbind(dist_df_long, all_methods)

all_methods$questionID = paste0(all_methods$id, "+", all_methods$name)

all_methods = subset(all_methods, select=-c(id, name))

write.csv(all_methods, file = "data/3classes_dist.csv")
