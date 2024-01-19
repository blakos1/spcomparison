library(tmap)
library(dplyr)
library(raster)
library(ggplot2)
library(readr)
library(patchwork)

#palette
lc_palette_df = read_csv("geodata/legend.csv")
names(lc_palette_df$color) = lc_palette_df$value


#raster reclassification ----------------------------------------------------
#reclassification matrix
input = c(1,2,3,4,5,6,7,8,9,10,11,12,
          16,18,
          20,21,22,23,24,25,26,27,
          29,30,31,32,33,
          35,36,
          40,41,42,44,128)
output = c(2,2,2,2,2,2,2,2,2,2,2,
           2,2,
           2,2,2,2,1,1,1,2,2,
           2,2,2,2,2,
           2,2,
           2,2,2,2,2)
matrix.rcl = cbind(input,output)

#clc1990 raster
clc1990 = raster("geodata/clc1990.tif")
clc1990_2 = reclassify(clc1990, matrix.rcl)
# writeRaster(clc1990_2, "geodata/clc1990_2.tif", format ="GTiff", overwrite = TRUE)
remove(clc1990)

#clc2018 raster
clc2018 = raster("geodata/clc2018.tif")
clc2018_2 = reclassify(clc2018, matrix.rcl)
# writeRaster(clc2018_2, "geodata/clc2018_2.tif", format ="GTiff", overwrite = TRUE)
remove(clc2018)

# plot(clc1990_2)
# plot(clc2018_2)

#reclassified raster maps ---------------------------------------------------
# clc1990_2 = raster("geodata/clc1990_2.tif")
# clc2018_2 = raster("geodata/clc2018_2.tif")

new_labels = c("1. lasy",
               "2. pozostałe tereny")


#1990
# tmap_mode('view')
lc1990 = tm_shape(clc1990_2) +
  tm_raster(palette = c("#00a600", "#e6e6e6"),
            # labels = lc_palette_df$label,
            labels = new_labels,
            style = "cat",
            title = "Formy pokrycia terenu w 1990:") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

# tmap_save(lc1990, filename = "prints/clc90.png",
#           width = 2100, height = 1400)

#2018
# tmap_mode('view')
lc2018 = tm_shape(clc2018_2) +
  tm_raster(palette = c("#00a600", "#e6e6e6"),
            # labels = lc_palette_df$label,
            labels = new_labels,
            style = "cat",
            title = "Formy pokrycia terenu w 2018:") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

# tmap_save(lc2018, filename = "prints/clc18.png",
#           width = 2100, height = 1400)




library(tmap)
library(dplyr)
library(raster)
library(ggplot2)
library(readr)
library(stars)
library(motif)
library(purrr)

#paleta
lc_palette_df = read_csv("geodata/legend.csv")
names(lc_palette_df$color) = lc_palette_df$value

#dane
# clc1990_2 = raster("geodata/clc1990_2.tif")
# clc2018_2 = raster("geodata/clc2018_2.tif")



# MEASURES ----------------------------------------------------------------
# 1.canberra
# 2.clark
# 3.divergence
# 4.euclidean



# canberra ----------------------------------------------------------------
canberra = lsp_compare(st_as_stars(clc2018_2), st_as_stars(clc1990_2),
                           type = "cove", dist_fun = "canberra",
                           window = 100, threshold = 0.9)

# canberra_sf = st_as_sf(canberra)
# canberra_sel = slice_max(canberra_sf, dist, n = 6)
# 
# canberra_ex = lsp_add_examples(x = canberra_sel,
#                                    y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))

map_full1 = tm_shape(canberra) +
  tm_raster("dist", style = "order", palette = "-viridis",
            title = "Canberra") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

map_full1
# tmap_save(map_full1 + tm_layout(scale = 0.5),
#           "prints/map_grid.png",
#           asp = NA, height = 650, width = 1000)



# clark -------------------------------------------------------------------
clark = lsp_compare(st_as_stars(clc2018_2), st_as_stars(clc1990_2),
                           type = "cove", dist_fun = "clark",
                           window = 100, threshold = 0.9)

# clark_sf = st_as_sf(clark)
# clark_sel = slice_max(clark_sf, dist, n = 6)
# 
# clark_ex = lsp_add_examples(x = clark_sel,
#                                    y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))

map_full1 = tm_shape(clark) +
  tm_raster("dist", style = "order", palette = "-viridis",
            title = "Clark") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

map_full1
# tmap_save(map_full1 + tm_layout(scale = 0.5),
#           "prints/map_grid.png",
#           asp = NA, height = 650, width = 1000)



# divergence --------------------------------------------------------------
divergence = lsp_compare(st_as_stars(clc2018_2), st_as_stars(clc1990_2),
                           type = "cove", dist_fun = "divergence",
                           window = 100, threshold = 0.9)

# divergence_sf = st_as_sf(divergence)
# divergence_sel = slice_max(divergence_sf, dist, n = 6)
# 
# divergence_ex = lsp_add_examples(x = divergence_sel,
#                                    y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))

map_full1 = tm_shape(divergence) +
  tm_raster("dist", style = "order", palette = "-viridis",
            title = "Divergence") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

map_full1
# tmap_save(map_full1 + tm_layout(scale = 0.5),
#           "prints/map_grid.png",
#           asp = NA, height = 650, width = 1000)



# euclidean ---------------------------------------------------------------
euclidean = lsp_compare(st_as_stars(clc2018_2), st_as_stars(clc1990_2),
                           type = "cove", dist_fun = "euclidean",
                           window = 100, threshold = 0.9)

# euclidean_sf = st_as_sf(euclidean)
# euclidean_sel = slice_max(euclidean_sf, dist, n = 6)
# 
# euclidean_ex = lsp_add_examples(x = euclidean_sel,
#                                    y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))

map_full1 = tm_shape(euclidean) +
  tm_raster("dist", style = "order", palette = "-viridis",
            title = "Euclidean") +
  tm_compass(north = 335) +
  tm_scale_bar() +
  tm_layout(legend.outside = TRUE,
            attr.position = c("left", "BOTTOM"))

map_full1
# tmap_save(map_full1 + tm_layout(scale = 0.5),
#           "prints/map_grid.png",
#           asp = NA, height = 650, width = 1000)



# dissimilarity differences -----------------------------------------------

library(scales)
# # nx <- minmax(canberra$dist)    
# # canberra$dist <- (s - nx[1,]) / (nx[2,] - nx[1,])
# 
# canberra$dist = scales::rescale(canberra$dist)
# clark$dist = scales::rescale(clark$dist)
# divergence$dist = scales::rescale(divergence$dist)
# euclidean$dist = scales::rescale(euclidean$dist)
# 
# combn(c("canberra", "clark", "divergence", "euclidean"), 2)
# # combn(c("canberra", "clark", "divergence"), 2)
# 
# test1 = abs(canberra - clark)
# test2 = abs(canberra - divergence)
# test3 = abs(canberra - euclidean)
# 
# test4 = abs(clark - divergence)
# test5 = abs(clark - euclidean)
# 
# test6 = abs(divergence - euclidean)
# 
# 
# map_full1 = tm_shape(test1) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# map_full1 = tm_shape(test2) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# map_full1 = tm_shape(test3) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# map_full1 = tm_shape(test4) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# map_full1 = tm_shape(test5) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# map_full1 = tm_shape(test6) +
#   tm_raster("dist", style = "order", palette = "-viridis",
#             title = "diff")
# map_full1
# 
# 
# 
# test = test1
# test$test1 = test1$dist
# test$test2 = test2$dist
# test$test3 = test3$dist
# test$test4 = test4$dist
# test$test5 = test5$dist
# test$test6 = test6$dist
# 
# rast_names = names(test)[5:10]
# 
# tm_shape(test) +
#   tm_raster(rast_names, style = "order", palette = "-viridis") +
#   tm_facets(free.scales.raster = TRUE)
# 
# library(terra)
# test_terra = rast(test)
# 
# test_terra = subset(test_terra, c("test1_lyr.1", "test2_lyr.1", "test3_lyr.1", "test4_lyr.1", "test5_lyr.1", "test6_lyr.1"))
# # test_terra = subset(test_terra, c("test1_lyr.1", "test2_lyr.1", "test4_lyr.1"))
# plot(test_terra)
# 
# test_cor = layerCor(test_terra, fun = "pearson", na.rm = TRUE)
# 
# round(test_cor$correlation,2)
# 
# 
# corrplot::corrplot(test_cor$correlation, method = "color", type = "lower")
# 



# maps & histograms -------------------------------------------------------

canberra$dist = scales::rescale(canberra$dist)
clark$dist = scales::rescale(clark$dist)
divergence$dist = scales::rescale(divergence$dist)
euclidean$dist = scales::rescale(euclidean$dist)

test2 = canberra
test2$canberra = canberra$dist
test2$clark = clark$dist
test2$divergence = divergence$dist
test2$euclidean = euclidean$dist

rast_names = names(test2)[5:8]

zestawienie_miar1 = tm_shape(test2) +
  tm_raster(rast_names, title = "Wartość\nniepodobieństwa", style = "equal", palette = "-viridis") +
  tm_facets() +
  tm_layout(panel.labels = c('Canberra','Clark','Divergence','Euclidean'),
            legend.outside = TRUE)

tmap_save(zestawienie_miar1, filename = "plots/zestawienie_miar1.png", width = 1400, height = 1000, units = "px")


# "cat", "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", and "log10_pretty"


test3 = as.data.frame(test2)[c("canberra", "clark", "divergence", "euclidean")]
library(tidyr)

test4 = test3 %>% 
  pivot_longer(cols = c(1:4) ,names_to = "measure", values_to = "value")

library(stringr)
test4$measure = str_to_sentence(test4$measure)


zestawienie_hist1 = ggplot(test4, aes(x = value)) +
  geom_histogram(color="black", fill="beige", bins = 20) +
  facet_wrap(measure ~ ., ncol = 2) +
  theme_bw() +
  labs(x = "Wartość niepodobieństwa", y = "Liczba obserwacji")

ggsave(plot = zestawienie_hist1, filename = "plots/zestawienie_hist1.png", width = 2000, height = 2000, units = "px")


# correlation -------------------------------------------------------------

library(terra)
test_terra = rast(test2)

test_terra = subset(test_terra, c("canberra_lyr.1", "clark_lyr.1", "divergence_lyr.1", "euclidean_lyr.1"))
# test_terra = subset(test_terra, c("test1_lyr.1", "test2_lyr.1", "test4_lyr.1"))
plot(test_terra)

test_cor = layerCor(test_terra, fun = "pearson", na.rm = TRUE)

corr_df = as.data.frame(round(test_cor$correlation,2))

colnames(corr_df) = str_to_sentence(sub("_lyr.*", "", colnames(corr_df)))
rownames(corr_df) = colnames(corr_df)

write.csv(corr_df, file = "plots/measure_corr_df.csv", row.names = TRUE)




corrplot::corrplot(test_cor$correlation, method = "color", type = "lower")

plot(canberra$dist, clark$dist) #warto zwrócić uwagę na ten jeden odstający punkt

plot(euclidean$dist, divergence$dist)


# df1 = t(data.frame(divergence = c(-0.46, -0.45, -0.63), clark = c(-0.46, -0.45, -0.63), pearson = c(-0.44, -0.34, -0.48)))
# 
# df2 = as.data.frame(abs(df1))
# df2$rank = rowMeans(df2)
# 
# df3 = as.data.frame(cbind(df1, df2$rank))



# ggplot(test4) +
#   geom_point()

library(GGally)

colnames(test3) = str_to_sentence(colnames(test3))

measures_corrplot = ggpairs(test3) +
  theme_bw()

ggsave(measures_corrplot, filename = "plots/measures_corrplot.png", width = 2000, height = 2000, units = "px")

