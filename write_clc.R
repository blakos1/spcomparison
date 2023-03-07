library(raster)
library(motif)
library(stars)
library(dplyr)
library(purrr)

clc1990_2 = read_stars("data/clc1990_2.tif")
clc2018_2 = raster("data/clc2018_2.tif")

str(clc1990_2)

# 1- antro
# 2 - ter rolne
# 3 - lasy
# 4 - tereny krzewiaste
# 5 - tereny z rzadka roslinnoscia
# 6 - obszary podmokle
# 7 - obszary wodne

input = c(1,2,3,4,5,6,7)
output = c(1,1,2,2,1,2,1)
# output = c(1,1,2,2,3,3,3)
matrix.rcl = cbind(input,output)

clc1990_2 = reclassify(clc2018_2, matrix.rcl)
plot(clc2018_2)


clc1990_2 = st_as_stars(clc1990_2)
st_crs(clc1990_2) = "EPSG:2180"

plot(lsp_extract(clc2018_2, window = 100, id = 1))


# #clc1990 raster
# clc1990 = raster("data/clc1990.tif")
# clc1990_2 = reclassify(clc1990, matrix.rcl)
# # writeRaster(clc1990_2, "data/clc1990_2.tif", format ="GTiff", overwrite = TRUE)
# remove(clc1990)



clc1990_2 = raster("data/clc1990_2.tif")
clc2018_2 = raster("data/clc2018_2.tif")

st_crs(clc1990_2) = "EPSG:2180"
st_crs(clc2018_2) = "EPSG:2180"

lc_compare_1 = lsp_compare(st_as_stars(clc2018_2), st_as_stars(clc1990_2),
                           type = "cove", dist_fun = "jensen-shannon",
                           window = 100, threshold = 0.9)

lc_compare_1_sf = st_as_sf(lc_compare_1)
lc_compare_1_sf = na.omit(lc_compare_1_sf) #??????????

lc_compare_1_sf = lc_compare_1_sf[order(lc_compare_1_sf$dist, decreasing=TRUE, na.last=TRUE),]
# lc_compare_1_sel = slice_max(lc_compare_1_sf, dist, n = 20)


lc_compare_1_ex = lsp_add_examples(x = lc_compare_1_sel,
                                   y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))

plot(lc_compare_1_ex$region[[5]][1])

# lc_compare_1_sel = slice_max(lc_compare_1_sf, dist, n = 6)
# 
# lc_compare_1_ex = lsp_add_examples(x = lc_compare_1_sel,
#                                    y = c(st_as_stars(clc1990_2), st_as_stars(clc2018_2)))
