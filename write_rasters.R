library(raster)
library(tmap)

# read files --------------------------------------------------------------
my_sims2_2classes = stack("my_sims2_2classes.tif")
my_sims2_3classes = stack("my_sims2_3classes.tif")

# change names ------------------------------------------------------------
colnums = rep(seq(1:6), 6)
rownums = rep(1:6, each = 6)
names(my_sims2_2classes) = paste0("img_2classes_", "col", colnums, "_row", rownums)

colnums = rep(seq(1:9), 6)
rownums = rep(1:6, each = 9)
names(my_sims2_3classes) = paste0("img_3classes_", "col", colnums, "_row", rownums)

# write plots -------------------------------------------------------------
# export_plots = function(x){
#   img = tm_shape(x) +
#     tm_raster(legend.show = FALSE)
#   
#   tmap_save(tm = img, filename = paste0("resources/", names(x), ".png"),
#             width = 600, height = 600)
# }
# 
# raster::stackApply(my_sims2_2classes, indices = c(1,2,3,4),
#                    fun = export_plots)

merged = stack(my_sims2_2classes, my_sims2_3classes)

for (i in 1:nlayers(merged)){
  x = my_sims2_2classes[[i]]
  img = tm_shape(x) +
    tm_raster(legend.show = FALSE)
  tmap_save(tm = img, filename = paste0("resources/", names(x), ".png"),
            width = 600, height = 600)
}
