library(raster)
library(tmap)

# read files --------------------------------------------------------------
my_sims2_2classes = stack("my_sims2_2classes.tif")
my_sims2_3classes = stack("my_sims2_3classes.tif")

# change names of raster layers  ------------------------------------------
colnums = rep(seq(1:6), 6)
rownums = rep(1:6, each = 6)
names(my_sims2_2classes) = paste0("img_2classes_", "row", rownums, "_col", colnums)

colnums = rep(seq(1:9), 6)
rownums = rep(1:6, each = 9)
names(my_sims2_3classes) = paste0("img_3classes_", "row", rownums, "_col", colnums)

# write plots -------------------------------------------------------------
merged = stack(my_sims2_2classes, my_sims2_3classes)

for (i in 1:nlayers(merged)){
  x = merged[[i]]
  img = tm_shape(x) +
    tm_raster(legend.show = FALSE)
  tmap_save(tm = img, filename = paste0("resources/", names(x), ".png"),
            width = 600, height = 600)
}
