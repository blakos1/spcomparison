library(tidyverse)  # data manipulation
library(raster)
library(landscapemetrics)


# read files --------------------------------------------------------------
data2 = stack("data/my_sims2_2classes.tif")

# 2classes row2 col1
img1 = data2[[7]]

plot(img1)
img1_ent = lsm_l_ent(img1)$value %>% round(2)
img1_rmi = lsm_l_relmutinf(img1)$value %>% round(2)


# 2classes row4 col3
img2 = data2[[21]]

plot(img2)
img2_ent = lsm_l_ent(img2)$value %>% round(2)
img2_rmi = lsm_l_relmutinf(img2)$value %>% round(2)



img1_ent - img2_ent
img1_rmi - img2_rmi

