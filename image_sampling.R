library(dplyr)
library(tidyr)


# image sampling ----------------------------------------------------------
filename_df = as.data.frame(list.files(path = "resources", full.names = TRUE))
colnames(filename_df) = "filenames"

filename_df$classes = as.integer(substr(filename_df$filenames, 15,15))
filename_df$row = as.integer(substr(filename_df$filenames, 27,27))
filename_df$col = as.integer(substr(filename_df$filenames, 32,32))


sample_images = function(filename_df){
  dataset = list()
  samplesize = 12
  
  #2 klasy
  for(j in 1:2){ #pick row or col
    for(k in 1:6){ #pick items in rows/cols
      
      if (j == 2){
        subset_df = subset(filename_df, classes == 2 & col == k)
      } else {
        subset_df = subset(filename_df, classes == 2 & row == k)
      }
      
      smpl = subset_df$filenames %>% 
        combn(m = 2) %>% 
        as.data.frame() %>%
        as.list() %>% 
        sample(size = 1)
      
      dataset = append(dataset, smpl)
    }
  }
  
  #wybranie wszystkich obrazów z 2 klasami
  subset2 = subset(filename_df, classes == 2)$filenames %>% 
    combn(m = 2) %>% 
    as.data.frame() %>%
    as.list()
  
  #samplowanie tylko z reszty obrazów (2 klasy)
  smpl2 = subset(subset2, !(subset2 %in% dataset)) %>% 
    sample(size = samplesize)
  
  dataset = append(dataset, smpl2)
  
  #3 klasy
  for(j in 1:2){ #pick row or col
    for(k in 1:6){ #pick items in rows/cols
      
      if (j == 2){
        subset_df = subset(filename_df, classes == 3 & col == k)
      } else {
        subset_df = subset(filename_df, classes == 3 & row == k)
      }
      
      smpl = subset_df$filenames %>% 
        combn(m = 2) %>% 
        as.data.frame() %>%
        as.list() %>% 
        sample(size = 1)
      
      dataset = append(dataset, smpl)
    }
  }
  
  #wybranie wszystkich obrazów z 3 klasami
  subset3 = subset(filename_df, classes == 3)$filenames %>% 
    combn(m = 2) %>% 
    as.data.frame() %>%
    as.list()
  
  #samplowanie tylko z reszty obrazów (3 klasy)
  smpl3 = subset(subset3, !(subset3 %in% dataset)) %>% 
    sample(size = samplesize)
  
  dataset = append(dataset, smpl3)
  return(dataset)
}

set.seed(2022-11-17)

sample1 = sample_images(filename_df)
sample2 = sample_images(filename_df)
sample3 = sample_images(filename_df)
sample4 = sample_images(filename_df)


# check how many items overlap between all lists --------------------------
intersect_all <- function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

test = intersect_all(sample1, sample2, sample3, sample4) 
paste(length(test), "repeating items in all lists")


# check how many items overlap between every list -------------------------
test1 = intersect(sample1, sample2)
# test2 = intersect(sample1, sample3)
# test3 = intersect(sample1, sample4)
# 
# test4 = intersect(sample2, sample3)
# test5 = intersect(sample2, sample4)
# 
# test6 = intersect(sample3, sample4)

paste(length(c(test1, test2, test3, test4, test5, test6)), "overlapping items in lists, with",
length(unique(c(test1, test2, test3, test4, test5, test6))), "unique items")

# write datasets into .csv files ------------------------------------------
write.csv(sample1, file = "sample1.csv")
write.csv(sample2, file = "sample2.csv")
# write.csv(sample3, file = "sample3.csv")
# write.csv(sample4, file = "sample4.csv")

# sample11 = read.csv("sample1.csv")
