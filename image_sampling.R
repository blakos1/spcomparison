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


# check how many items overlap between all lists --------------------------
intersect_all = function(a,b,...){
  Reduce(intersect, list(a,b,...))
}

test = intersect_all(sample1, sample2) 
paste(length(test), "repeating items in all lists")


# check how many items overlap between every list -------------------------
test1 = intersect(sample1, sample2)


# write datasets into .csv files ------------------------------------------
write.csv(sample1, file = "data/sample1.csv")
write.csv(sample2, file = "data/sample2.csv")
