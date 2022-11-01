library(stringr)
# library(NLMR)
# simulate_composition = function(fract_dim){
#   sim1 = nlm_fbm(100, 100, fract_dim = fract_dim)
#   sim1
# }
# 
# x = simulate_composition(0.2)
# y = simulate_composition(1.6)
# 
# plot(x)
# plot(y)

filename_df = as.data.frame(list.files(path = "resources", full.names = TRUE))
colnames(filename_df) = "filenames"

filename_df$classes = as.integer(substr(filename_df$filenames, 15,15))
filename_df$row = as.integer(substr(filename_df$filenames, 27,27))
filename_df$col = as.integer(substr(filename_df$filenames, 32,32))


#2 klasy:
#6 par po 1 na każdą kolumnę
#6 par po 1 na każdy wiersz

#3 klasy:
#6 par po 1 na każdą kolumnę (powinno być 9 - jak naprawić?)
#6 par po 1 na każdy wiersz

dataset = list()

for(i in 2:3){ #pick num of classes
  for(j in 1:2){ #pick row or col
    for(k in 1:6){ #pick items in rows/cols
      
      if (j == 1){
        subset_df = subset(filename_df, classes == i & col == k)
      } else {
        subset_df = subset(filename_df, classes == i & row == k)
      }
      
      smpl = subset_df$filenames %>% 
        combn(m = 2) %>% 
        as.data.frame() %>%
        as.list() %>% 
        sample(size = 1)
      
      dataset = append(dataset, smpl)
    }
  }
}

subset2 = subset(filename_df, classes == 2)$filenames %>% 
  combn(m = 2) %>% 
  as.data.frame() %>%
  as.list()
subset3 = subset(filename_df, classes == 3)$filenames %>% 
  combn(m = 2) %>% 
  as.data.frame() %>%
  as.list()

df = as.list(c(subset2, subset3))

residuals_df = subset(df, !(df %in% dataset)) 
