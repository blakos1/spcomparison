library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


# read files --------------------------------------------------------------
data2 = read.csv("data/2classes_dist.csv")
data3 = read.csv("data/3classes_dist.csv")

# remove inf's & na's -----------------------------------------------------
data2[sapply(data2, is.infinite)] = NA
data22 = data2 %>% 
  subset(select = -c(X, minkowski)) %>% 
  na.omit()

data3[sapply(data3, is.infinite)] = NA
data33 = data3 %>% 
  subset(select = -c(X, minkowski)) %>% 
  na.omit()

nrow(data2) + nrow(data3) - nrow(data22) - nrow(data33) #aż tyle danych ma jakieś wartości NA lub Inf

merged = rbind(data22, data33)

# transpose dataframe -----------------------------------------------------
data22_t = data22 %>% 
  pivot_longer(cols = c(1:ncol(data22)-1),
               names_to = "method", values_to = "value") %>% 
  pivot_wider(names_from = "questionID", values_from = "value")

names = data22_t$method
data22_t = as.data.frame(subset(data22_t, select = -c(method)))
rownames(data22_t) = names

colnames(data22_t) = c(1:ncol(data22_t))


data33_t = data33 %>% 
  pivot_longer(cols = c(1:ncol(data33)-1),
               names_to = "method", values_to = "value") %>% 
  pivot_wider(names_from = "questionID", values_from = "value")

names = data33_t$method
data33_t = as.data.frame(subset(data33_t, select = -c(method)))
rownames(data33_t) = names

colnames(data33_t) = c(1:ncol(data33_t))


merged_t = merged %>% 
  pivot_longer(cols = c(1:ncol(merged)-1),
               names_to = "method", values_to = "value") %>% 
  pivot_wider(names_from = "questionID", values_from = "value")

names = merged_t$method
merged_t = as.data.frame(subset(merged_t, select = -c(method)))
rownames(merged_t) = names

colnames(merged_t) = c(1:ncol(merged_t))

# scale the values --------------------------------------------------------
data22_scaled = scale(data22_t)
data33_scaled = scale(data33_t)
merged_scaled = scale(merged_t)

# AGNES -------------------------------------------------------------------
# methods to assess
m <- c("average", "single", "complete", "ward", "weighted")
names(m) <- c("average", "single", "complete", "ward", "weighted")

# agglomerative coefficient
ac = data.frame(method = m)
for (i in 1:length(ac$method)){
  ac$two_man[i] = agnes(data22_scaled, method = ac$method[i], metric = "manhattan")$ac
  ac$three_man[i] = agnes(data33_scaled, method = ac$method[i], metric = "manhattan")$ac
  ac$all_man[i] = agnes(merged_scaled, method = ac$method[i], metric = "manhattan")$ac
  ac$two_euc[i] = agnes(data22_scaled, method = ac$method[i], metric = "euclidean")$ac
  ac$three_euc[i] = agnes(data33_scaled, method = ac$method[i], metric = "euclidean")$ac
  ac$all_euc[i] = agnes(merged_scaled, method = ac$method[i], metric = "euclidean")$ac
}

# divise coefficient / DIANA
diana(data22_scaled)$dc
diana(data33_scaled)$dc
diana(merged_scaled)$dc

# Cut agnes() tree into N groups
nclust = 9

agnes22 <- agnes(data22_scaled, method = "ward", metric = "manhattan")
sub_grp22 = cutree(as.hclust(agnes22), k = nclust)

agnes33 <- agnes(data33_scaled, method = "ward", metric = "manhattan")
sub_grp33 = cutree(as.hclust(agnes33), k = nclust)

agnes_all <- agnes(merged_scaled, method = "ward", metric = "manhattan")
sub_grp_all = cutree(as.hclust(agnes_all), k = nclust)

# Number of members in each cluster
table(sub_grp22)
table(sub_grp33)
table(sub_grp_all)

# add groups to df
data22_mut = data22_scaled %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp22)

data33_mut = data33_scaled %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp33)

merged_mut = merged_scaled %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp_all)

# groups & corrs df
corrs = read.csv("data/corrs.csv", row.names = 2) %>% subset(select = -X)

groups = as.data.frame(rownames(data22_mut)) %>% 
  cbind(merged_mut$cluster) %>% 
  cbind(data22_mut$cluster) %>% 
  cbind(data33_mut$cluster) %>% 
  cbind(corrs)

colnames(groups)[1:4] = c("measure", "two_cat", "three_cat", "all_cat")


# agnes plots
pltree(agnes22, cex = 0.6, hang = -1, main = "Dendrogram of agnes - 2 kat")
rect.hclust(agnes22, k = nclust, border = 2:5)

fviz_cluster(list(data = data22_mut, cluster = sub_grp22))


pltree(agnes33, cex = 0.6, hang = -1, main = "Dendrogram of agnes - 3 kat")
rect.hclust(agnes33, k = nclust, border = 2:5)

fviz_cluster(list(data = data33_mut, cluster = sub_grp33))


pltree(agnes_all, cex = 0.6, hang = -1, main = "Dendrogram of agnes - 2 & 3 kat")
rect.hclust(agnes_all, k = nclust, border = 2:5)

fviz_cluster(list(data = merged_mut, cluster = sub_grp_all))

# entanglement ------------------------------------------------------------
# Compute distance matrix
res.dist22 <- dist(data22_scaled, method = "euclidean")
res.dist33 <- dist(data33_scaled, method = "euclidean")

# Compute 2 hierarchical clusterings
hc22 <- hclust(res.dist22, method = "ward.D2")
hc33 <- hclust(res.dist33, method = "ward.D2")

# Create two dendrograms
dend22 <- as.dendrogram(hc22)
dend33 <- as.dendrogram(hc33)

dend_list <- dendlist(dend22, dend33)

round(entanglement(dend_list), 2) #ENTANGLEMENT

tanglegram(dend22, dend33,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

# Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement).
# A lower entanglement coefficient corresponds to a good alignment. 
