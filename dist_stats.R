library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms


# read files --------------------------------------------------------------
data2 = read.csv("data/2classes_dist.csv")
data3 = read.csv("data/3classes_dist.csv")
merged = rbind(data2, data3)

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

# scale the values --------------------------------------------------------
data22_scaled = scale(data22_t)
data33_scaled = scale(data33_t)

# AGNES -------------------------------------------------------------------
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# agglomerative coefficient
ac = data.frame(method = m)
for (i in 1:length(ac$method)){
  ac$two[i] = agnes(data22_scaled, method = ac$method[i])$ac
  ac$three[i] = agnes(data33_scaled, method = ac$method[i])$ac
}

# divise coefficient / DIANA
diana(data22_scaled)$dc
diana(data33_scaled)$dc

# Cut agnes() tree into 5 groups
agnes22 <- agnes(data22_scaled, method = "ward")
sub_grp22 = cutree(as.hclust(agnes22), k = 5)

agnes33 <- agnes(data33_scaled, method = "ward")
sub_grp33 = cutree(as.hclust(agnes33), k = 5)

# Number of members in each cluster
table(sub_grp22)
table(sub_grp33)

# add groups to df
data22_mut = data22_scaled %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp22)

data33_mut = data33_scaled %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp33)

# agnes plots
pltree(agnes22, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
rect.hclust(agnes22, k = 5, border = 2:5)

fviz_cluster(list(data = data22_mut, cluster = sub_grp22))

pltree(agnes33, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
rect.hclust(agnes33, k = 5, border = 2:5)

fviz_cluster(list(data = data33_mut, cluster = sub_grp33))



# entanglement ------------------------------------------------------------
# Compute distance matrix
res.dist22 <- dist(data22_scaled, method = "euclidean")
res.dist33 <- dist(data33_scaled, method = "euclidean")

# Compute 2 hierarchical clusterings
hc22 <- hclust(res.dist22, method = "complete")
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
