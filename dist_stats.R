library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# read files --------------------------------------------------------------
data2 = read.csv("data/2classes_dist.csv")
# data2 = read.csv("data/3classes_dist.csv")

# remove inf's & na's -----------------------------------------------------
data2[sapply(data2, is.infinite)] = NA
data2 = subset(data2, select = -c(X, minkowski))
data2 = na.omit(data2)

# transpose dataframe -----------------------------------------------------
data2_2 = data2 %>% 
  pivot_longer(cols = c(1:ncol(data2)-1),
               names_to = "method", values_to = "value") %>% 
  pivot_wider(names_from = "questionID", values_from = "value")

names = data2_2$method
data2_2 = as.data.frame(subset(data2_2, select = -c(method)))
rownames(data2_2) = names

colnames(data2_2) = c(1:ncol(data2_2))

# scale the values --------------------------------------------------------
data2_2 = scale(data2_2)

# HCLUST ------------------------------------------------------------------
df = data2_2

# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete")

# Plot the obtained dendrogram
plot(as.dendrogram(hc1), horiz = TRUE)
plot(hc1, cex = 0.6, hang = -1)


# dendro ------------------------------------------------------------------
library(ggdendro)
dendr <- dendro_data(hc1, type="rectangle") #convert cluster object to use with ggplot

ggplot() + 
  geom_segment(data=segment(dendr), aes(x=x, y=y, xend=xend, yend=yend)) + 
  geom_text(data=label(dendr), aes(x=x, y= c(y -0.5), label=label, hjust=1), size=3) +
  coord_flip() +
  # scale_y_reverse(expand=c(0.2, 0)) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())



# agnes -------------------------------------------------------------------
# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac
## [1] 0.9204506
#agglomerative coefficient, which measures the amount of clustering structure found
#(values closer to 1 suggest strong clustering structure)


# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
##   average    single  complete      ward 
## 0.9085222 0.9005353 0.9204506 0.9231909 

hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes")


# diana -------------------------------------------------------------------
# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc
## [1] 0.9174377

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")

# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 4 groups
sub_grp <- cutree(hc5, k = 8)

# Number of members in each cluster
table(sub_grp)

df = df %>%
  as.data.frame() %>% 
  mutate(cluster = sub_grp)


plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 8, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 8)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 8)

# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)
# Entanglement is a measure between 1 (full entanglement) and 0 (no entanglement).
# A lower entanglement coefficient corresponds to a good alignment. 


# Determining Optimal Clusters --------------------------------------------
fviz_nbclust(df, FUN = hcut, method = "wss")

fviz_nbclust(df, FUN = hcut, method = "silhouette")


