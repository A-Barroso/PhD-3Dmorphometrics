
#### You will need to install Rtools before proceeding, which can be found here: https://cran.rstudio.com/bin/windows/Rtools/

### Step 1: Using the relevant packages
# List of required packages
packages <- c("plotly", "tidyverse", "ggplot2", "dplyr", "tidyr", "reshape2", "energy", "MASS", "vegan", "ape", "dendextend")

# Install any missing packages
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Load all of the required packages
lapply(packages, library, character.only = TRUE)

# reshape 2 > boxplots
# mass > LDA
# vegan > PERMOVA (alternative to MANOVA)


### STEP 2: Import Principle Component Data

GMdata <- read.csv("Aston-Beckford_Unit.csv")

GMdata$Site <- as.factor(GMdata$Site)
is.factor(GMdata$Site)




### STEP 3: PCA Scatterplots 

ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site)) +
  geom_point(size = 2) +
  geom_rug() +
  stat_ellipse(size = 1.2) +
  scale_color_manual(values=c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E'))+
  theme_classic() +
  theme(legend.position="bottom")

ggsave("PCA1a.png", dpi = 800)



ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site)) +
  geom_point(size = 2) +
  geom_rug() +
  stat_ellipse(size = 1.2) +
  scale_color_manual(values=c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E'))+
  geom_label(aes(label = Object), size = 1.4) + 
  theme_classic() +
  theme(legend.position="bottom")

ggsave("PCA1b.png", dpi = 800)


ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site)) +
  geom_point(size = 2) +
  geom_rug() +
  stat_ellipse(size = 1.2) +
  scale_color_manual(values=c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E'))+
  geom_text(aes(label = Object), hjust = 0.5, vjust = -0.5, size = 1.5) +  
  theme_classic() +
  theme(legend.position="bottom")

ggsave("PCA1c.png", dpi = 800)




Hull <- GMdata %>%
  group_by(Site) %>%
  do(slice(., chull(.$PC1, .$PC2))) %>%
  ungroup()  # Ungroup to avoid issues in ggplot

Plot <- ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site)) +
  geom_point(size = 2) +
  geom_polygon(data = Hull, aes(x = PC1, y = PC2, group = Site, fill = Site), colour = NA, alpha = 0.3) +
  scale_color_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  scale_fill_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  theme_light() +
  theme(legend.position = "bottom")

Plot

ggsave("PCA2.png", dpi = 800)








Hull <- GMdata %>%
  group_by(Site) %>%
  do(slice(., chull(.$PC1, .$PC2))) %>%
  ungroup()  # Ungroup to avoid issues in ggplot

Plot <- ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site, shape = Intent)) +
  geom_point(size = 2) +
  geom_polygon(data = Hull, aes(x = PC1, y = PC2, group = Site, fill = Site), colour = NA, alpha = 0.3) +
  scale_color_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  scale_fill_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  theme_light() +
  theme(legend.position = "bottom")

Plot


ggsave("PCA3.png", dpi = 800)




Hull <- GMdata %>%
  group_by(Site) %>%
  do(slice(., chull(.$PC1, .$PC2))) %>%
  ungroup()  # Ungroup to avoid issues in ggplot

Plot <- ggplot(GMdata, aes(x = PC1, y = PC2, colour = Site, shape = Shape)) +
  geom_point(size = 2) +
  geom_polygon(data = Hull, aes(x = PC1, y = PC2, group = Site, fill = Site), colour = NA, alpha = 0.3) +
  scale_color_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  scale_fill_manual(values = c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  theme_light() +
  theme(legend.position = "bottom")

Plot


ggsave("PCA4.png", dpi = 800)



### STEP 4: Boxplots

GMdataC <- read.csv("Aston-Beckford_UnitCLEAN.csv")
df.m <- melt(GMdataC, id.var = "Site")
df.m

ggplot(df.m) + 
  geom_boxplot(aes(x=variable, y=value, fill=Site)) +
  theme_light() +
  scale_fill_manual(values=c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  labs(x = "Principal Component") +
  theme(legend.position = "bottom")


ggsave("PCA5.png", dpi = 800)


ggplot(df.m) + 
  geom_boxplot(aes(x=variable, y=value, fill=Site)) +
  theme_light() +
  scale_fill_manual(values=c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E')) +
  labs(x = "Principal Component") +
  theme(legend.position = "bottom") +
  facet_wrap(~Site)


ggsave("PCA6.png", dpi = 800)










### Step 5a: stats - between two groups

# Wilcoxon test for PC1 if data is not normally distributed
wilcox_test_PC1 <- wilcox.test(PC1 ~ Site, data = GMdata)
print(wilcox_test_PC1)


# The multivariate Energy Test for PCs 1-10 (non-parametric)
group1_data <- subset(GMdata, Site == "Aston_Mill")[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]
group2_data <- subset(GMdata, Site == "Beckford_Quarry")[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]

etest_result <- energy::eqdist.etest(rbind(group1_data, group2_data), 
                                     sizes = c(nrow(group1_data), nrow(group2_data)),
                                     R = 999)  # R is the number of permutations
print(etest_result)






### Step 5b: stats - between all groups

# One-way ANOVA for PC1 across multiple groups, comparing the means of PC1
anova_PC1 <- aov(PC1 ~ Site, data = GMdata)
summary(anova_PC1)


# If the ANOVA indicates significant differences, follow up with
# Tukey's HSD (Honestly Significant Difference) test for pairwise comparisons between the groups
tukey_PC1 <- TukeyHSD(anova_PC1)
print(tukey_PC1)


# To test multiple groups: the Kruskal-Wallis test (non-parametric equivalent of ANOVA)
kruskal_PC1 <- kruskal.test(PC1 ~ Site, data = GMdata)
print(kruskal_PC1)

kruskal_PC2 <- kruskal.test(PC2 ~ Site, data = GMdata)
print(kruskal_PC2)

# If Kruskal-Wallis indicates significant differences, 
# run a Dunn's test for pairwise comparisons (post-hoc)
library(dunn.test)
dunn_PC1 <- dunn.test(GMdata$PC1, GMdata$Site)
print(dunn_PC1)





### Step 5c:  Multivariate Energy Test for multiple groups

groups <- unique(GMdata$Site) # Groups: Aston_Mill, Beckford, Le_Moustier, GreatPanFarm

run_energy_test <- function(group1, group2, data) {
  group1_data <- subset(data, Site == group1)[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]
  group2_data <- subset(data, Site == group2)[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")]
  result <- energy::eqdist.etest(rbind(group1_data, group2_data), 
                                 sizes = c(nrow(group1_data), nrow(group2_data)),
                                 R = 999)
  
  return(result)
}

# Run the Energy test for all pairs of groups
energy_results <- list()

# Loop over all unique pairs of groups and run the energy test for the current pair of groups
for (i in 1:(length(groups) - 1)) {
  for (j in (i + 1):length(groups)) {
    group1 <- groups[i]
    group2 <- groups[j]
    energy_results[[paste(group1, group2, sep = "_vs_")]] <- run_energy_test(group1, group2, GMdata)
  }
}

# Print results for each group comparison
for (result_name in names(energy_results)) {
  cat("\nResults for", result_name, ":\n")
  print(energy_results[[result_name]])
}






### STEP 5d: Permutational Multivariate Analysis of Variance (PERMANOVA)
# This is a non-parametric alternative to MANOVA that is robust to non-normal distributions.

permanova_result <- adonis(cbind(PC1, PC2) ~ Site, data = GMdata, method = "euclidean")
print(permanova_result)









### STEP 6: Linear Discriminant Analysis

GMdata2 <- read.csv("RawPCdata2.csv")
GMdata2$Site <- as.factor(GMdata2$Site)


lda_model <- lda(Site ~ PC1 + PC2 + PC3 + PC4 +PC5 +PC6 +PC7 +PC8 +PC9 +PC10 +PC11, data = GMdata2)

print(lda_model)

predictions <- predict(lda_model)

# Confusion matrix (Predicted vs Actual)
table(Predicted = predictions$class, Actual = GMdata2$Site)

# Classification accuracy
accuracy <- mean(predictions$class == GMdata2$Site)
cat("Classification Accuracy: ", accuracy, "\n")

mean(predictions$class == GMdata2$Site) # Classification accuracy


# Check how many linear discriminants were generated
num_discriminants <- ncol(predictions$x)
num_discriminants

predictions$x

cor(GMdata2[, c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10", "PC11")])


# Add predictions to data
GMdata2$LD1 <- predictions$x[, 1]  # First linear discriminant
GMdata2$LD2 <- predictions$x[, 2]  # Second linear discriminant (if available)


ggplot(GMdata2, aes(x = LD1, y = LD2, color = Site)) +
  geom_point() +
  theme_minimal() +
  labs(title = "LDA Plot of Site by Linear Discriminants", x = "LD1", y = "LD2") +
  theme(legend.position = "bottom")




hulls <- GMdata2 %>%
  group_by(Site) %>%
  slice(chull(LD1, LD2))

ggplot(GMdata2, aes(x = LD1, y = LD2, color = Site)) +
  geom_point() +
  geom_polygon(data = hulls, aes(x = LD1, y = LD2, fill = Site), alpha = 0.2, color = NA) +
  theme_light() +
  labs(title = "LDA Plot of Site by Linear Discriminants", x = "LD1", y = "LD2") +
  theme(legend.position = "bottom")


ggsave("LDA.png", dpi = 800)

######################## WITH SHAPES

# Calculate convex hulls
hulls <- GMdata2 %>%
  group_by(Site) %>%
  slice(chull(LD1, LD2))

# Plot with convex hulls and shapes for SecondaryGroup
ggplot(GMdata2, aes(x = LD1, y = LD2, color = Site, shape = Shape)) +
  geom_point(size = 2) +
  geom_polygon(data = hulls, aes(x = LD1, y = LD2, fill = Site), alpha = 0.2, color = NA) +
  theme_minimal() +
  labs(title = "LDA Plot of Site by Linear Discriminants", x = "LD1", y = "LD2") +
  theme(legend.position = "bottom")

ggsave("LDA.png", dpi = 800)

######################
######################

###### HOWEVER!!!!! If LDA model only produced one linear discriminant (e.g. tiny sites),
# it might be due to this:
# You have only two groups in Site. In LDA, the maximum number of linear discriminants is one less than the number of groups.
# The model determined that additional discriminants did not add explanatory power in separating the groups, which can also happen occasionally if there’s limited variation in certain predictors.

length(unique(GMdata2$Site))  # If this returns 2, there's only one discriminant.

GMdata2$LD1 <- predictions$x[, 1]  # This modifies the code to only use LD1.

# Plot using only LD1
ggplot(GMdata2, aes(x = LD1, color = Site)) +
  geom_density() +
  theme_minimal() +
  labs(title = "LDA Plot of Site by Linear Discriminant (LD1)")
















### STEP 7: Cluster Analysis with PCs
# could also use clustering methods (e.g. k-means) to see if the clusters align with the predefined groups.

# Perform k-means clustering with 2 clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(GMdata[, c("PC1", "PC2")], centers = 2)

# Compare clusters with actual groups
table(Cluster = kmeans_result$cluster, Group = GMdata$Site)

# These tests provide a range of options to compare the groups, depending on whether
# interested in testing mean differences, clustering patterns, or classification accuracy.



#### HCA

#install.packages("ggdendro")
#library(ggdendro) #not used


# Load your data
HCData <- read.csv("Aston-Beckford_Unit.csv", header = TRUE)

object_names <- HCData$Object
object_groups <- HCData$Site

pc_data <- HCData %>% 
  select_if(is.numeric)  # Select only numeric columns for clustering
head(pc_data)

dist_matrix <- dist(pc_data, method = "euclidean")
hc <- hclust(dist_matrix, method = "ward.D2")
hc_dend <- as.dendrogram(hc)




# Customize and plot the dendrogram
dend <- dend %>% color_branches(k = 6)
plot(dend, main = "Hierarchical Clustering with Custom Labels")






# Assign labels to the dendrogram
labels(dend) <- object_names

# Define colors for each group
group_colors <- c("red", "blue", "green", "purple", "orange", "cyan") # Customize as needed
group_color_map <- setNames(group_colors, unique(object_groups)) # Map colors to group names

# Color the labels based on groups
dend <- dend %>%
  set("labels_colors", value = group_color_map[object_groups]) %>%
  set("labels_cex", 0.8) # Adjust label size

# Plot the dendrogram with colored labels
plot(dend, main = "Hierarchical Clustering with Group-Colored Labels")




#####HCA based on

dend <- as.dendrogram(hc)
labels(dend) <- object_names
dend <- dend %>% 
  color_branches(k = 6) %>%    # Color branches by clusters (e.g., 6 clusters)
  color_labels(k = 6)          # Color labels by clusters

# Plot the customized dendrogram
plot(dend, labels = object_names, main = "Customized Hierarchical Clustering")







##########




# Assign custom labels (Object names) to the dendrogram
labels(hc_dend) <- object_names

# Define colors for each group (Site)
unique_groups <- unique(object_groups)
group_colors  <- setNames(c('#FEA98A', '#8FC0A9', '#227C9D', '#E38AAE', '#E38AFE', '#E38D0E', "red")) # Add more if needed
group_color_map <- setNames(group_colors[1:length(unique_groups)], unique_groups)  # Map colors to groups


# Add group colors to the dendrogram
dend <- hc_dend %>%
  set("labels_colors", value = group_colors[group_labels]) %>%
  set("labels_cex", 0.8) # Adjust label size

# Convert the dendrogram for ggplot2
dend_data <- as.ggdend(dend)

# Create the plot
ggplot(dend_data$segments) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = dend_data$labels, 
            aes(x = x, y = y, label = label, color = label), 
            size = 3, hjust = 0, vjust = -0.5) +
  scale_color_manual(values = group_colors) +
  theme_light() +
  labs(title = "Hierarchical Clustering with Group-Colored Labels") +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = "Groups"))