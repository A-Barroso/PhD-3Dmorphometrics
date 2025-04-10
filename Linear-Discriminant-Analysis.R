

# Install and turn on the required packages
packages <- c("ggplot2", 
              "dplyr",
              "MASS",
              "gridExtra",
              "grid")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

lapply(packages, library, character.only = TRUE)





######## STEP 5: Linear Discriminant Analysis (LDA)

### LDA is a supervised machine learning algorithm that can compare differences across multiple assemblages
## (whereas PCA only compares similarities) and  works well if your goal is to separate existing groups.
## It uses the data's known group labels (e.g. Site) to create linear discriminants - the maximum number
## of which is always one less than the number of groups in the data. 

### For example, if there are 5 assemblages in the analysis, the maximum number of linear discriminants
## available for analysis will be 4. If you only have two groups, then only one linear discriminant will
## be produced - this will mean that further analysis will not be possible, but you can produce a line
## graph that shows how your data is distributed across that linear discriminant, which can still be a
## useful insight. A similar problem may also occur if your data groups (i.e. assemblages) are too
## small: the model might determine that additional discriminants did not add explanatory power in
## separating the groups, which can happen occasionally if there’s limited variation in certain predictors.

### In LDA, the classification boundaries are influenced by on the 'prior probabilities' of each group, 
## which represent the proportion of each class in the dataset. By default, LDA assumes equal priors for 
## all classes. However, if the data is imbalanced (e.g. one class is much more frequent than others), it
## can affect how easily your groups are separated. 
### To mitigate this, you can adjust the priors to change the influence each group has on the LDA model by
## calculating the proportions of each class in your data. All of the priors should sum to 1. You can also
## adjust them based on your knowledge of the data or the class distribution.

# Note: this won't directly change the appearance of the decision boundaries (convex hulls) but it will
# affect the model's internal calculations for classification.

# If your groups are biologically meaningful (e.g., species, developmental stages, or functional categories),
# adjusting priors can artificially skew interpretations by making some groups appear more or less distinct
# than they actually are.
# If you have unequal sample sizes, priors can help counteract bias—setting priors to be proportional to
# real-world frequencies can provide a more realistic classification.
# If you are testing classification accuracy, using equal priors might give a better sense of how well
# LDA distinguishes shape groups without bias toward more frequent groups.

## Higher Prior: Increasing the prior probability for a class gives it more weight in determining
# the decision boundary, making that class more likely to be assigned to a new observation.
# In other words: the LDA will favor assigning new observations to that group more frequently. This can 
# lead to an over-representation of that group in the classification results, even if the actual shape 
# variation doesn't strongly support it. This means the LDA model will pull the decision boundary closer 
# to other groups, making the favored group "claim" more of the morphospace.

## Lower Prior: Decreasing the prior probability for a class reduces its influence on the model
# and increases the likelihood of assigning observations to other classes.
# In other words: the model will be less likely to assign new observations to that group. The
# decision boundary will shift away from that group, making it more likely for observations to
# be classified into other groups instead. This could lead to underrepresentation of that group
# in your results, even if its shape variation overlaps with others.

# In the example given below, which is specific to my dataset, I chose to calculate the priors for the
# groups in my LDA in order to specify values more accurate than the default LDA priors, since my dataset 
# is very imbalanced. I also modified them slightly to decrease the total difference between the highest 
# and lowest prior values to mitigate for observation reassignment in the internal model.





### Part A: Load, clean, and prep the data


LDA_data <- read.csv("GPF-RawPCdata.csv")
LDA_data$Assemblage <- as.factor(LDA_data$Assemblage)

LDA_data <- LDA_data[, !names(LDA_data) %in% 
                       c("Object", "Typology", "Shaping_Intent", "Prominent_Edge", "Recycled")]  # Remove unnecessary columns for LDA model

# Optional: further data cleaning
LDA_data <- LDA_data[-c(1, 2, 3), ]   # Delete any rows IF necessary (e.g. tiny assemblages)
LDA_data$Assemblage <- droplevels(LDA_data$Assemblage)   # Drop levels of deleted rows
str(LDA_data)  # check if successfully deleted


# Optional: Calculate class proportions to use as priors
class_proportions <- table(LDA_data$Assemblage) / nrow(LDA_data)
print(class_proportions)

levels(LDA_data$Assemblage)  # check which order your sites are in - priors are automatically linked to this order

priors <- c(0.14, 0.05, 0.3, 0.51)  # Specify prior probabilities (modify this if you have a specific preference)


### Part B: Generating the internal model

## In this section, I chose to manually input the first PCs which contribute to 95% cumulative shape variability.
## If you choose to do the same, be aware that this method may include PCs that contribute less to discrimination
## or that have high collinearity, affecting how LDA constructs the discriminant functions. For example, PC1 and
## PC2 explain the most shape variance, but they may not be the best at distinguishing groups in your particular
## dataset. Sometimes, later PCs (e.g., PC8, PC12, PC18) may contribute more to group separation.

## To mitigate this, you can use use lda(tol = 1e-1) to set a tolerance threshold for collinear PCs. This means
## that the LDA will choose the most useful PCs for discrimination rather than just the first ones listed.
## However, this will not work properly for datasets whose values all (or mostly all) have high collinearity.

# Fit the LDA model with adjusted prior probabilities
lda_model <- lda(Assemblage ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
                   PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 +
                   PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29 + PC30,
                 data = LDA_data,
                 prior = priors, 
                 tol = 1e-1   # set a threshold for ignoring variables that contribute very little variance
)  
print(lda_model)

pc_contributions <- apply(abs(lda_model$scaling), 1, sum)  # Compute absolute contributions
sorted_pcs <- sort(pc_contributions, decreasing = TRUE)    # Sort in decreasing order
print(sorted_pcs)                             # check contributions of individual PCs



# Predict using the LDA model
LDA_predictions <- predict(lda_model)

LDA_predictions$class == LDA_data$Assemblage    # Classification accuracy - % of predicted group labels which matched the actual labels.
mean(LDA_predictions$class == LDA_data$Assemblage)  # Classification accuracy based on values of 'TRUE' (=1) and 'FALSE' (=0).


# Confusion matrix: Predicted vs Actual
table(Predicted = LDA_predictions$class, Actual = LDA_data$Assemblage)

conf_matrix <- table(Predicted = LDA_predictions$class, Actual = LDA_data$Assemblage)   # Generate the confusion matrix to save as an image file
table_grob <- tableGrob(conf_matrix)   # Create a table grob (graphical object)
png("confusion_matrix.png", width = 13, height = 6, units = "in", res = 800)  # Open a PNG device; adjust width/height for your table
grid.draw(table_grob)  # Draw the table
dev.off()   # Close the PNG device to save the file



# Number of linear discriminants generated
num_discriminants <- ncol(LDA_predictions$x)
print(num_discriminants)
# Predictions for the linear discriminants
print(LDA_predictions$x)


# Add predictions to data Add the first two linear discriminants to the data for plotting
LDA_data$LD1 <- LDA_predictions$x[, 1]   # First linear discriminant
LDA_data$LD2 <- LDA_predictions$x[, 2]   # Second linear discriminant (if available)

length(unique(LDA_data$Assemblage))   # Number of unique groups - if this returns 2, there's only one discriminant.


# Visualize the LDA results as a scatterplot with convex hulls.
hulls <- LDA_data %>%
  group_by(Assemblage) %>%
  slice(chull(LD1, LD2))

ggplot(LDA_data, aes(x = LD1, y = LD2, color = Assemblage)) +
  geom_polygon(data = hulls, 
               aes(x = LD1, y = LD2, 
                   fill = Assemblage),
               alpha = 0.2, color = NA) +
  geom_point() +
  theme_light() +
  labs(title = "LDA Plot of Site by Linear Discriminants",
       x = "LD1", y = "LD2") +
  theme(legend.position = "bottom",
        legend.margin = margin(t = 10),
        plot.margin = margin(20, 20, 20, 15),
        plot.title = element_text(size = 18, margin = margin(b = 20)),
        axis.title.x = element_text(margin = margin(t = 10))) +
  guides(colour = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2)) +
  coord_cartesian(expand = TRUE)  


ggsave("LDA.png", dpi = 800, width = 9, height = 7, units = "in")







# ... Or plot using only LD1 if no second discriminant is available
ggplot(LDA_data, aes(x = LD1, color = Assemblage)) +
  geom_density() +
  theme_light() +
  labs(title = "LDA Plot of Site by Linear Discriminant (LD1)")







