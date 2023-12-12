# clear out the past
if(!is.null(dev.list())) dev.off()  
rm(list = ls())
cat("\014")
install.packages('caret')
install.packages("tibble")
library(caret)
library(dplyr) #data manipulation that provides easy-to-use functions for filtering, grouping, and summarizing data, among other tasks.
library(tidyr) #data tidying that provides functions for reshaping data from "wide" to "long" format, and vice versa.
library(moments)
library(ggplot2)
library(ILS)
library(gridExtra)
setwd("/Users/siddhantpatil/Desktop/MA335 /Final project-20230603")
Proj_data <-  read.csv("project data.csv") # you use V2 or V3
head(Proj_data)


#**Data Cleaning**#

###Convert M/F into numeric values###
Proj_data$M.F <- ifelse(Proj_data$M.F == "M", 0, 1)
head(Proj_data)


###Remove rows with Group = "Converted"
dim(Proj_data)
# Count occurrences of "Converted" in the Group column
converter_count <- sum(Proj_data$Group == "Converted")
converter_count
# Remove rows with Group = "Converted"
Proj_data1 <- Proj_data[Proj_data$Group != "Converted", ]
dim(Proj_data1)

####
dim(Proj_data1)
# Count missing values in each column
missing_counts <- colSums(is.na(Proj_data1))
missing_counts
# Remove rows with missing values
Proj_data1 <- na.omit(Proj_data1)
dim(Proj_data1)
head(Proj_data1)
Proj_data2<-Proj_data1
Proj_data2$Group <- ifelse(Proj_data2$Group == "Nondemented", 1, 0)
final_data<-Proj_data2
final_data1<-final_data
fullscale_data<-scale(final_data)
final_data1[,5:10]<- scale(final_data1[,5:10])
halfscale_data<-final_data1

head(data)
head(Proj_data2)
head(final_data)
head(final_data1)
head(fullscale_data)
head(halfscale_data)


#**Preliminary Data Analysis**#
# Subset the dataset for Demented and Nondemented groups
demented_gender <- Proj_data1$M.F[Proj_data1$Group == "Demented"]
nondemented_gender <- Proj_data1$M.F[Proj_data1$Group == "Nondemented"]

# Independent t-test
t_test_result <- t.test(demented_gender, nondemented_gender)

# Print the result
t_test_result


#boxplot
boxplot(EDUC ~ Group, data = Proj_data2, main = "Box Plot of Education (year) by Group", xlab = "Group", ylab = "Education (Year)", col = c("#1f77b4", "#ff7f0e", "#2ca02c"))


#Histogram of Age and Education
par(mfrow = c(1, 2))  # Set up a 1x2 layout for the histograms
# Histogram for Age
a <- hist(Proj_data2$Age, main = "Age Histogram", xlab = "Age (year)", col = "blue", ylim = c(0, max(a$counts) + 5))
# Histogram for Education
b <- hist(Proj_data2$EDUC, main = "Education Histogram", xlab = "Education (year)", col = "green", ylim = c(0, max(b$counts) + 5))


# Create correlation plot with color and numerical values
# Calculate correlation matrix
cor_matrix <- cor(Proj_data2[, c("Age", "EDUC", "MMSE", "CDR", "eTIV", "nWBV", "ASF")])
corrplot(cor_matrix, method = "shade", type = "full", tl.cex = 0.7, addCoef.col = "black")


#**Demonstration of Clustering Algorithms**#

data1 <- scale(final_data)          
set.seed(123)

fviz_nbclust(data1, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)

kmeans2 <- kmeans(data1, centers = 2, nstart = 100)
kmeans3 <- kmeans(data1, centers = 3, nstart = 100)
kmeans4 <- kmeans(data1, centers = 4, nstart = 100)
f1 <- fviz_cluster(kmeans2, geom = "point", data = data1) + ggtitle("k = 2")
f2 <- fviz_cluster(kmeans3, geom = "point", data = data1) + ggtitle("k = 3")
f3 <- fviz_cluster(kmeans4, geom = "point", data = data1) + ggtitle("k = 4")
grid.arrange(f1, f2, f3, nrow = 2)

#Hierarchical clustering
idx <- sample(1:dim(Proj_data2)[1], 50);idx
df1<-Proj_data2[idx,]
dfX<-df1[,-(c(1,2))] 
dfX<- na.omit(dfX)
dfX<-scale(dfX)
#Start my calculating the distance matrix
d <- dist(dfX, method = "euclidean")
#Apply hierarchical clustering for differnt linkage methods
fit.single <- hclust(d, method="single")
fit.complete <- hclust(d, method="complete")
fit.average <- hclust(d, method="average")
fit.centroid <-hclust(d, method="centroid")
#par(mfrow=c(2,2))
plot(fit.single) # print the dendrogram
groups.fit.single <- cutree(fit.single, k=4) # cut tree into k=4 clusters # draw dendrogram with red borders around the 4 clusters rect.hclust(fit.single, k=4, border="red")

#Checking how many observations are in each cluster
table(groups.fit.single)
plot(fit.complete)
groups.fit.complete <- cutree(fit.complete, k=4)
rect.hclust(fit.complete, k=4, border="red")
table(groups.fit.complete)
plot(fit.average)
groups.fit.average <- cutree(fit.average, k=4)
rect.hclust(fit.average, k=4, border="red")
table(groups.fit.average)
plot(fit.centroid)
groups.fit.centroid <- cutree(fit.centroid, k=4)
rect.hclust(fit.centroid, k=4, border="red")
table(groups.fit.centroid)


#**Perform logistic regression**#


logistic_model0 <- glm(Group ~M.F+eTIV+nWBV+ASF+Age+EDUC+SES+MMSE+CDR, data = halfscale_data, family = binomial)

summary(logistic_model0)


#**Feature selection**#

X <- halfscale_data[, c("M.F", "Age", "EDUC", "SES", "MMSE", "CDR", "eTIV", "nWBV", "ASF")]  # Replace with the column names you want as X variables

# Create the Y variable vector
Y <- halfscale_data[["Group"]]  # Replace with the column name of your target variable
set.seed(10)
control <- rfeControl(functions = lmFuncs, method = "repeatedcv",#cv
                      repeats = 10,
                      number = 10)
lmProfile <- rfe(X, Y,
                 sizes = c(1:6),
                 rfeControl = control)
lmProfile
plot(lmProfile, type = c("g", "o"))

plot(lmProfile, metric = "Rsquared", type = c("g", "o"))

predictors(lmProfile)
lmProfile$fit

#*Again logistic regression*#

# Perform logistic regression again using the top features 
logistic_model2 <- glm(Group ~ M.F+nWBV+ASF+CDR+eTIV, data = halfscale_data, family = binomial)
summary(logistic_model2)

logistic_model3 <- glm(Group ~ M.F+nWBV+ASF, data = halfscale_data, family = binomial)
summary(logistic_model3)


#checking accuracy of this model 

data00 <- as.data.frame(halfscale_data)

train_index1 <- sample(1:nrow(data00), nrow(data00) * 0.8)
train_data1 <- data00[train_index1, ]
test_data1 <- data00[-train_index1, ]

logistic_model3 <- glm(Group ~ M.F+nWBV+ASF, data = train_data1, family = binomial)
# Make predictions on the test data
test_predictions1 <- predict(logistic_model3, newdata = test_data1, type = "response")


# Evaluate the model performance on the test data using pROC
library(pROC)
roc_obj2 <- roc(test_data1$Group, test_predictions1)
auc2 <- auc(roc_obj2)
# Print the AUC value
cat("AUC:", auc2, "\n")



# Assuming you have loaded your dataset into a variable called 'df'

# Perform ANOVA
model <- aov(CO2_EMISSIONS ~ FUEL_TYPE, data = df)
anova_results <- summary(model)




# Assuming you have loaded your dataset into a variable called 'df'

# Perform ANOVA
model <- aov(CO2_EMISSIONS ~ FUEL_TYPE, data = df)
anova_results <- anova(model)

# Check for significant effects
if (anova_results$"Pr(>F)"[1] < 0.05) {
  # Significant effect of fuel type on CO2 emissions
  
  # Post-hoc test (Tukey's HSD)
  posthoc <- TukeyHSD(model)
  
  # Compare means for CO2 emissions
  co2_emissions_posthoc <- posthoc$FUEL_TYPE
  
  # Compare means for city fuel consumption
  city_fuel_posthoc <- TukeyHSD(aov(FUEL_CONSUMPTION_CITY ~ FUEL_TYPE, data = df))
  
  # Compare means for highway fuel consumption
  highway_fuel_posthoc <- TukeyHSD(aov(FUEL_CONSUMPTION_HWY ~ FUEL_TYPE, data = df))
  
  # Print post-hoc test results
  print(co2_emissions_posthoc)
  print(city_fuel_posthoc)
  print(highway_fuel_posthoc)
} else {
  # No significant effect of fuel type on CO2 emissions
  print("No significant effect of fuel type on CO2 emissions.")
}





# Assuming you have loaded your dataset into a variable called 'df'

# Create a contingency table for the variables you want to analyze
contingency_table <- table(df$FUEL_TYPE, df$VEHICLE_CLASS)

# Perform the chi-square test
chi_square_result <- chisq.test(contingency_table)

# Print the test result
print(chi_square_result)




















