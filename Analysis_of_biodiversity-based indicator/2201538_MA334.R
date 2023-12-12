
####MA334_COURSE WORK
######Registration Number- 2201538

# clear out the past
if(!is.null(dev.list())) dev.off()  
rm(list = ls())
cat("\014")
library(dplyr) #data manipulation that provides easy-to-use functions for filtering, grouping, and summarizing data, among other tasks.
library(tidyr) #data tidying that provides functions for reshaping data from "wide" to "long" format, and vice versa.
library(moments)#computing various statistical moments, such as skewness and kurtosis, from a vector of numerical data.
library(reshape2)#data reshaping that provides functions for transforming data from "wide" to "long" format, and vice versa.
par(mfrow=c(1, 1))


setwd("/Users/siddhantpatil/Desktop/MA334")
Proj_data <-  read.csv("proportional_species_richness_V3.csv") # you use V2 or V3
Proj_data$period <- as.factor(Proj_data$period) # must set categorical vars
Proj_data$dominantLandClass <- as.factor(Proj_data$dominantLandClass)
names(Proj_data)


# select 7 spacies 
all <- c(2:12)
eco_selected <- c(3,5,6,7,9,10,12)   # a particular troublesome case
eco_not_selected <- all[!(all%in%eco_selected)]
eco_names <- names(Proj_data[,2:12])
eco_selected_names <- names(Proj_data)[eco_selected]
eco_selected_names

# calculate the bio div measure over 7 taxinomic groups
mean_selected <- rowMeans(Proj_data[,eco_selected],na.rm=TRUE) # mean the 7 columns 
sum(is.na(mean_selected)) # check that there are no NAs in mean_selected
# add in the biodiversity measure which is the mean over 7 taxonomic groups
main_dataset <- Proj_data%>%mutate(eco_status_7=mean_selected)
names(main_dataset)
head(main_dataset)


##################################################################################
library(ggplot2)

# create a subset of the data containing only the columns we want to plot
data_subset <- main_dataset[, eco_selected_names]

# create a grid of density plots
ggplot(melt(data_subset), aes(value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

#####################################################################################################
names(main_dataset)
head(main_dataset)
# Select the columns of interest
columns_of_interest <- c( "Bird", "Butterflies", "Carabids", 
                          "Hoverflies","Ladybirds", "Macromoths", "Vascular_plants","Easting","Northing","eco_status_7")

# Calculate the correlation matrix
cor_matrix <- cor(main_dataset[, columns_of_interest])

# Print the correlation matrix
print(cor_matrix)

library(corrplot)

# Create correlation matrix
corr_matrix <- cor(main_dataset[,columns_of_interest])

# Visualize correlation matrix
corrplot(corr_matrix, method="color")

###################################################################################################

# you could split the data by period and compare these stats before and after 
table <- data.frame()
for(i in eco_selected){
  table <- rbind(table,
                 c(eco_names[i-1],
                   round(mean(main_dataset[,i],na.rm = TRUE),digits = 2),
                   round(sd(main_dataset[,i],na.rm = TRUE),digits = 2),
                   round(skewness(main_dataset[,i],na.rm = TRUE),digits = 2)
                 ))}
colnames(table) <- c("taxi_group","mean","sd","skewness")
table%>%arrange(sd,skewness) # something more could be done here 

Proj_data_MA334_Y70 <- main_dataset%>%filter(period=="Y70")
head(Proj_data_MA334_Y70)
Proj_data_MA334_Y00 <- main_dataset%>%filter(period=="Y00")
head(Proj_data_MA334_Y00)
eco_selected
table1 <- data.frame()
for(i in eco_selected){
  table1 <- rbind(table1,
                  c(eco_names[i-1],
                    round(mean(Proj_data_MA334_Y70[,i],na.rm = TRUE),digits = 2),
                    round(sd(Proj_data_MA334_Y70[,i],na.rm = TRUE),digits = 2),
                    round(skewness(Proj_data_MA334_Y70[,i],na.rm = TRUE),digits = 2)
                  ))}
colnames(table1) <- c("taxi_group","mean","sd","skewness")
table1%>%arrange(sd,skewness) # something more could be done here 


table2 <- data.frame()
for(i in eco_selected){
  table2 <- rbind(table2,
                  c(eco_names[i-1],
                    round(mean(Proj_data_MA334_Y00[,i],na.rm = TRUE),digits = 2),
                    round(sd(Proj_data_MA334_Y00[,i],na.rm = TRUE),digits = 2),
                    round(skewness(Proj_data_MA334_Y00[,i],na.rm = TRUE),digits = 2)
                  ))}
colnames(table2) <- c("taxi_group","mean","sd","skewness")
table2%>%arrange(sd,skewness) # something more could be done here


library(ggplot2)
library(gridExtra)

# Combine the two tables into a single data frame with a "period" variable
table1$period <- "Period Y70"
table2$period <- "Period Y00"
combined <- rbind(table1, table2)

# Create a line plot for each variable
plot_mean <- ggplot(combined, aes(x = period, y = mean, color = taxi_group, group = taxi_group)) +
  geom_line() +
  ggtitle("Mean Values") +
  xlab("") +
  ylab("Mean") +
  theme(legend.position = "top")
plot_mean
plot_sd <- ggplot(combined, aes(x = period, y = sd, color = taxi_group, group = taxi_group)) +
  geom_line() +
  ggtitle("Standard Deviation Values") +
  xlab("") +
  ylab("Standard Deviation") +
  theme(legend.position = "top")

plot_skewness <- ggplot(combined, aes(x = period, y = skewness, color = taxi_group, group = taxi_group)) +
  geom_line() +
  ggtitle("Skewness Values") +
  xlab("") +
  ylab("Skewness") +
  theme(legend.position = "top")

# Arrange the plots side-by-side
grid.arrange(plot_mean, plot_sd, plot_skewness, ncol = 3)

##############################################################################################

# extend data exploration; with correlations between continuous variables
names(main_dataset)
cont_vars <- main_dataset%>%select(c(eco_selected,13,14)) # includes easting and northing 
names(cont_vars)
cormat <- round(x = cor(cont_vars,use="pairwise.complete.obs"), digits = 2)


melt(cormat)%>%mutate(R2 = value^2)%>%arrange(value)
melt(cormat)%>%mutate(R2 = value^2)%>%arrange(Var1,value)

plot(cont_vars$Northing~cont_vars$Easting) # a map appears !!!
# now use the eastings and northings (these may be better used as predictors )
plot(main_dataset$eco_status_7~main_dataset$Easting)
cor(main_dataset$eco_status_7,main_dataset$Easting)
plot(main_dataset$eco_status_7~main_dataset$Northing)  # for BD7
cor(main_dataset$eco_status_7,main_dataset$Northing)


# Reshape the data into long format
main_dataset_long <- main_dataset %>%
  gather(key = "variable", value = "value", Northing, Easting)

# Create the violin plot
ggplot(main_dataset_long, aes(x = variable, y = value, fill = eco_status_7)) +
  geom_violin(trim = FALSE) +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Distribution of Easting and Northing by Ecological Status of 7 element",
       x = "", y = "Value") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~ variable, scales = "free_y")



# doing a linear regression with only Northing as a predictor 
lin_mod1 <- lm(main_dataset$eco_status_7~Proj_data$Northing)
summary(lin_mod1)
abline(lin_mod1,col="green")
plot(jitter(fitted(lin_mod1)),residuals(lin_mod1),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(lin_mod1$residuals)
qqline(lin_mod1$residuals,col="red")
plot(lin_mod1)
AIC(lin_mod1)



# doing a linear regression with only Easting as a predictor 
lin_mod2 <- lm(main_dataset$eco_status_7~Proj_data$Easting)
summary(lin_mod2)
abline(lin_mod2,col="blue")
plot(jitter(fitted(lin_mod2)),residuals(lin_mod2),xlab="Fitted",ylab="Residuals")
abline(h=0)
qqnorm(lin_mod2$residuals)
qqline(lin_mod2$residuals,col="red")
AIC(lin_mod2)


library(gridExtra)

# Create the first plot of eco_status_7 vs. Northing
plot1 <- ggplot(main_dataset, aes(Northing, eco_status_7)) +
  geom_point(color = "orange") +
  geom_abline(intercept = coef(lin_mod1)[1], slope = coef(lin_mod1)[2], color = "black") +
  labs(x = "Northing", y = "Eco Status 7")

# Create the second plot of eco_status_7 vs. Easting
plot2 <- ggplot(main_dataset, aes(Easting, eco_status_7)) +
  geom_point(color = "blue") +
  geom_abline(intercept = coef(lin_mod2)[1], slope = coef(lin_mod2)[2], color = "black") +
  labs(x = "Easting", y = "Eco Status 7")

# Arrange the plots in a grid with two rows and one column
grid.arrange(plot1, plot2, nrow = 2)

######################################################################################
#################################################################################
# following code splits between the two periods to find the BD7 change
# however it may be better to use period as a predictor 

# box plot comparisons for the two periods ignoring all other variables 

eco_status <- main_dataset%>%pull(eco_status_7)
eco_period <- main_dataset%>%pull(period)
plot(eco_status~eco_period)


names(main_dataset)
head(main_dataset)
Proj_data_MA334_period <- main_dataset%>%select(Location,period,eco_status_7)
Proj_data_MA334_period


Proj_data_MA334_split <- Proj_data_MA334_period %>%
  pivot_wider(names_from = period, values_from = eco_status_7)
Proj_data_MA334_split <- Proj_data_MA334_period %>%
  group_by(Location) %>%
  summarise(Y00 = eco_status_7[period == "Y00"],
            Y70 = eco_status_7[period == "Y70"]) %>%
  mutate(BD7_change = Y00 - Y70)
names(Proj_data_MA334_split)
head(Proj_data_MA334_split)

hist(Proj_data_MA334_split$BD7_change)  # the distribution of the BD7 change 

#model1 t-test

BD7_change <- Proj_data_MA334_split%>%pull(BD7_change)

hyp_model1<-t.test(BD7_change,mu=0)  # t test with H0: mu=0
hyp_model1

#model2: two sided test for differnce of mean 

hyp_model2<-Proj_data_MA334 %>% filter(eco_status_7,ecologicalStatus) %>% t.test(eco_status_7,ecologicalStatus,data=.,alternative = "less",conf.level = 0.005)
hyp_model2

#model3:anova test

hyp_model3<-aov(eco_status_7 ~ dominantLandClass, data = main_dataset)
summary(hyp_model3)


# select the relevant columns and rename them
main_dataset_select <- main_dataset %>%
  select(Location, Bird, Butterflies, Carabids, Hoverflies, Ladybirds, Macromoths, Vascular_plants, period, eco_status_7)


# calculate the percentage for each species in eco_status_7
main_dataset_percent <- main_dataset_select %>%
  mutate(across(Bird:Vascular_plants, ~ . * eco_status_7)) %>%
  group_by(period) %>%
  summarise(across(Bird:Vascular_plants, ~ sum(.)/sum(eco_status_7)))

# convert the data frame to long format for plotting
main_dataset_long <- tidyr::pivot_longer(main_dataset_percent, cols = Bird:Vascular_plants, names_to = "Species", values_to = "Percentage")

main_dataset_long
#model4

# Perform the ANOVA test
model4 <- aov(Percentage ~ Species, data = main_dataset_long)
summary(model4)


################################################################################################################################

# Simple linear regression part of the specified assignment
# regressions of eco_status_7 against ecologicalstatus based on all 11

plot(main_dataset$eco_status_7~main_dataset$ecologicalStatus)
abline(0,1,col="red")
lin_mod1 <- lm(main_dataset$eco_status_7~main_dataset$ecologicalStatus)
summary(lin_mod1)
AIC(lin_mod1)
plot(lin_mod1)
abline(lin_mod1,col="orange")
plot(jitter(fitted(lin_mod1)),residuals(lin_mod1),xlab="Fitted",ylab="Residuals")
abline(h=0,col="blue")
qqnorm(residuals(lin_mod1))
qqline(residuals(lin_mod1),col="red")

# do the same for each period report and differences 

Proj_data_MA334_Y70 <- main_dataset%>%filter(period=="Y70")
lin_mod2 <- lm(Proj_data_MA334_Y70$eco_status_7~Proj_data_MA334_Y70$ecologicalStatus)
summary(lin_mod2)
AIC(lin_mod2)
plot(lin_mod2)
lin_mod2$coefficients

# for later period 
Proj_data_MA334_Y00 <- main_dataset%>%filter(period=="Y00")
lin_mod3 <- lm(Proj_data_MA334_Y00$eco_status_7~Proj_data_MA334_Y00$ecologicalStatus)
summary(lin_mod3)
AIC(lin_mod3)
plot(lin_mod3)
lin_mod3$coefficients


##########################################################################################################
#multi-linear regression 
# linear regression of BD4 on BD7
mean_selected <- rowMeans(Proj_data[,eco_not_selected ],na.rm=TRUE) # mean the rem 4 columns 
sum(is.na(mean_selected)) # check that there are no NAs in mean_selected
# add in the biodiversity measure which is the mean over 7 taxonomic groups
Proj_data_MA334 <- main_dataset%>%mutate(eco_status_4=mean_selected)
names(Proj_data_MA334)


names(Proj_data_MA334)


mult_lin_mod_7<- lm(eco_status_4 ~Bird+Butterflies+Carabids+Hoverflies+Ladybirds+Macromoths+Vascular_plants, data = Proj_data_MA334)

summary(mult_lin_mod_7)
AIC(mult_lin_mod_7)
plot(mult_lin_mod_7)
# fit the full model
full_model <- lm(eco_status_4 ~ Butterflies+Carabids + Hoverflies + Ladybirds + Macromoths + Vascular_plants, data = Proj_data_MA334)
summary(full_model)
# fit models with different combinations of predictor variables
model1 <- lm(eco_status_4 ~ Butterflies + Hoverflies + Macromoths, data = Proj_data_MA334)
model2 <- lm(eco_status_4 ~ Butterflies + Hoverflies, data = Proj_data_MA334)
model3 <- lm(eco_status_4 ~ Butterflies + Macromoths, data = Proj_data_MA334)
model4 <- lm(eco_status_4 ~ Hoverflies + Macromoths, data = Proj_data_MA334)

# calculate AIC for each model
AIC(full_model, model1, model2, model3, model4)

########################################################################################################
##open analysis 
##A

names(main_dataset)


main_dataset$country <- ifelse(substr(main_dataset$dominantLandClass, 3, 3) == "e", "eng",
                               ifelse(substr(main_dataset$dominantLandClass, 3, 3) == "w", "Wales", "scot"))
summary(main_dataset$country )
unique(main_dataset$country )

main_data_filtered <- main_dataset %>%
  filter(country != "eng") %>%
  select(ecologicalStatus, period)

a <-ggplot(main_data_filtered, aes(x = ecologicalStatus, fill = period)) +
  geom_density(alpha = 0.5) +
  labs(x = "Ecological Status", y = "Density", fill = "Period")+ggtitle("England")


main_data_filtered2<- main_dataset %>%
  filter(country != "Wales") %>%
  select(ecologicalStatus, period)


b<-ggplot(main_data_filtered2, aes(x = ecologicalStatus, fill = period)) +
  geom_density(alpha = 0.5) +
  labs(x = "Ecological Status", y = "Density", fill = "Period")+ggtitle("Wales")



main_data_filtered3<- main_dataset %>%
  filter(country != "scot") %>%
  select(ecologicalStatus, period)


c<-ggplot(main_data_filtered2, aes(x = ecologicalStatus, fill = period)) +
  geom_density(alpha = 0.5) +
  labs(x = "Ecological Status", y = "Density", fill = "Period")+ggtitle("Scotland")

# Combine the plots using cowplot
library(cowplot)
plot_grid(a, b, c, ncol = 3)



##################################################################################
#B 



period_Y70 <- main_dataset %>%
  filter(period == "Y70") %>%
  select(Bees:Vascular_plants, ecologicalStatus) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

period_Y70$ecologicalStatus <- NULL
period_Y70_percent <- period_Y70 / rowSums(period_Y70) * 100
period_Y70_percent<-period_Y70_percent %>%
  select( Bird, Butterflies, Carabids, Hoverflies, Ladybirds, Macromoths, Vascular_plants)


period_Y00 <- main_dataset %>%
  filter(period == "Y00") %>%
  select(Bees:Vascular_plants, ecologicalStatus) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

period_Y00$ecologicalStatus <- NULL
period_Y00_percent <- period_Y00 / rowSums(period_Y00) * 100
period_Y00_percent<-period_Y00_percent %>%
  select( Bird, Butterflies, Carabids, Hoverflies, Ladybirds, Macromoths, Vascular_plants)

# Subtract percentage values for each species in Y70 from Y00
difference <- period_Y00_percent - period_Y70_percent

# Create a new table with the difference values
difference_table <- data.frame(Species = rownames(difference), difference)

# View the difference table
difference_table



library(dplyr)

main_dataset_select <- main_dataset %>%
  select(Location, Bird, Butterflies, Carabids, Hoverflies, Ladybirds, Macromoths, Vascular_plants, period, eco_status_7)



period_Y70 <- main_dataset_select %>%
  filter(period == "Y70") %>%
  select(Bird:Vascular_plants, eco_status_7) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

period_Y70$eco_status_7 <- NULL

period_Y70_percent <- period_Y70 / rowSums(period_Y70) * 100


period_Y00 <- main_dataset_select %>%
  filter(period == "Y00") %>%
  select(Bird:Vascular_plants, eco_status_7) %>%
  summarise(across(everything(), mean, na.rm = TRUE))

period_Y00$eco_status_7 <- NULL

period_Y00_percent <- period_Y00 / rowSums(period_Y00) * 100



# Subtract percentage values for each species in Y70 from Y00
difference <- period_Y00_percent - period_Y70_percent

# Create a new table with the difference values
difference_table2 <- data.frame(Species = rownames(difference), difference)

# View the difference table
difference_table2

library(ggplot2)

# Create a long format of the difference table
difference_table2_long <- tidyr::pivot_longer(difference_table2, cols = -Species, names_to = "Period", values_to = "Percentage")

# Plot the stacked bar chart
library(viridis)
x <- ggplot(difference_table2_long, aes(x = Period, y = Percentage, fill = Species)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Percentage, 2)), vjust = -0.5) + 
  labs(x = "Species", y = "Percentage difference", fill = "Species") +
  ggtitle("Percentage difference of species between two periods for 7 species")+
  theme_gray()

print(x)


# Create a long format of the difference table
difference_table_long <- tidyr::pivot_longer(difference_table, cols = -Species, names_to = "Period", values_to = "Percentage")

# Plot the stacked bar chart
y<-ggplot(difference_table_long, aes(x = Period, y = Percentage, fill = Species, label = round(Percentage, 2))) +
  geom_bar(stat = "identity") +
  labs(x = "Period", y = "Percentage difference", fill = "Species") +
  ggtitle("Percentage difference of species between two periods for 11 species") +
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  theme_gray()


grid.arrange(x, y, nrow = 2)


####################################################













