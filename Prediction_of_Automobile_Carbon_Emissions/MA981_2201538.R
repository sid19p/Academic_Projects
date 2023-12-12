library(readxl)
df <- read_excel("~/Desktop/karoyamaro/canada_emissions_trial/imputed_data_0.xlsx")
View(df)
head(df)
# Rename the column names
colnames(df) <- c("MODEL_YEAR", "COMPANY_NAME", "MODEL_NAME","VEHICLE_CLASS", "ENGINE_SIZE","CYLINDERS","TRANSMISSION","FUEL_TYPE","FUEL_CONSUMPTION_CITY","FUEL_CONSUMPTION_HWY","COMB_km","COMB_mpg","CO2_EMISSIONS","CO2_Rating","Smog_Rating")

# Check the data types of all columns
data_types <- sapply(df, class)
print(data_types)

# Convert specific columns to numeric data type
df$ENGINE_SIZE <- as.numeric(df$ENGINE_SIZE)
df$CYLINDERS <- as.numeric(df$CYLINDERS)
df$FUEL_CONSUMPTION_CITY <- as.numeric(df$FUEL_CONSUMPTION_CITY)
df$FUEL_CONSUMPTION_HWY <- as.numeric(df$FUEL_CONSUMPTION_HWY)
df$COMB_km <- as.numeric(df$COMB_km)
df$COMB_mpg <- as.numeric(df$COMB_mpg)
df$CO2_EMISSIONS <- as.numeric(df$CO2_EMISSIONS)
df$CO2_Rating <- as.numeric(df$CO2_Rating)
df$Smog_Rating <- as.numeric(df$Smog_Rating)

# Verify the updated data types
data_types <- sapply(df, class)
print(data_types)



library(lubridate)



duplicates <- duplicated(df)
df <- subset(df, !duplicates)

missing_values <- is.na(df)
df <- df[complete.cases(df), ]

library(dplyr)


# Count the levels in a column
column_counts <- table(df$VEHICLE_CLASS)
column_counts

df$VEHICLE_CLASS <- gsub("Compact","COMPACT", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Full-size", "FULL-SIZE", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Mid-size","MID-SIZE", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("MiniCompact", "MINICOMPACT", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Minivan", "MINIVAN", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Special purpose vehicle", "SPECIAL PURPOSE VEHICLE", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Station wagon: Small", "STATION WAGON - SMALL", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Station wagon: Mid-size", "STATION WAGON - Mid-size", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("STATION WAGON - Mid-size", "STATION WAGON - MID-SIZE", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("SUV: Small", "SUV - SMALL", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Two-seater", "TWO-SEATER", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Van: Passenger", "VAN - PASSENGER", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("Pickup truck: Standard","PICKUP TRUCK - STANDARD", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub( "Pickup truck: Small","PICKUP TRUCK - SMALL", df$VEHICLE_CLASS, ignore.case = TRUE)
df$VEHICLE_CLASS <- gsub("SubCompact", "SUBCOMPACT", df$VEHICLE_CLASS, ignore.case = TRUE)
table(df$VEHICLE_CLASS )

head(df)


df$COMPANY_NAME <- recode(df$COMPANY_NAME,
                          "ACURA" = "Acura",
                          "ALFA ROMEO" = "Alfa Romeo",
                          "ASTON MARTIN" = "Aston Martin",
                          "AUDI" = "Audi",
                          "BENTLEY" = "Bentley",
                          "BUGATTI" = "Bugatti",
                          "BUICK" = "Buick",
                          "CADILLAC" = "Cadillac",
                          "CHEVROLET" = "Chevrolet",
                          "CHRYSLER" = "Chrysler",
                          "DODGE" = "Dodge",
                          "FERRARI" = "Ferrari",
                          "FIAT" = "Fiat",
                          "FORD" = "Ford",
                          "GENESIS" = "Genesis",
                          "GEO" = "Geo",
                          "GMC" = "GMC",
                          "HONDA" = "Honda",
                          "HUMMER" = "Hummer",
                          "HYUNDAI" = "Hyundai",
                          "INFINITI" = "Infiniti",
                          "ISUZU" = "Isuzu",
                          "JAGUAR" = "Jaguar",
                          "JEEP" = "Jeep",
                          "KIA" = "Kia",
                          "LAMBORGHINI" = "Lamborghini",
                          "LAND ROVER" = "Land Rover",
                          "LEXUS" = "Lexus",
                          "LINCOLN" = "Lincoln",
                          "MASERATI" = "Maserati",
                          "MAZDA" = "Mazda",
                          "MERCEDES-BENZ" = "Mercedes-Benz",
                          "MERCURY" = "Mercury",
                          "MINI" = "Mini",
                          "MITSUBISHI" = "Mitsubishi",
                          "NISSAN" = "Nissan",
                          "OLDSMOBILE" = "Oldsmobile",
                          "PLYMOUTH" = "Plymouth",
                          "PONTIAC" = "Pontiac",
                          "PORSCHE" = "Porsche",
                          "RAM" = "Ram",
                          "ROLLS-ROYCE" = "Rolls-Royce",
                          "SAAB" = "Saab",
                          "SATURN" = "Saturn",
                          "SCION" = "Scion",
                          "SMART" = "Smart",
                          "SRT" = "Srt",
                          "SUBARU" = "Subaru",
                          "SUZUKI" = "Suzuki",
                          "TOYOTA" = "Toyota",
                          "VOLKSWAGEN" = "Volkswagen",
                          "VOLVO" = "Volvo"
)

# Check the updated company names
table(df$COMPANY_NAME)



head(df)
colnames(df)
head(df)
class(df)
#########################################################################################################


# Required packages
library(ggplot2)
library(dplyr)
library(tidyr)

# Get the numerical columns in the dataset
numeric_columns <- df %>% select_if(is.numeric)

summary(numeric_columns)

# Reshape the dataset to long format
df_long <- numeric_columns %>%
  tidyr::gather(key = "Column", value = "Value")

# Calculate median and mean values for each variable
summary_values <- df_long %>%
  group_by(Column) %>%
  summarize(Median = median(Value), Mean = mean(Value))

# Plotting the distribution curves with median and mean lines
ggplot(df_long, aes(x = Value, fill = Column)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Column, scales = "free") +
  labs(x = "Value", y = "Density") +
  ggtitle("Distribution Curves of Variables") +
  geom_vline(data = summary_values, aes(xintercept = Median),
             color = "red", linetype = "dotted", size = 1) +
  geom_vline(data = summary_values, aes(xintercept = Mean),
             color = "yellow", linetype = "dashed", size = 1) +
  theme_bw() +
  theme(legend.position = "none")  # Remove the legend from the graph




# Calculate correlation matrix
cor_matrix <- cor(numeric_columns)


# Plot correlation matrix with half size, numbers, and circles
corrplot::corrplot(cor_matrix, method = "circle", type = "upper",
                   tl.cex = 0.8, tl.col = "black",
                   addCoef.col = "black", number.cex = 0.7,
                   pch = 21, bg = "white")


# Define a new color palette
new_color_palette <- colorRampPalette(c("yellow", "white", "red3"))(80)

# Plot correlation matrix with modified color scheme
corrplot::corrplot(cor_matrix, method = "square", type = "upper",
                   tl.cex = 0.8, tl.col = "black",
                   addCoef.col = "black", number.cex = 0.7,
                   pch = 21, bg = "white",
                   col = new_color_palette)  # Specify the new color palette

####################################################################################

names(df)
df_s<-df[,9:13]

plot(df_s)


names(df)

# Group the data by model year and calculate the sum of CO2 emissions
# Convert CO2_EMISSIONS column to numeric data type
df$CO2_EMISSIONS <- as.numeric(df$CO2_EMISSIONS)

# Group the data by model year and calculate the sum of CO2 emissions
co2_emissions <- df %>%
  group_by(MODEL_YEAR) %>%
  summarise(total_co2 = sum(CO2_EMISSIONS))

# Print the resulting data
print(co2_emissions)
View(co2_emissions)


# Convert the 'MODEL YEAR' column to numeric
co2_emissions$`MODEL_YEAR` <- as.numeric(as.character(co2_emissions$`MODEL_YEAR`))


library(ggplot2)

# Create a line graph
ggplot(co2_emissions, aes(x = MODEL_YEAR, y = total_co2)) +
  geom_line(color = "pink") +
  geom_point(size = 1, color = "red") +
  geom_text(aes(label = `MODEL_YEAR`), nudge_x =0.5, color = "grey2") +
  xlab("Model Year") +
  ylab("Total CO2 Emissions") +
  ggtitle("CO2 Emissions Over the Years") +
  theme_bw()


# Calculate average fuel consumption per year for city and highway
avg_fuel_data <- aggregate(cbind(FUEL_CONSUMPTION_CITY, FUEL_CONSUMPTION_HWY) ~ MODEL_YEAR, df, mean)

# Create the plot
ggplot(avg_fuel_data, aes(x = MODEL_YEAR)) +
  geom_line(aes(y = FUEL_CONSUMPTION_CITY, color = "City")) +
  geom_line(aes(y = FUEL_CONSUMPTION_HWY, color = "Highway")) +
  labs(title = "Average Fuel Consumption Over the Years",
       x = "Model Year",
       y = "Average Fuel Consumption") +
  scale_color_manual(values = c("City" = "green3", "Highway" = "red")) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(avg_fuel_data$MODEL_YEAR), max(avg_fuel_data$MODEL_YEAR), by = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Required Libraries
library(ggplot2)


table(df$FUEL_TYPE)
w<- df %>%select(FUEL_TYPE,CO2_EMISSIONS) %>% group_by(FUEL_TYPE)
summary(w)

# Required Libraries
library(ggplot2)

# Create the plot
ggplot(df, aes(x = FUEL_TYPE, y = CO2_EMISSIONS, fill = FUEL_TYPE)) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribution of CO2 Emissions by Fuel Type",
       x = "Fuel Type",
       y = "CO2 Emissions (g/km)") +
  scale_fill_manual(values = c("D" = "red", "E" = "green", "N" = "blue", "X" = "orange", "Z" = "purple")) +
  theme_minimal()

# Load required libraries
library(ggplot2)

# Calculate mean CO2 emissions by vehicle class
mean_co2 <- aggregate(CO2_EMISSIONS ~ VEHICLE_CLASS, df, mean)
mean_co2 <- mean_co2[order(mean_co2$CO2_EMISSIONS, decreasing = TRUE), ]


# Create the box plot with angled x-axis labels and a different theme
ggplot(df, aes(x = factor(VEHICLE_CLASS, levels = mean_co2$VEHICLE_CLASS), y = CO2_EMISSIONS, fill = VEHICLE_CLASS)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions by Vehicle Class",
       x = "Vehicle Class",
       y = "CO2 Emissions") +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.position = "none")

# Arrange levels of TRANSMISSION variable in descending order
df$TRANSMISSION <- factor(df$TRANSMISSION, levels = rev(unique(df$TRANSMISSION)))
table(df$TRANSMISSION)

# Calculate the average CO2 emission per transmission type
avg_co2_by_transmission <- df %>%
  group_by(TRANSMISSION) %>%
  summarize(avg_CO2_Emission = mean(CO2_EMISSIONS))

View(avg_co2_by_transmission)
# Create the boxplot with descending order of TRANSMISSION
ggplot(df, aes(x = reorder(TRANSMISSION, -CO2_EMISSIONS), y = CO2_EMISSIONS, fill = TRANSMISSION)) +
  geom_boxplot() +
  labs(x = "Transmission", y = "CO2 Emissions (g/km)") +
  ggtitle("CO2 Emissions Distribution by Transmission") +
  theme_gray()+theme(legend.position = "none")

# Recode the 'TRANSMISSION' column into three categories based on specific mappings
df1 <- df1 %>%
  mutate(TRANSMISSION_TYPE = recode(TRANSMISSION,
                                    "A4"   = "Automatic",
                                    "M5"   = "Manual",
                                    "AS9"  = "Semi-Automatic",
                                    # Add other mappings for all unique transmission types
                                    "AV"   = "Automatic",
                                    "A6"   = "Automatic",
                                    "AM6"  = "Semi-Automatic",
                                    "A7"   = "Automatic",
                                    "AM7"  = "Semi-Automatic",
                                    "AS7"  = "Automatic",
                                    "AS8"  = "Automatic",
                                    "A8"   = "Automatic",
                                    "M7"   = "Manual",
                                    "AV7"  = "Automatic",
                                    "AV8"  = "Automatic",
                                    "AV6"  = "Automatic",
                                    "AM5"  = "Semi-Automatic",
                                    "A9"   = "Automatic",
                                    "AS9"  = "Semi-Automatic",
                                    "AM8"  = "Semi-Automatic",
                                    "AM9"  = "Semi-Automatic",
                                    "AS10" = "Automatic",
                                    "A10"  = "Automatic",
                                    "AV10" = "Automatic",
                                    "AV1"  = "Automatic",
                                    "A3" = "Automatic",
                                    "A5"  = "Automatic",
                                    "AS4" = "Semi-Automatic",
                                    "AS5"  = "Semi-Automatic",
                                    "AS6"  = "Semi-Automatic",
                                    "AS4"  = "Semi-Automatic",
                                    "M4"  = "Manual",
                                    "M6"  = "Manual"
  ))

# View the updated dataset with the new column
table(df1$TRANSMISSION_TYPE)

names(df1)

# Create the boxplot
x<-ggplot(df1, aes(x = TRANSMISSION_TYPE, y = CO2_EMISSIONS)) +
  geom_boxplot(fill = "purple", color = "black") +
  labs(title = "CO2 Emissions ",
       x = "Transmission Type",
       y = "CO2 Emissions")

# Create the boxplot
y<-ggplot(df1, aes(x = TRANSMISSION_TYPE, y = FUEL_CONSUMPTION_CITY)) +
  geom_boxplot(fill = "orange", color = "black") +
  labs(title = "City",
       x = "Transmission Type",
       y = "Fuel consumption ")

# Create the boxplot
z<-ggplot(df1, aes(x = TRANSMISSION_TYPE, y = FUEL_CONSUMPTION_HWY)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Highway",
       x = "Transmission Type",
       y = "Fuel consumption")
library(gridExtra)

library(gridExtra)
library(cowplot)  # Make sure cowplot is installed and loaded

# Assuming x, y, and z are your plots
combined_plot <- grid.arrange(x, y, z, ncol = 2)

# Add title using cowplot
combined_plot_with_title <- ggdraw() +
  draw_plot(combined_plot) +
  draw_label("Comparative study \n of featured trasmission system", x = 0.75, y = 0.3, hjust = 0.5, vjust = 0, fontface = 'bold', size = 12)

# Print the combined plot with the title
print(combined_plot_with_title)




# Graph - Bar plot for Research Question 10
top_performing <- df %>% 
  group_by(VEHICLE_CLASS) %>% 
  top_n(-1, CO2_EMISSIONS)
View(top_performing[,c(2,4,13)])
ggplot(top_performing, aes(x = COMPANY_NAME, y = CO2_EMISSIONS, fill = VEHICLE_CLASS)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(x = "Company names", y = "Mean CO2 Emissions") +
  ggtitle("Top-Performing Vehicles")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+





#########################################################################################################################
##############################################################################################################################


#hypothesis for fuel consuption:

#1.a Welch Two Sample t-test

t.test(df$FUEL_CONSUMPTION_CITY, df$FUEL_CONSUMPTION_HWY, paired = FALSE)



#2.# One-way ANOVA on "FUEL_CONSUMPTION_CITY" among different fuel types
fit <- aov(CO2_EMISSIONS ~ FUEL_TYPE, data = df)
summary(fit)
# Post-hoc tests for "FUEL_CONSUMPTION_CITY"
posthoc<- TukeyHSD(fit)
posthoc
plot(posthoc)


#3.# One-way ANOVA on "Combined" among different fuel types
fit_city <- aov(COMB_km ~ FUEL_TYPE, data = df)
summary(fit_city)
# Post-hoc tests for "FUEL_CONSUMPTION_CITY"
posthoc_city <- TukeyHSD(fit_city)
posthoc_city
plot(posthoc_city)




























