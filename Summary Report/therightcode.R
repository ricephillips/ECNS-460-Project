# Clear environment
rm(list = ls())
cat("\014")

#Bring in required libraries.
rm(list=ls())
library(readr)
library(tidyverse)
library(tidyr)

library(stringr)
library(dplyr)
library(ggplot2)

library(cowplot)
library(webshot)
library(kableExtra)
library(htmlTable)

options(knitr.kable.N = Inf, knitr.kable.longtable = TRUE)
#Data Cleaning Process:
#Data was converted from .xls to .csv.
######################################################
#Data Read Ins:

# Set your file paths
# file_paths <- c(
#  "CAN_exp.csv",
#  "CAN_imp.csv",
#  "MEX_exp.csv",
#  "MEX_imp.csv"
#)

CAN_exp = read_csv("/Users/phelpsinvestmentsllc/Desktop/R/EIND_460/ECNS-460-Project-main/Raw Data/CAN_exp.csv") 
CAN_imp = read_csv("/Users/phelpsinvestmentsllc/Desktop/R/EIND_460/ECNS-460-Project-main/Raw Data/CAN_Imp.csv") 
MEX_exp = read_csv("/Users/phelpsinvestmentsllc/Desktop/R/EIND_460/ECNS-460-Project-main/Raw Data/MEX_exp.csv") 
MEX_imp = read_csv("/Users/phelpsinvestmentsllc/Desktop/R/EIND_460/ECNS-460-Project-main/Raw Data/MEX_imp.csv") 

#############################################################
#CANADA wrangle:
#Turning my data sets long and setting the Primary Key as year being the columns, which will now be rows instead so we also create a value column.
CAN_exp_long = gather(CAN_exp, key = "Year", value = "Value", -`Reporter Name`, -`Partner Name`, -`Trade Flow`, -`Product Group`, -`Indicator`)
CAN_imp_long = gather(CAN_imp, key = "Year", value = "Value", -`Reporter Name`, -`Partner Name`, -`Trade Flow`, -`Product Group`, -`Indicator`)

#merging my long export and import data sets for Canada using bind_rows instead of column_bind since this is long. We turned our years 1991-2021 as our obs. 
tidy_CAN_data = bind_rows(CAN_exp_long, CAN_imp_long)

#########################################################################################
#MEXICO wrangle: Exact same process as above. 

MEX_exp_long = gather(MEX_exp, key = "Year", value = "Value", -`Reporter Name`, -`Partner Name`, -`Trade Flow`, -`Product Group`, -`Indicator`)
MEX_imp_long = gather(MEX_imp, key = "Year", value = "Value", -`Reporter Name`, -`Partner Name`, -`Trade Flow`, -`Product Group`, -`Indicator`)

tidy_MEX_data = bind_rows(MEX_exp_long, MEX_imp_long)

###############################################################################################
#Final merging of data sets:

trade_volume_df = bind_rows(tidy_CAN_data, tidy_MEX_data)

##################################################################################################
# Data Manipulation 

# Clean data types and values
trade_volume_df$Year <- as.numeric(as.character(trade_volume_df$Year))
trade_volume_df$`Partner Name` <- as.factor(trade_volume_df$`Partner Name`)
trade_volume_df$`Trade Flow` <- as.factor(trade_volume_df$`Trade Flow`)
trade_volume_df$`Product Group` <- as.factor(trade_volume_df$`Product Group`)

# Applying log transformation with a check for positive values
trade_volume_df$Value <- ifelse(trade_volume_df$Value > 0, log(trade_volume_df$Value), NA)

hist(trade_volume_df$Value)
colnames(trade_volume_df)

library(dplyr)

trade_volume_df <- trade_volume_df %>%
  mutate(`Product Group` = case_when(
    `Product Group` %in% c("Food products", "Animal", "Vegetables", "Hides and Skins") ~ "Agriculture",
    `Product Group` %in% c("Stone and Glass", "Wood", "Plastic or Rubber", "Raw materials") ~ "Raw materials",
    `Product Group` %in% c("Textiles and Clothing", "Footwear") ~ "Textiles, Clothing & Footwear",
    TRUE ~ `Product Group`
  ))

# Check the unique values in 'Product Group' after the update
unique_values_product_group <- unique(trade_volume_df$`Product Group`)
print(unique_values_product_group)


# Data Manipulation 

library(dplyr)

# Corrected Lists of Product Groups
nafta_target <- c("Capital goods", "Mach and Elec", "Transportation", "Intermediate Goods", "Agriculture")

not_nafta_target <- c("Miscellaneous", "Agriculture", "Minerals", "Metals",
                      "Raw materials" , "Consumer goods", "Chemicals", 
                      "Textiles and Clothing", "Fuels")



# Filter data into two groups
nafta_data <- trade_volume_df %>%
  filter(`Product Group` %in% nafta_target)

not_nafta_data <- trade_volume_df %>%
  filter(`Product Group` %in% not_nafta_target)
###############################################################################
# Individual Histograms 

library(ggplot2)

# Box plot for NAFTA data
ggplot(nafta_data, aes(x = `Product Group`, y = Value)) +
  geom_boxplot() +
  labs(title = "Box Plot of NAFTA Data", x = "Product Group", y = "Trade Volume")

# Box plot for non-NAFTA data
ggplot(not_nafta_data, aes(x = `Product Group`, y = Value)) +
  geom_boxplot() +
  labs(title = "Box Plot of Non-NAFTA Data", x = "Product Group", y = "Trade Volume")

###############################################################################
# Between Groups 
combined_data <- rbind(mutate(nafta_data, Group = "NAFTA"),
                       mutate(not_nafta_data, Group = "Not NAFTA"))
ggplot(combined_data, aes(x = Group, y = Value, fill = `Product Group`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Stacked Bar Chart of Trade Volumes", x = "Group", y = "Trade Volume")

ggplot(combined_data, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot() +
  labs(title = "Box Plot of Trade Volumes", x = "Group", y = "Trade Volume")

library(ggplot2)
combined_totals <- combined_data %>%
  group_by(Group) %>%
  summarize(Total = sum(Value))


# Filter for each subset
canada_exports <- trade_volume_df %>% 
  filter(`Partner Name` == "Canada", `Trade Flow` == "Export", `Product Group` == "All Products")

canada_imports <- trade_volume_df %>% 
  filter(`Partner Name` == "Canada", `Trade Flow` == "Import", `Product Group` == "All Products")

mexico_exports <- trade_volume_df %>% 
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Export", `Product Group` == "All Products")

mexico_imports <- trade_volume_df %>% 
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Import", `Product Group` == "All Products")






#####################################
library(ggplot2)
library(dplyr)

# Calculate the mean of 'Value' for each 'Group'
mean_values <- combined_data %>%
  group_by(Group) %>%
  summarize(MeanValue = mean(Value, na.rm = TRUE))

# Create the density plot and add mean lines
density_plot <- ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_values, aes(xintercept = MeanValue, color = Group), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("blue", "red")) +  # Adjust the color values as needed
  labs(title = "Density Plot of Trade Volumes", x = "Trade Volume", y = "Density") +
  theme_minimal()

# Print the density plot
density_plot

# Function to find the peak (mode) of a density
find_density_peak <- function(data) {
  d <- density(data$Value, na.rm = TRUE)
  data.frame(Value = d$x[which.max(d$y)])
}

# Apply the function to each group and get peaks
peaks <- combined_data %>%
  group_by(Group) %>%
  do(find_density_peak(.))

# Print peaks
print(peaks)

# Create the density plot and add peak lines
density_plot <- ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = peaks, aes(xintercept = Value, color = Group), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("blue", "red")) +  # Adjust these colors to match your groups
  labs(title = "Density Plot of Trade Volumes with Peaks", x = "Trade Volume", y = "Density") +
  theme_minimal()

# Print the density plot
density_plot


library(dplyr)

# Function to find the peak of the density for each group
find_density_peak <- function(data) {
  d <- density(data$Value, na.rm = TRUE)
  peak <- d$x[which.max(d$y)]
  return(data.frame(Value = peak))
}




# Create the density plot and add peak lines and labels
density_plot <- ggplot(combined_data, aes(x = Value, fill = Group)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = peaks, aes(xintercept = Value, color = Group), linetype = "dashed", size = 1) +
  geom_text(data = peaks, aes(x = Value, y = 0, label = sprintf("%.2f", Value), color = Group),
            vjust = -1, hjust = ifelse(peaks$Value > median(combined_data$Value), 1, 0),
            angle = 90, size = 3) +
  scale_fill_manual(values = c("blue", "red"), name = "Group") +
  scale_color_manual(values = c("blue", "red"), name = "Group") +
  labs(title = "Density Plot of Trade Volumes with Peaks", x = "Trade Volume", y = "Density") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the density plot
density_plot




###############################################
# fix date: and organize nafta to nonafta 
combined_data$Year <- as.factor(combined_data$Year)  # Convert Year to a factor for heat map

# Calculate the first and last year's value for each product group
percent_increase_data <- combined_data %>%
  group_by(`Product Group`) %>%
  summarize(Start = first(Value),
            End = last(Value)) %>%
  mutate(PercentIncrease = ((End - Start) / Start) * 100) %>%
  ungroup() %>%
  select(`Product Group`, PercentIncrease)  # Select only necessary columns

# Create the heatmap and add the percent increase
heatmap_plot <- ggplot(combined_data, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_x_discrete(breaks = year_breaks) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = median(combined_data$Value, na.rm = TRUE),
                       limit = c(min(combined_data$Value, na.rm = TRUE), max(combined_data$Value, na.rm = TRUE))) +
  geom_text(data = percent_increase_data, aes(label = sprintf("%.1f%%", PercentIncrease), y = `Product Group`, x = 1.01),
            hjust = -0.1, color = "black", inherit.aes = FALSE) +
  labs(title = "Heat Map of Trade Volumes with Percent Increase", x = "Year", y = "Product Group") +
  theme_minimal() +
  theme(plot.margin = margin(1, 8, 1, 1))  # Adjust the margin to make room for the text

# Print the heatmap
heatmap_plot



################################################################################


library(ggplot2)
library(dplyr)

# Assuming your data frame is named trade_volume_df

# Filter for a specific Reporter Name and Partner Name if needed
# Example: Filtering for trade data involving the United States and Canada
filtered_data <- trade_volume_df %>%
  filter(`Reporter Name` == "United States", `Partner Name` == "Canada")

# Time Series Line Chart
ggplot(filtered_data, aes(x = Year, y = Value, color = `Product Group`)) +
  geom_line() +
  facet_wrap(~`Product Group`, scales = "free_y") +
  labs(title = "Trade Volume Trends Over Time by Product Group",
       x = "Year",
       y = "Trade Volume (log scale)",
       color = "Product Group") +
  theme_minimal() +
  geom_vline(xintercept = 1994, linetype = "dashed", color = "red")  # Marking the year NAFTA was implemented


############################################################

library(ggplot2)
library(dplyr)

# Ensure the correct format for Year and Value
trade_volume_df$Year <- as.numeric(as.character(trade_volume_df$Year))
trade_volume_df$Value <- as.numeric(trade_volume_df$Value)

# Define the NAFTA product groups
nafta_target <- c("Capital goods", "Mach and Elec", "Transportation", "Intermediate Goods", "Agriculture")

# Filter the data for NAFTA groups
nafta_data <- trade_volume_df %>% 
  filter(`Product Group` %in% nafta_target)

# Function to create a plot
plot_trade_volume <- function(data, title) {
  ggplot(data, aes(x = Year, y = Value, color = `Product Group`)) +
    geom_line(size = 1) +
    facet_wrap(~`Product Group`, scales = "free_y") +
    labs(title = title,
         subtitle = "Trends over time, marked by NAFTA implementation in 1994",
         x = "Year",
         y = "Trade Volume (log scale)") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    geom_vline(xintercept = 1994, linetype = "dashed", color = "red")  # Marking NAFTA implementation year
}

# Create and print the NAFTA plot
nafta_plot <- plot_trade_volume(nafta_data, "NAFTA Trade Volume Trends Over Time")
nafta_plot



#############################################################
library(dplyr)
library(ggplot2)

# Combine the data for Canada and Mexico imports
combined_imports <- bind_rows(
  canada_imports %>% mutate(Value = -Value),  # Negate values for Canada for the left side of the pyramid
  mexico_imports
)

# Ensure the Year is a factor for proper ordering in the plot
combined_imports$Year <- as.factor(combined_imports$Year)

# Create the population pyramid-style chart
pyramid_chart <- ggplot(combined_imports, aes(x = Year, y = Value, fill = `Partner Name`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs, breaks = scales::pretty_breaks(n = 10)) +  # Use absolute values for y-axis labels
  labs(title = "Comparison of Imports to Canada and Mexico for 'All Products'",
       x = "Year", y = "Import Value") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())

# Print the chart
pyramid_chart




