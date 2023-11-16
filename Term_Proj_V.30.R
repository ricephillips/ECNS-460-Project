#Bring in required libraries.
rm(list=ls())
library(readr)
library(tidyverse)
library(tidyr)
library(stringr)
library(dplyr)
library(ggplot2)
library(cowplot)
options(knitr.kable.N = Inf, knitr.kable.longtable = TRUE)
#Data Cleaning Process:
#Data was converted from .xls to .csv.
######################################################
#Data Read Ins:
CAN_exp = read_csv("CAN_exp.csv") 
CAN_imp = read_csv("CAN_Imp.csv") 
MEX_exp = read_csv("MEX_exp.csv") 
MEX_imp = read_csv("MEX_imp.csv") 

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
#Make sure final data frame is tidy and has no corrupt, garbage, NA's, or need for transformations 

sum(is.na(trade_volume_df)) #No NA Values
str(trade_volume_df) #From Conversions we should turn year into numeric from char, all my categorical variables should also be converted to factors and their corresponding levels 

trade_volume_df$Year = as.numeric(trade_volume_df$Year)
trade_volume_df$`Partner Name` <- as.factor(trade_volume_df$`Partner Name`)
trade_volume_df$`Trade Flow` <- as.factor(trade_volume_df$`Trade Flow`)
trade_volume_df$`Product Group` <- as.factor(trade_volume_df$`Product Group`)

# Remove the Indicator column
trade_volume_df <- trade_volume_df %>% select(-`Indicator`)

#arrange my final data set column order
trade_volume_df <- trade_volume_df %>%
  select(`Reporter Name`, `Year`, `Partner Name`, `Trade Flow`, `Product Group`, Value)


# Convert Value from Thousands in USD to log(Thousands In USD)

trade_volume_df$Value <- log(trade_volume_df$Value)

str(trade_volume_df)

save(trade_volume_df, file = "trade_volume_data.RData")
#Now in Billions of USD

summary_stats <- trade_volume_df %>%
  group_by(`Product Group`) %>%
  summarize(
    Max_Value = max(Value),
    Min_Value = min(Value),
    mean_value = mean(Value),
    sd_value = sd(Value)
  )  %>%
  arrange(desc(mean_value))

#Visualization 1

knitr::kable(summary_stats)

#I was thinking of removing some Product Group members but I rather group them then remove them now that I think about it. Ill most likely do this in the next step for the project. I think I plan on making 3 groups of 7 from the 21. 

#product_groups_to_remove <- c("Footwear", "Hides and Skins", "Animal", "Minerals", "Miscellaneous", "Stone and Glass")
# Remove rows with specified Product Groups
#trade_volume_df <- trade_volume_df %>%
# filter(!`Product Group` %in% product_groups_to_remove)


group_1 <- c("Capital goods", "Consumer goods", "Mach and Elec", "Transportation")

group_2 <- c("Intermediate goods", "Fuels", "Raw materials", "Miscellaneous", "Metals", "Chemicals", "Plastic or Rubber","Wood")

group_3 <- c("Vegetable", "Food Products", "Stone and Glass", "Textiles and Clothing", "Animal", "Minerals","Hides and Skins", "Footwear")

group1_df <- trade_volume_df[trade_volume_df$`Product Group` %in% group_1, ]

group2_df <- trade_volume_df[trade_volume_df$`Product Group` %in% group_2, ]

group3_df <- trade_volume_df[trade_volume_df$`Product Group` %in% group_3, ]


################################################################################################
#Explore: 
#Group 1
g1mexico_data_imp <- group1_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Import")

g1mexico_data_exp <- group1_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Export")

g1canda_data_imp <- group1_df %>%   
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Import")

g1canda_data_exp <- group1_df %>%  
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Export")

#Group 2

g2mexico_data_imp <- group2_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Import")

g2mexico_data_exp <- group2_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Export")

g2canda_data_imp <- group2_df %>%   
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Import")

g2canda_data_exp <- group2_df %>%  
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Export")

#Group 3

g3mexico_data_imp <- group3_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Import")

g3mexico_data_exp <- group3_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Export")

g3canda_data_imp <- group3_df %>%   
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Import")

g3canda_data_exp <- group3_df %>%  
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Export")

#All kept

g4mexico_data_imp <- trade_volume_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Import")

g4mexico_data_exp <- trade_volume_df %>%  
  filter(`Partner Name` == "Mexico", `Trade Flow` == "Export")

g4canda_data_imp <- trade_volume_df %>%   
  filter(`Partner Name` == "Canada",  `Trade Flow` == "Import")

g4canda_data_exp <- trade_volume_df %>%  
  filter(`Partner Name` == "canada",  `Trade Flow` == "Export")




#MEX_PG_imp <- ggplot(mexico_data_imp, aes(x = Year, y = `Product Group`, fill = log(Value))) +
# geom_tile() +
#scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
#labs(title = "MEX: Imports", x = "Year", y = "Product Group", fill = "Value (Millions USD)") +
#theme(legend.position = "none") 

#MEX_PG_exp <- ggplot(mexico_data_exp, aes(x = Year, y = `Product Group`, fill = log(Value))) +
# geom_tile() +
# scale_fill_gradient(low = "white", high = "green") +  # Adjust the color scale as needed
# labs(title = "Exports", x = "Year", y = "Product Group", fill = "Value (Millions USD)") +
#theme(legend.position = "none") 

#Group 1 Heat Map

g1MEX_PG_imp <- ggplot(g1mexico_data_imp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
  labs(title = "MEX: Imports", x = "Year", y = "Product Group 1", fill = "Log(Val)") +
  theme(legend.position = "bottom", legend.justification = "right")

g1MEX_PG_exp <- ggplot(g1mexico_data_exp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +  # Adjust the color scale as needed
  labs(title = "Exports", x = "Year", y = "Product Group 1", fill = "Log(Val)") + 
  theme(legend.position = "bottom", legend.justification = "right")

g1combined_heatmaps_MEX <- plot_grid(g1MEX_PG_imp, g1MEX_PG_exp, ncol = 2, align = "h", axis = "tb")

#Group 2 Heat Map

g2MEX_PG_imp <- ggplot(g2mexico_data_imp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
  labs(title = "MEX: Imports", x = "Year", y = "Product Group 2", fill = "Log(Val)") + 
  theme(legend.position = "bottom", legend.justification = "right")

g2MEX_PG_exp <- ggplot(g2mexico_data_exp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +  # Adjust the color scale as needed
  labs(title = "Exports", x = "Year", y = "Product Group 2", fill = "Log(Val)") +
  theme(legend.position = "bottom", legend.justification = "right")

g2combined_heatmaps_MEX <- plot_grid(g2MEX_PG_imp, g2MEX_PG_exp, ncol = 2, align = "h", axis = "tb")


#Group 3 Heat Map

g3MEX_PG_imp <- ggplot(g3mexico_data_imp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +  # Adjust the color scale as needed
  labs(title = "MEX: Imports", x = "Year", y = "Product Group 3", fill = "Log(Val)") + 
  theme(legend.position = "bottom", legend.justification = "right")

g3MEX_PG_exp <- ggplot(g3mexico_data_exp, aes(x = Year, y = `Product Group`, fill = Value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "green") +  # Adjust the color scale as needed
  labs(title = "Exports", x = "Year", y = "Product Group 3", fill = "Log(Val)") +
  theme(legend.position = "bottom", legend.justification = "right")

g3combined_heatmaps_MEX <- plot_grid(g3MEX_PG_imp, g3MEX_PG_exp, ncol = 2, align = "h", axis = "tb")

# Add a common legend to g1 g2 and g3 combined_heatmaps_MEX

#Visualization 2


g1combined_heatmaps_MEX + theme(legend.position = "bottom", legend.justification = "right")

g2combined_heatmaps_MEX + theme(legend.position = "bottom", legend.justification = "right")

g3combined_heatmaps_MEX + theme(legend.position = "bottom", legend.justification = "right")



###########################
#Explore:
#Group 1
g1MEX_PG_imp2 <- ggplot(g1mexico_data_imp, aes(x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "MEX: Imports",x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 1") +
  theme(legend.position = "right")

g1MEX_PG_exp2 <- ggplot(g1mexico_data_exp, aes( x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "Exports", x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 1") +
  theme(legend.position = "right")

g1combined_line_plots_MEX <- plot_grid(g1MEX_PG_imp2 + theme(legend.position = "none"),
                                       g1MEX_PG_exp2 + theme(legend.position = "none"),
                                       ncol = 2, align = "h", axis = "tb")

g1legend_MEX <- get_legend(g1MEX_PG_imp2 + theme(legend.position = "right"))
g1combined_line_plots_MEX <- cowplot::plot_grid(
  g1combined_line_plots_MEX, 
  g1legend_MEX, 
  ncol = 2, 
  align = "v", 
  rel_widths = c(2, 1.4)
)

#Group 2

g2MEX_PG_imp2 <- ggplot(g2mexico_data_imp, aes(x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "MEX: Imports",x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 2") +
  theme(legend.position = "right")

g2MEX_PG_exp2 <- ggplot(g2mexico_data_exp, aes( x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "Exports", x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 2") +
  theme(legend.position = "right")

g2combined_line_plots_MEX <- plot_grid(g2MEX_PG_imp2 + theme(legend.position = "none"),
                                       g2MEX_PG_exp2 + theme(legend.position = "none"),
                                       ncol = 2, align = "h", axis = "tb")

g2legend_MEX <- get_legend(g2MEX_PG_imp2 + theme(legend.position = "right"))
g2combined_line_plots_MEX <- cowplot::plot_grid(
  g2combined_line_plots_MEX, 
  g2legend_MEX, 
  ncol = 2, 
  align = "v", 
  rel_widths = c(2, 1.4)
)



#Group 3

g3MEX_PG_imp2 <- ggplot(g3mexico_data_imp, aes(x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "MEX: Imports",x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 3") +
  theme(legend.position = "right")

g3MEX_PG_exp2 <- ggplot(g3mexico_data_exp, aes( x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "Exports", x = "Year", y = "Log Value (Thousands USD)", color = "Product Group 3") +
  theme(legend.position = "right")

g3combined_line_plots_MEX <- plot_grid(g3MEX_PG_imp2 + theme(legend.position = "none"),
                                       g3MEX_PG_exp2 + theme(legend.position = "none"),
                                       ncol = 2, align = "h", axis = "tb")

g3legend_MEX <- get_legend(g3MEX_PG_imp2 + theme(legend.position = "right"))
g3combined_line_plots_MEX <- cowplot::plot_grid(
  g3combined_line_plots_MEX, 
  g3legend_MEX, 
  ncol = 2, 
  align = "v", 
  rel_widths = c(2, 1.4)
)

#All Logged


g4MEX_PG_imp2 <- ggplot(g4mexico_data_imp, aes(x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "MEX: Imports",x = "Year", y = "Log Value (Thousands USD)", color = "Product Group") +
  theme(legend.position = "right")

g4MEX_PG_exp2 <- ggplot(g4mexico_data_exp, aes( x = Year, y = as.numeric(Value), color = `Product Group`)) +
  geom_line() +
  labs(title = "Exports", x = "Year", y = "Log Value (Thousands USD)", color = "Product Group") +
  theme(legend.position = "right")

g4combined_line_plots_MEX <- plot_grid(g4MEX_PG_imp2 + theme(legend.position = "none"),
                                       g4MEX_PG_exp2 + theme(legend.position = "none"),
                                       ncol = 2, align = "h", axis = "tb")

g4legend_MEX <- get_legend(g4MEX_PG_imp2 + theme(legend.position = "right"))
g4combined_line_plots_MEX <- cowplot::plot_grid(
  g4combined_line_plots_MEX, 
  g4legend_MEX, 
  ncol = 2, 
  align = "v", 
  rel_widths = c(2, 1.4)
)



#Visualization 3

g1combined_line_plots_MEX

g2combined_line_plots_MEX

g3combined_line_plots_MEX

g4combined_line_plots_MEX

#######################################################################################################
#Explore:

All_prod_exp = ggplot(trade_volume_df |> filter(`Product Group` == "All Products", `Trade Flow` == "Export"), aes(x= Year, y = Value, color = `Partner Name`)) + geom_line() + labs(title = "All Product Exports USA", x = "Year", y = "Log Value (Thousands USD)")

#Visualization 4

All_prod_exp

summary_stats$`Product Group`
summary_stats$mean_value

#--#

num_beg <- max(trade_volume_df$Value[trade_volume_df$`Partner Name` == "Mexico" & trade_volume_df$Year == 1994 & trade_volume_df$`Trade Flow` == "Export" & trade_volume_df$`Product Group` == "All Products"])
num_end <- max(trade_volume_df$Value[trade_volume_df$`Partner Name` == "Mexico" & trade_volume_df$Year == 2020 & trade_volume_df$`Trade Flow` == "Export" & trade_volume_df$`Product Group` == "All Products"])

percent_increase <- ((num_end / num_beg)) * 100
percent_increase