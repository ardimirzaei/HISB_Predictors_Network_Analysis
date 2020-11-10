# Clean Code

# Load Libraries
###############################################
library(tidyverse)
library(lubridate)
# library(googlesheets4) Not needed for offline download.
library(dplyr)
library(stringr)
library(gtools)
library(igraph)
library(anytime)
library(ggraph)
library(scales)
library(RColorBrewer)
library(qgraph)

setwd("HISB_Pred_NetworkAnalysis\\") # Change to Folder where everything is

# Retrived Data
###############################################

# Downloaded Copy
url <- 'HISB_Predictors_Coded.csv'
# Straight from the internet.
url <- 'https://raw.githubusercontent.com/ardimirzaei/HISB_Predictors_Network_Analysis/master/HISB_Predictors_Coded.csv'

df_load <- read.csv(url, header = FALSE)

df_backup <- df_load # Backup to prevent re load
df <- as.matrix(df_load) # Need to do this because of the new API for google sheet. Not need really needed for CSV


# Source Custom Functions
###############################################

source("Functions_and_Codes.R")

# Clean and Correct Database
###############################################
Cleaned_Data <- Clean_and_Correct_Database(df, AbbreviatedColumn = 3)

dfUniqueSpread <- Cleaned_Data[1][[1]]
dfYears <- Cleaned_Data[2][[1]]

# Descriptives
###############################################
# Total Number of predictors
Total_Number_of_Predictors(dfUniqueSpread)
# Top Predictors and cluster numbers
Top_Predictors(dfUniqueSpread)
# 

# Edges
###############################################
# RemoveList <- c("Sociodemographic", "Affect", "Health", "Source", "Information")
RemoveList = ""
Edge_List <- Produce_edges(dfUniqueSpread, dfYears, RemoveList)

edgeFile_Dynamic <- Edge_List[1][[1]]

# Nodes
###############################################
Node_List <- Produce_Nodes(dfUniqueSpread, edgeFile_Dynamic)

nodeFile_Dynamic <- Node_List[1][[1]]


# Export for Gephi
###############################################
write.csv(edgeFile_Dynamic, 'edge_file_dynamic_for_gephi.csv', row.names=FALSE)
write.csv(nodeFile_Dynamic, 'node_file_dynamic_for_gephi.csv', row.names=FALSE)


###############################################
# Cut off for graphs
###############################################
CutOff <- 2008

# Pre + Including 2008
###############################################
# Edges
edgeFile_Pre2008 <- edgeFile_Dynamic[edgeFile_Dynamic$Year<=CutOff,]
# Nodes
nodeFile_Pre2008 <- nodeFile_Dynamic[nodeFile_Dynamic$year<=CutOff,]
# Export for Gephi
write.csv(edgeFile_Pre2008, 'edge_file_Pre2008_for_gephi.csv', row.names=FALSE)
write.csv(nodeFile_Pre2008, 'node_file_Pre2008_for_gephi.csv', row.names=FALSE)


# Post 2008
###############################################
# Edges
edgeFile_Post2008 <- edgeFile_Dynamic[edgeFile_Dynamic$Year>CutOff,]
# Nodes
post2008_cull <- nodeFile_Dynamic$label %in% edgeFile_Post2008$targetName | nodeFile_Dynamic$label %in% edgeFile_Post2008$sourceName
nodeFile_Post2008 <- nodeFile_Dynamic[post2008_cull,] # Need the labels to show up. 
# Export for Gephi
write.csv(edgeFile_Post2008, 'edge_file_Post2008_for_gephi.csv', row.names=FALSE)
write.csv(nodeFile_Post2008, 'node_file_Post2008_for_gephi.csv', row.names=FALSE)


###############################################
# Clusters and groups for the predictors
###############################################

# All Years
Modularity_of_Predictors(edgeFile_Dynamic, nodeFile_Dynamic)
# Pre 2008
Modularity_of_Predictors(edgeFile_Pre2008, nodeFile_Pre2008)
# Post 2008
Modularity_of_Predictors(edgeFile_Post2008, nodeFile_Post2008)

###############################################
# Descriptive statistics for graphs
###############################################

Descriptives_All_Years <- Graph_Descriptive_Statistics(edgeFile = edgeFile_Dynamic, nodeFile = nodeFile_Dynamic, Title = "Descriptives for all years")
Descriptives_Pre_Cutoff <- Graph_Descriptive_Statistics(edgeFile = edgeFile_Pre2008, nodeFile = nodeFile_Pre2008, Title = "Descriptives for Pre 2008 Model")
Descriptives_Post_Cutoff <- Graph_Descriptive_Statistics(edgeFile = edgeFile_Post2008, nodeFile = nodeFile_Post2008, Title = "Descriptives for Post 2008 Model")

Descriptives_Export <-  cbind(unlist(Descriptives_All_Years),cbind(unlist(Descriptives_Pre_Cutoff), unlist(Descriptives_Post_Cutoff)))
write.csv(Descriptives_Export, 'Descriptives_Export.csv', row.names=TRUE)


###############################################
# Ordered List
###############################################

# Order list of the predictor terms. 

write.csv(nodeFile_Dynamic[order(nodeFile_Dynamic$year),], 'Ordered_Nodes_for_results.csv', row.names=FALSE)

###############################################
# Print Graphs
###############################################

FILENAME.pdf <-  paste0("All_Year_Graph_",format(Sys.time(), '%d%m%Y'),".pdf")


pdf(file = FILENAME.pdf,   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12)
plot_networks(edgeFile_Dynamic, nodeFile_Dynamic, "Network Model for all years to 2019")
plot_networks(edgeFile_Pre2008, nodeFile_Pre2008, "Network Model for Pre 2008")
plot_networks(edgeFile_Post2008, nodeFile_Post2008, "Network Model for Post 2008")
dev.off()


###############################################
# Use of Theory over Time Figure
###############################################

df_Theories <- Cleaned_Data[3][[1]]

df_Theories %>% 
	ggplot(aes(x=Year)) + 
	geom_histogram(binwidth = 1.0, fill="black", col="grey") + 
	theme_classic() + 
	coord_cartesian(xlim = c(1990,2020)) + 
	scale_x_continuous("Year", breaks = seq(1990,2020,5), limits = c(1990, 2020)) + 
	coord_cartesian(ylim = c(0,35)) + 
	scale_y_continuous('Count of Theories', breaks = seq(0, 35, 5)) + 
	ggtitle("Count of Discussed Theories by Year")

df_1 <- unique(df_Theories[,1:2]) %>% 
	group_by(Year) %>% 
	summarise(count = n()) %>% 			
	mutate(Type = "Theories") # Changed from Unique Theories

df_3 <- df_Theories %>% 
	group_by(Year) %>% 
	summarise(count = n()) %>% 
	mutate(Type = "Theories")

df_2 <- as.data.frame(dfYears) %>% 
	group_by(Year) %>% 
	summarise(count = n()) %>% 
	mutate(Type = "Articles")

rbind(df_1, df_2) %>% # Use the unqiue theories database
	ggplot(aes(x=Year, y=count, fill=Type, label=count)) + 
	geom_bar(stat="identity", position=position_dodge()) +
	geom_text(size = 4, position = position_dodge(width = 1), vjust=-0.25) +
	theme_classic() + 
	coord_cartesian(xlim = c(1990,2020)) + 
	scale_x_continuous("Year", breaks = seq(1990,2020,5), limits = c(1990, 2020)) + 
	coord_cartesian(ylim = c(0,40)) + 
	scale_y_continuous('Count', breaks = seq(0, 40, 5)) +
	theme(legend.position = c(0.2, 0.8)) + 
	ggtitle("Count of Theories and Articles by Year")

ggsave("Articles_Theories_By_Year.pdf", width = 20, height = 20, units = "cm")





full_join(df_2, df_1, by='Year') %>%
	mutate(comparison = count.y/count.x*100, 
		ComparisonAsText = paste0(count.y,"/",count.x)) %>%
	ggplot(aes(x=Year, y=comparison, label = ComparisonAsText )) +
	geom_bar(stat="identity", fill='#F8766D', col='black') +
	geom_text(size = 3, position = position_stack(vjust = 0.8)) +
	theme_classic() + 
	coord_cartesian(xlim = c(1990,2020)) + 
	scale_x_continuous("Year", breaks = seq(1990,2020,5), limits = c(1990, 2020)) + 
	coord_cartesian(ylim = c(0,100)) + 
	scale_y_continuous('Proportion (as %)', breaks = seq(0, 100, 10)) +
	theme(legend.position = c(0.2, 0.8)) + 
	ggtitle("Comparison of Articles vs Theories")


###############################################
# Descriptive statistics for graphs
###############################################		
# All_Cuts <- Descriptives_All_Years
# for (i in sort(unique(dfYears[,2]))) {
# 	CutOff <- i
# 	# Edges
# 	edgeFile_CutOff <- edgeFile_Dynamic[edgeFile_Dynamic$Year<=CutOff,]
# 	# Nodes
# 	nodeFile_CutOff <- nodeFile_Dynamic[nodeFile_Dynamic$year<=CutOff,]
# 	temp <- Graph_Descriptive_Statistics(edgeFile = edgeFile_CutOff, nodeFile = nodeFile_CutOff, Title = paste0("Descriptives for Pre ",i," Model"), RecursiveAdd=TRUE)
# 	All_Cuts <- cbind(unlist(All_Cuts),unlist(temp))
# }

# write.csv(All_Cuts, 'All_Cuts_Descriptive.csv', row.names=TRUE)

# All_Cuts_Standalone <- Descriptives_All_Years
# for (i in sort(unique(dfYears[,2]))) {
# 	CutOff <- i
# 	# Edges
# 	edgeFile_CutOff <- edgeFile_Dynamic[edgeFile_Dynamic$Year==CutOff,]
# 	# Nodes
# 	nodeFile_CutOff <- nodeFile_Dynamic[nodeFile_Dynamic$year<=CutOff,]
# 	temp <- Graph_Descriptive_Statistics(edgeFile = edgeFile_CutOff, nodeFile = nodeFile_CutOff, Title = paste0("Descriptives for ",i," Model"), RecursiveAdd=TRUE)
# 	All_Cuts_Standalone <- cbind(unlist(All_Cuts_Standalone),unlist(temp))
# }


PAPER_OUTPUTS()

# write.csv(All_Cuts_Standalone, 'All_Cuts_Standalone_Descriptive.csv', row.names=TRUE)