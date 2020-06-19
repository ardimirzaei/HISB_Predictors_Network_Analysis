# Clean Code

# Load Libraries
###############################################
library(tidyverse)
library(lubridate)
library(dplyr)
library(stringr)
library(gtools)
library(anytime)
library(ggraph)
library(scales)
library(RColorBrewer)
library(qgraph)

# Descriptives
###############################################
# Total Number of predictors
Total_Number_of_Predictors <- function(data_file){ # Assumes datafile is dfUniqueSpread
  print(paste0("Total number of predictors: ",nrow(data_file)))
}
# Top Predictors
Top_Predictors <- function(data_file, print_list=FALSE, print_cluster_groups=TRUE){ # Assumes datafile is dfUniqueSpread
  sorted_list <- sort(
    table(
      paste0(
        unlist(data_file[,3])#,
        # unlist(data_file[,4]),
        # unlist(data_file[,5]),
        # unlist(data_file[,6])
      )
    ),
  decreasing = TRUE)

  if (print_list){
    print(sorted_list)
  }
  
  if (print_cluster_groups){
    cat(paste0("\nTotal number of clustered predictor groups : \n",length(sorted_list),"\n"))
  }

  cat("\nThe top predictors: \n")
  print(head(sorted_list))
}
# Number of Predictor Clusters
Modularity_of_Predictors <- function(edge_file, node_file) {
  EDGELIST <- edge_file
  NODELIST <- node_file
  g <- graph_from_data_frame(EDGELIST,vertices = NODELIST, directed=FALSE)
  mbrship <-  multilevel.community(g)$membership
  cat (paste0("\n\n Number of Modularity in this Graph: ",max(mbrship),"\n"))


  for (CLUSTERGROUP in sort(unique(mbrship))){
    cat(paste0("\n Modularity Group ",CLUSTERGROUP,": \n\n"))
      print(NODELIST[which(mbrship == CLUSTERGROUP),1])

  }

}


# Download Database
###############################################

# df_load <- gs_title('Literature Search and Results') %>%
#   gs_read('HISB Predictors Coded')

# df_backup <- df_load # Backup of download to prevent re download
# df <- df_load

# Clean and Correct Database
###############################################
Clean_and_Correct_Database <- function(df, headers = 1, AbbreviatedColumn = FALSE){
  # Headers can be Numerical or TRUE
  if (headers== 0 | !headers) {
    stop("Headers can be integar > 0 or FALSE ")
  }

  # Years
  # dfYears <- df[-headers,c(1,8)] # For dynamic modelling
  df <- df[!df[,3]=="NA",] # Remove NA Coding
  df <- df[!df[,3]=="NOENTRY",] # Remove the NO entry
  df <- df[!df[,3]=="NA_Interaction",] # Remove the Interaction entry
  df <- df[!df[,3]=="Unclassified",] # Remove the Unclassified  entry

  # Choose a number and this will run
  if (is.numeric(AbbreviatedColumn)){
    df <- abbrv_names(df, COLUMN_TO_CHANGE = AbbreviatedColumn )
  }




  df_RecordNumber <- as.numeric(unlist(df[-headers,1]))
  df_Year <- year(anydate(as.numeric(unlist(df[-headers,5]))))
  dfYears <- cbind(df_RecordNumber, df_Year)

  colnames(dfYears) <- c("RecordNumber","Year")
  dfYears <- unique(dfYears)

  # Unique Terms
  df_terms <- df[-headers,c(1:3)] # First row contained string terms for the headers
  colnames(df_terms) <- c('RecordNumber', 'Variable', 'Coding') # Assign headers
  dfUnique <- unique(df_terms[,c(1,3)]) # Only want the unique connections of terms in a paper. Duplicates are not needed. 
  dfUnique <- dfUnique[complete.cases(dfUnique),] # Remove columns with NA Entry
  dfUnique <- dfUnique[!dfUnique[,2]=="NOENTRY",] # Remove the NO entry
  dfUnique <- dfUnique[!dfUnique[,2]=="NA_Interaction",] # Remove the Interaction entry
  dfUnique <- dfUnique[!dfUnique[,2]=="Unclassified",] # Remove the Unclassified  entry
  # dfUnique$RecordNumber <- as.numeric(as.character(dfUnique$RecordNumber)) 

  # dfUniqueSpread <- as.data.frame(cbind(unlist(dfUnique[,1]), str_split_fixed(unlist(dfUnique[,2]), "/", 4))) # Split the connected terms. 4 connections is the max. 
  dfUniqueSpread <- dfUnique
  colnames(dfUniqueSpread) <- c("RecordNumber","Coding")

  dfUniqueSpread <- (merge(dfYears, dfUniqueSpread)) # Merge years and terms by record number
  dfUniqueSpread$Coding <- unlist(dfUniqueSpread$Coding)

  # Theories
  df_theories_unclean <- df[-headers,c(1,4)] #Record number and Theory columns only
  df_theories_unclean <- unique(df_theories_unclean[,c(1,2)]) # Only want the unique theories in each paper. Duplicates are not needed.

  df_theories_clean <- matrix(0, nrow = 1, ncol = 2, dimnames = list(c(), c("RecordNumber", "Theory")))
  for (i in 1:nrow(df_theories_unclean)){
    df_theories_clean <- rbind(df_theories_clean,
      cbind(RecordNumber = as.character(df_theories_unclean[i,1]),
        Theory = unlist((str_split(df_theories_unclean[i,2], pattern=";")))
        )
      )
  }

  df_Theories <- df_theories_clean[-1,] # Can't make this as a tibble because of the below assignment of NA's
  colnames(df_Theories) <- c('RecordNumber', 'Theory') # Assign headers
  df_Theories[,'RecordNumber'] <- as.numeric(as.character(df_Theories[,'RecordNumber']))
  df_Theories[df_Theories[,'Theory'] == "None",] <-  "NA"
  df_Theories[df_Theories[,'Theory'] == "",] <-  "NA"
  df_Theories <- df_Theories[!df_Theories[,'Theory'] == "Delete",]
  df_Theories <- df_Theories[!df_Theories[,'Theory'] == "NA",]  
  df_Theories <- as_tibble(merge(dfYears, df_Theories)) # Merge years and terms by record number


  return(list(dfUniqueSpread, dfYears, df_Theories))

}



# Edges
###############################################
Produce_edges <- function(dfUniqueSpread, dfYears, RemoveLists=""){


  code_list <- gather((dfUniqueSpread), "Column", "Coding", -RecordNumber, -Year)
  code_list <- code_list[,-3]
  code_list <- (unique(code_list[order(code_list[1]),c(1,3)]))
  code_list <- code_list[!code_list$Coding=="",] #remove balnks
  code_list <- code_list %>% 
    filter(!(Coding %in% RemoveList))



  edge_file <- t(matrix(c(0,0,0)))
  for (RecordNumber in unique(code_list$RecordNumber)){
    tempPredictors <- unlist(code_list[code_list$RecordNumber==RecordNumber,2])  # select only the predicot terms #edit 2, add unlist if we not splitting
    tempPredictors <- tempPredictors[!tempPredictors==""] # Remove the blank entries
    if (length(tempPredictors)>1){
      totalPredictors <- length(tempPredictors)
        tempBuild <- combinations(n=totalPredictors, r=2,v=tempPredictors, repeats.allowed=F) 
        tempBuild <- cbind(RecordNumber, tempBuild)
        edge_file <- rbind(edge_file, tempBuild)
    }
  }

  edgeFile <- edge_file[-1,] # remove the zeros
  # edgeFile <- unique(edgeFile) # the edgeFile will repeat the process for duplicate numbers. Can reduce runtime by possibly adding unique() to the record unmbers.  
  edgeFile_backup <- edgeFile # Store this file in there for later

  ####
  # I have yet to find a way around this
  ####
  NodeSize <- table(as.vector(as.matrix(unlist(dfUniqueSpread[,3:ncol(dfUniqueSpread)]))))
  # NodeSize <- NodeSize[-1] # Remove the blanks

  #might not need this
  nodes <- rownames(NodeSize)
  nodes <- as.data.frame(nodes) %>% rowid_to_column("id")
  colnames(nodes) <- c('id','label')
  ####

  # For Dynamic Edge list to export to Gephi
  edgeFile_Dynamic <- as.data.frame(edgeFile)
  edgeFile_Dynamic$RecordNumber <- as.numeric(as.character(edgeFile_Dynamic$RecordNumber))
  edgeFile_Dynamic <- merge(edgeFile_Dynamic, dfYears, by.x = 'RecordNumber', by.y = 'RecordNumber')
  edgeFile_Dynamic$StartDate <- paste0("[",edgeFile_Dynamic$Year)
  edgeFile_Dynamic$YearStop <- 2019
  edgeFile_Dynamic$EndDate <- ",2019]"
  edgeFile_Dynamic$Interval <- paste0(edgeFile_Dynamic$StartDate,edgeFile_Dynamic$EndDate)
  edgeFile_Dynamic$type <- "Undirected"
  colnames(edgeFile_Dynamic)[2:3] <- c("sourceName", "targetName")
  edgeFile_Dynamic <- merge(edgeFile_Dynamic, nodes, by.x = 'sourceName', by.y = 'label')
  edgeFile_Dynamic <- merge(edgeFile_Dynamic, nodes, by.x = 'targetName', by.y = 'label')
  colnames(edgeFile_Dynamic)[10:11] <- c("source","target")
  edgeFile_Dynamic <- edgeFile_Dynamic[complete.cases(edgeFile_Dynamic$Year),]

  edgeFile_Dynamic <- as_tibble(edgeFile_Dynamic) # Just to be a bit clean

  return(list(edgeFile_Dynamic,edgeFile_backup))
}



# Nodes
###############################################

Produce_Nodes <- function(dfUniqueSpread, edgeFile_Dynamic){

  NodeSize <- table(as.vector(as.matrix(unlist(dfUniqueSpread[,3:ncol(dfUniqueSpread)])))) # Every mentioned predictor term. # added the unlist
  # NodeSize <- NodeSize[-1] # Remove the blanks

  NodesSizeLabeled <- rownames_to_column(as.data.frame(NodeSize))
  colnames(NodesSizeLabeled)<-c("id", "label","weight")
  nodeFile_backup <- NodesSizeLabeled # Store this file in there for later

  # Dynamc Node list
  # temp1 <- edgeFile_Dynamic %>% 
  #   group_by(sourceName) %>% 
  #   summarise(Year = min(Year))

  # temp2 <- edgeFile_Dynamic %>%
  #   group_by(targetName) %>% 
  #   summarise(Year = min(Year))

  # temp3 <- as.data.frame(rbind(as.matrix(temp1), as.matrix(temp2)))
  # rm(list=c('temp1', 'temp2'))

  # temp3$Year <- as.numeric(as.character(temp3$Year))

  temp <- dfUniqueSpread %>%
    group_by(Coding) %>%
    summarise(year = min(Year))

  nodeFile_Dynamic <- as.data.frame(temp) #%>%
    # group_by(sourceName) %>% 
    # summarise(year = min(Year))

  nodeFile_Dynamic <- merge(NodesSizeLabeled, nodeFile_Dynamic, by.x = 'label', by.y = 'Coding')
  nodeFile_Dynamic$id <- as.numeric(nodeFile_Dynamic$id)

  g <- graph_from_data_frame(edgeFile_Dynamic,vertices = nodeFile_Dynamic, directed=FALSE)

  nodeFile_Dynamic$Membership <- multilevel.community(g)$membership

  # nodeFile_Dynamic$Membership <- 1

  # nodeFile_Dynamic[grepl("Affect",nodeFile_Dynamic[,1]),5] <- 2
  # nodeFile_Dynamic[grepl("Health\\/",nodeFile_Dynamic[,1]),5] <- 3
  # nodeFile_Dynamic[grepl("Information\\/",nodeFile_Dynamic[,1]),5] <- 4
  # nodeFile_Dynamic[grepl("Sociodemographic\\/",nodeFile_Dynamic[,1]),5] <- 5
  # nodeFile_Dynamic[grepl("Source\\/",nodeFile_Dynamic[,1]),5] <- 6

  return(list(nodeFile_Dynamic,nodeFile_backup))

}



# Descriptive Statistics for graphs
###############################################
Graph_Descriptive_Statistics <- function(edgeFile, nodeFile, Title = "Graph Descriptive Statistics", RecursiveAdd = FALSE){

  require(igraph)

  g <- graph_from_data_frame(d=edgeFile, vertices=nodeFile, directed=FALSE)


  sum_table <- list(
    Title = Title,
    Nodes = paste0("Node Count: ", vcount(g)),
    Edges = paste0("Edge Count: ", ecount(g)),
    Degree = paste0("Degree: ",   round(mean(degree(g)),3)),
    Strength = paste0("Strength: ",   round(mean(strength(g)),3)),
    Closeness = paste0("Closeness: ",   round(mean(closeness(g, normalized=TRUE)),3)),
    Betweenness = paste0("Betweenness: ",   round(mean(betweenness(g)),3)),
    Eigenvector = paste0("Eigenvector centrality: ",   round(mean(eigen_centrality(g)$vector),3)),
    PageRank = paste0("Page rank: ",   round(mean(page_rank(g)$vector),3)),
    Authority = paste0("Authority score: ",   round(mean(authority_score(g)$vector),3)),
    NetworkDiameter = paste0("Network Diameter: ",   diameter(g, directed=FALSE, weights=NA)),
    Distance = paste0("Mean Distance: ",   round(mean_distance(g, directed=FALSE),3)),
    Density = paste0("Edge Density Graph Density: ",   round(edge_density(g),3)),
    Reciprocity = paste0("Reciprocity: ",   round(reciprocity(g),3)),
    Transitivity = paste0("Transitivity: ",   round(transitivity(g),3)),
    Components = paste0("Connected Components: ",   components(g)$n), 
    ModularityClass = paste0("Modularity Class: ",  max(multilevel.community(g)$membership))
    )

if (RecursiveAdd){
    sum_table <- list(
    Title = Title,
    Nodes = paste0(vcount(g)),
    Edges = paste0(ecount(g)),
    Degree = paste0(round(mean(degree(g)),3)),
    Strength = paste0(round(mean(strength(g)),3)),
    Closeness = paste0(round(mean(closeness(g, normalized=TRUE)),3)),
    Betweenness = paste0(round(mean(betweenness(g)),3)),
    Eigenvector = paste0(round(mean(eigen_centrality(g)$vector),3)),
    PageRank = paste0(round(mean(page_rank(g)$vector),3)),
    Authority = paste0(round(mean(authority_score(g)$vector),3)),
    NetworkDiameter = paste0(diameter(g, directed=FALSE, weights=NA)),
    Distance = paste0(round(mean_distance(g, directed=FALSE),3)),
    Density = paste0(round(edge_density(g),3)),
    Reciprocity = paste0(round(reciprocity(g),3)),
    Transitivity = paste0(round(transitivity(g),3)),
    Components = paste0(components(g)$n), 
    ModularityClass = paste0( max(multilevel.community(g)$membership))
  )
  }

  cat(paste0(matrix(unlist(sum_table, use.names=FALSE)),"\n"))  
  return(sum_table)
}



# Plot the graphs
###############################################

plot_networks <- function(EDGELIST, NODELIST, TITLE, PLotCentrality = FALSE){

  require(ggraph)
  require(igraph)
  require(scales)
  require(RColorBrewer)
  require(qgraph)


  g <- graph_from_data_frame(EDGELIST,vertices = NODELIST, directed=FALSE)

  strgth <- strength(g)
  strgth <- rescale(strgth, to = c(3 ,10 ))

  e <- get.edgelist(g, names=FALSE)
  l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
        area=8*(vcount(g)^1.99999),repulse.rad=(vcount(g)^2.306))

  # Blue, yellow, orange, dark blue, red
  # https://coolors.co/a8acc5-fac4c3-fff3c7-b3c7e7-dda2ae

  colrs_light <- c("#B3C7E7","#8DD3C7","#FAC4C3","#FFF3C7","#FAC4C3","#8DD3C7","#A8ACC5","#DDA2AE")
  colrs <- c("#80B1D3","#8DD3C7","#FB8072","#FFFFB3","#BEBADA")

  colrs <- brewer.pal(6, "Set1")[c(6,3,4,2,5,1)]
  colrs_light <- brewer.pal(8, "Set3")[c(2,1,3,5,6,4)]
  mbrship <-  NODELIST$Membership
  # mbrship <-  multilevel.community(g)$membership
  edge.start <- ends(g, es=E(g), names=F)[,1]

  edge.col <- colrs_light[mbrship[edge.start]]

  node.labels <- NODELIST$label

  plot.igraph(g, 
    layout = l,
    main = TITLE, 
    vertex.size = strgth, 
    vertex.label.color = "black",
    vertex.color = colrs[mbrship],
    vertex.label.font=2,
    label = node.labels,
    edge.color = edge.col,
    edge.size = 0.1
    ) 

  if(PLotCentrality){
    centralityPlot(g)

  }
  
}


abbrv_names <- function(df, COLUMN_TO_CHANGE=3){
  df[,COLUMN_TO_CHANGE] <- gsub("Affect","Affect",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Affect/Attitude","Attitude",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Affect/Attitude/Concern","Attitude/Concern",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Affect/Internal locus of control","Internal locus of control",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Affect/Self-Efficacy","Self-Efficacy",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Affect/Self-Efficacy/Perceived Behavioral Control","Self-Efficacy/PBC",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Behaviour/Adherence","Behav/Adherence",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Behaviour/Experience","Behav/Experience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Behaviour/Intention","Behav/Intention",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Environment/Network","Environ/Network",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Environment/Network/Internet","Environ/Network/Internet",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Beliefs","Health/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Behaviour","Health Behav",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition","Health Cond",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Beliefs","Hlth Cond/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Duration","Hlth Cond/Duration",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Experience","Hlth Cond/Experience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Family","Hlth Cond/Family",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Knowledge","Hlth Cond/Knowledge",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Side Effects","Hlth Cond/Side Effects",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Condition/Treatment","Health Cond/Treatment",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Health Status","Health Status",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Health/Risk/Beliefs","Health/Risk/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Attitude/Trust","Info/Attitude/Trust",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Beliefs","Info/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Beliefs/Salience","Info/Beliefs/Salience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Health/Information Seeking","Info/Health/Info Seeking",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Health/Information Seeking/Beliefs","Info/Health/Info Seeking/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Information Needs","Info/Info Needs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Information/Non-healthcare/Information Seeking","Info/Non-healthcare/Info Seeking",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Non-healthcare/Beliefs","Non-healthcare/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Society/Subjective Norms","Society/Subjective Norms",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Age","S/Age",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Carer","S/Carer",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Education","S/Education",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Employment","S/Employment",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Financial-Capital","S/Financial-Capital",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Financial-Health Insurance","S/Financial-Health Insurance",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Financial-Income","S/Financial-Income",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Gender","S/Gender",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Health/Health Literacy","S/Health/Health Literacy",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Household","S/Household",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Language","S/Language",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Location","S/Location",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Other","S/Other",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Race or Ethnicity","S/Race or Ethnicity",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Sexual Orientation","S/Sexual Orientation",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Sociodemographic/Society/Engagement","S/Society/Engagement",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Beliefs","Source/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Count","Source/Count",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare","Source/Healthcare",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Access","Healthcare/Access",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Attitude/Trust","Healthcare/Attitude/Trust",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Beliefs","Healthcare/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Engagement","Healthcare/Engagement",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Experience","Healthcare/Experience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Familiarity","Healthcare/Familiarity",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Healthcare/Type","Healthcare/Type",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Internet/Access","Internet/Access",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Internet/Attitude/Trust","Internet/Attitude/Trust",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Internet/Beliefs","Internet/Beliefs",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Internet/Experience","Internet/Experience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Internet/Type","Internet/Type",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Knowledge","Source/Knowledge",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Non-healthcare/Attitude/Trust","Non-healthcare/Attitude/Trust",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Non-healthcare/Experience","Non-healthcare/Experience",df[,COLUMN_TO_CHANGE])
  df[,COLUMN_TO_CHANGE] <- gsub("Source/Non-healthcare/Type","Non-healthcare/Type",df[,COLUMN_TO_CHANGE])

  return(df)
}





PAPER_OUTPUTS <- function (){
  # Ouptut for the paper
  ###############################################
  # Total Number of predictors
  Total_Number_of_Predictors(dfUniqueSpread)
  # Top Predictors and cluster numbers
  Top_Predictors(dfUniqueSpread)
  # 
  cat("All Year Edges \n")
  print(Descriptives_All_Years$Edges)
  cat("All Years Modularity \n")
  print(Descriptives_All_Years$ModularityClass)
  cat("Nodes with the Highets Degree\n")
  g <- graph_from_data_frame(edgeFile_Dynamic,vertices = nodeFile_Dynamic, directed=FALSE)
  g_p08 <- graph_from_data_frame(edgeFile_Pre2008,vertices = nodeFile_Pre2008, directed=FALSE)
  g_p <- graph_from_data_frame(edgeFile_Post2008,vertices = nodeFile_Post2008, directed=FALSE)
  print(data.frame(Degree = (head(sort(degree(g), decreasing=TRUE),10))))
  cat("New predictors since CutOff: \n")
  print(nodeFile_Dynamic[nodeFile_Dynamic$year>CutOff,c('label','year')])
  cat("Edges pre 2008 compare to post : \n")
  print(paste0((ecount(g_p)/ecount(g_p08)), " times more edges in post 2008 than pre 2008"))
  cat("Mean Degree pre 2008 compare to post : \n")
  print(paste0(mean(degree(g_p))/(mean(degree(g_p08))), " times more edges in post 2008 than pre 2008"))
  cat("Eigen Vector Centrality (Top 10) : \n")
  print(sort(eigen_centrality(g)$vector, decreasing=TRUE)[1:10])

}
