plot_networks <- function(EDGELIST, NODELIST, TITLE){

  require(ggraph)
  require(igraph)
  require(scales)
  require(RColorBrewer)
  require(qgraph)


  g <- graph_from_data_frame(EDGELIST,vertices = NODELIST, directed=FALSE)

  # g1 <- graph_from_data_frame(unique(edgeFile_Dynamic[,1:2]), vertices = nodeFile_Dynamic,directed=FALSE)
  # g<- g1


  strgth <- strength(g)
  strgth <- rescale(strgth, to = c(3 ,10 ))

  # l <- layout.fruchterman.reingold(g,niter=500,grid =c('auto'))
  # l <- layout.auto(g)
  # # l <- layout_on_sphere(g)

  e <- get.edgelist(g, names=FALSE)
  l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
        area=8*(vcount(g)^1.95),repulse.rad=(vcount(g)^2.1))

  # Blue, yellow, orange, dark blue, red
  # https://coolors.co/a8acc5-fac4c3-fff3c7-b3c7e7-dda2ae

  # colrs <- c("#3066BE", "#FFE066", "#F25F5C", "#121B60","#A20021")
  # colrs_light <- c("#B3C7E7","#FFF3C7","#FAC4C3","#8DD3C7","#A8ACC5","#DDA2AE")

  colrs <- brewer.pal(5,"Set1") # must reaarrange
  # Red,  gree, blue, purple, orange
  colrs <- c("#E41A1C", "#4DAF4A","#377EB8", "#984EA3" ,"#FF7F00", "#FFFF33", "#FFFFFF")


  colrs_light <- brewer.pal(5, "Set3") # must reaarrange
  colrs_light <- c("#FB8072","#B3DE69","#80B1D3","#BEBADA" ,"#FDB462","#FFFFB3", "#696969" )

  mbrship <-  multilevel.community(g)$membership
  mbrship <- c(5,6,4,7,6,7,4,4,1,5,2,4,4,7,6,5,7,2,4,2,2,4,4,2,5,4,4,3,3,6,1,7,7,4,4,4,6,7,4,1,4,2,3,7,4,2,4,6,2,7,5)

  edge.start <- ends(g, es=E(g), names=F)[,1]

  edge.col <- colrs_light[mbrship[edge.start]]

  node.labels <- Node_List[2][[1]][2]

  # cut.off <- mean(links$weight) 
  # net.sp <- delete_edges(net, E(net)[weight<cut.off])


  plot.igraph(g, 
    layout = l,
    main = TITLE, 
    vertex.size = strgth, 
    vertex.label.color = "black",
    # vertex.color = colrs[mbrship],
    vertex.label.font=2,
    label = node.labels,
    # edge.color = edge.col,
    edge.size = 0.1 
    )

}
