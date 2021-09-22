library(tidyverse)
library(igraph)
library(animation)

vol2_data <- read_csv("netball-numbers-challenge/datasets/vol2/askAndYouShallReceive.csv")

net_data <- vol2_data %>% 
  filter(centrePassRec != 'UNKNOWN', secondPhaseRec != 'UNKNOWN', centrePassRec != secondPhaseRec) %>% 
  select(teamName, centrePassRec, secondPhaseRec) %>% 
  group_by(teamName, centrePassRec) %>% 
  count(secondPhaseRec) %>% 
  rename(weight = n) %>% 
  ungroup()

list.net <- net_data %>% 
  group_by(teamName) %>% 
  group_split()

names(list.net) <- unique(net_data$teamName)

graph.net <- list()
for(i in list.net){
  name <- unique(i$teamName)
  i$teamName <- NULL
  routes_igraph <- graph_from_data_frame(i, directed = TRUE)
  graph.net[[name]] <- routes_igraph
}

saveGIF(
  {
    for(name in names(graph.net)){
      j <- graph.net[[name]]
      par(bg="black")
      plot(j,layout=layout.circle, 
           # === vertex
           vertex.color = rgb(0.560, 0.917, 0.839,0.8),  # Node color
           vertex.frame.color = "white",                 # Node border color
           vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
           vertex.size=25,                               # Size of the node (default is 15)
           vertex.size2=NA,                              # The second size of the node (e.g. for a rectangle)
           
           # === vertex label
           #vertex.label=LETTERS[1:10],                   # Character vector used to label the nodes
           vertex.label.color="white",
           #vertex.label.family="Times",                  # Font family of the label (e.g.“Times”, “Helvetica”)
           vertex.label.font=2,                          # Font: 1 plain, 2 bold, 3, italic, 4 bold italic, 5 symbol
           vertex.label.cex=1,                           # Font size (multiplication factor, device-dependent)
           vertex.label.dist=0,                          # Distance between the label and the vertex
           vertex.label.degree=0 ,                       # The position of the label in relation to the vertex (use pi)
           
           # === Edge
           edge.color="white",                           # Edge color
           edge.width=E(j)$weight/25,                    # Edge width, defaults to 1
           edge.arrow.size=1,                            # Arrow size, defaults to 1
           edge.arrow.width=1,                           # Arrow width, defaults to 1
           edge.lty="solid",                             # Line type, could be 0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”, 6 or “twodash”
           edge.curved=0.3)                              # Edge curvature, range 0-1 (FALSE sets it to 0, TRUE to 0.5)
      title(
           main = paste0(name, ': 2018'),
           sub = "2nd Phase passing network \n edge thickness standardised to number of passes",
           col.main = "white")
    }
  },
  movie.name = "test.gif", 
  interval = 4, 
  ani.width = 500, 
  ani.height = 500,
  outdir = getwd()
)

