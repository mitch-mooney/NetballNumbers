library(tidyverse)
library(igraph)
library(animation)
# Read in second phase data
vol2_data <- read_csv("netball-numbers-challenge/datasets/vol2/askAndYouShallReceive.csv")
# clean the data capture frequency for edge weights.
net_data <- vol2_data %>% 
  filter(centrePassRec != 'UNKNOWN', secondPhaseRec != 'UNKNOWN', centrePassRec != secondPhaseRec) %>% 
  select(teamName, centrePassRec, secondPhaseRec) %>% 
  group_by(teamName, centrePassRec) %>% 
  count(secondPhaseRec) %>% 
  rename(weight = n) %>% 
  ungroup()
# create list of data frames per team
list.net <- net_data %>% 
  group_by(teamName) %>% 
  group_split()
# rename data frames to names of team
names(list.net) <- unique(net_data$teamName)
# loop through data frames and create a network object in a new list
graph.net <- list()
for(i in list.net){
  name <- unique(i$teamName)
  i$teamName <- NULL
  routes_igraph <- graph_from_data_frame(i, directed = TRUE)
  graph.net[[name]] <- routes_igraph
}

# create vector of colors
team.colors <- c(Thunderbirds = rgb(0.901, 0.576, 0.898, 0.8), Fever = rgb(0.505, 0.976, 0.203, 0.8), 
                 Giants = rgb(0.976, 0.737, 0.203, 0.8), Lightning = rgb(0.203, 0.274, 0.976, 0.8),
                 Magpies = rgb(0.050, 0.050, 0.050, 0.8), Vixens = rgb(0.180, 0.619, 0.878, 0.8), 
                 Swifts = rgb(0.937, 0.023, 0.074, 0.8), Firebirds = rgb(0.596, 0.023, 0.937, 0.8))

# create vector of colors with secondary team colors for title
name.colors <- c(Thunderbirds = rgb(1, 1, 1, 0.8), Fever = rgb(0.505, 0.976, 0.203, 0.8), 
                 Giants = rgb(0.243, 0.705, 0.937, 0.8), Lightning = rgb(0.937, 0.874, 0.243, 0.8),
                 Magpies = rgb(1, 1, 1, 0.8), Vixens = rgb(0.937, 0.243, 0.427, 0.8), 
                 Swifts = rgb(1, 1, 1, 0.8), Firebirds = rgb(0.937, 0.780, 0.243, 0.8))

# loop through the network objects to create separate images then put into a .gif
saveGIF(
  {
    for(name in names(graph.net)){
      j <- graph.net[[name]] # call network graph object
      par(bg=rgb(0.411, 0.411, 0.411,1))  # make background grey
      # build plot
      plot(j,layout=layout.circle, 
           # === vertex
           vertex.color = team.colors[[name]], #rgb(0.560, 0.917, 0.839, 0.8),  # Node color
           vertex.frame.color = "white",                 # Node border color
           vertex.shape="circle",                        # One of “none”, “circle”, “square”, “csquare”, “rectangle” “crectangle”, “vrectangle”, “pie”, “raster”, or “sphere”
           vertex.size=28,                               # Size of the node (default is 15)
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
      # Add title with different colors to match teams
      title(
           main = paste0(name, ': 2018'),
           sub = "Passing network by position for 2nd phase of center pass in SSN.\n Edge thickness standardised to number of passes between positions",
           col.main = name.colors[[name]],
           cex.main=2,
           col.sub = 'white')
    }
  },
  movie.name = "test.gif", 
  interval = 4, 
  ani.width = 500, 
  ani.height = 500,
  outdir = getwd()
)

