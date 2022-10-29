## loading data
treedata <- read.csv("../data/trees.csv", header = TRUE)

TreeHeight <- function(degrees, distance) {

    radians <- degrees * pi / 180
    height <- distance * tan(radians)

    return(height)
}

for (Species in treedata) {
    print(paste("Tree height of ", treedata$Species, " is: ", TreeHeight(treedata$Angle.degrees, treedata$Distance.m)))
    treehgt = TreeHeight(treedata$Angle.degrees, treedata$Distance.m)
    return(treehgt)
} 

## add the column of height to the data
treedata$Tree.Height.m <- treehgt

## select the first two rows of data
height_selected <- treedata[1:2,]

## saving the output file
write.csv(height_selected, "../results/TreeHts.csv")