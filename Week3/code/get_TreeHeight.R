#!/usr/bin/env Rscript

# loading the packages
library(tools)

# loading data from the command line
argv <- commandArgs(trailingOnly = TRUE)
treedata <- read.csv(paste("../data/", argv[1], sep = ""))

TreeHeight <- function(degrees, distance) {

    radians <- degrees * pi / 180
    height <- distance * tan(radians)

    return(height)
}

for (Species in treedata) {
    treehgt <- TreeHeight(treedata$Angle.degrees, treedata$Distance.m)
    return(treehgt)
} 

## add the column of height to the data
treedata$Tree.Height.m <- treehgt

## select the first two rows of data
height_selected <- treedata[1:2,]
height_selected$Input_Name <- argv[1]

## saving the output file
output <- basename(file_path_sans_ext(argv[1]))
write.csv(height_selected, file = paste("../results/", output, "_treeheights.csv", sep = ""))