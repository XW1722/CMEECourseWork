TreeHeight <- function(degrees, distance) {
    radians <- degrees * pi / 180
    height <- distance * tan(radians)
    print(paste("Tree height is: ", height))

    return(height)
}

TreeHeight(37, 40)