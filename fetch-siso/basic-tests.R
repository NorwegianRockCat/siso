# We are dependent on the pysch package
library(psych)

readFetchSisoData <- function() {
    results <- read.csv("results.csv", header = TRUE)
    results
}

# Return a list of psych alpha results, one for each movement thing
listOfAlphasForSisoData <- function(resultsDataFrame, iteration = 1) {

    # Pull the frames out, put calculate the index
    anthroIndex <- 6
    if (iteration == 2)
        anthroIndex <- 6 + 26
    else if (iteration == 3)
        anthroIndex <- 6 + 26 + 26
    else if (iteration == 4)
        anthroIndex <- 6 + 26 + 26 + 26

    anthroEndIndex <- anthroIndex + 4 # 10

    animacyIndex <- anthroEndIndex + 2 # 12
    animacyEndIndex <- animacyIndex + 4 # 16

    likeabilityIndex <- animacyEndIndex + 1 # 17
    likeabilityEndIndex <- likeabilityIndex + 4 # 21

    intelligenceIndex <- likeabilityEndIndex + 1 # 22
    intelligenceEndIndex <- intelligenceIndex + 5 # 27

    safetyIndex <- intelligenceEndIndex + 1 # 28
    safetyEndIndex <- safetyIndex + 2 # 30

    
    anthroFrame <- resultsDataFrame[anthroIndex:anthroEndIndex]
    animacyFrame <- resultsDataFrame[animacyIndex:animacyEndIndex]
    likeabilityFrame <- resultsDataFrame[likeabilityIndex:likeabilityEndIndex]
    intelligenceFrame <- resultsDataFrame[intelligenceIndex:intelligenceEndIndex]
    safetyFrame <- resultsDataFrame[safetyIndex:safetyEndIndex]

    alphaAnthro <- alpha(anthroFrame)
    alphaAnimacy <- alpha(animacyFrame)
    alphaLikeability <- alpha(likeabilityFrame)
    alphaInt <- alpha(intelligenceFrame)
    alphaSafety <- alpha(safetyFrame, keys = c(paste("GSS", iteration, ".1", sep = '')))
    listOfAlphas <- list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety)
    listOfAlphas
}
