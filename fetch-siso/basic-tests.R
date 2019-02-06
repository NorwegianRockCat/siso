# We are dependent on the pysch package
library(psych)
library(dplyr)

readFetchSisoData <- function() {
    results <- read.csv("results.csv", header = TRUE)
    rename(results,  Experience.Robots = Experience.with.Robots.Unedited)
}

# Return a list of psych alpha results, one for each movement thing
listOfAlphasForSisoData <- function(resultsDataFrame, iteration = 1) {

    # Pull the frames out, put calculate the index
    anthroIndex <- 6 + (iteration - 1) * 26

    # The numbers on the side are calculated based on the first iteration
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
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety)
}

filter_movement_for_instance <- function(resultsDataFrame, instance, movementType = 'Siso') {
    quo_instance_var <- enquo(instance)
    filter(resultsDataFrame, !!quo_instance_var == movementType)
}

is_movement <- function(x) { grepl("Movement.", x, fixed = TRUE) }

summary_averages_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    GSAnthro1 = quo(GSAnthro1.1)
    GSAnthro2 = quo(GSAnthro1.2)
    GSAnthro3 = quo(GSAnthro1.3)
    GSAnthro4 = quo(GSAnthro1.4)
    GSAnthro5 = quo(GSAnthro1.5)

    movement_list <- Filter(f = is_movement, Map(f = quo_name, group_var))
    
    if (length(movement_list) > 0) {
        movement <- movement_list[[1]]
        if (movement == "Movement.2") {
            GSAnthro1 = quo(GSAnthro2.1)
            GSAnthro2 = quo(GSAnthro2.2)
            GSAnthro3 = quo(GSAnthro2.3)
            GSAnthro4 = quo(GSAnthro2.4)
            GSAnthro5 = quo(GSAnthro2.5)
        } else if (movement == "Movement.3") {
            GSAnthro1 = quo(GSAnthro3.1)
            GSAnthro2 = quo(GSAnthro3.2)
            GSAnthro3 = quo(GSAnthro3.3)
            GSAnthro4 = quo(GSAnthro3.4)
            GSAnthro5 = quo(GSAnthro3.5)
        } else if (movement == "Movement.2") {
            GSAnthro1 = quo(GSAnthro4.1)
            GSAnthro2 = quo(GSAnthro4.2)
            GSAnthro3 = quo(GSAnthro4.3)
            GSAnthro4 = quo(GSAnthro4.4)
            GSAnthro5 = quo(GSAnthro4.5)
        }
    }
    
    resultsDataFrame %>%
        group_by(!!!group_var) %>%
        summarize(
            count = n(),
            anthro1 = mean(!!GSAnthro1, na.rm = TRUE),
            anthro2 = mean(!!GSAnthro2, na.rm = TRUE),
            anthro3 = mean(!!GSAnthro3, na.rm = TRUE),
            anthro4 = mean(!!GSAnthro4, na.rm = TRUE),
            anthro5 = mean(!!GSAnthro5, na.rm = TRUE),
            pm = mean(PM1.1, na.rm = TRUE),
            animacy1 = mean(GSAnimacy1.1, na.rm = TRUE),
            animacy2 = mean(GSAnimacy1.2, na.rm = TRUE),
            animacy3 = mean(GSAnimacy1.3, na.rm = TRUE),
            animacy4 = mean(GSAnimacy1.4, na.rm = TRUE),
            animacy5 = mean(GSAnimacy1.5, na.rm = TRUE),
            like1 = mean(GSL1.1, na.rm = TRUE),
            like2 = mean(GSL1.2, na.rm = TRUE),
            like3 = mean(GSL1.3, na.rm = TRUE),
            like4 = mean(GSL1.4, na.rm = TRUE),
            like5 = mean(GSL1.5, na.rm = TRUE),
            intel1 = mean(GSI1.1, na.rm = TRUE),
            intel2 = mean(GSI1.2, na.rm = TRUE),
            intel3 = mean(GSI1.3, na.rm = TRUE),
            intel4 = mean(GSI1.4, na.rm = TRUE),
            intel5 = mean(GSI1.5, na.rm = TRUE),
            intel6 = mean(GSI1.6, na.rm = TRUE),
            safe1 = mean(GSS1.1, na.rm = TRUE),
            safe2 = mean(GSS1.2, na.rm = TRUE),
            safe3 = mean(GSS1.3, na.rm = TRUE))
}
