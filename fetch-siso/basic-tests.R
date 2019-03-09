require(psych)
require(dplyr)

rawFetchSisoResults <- function() {
    results <- read.csv("results.csv", header = TRUE)
    rename(results,  Experience.Robots = Experience.with.Robots.Unedited)
}

MovementVariableNames <- function() {
    tws.AverageVariableNames <- list(
        Siso.GSAnthro1 = quo(Siso.GSAnthro1),
        Siso.GSAnthro2 = quo(Siso.GSAnthro2),
        Siso.GSAnthro3 = quo(Siso.GSAnthro3),
        Siso.GSAnthro4 = quo(Siso.GSAnthro4),
        Siso.GSAnthro5 = quo(Siso.GSAnthro5),
        Siso.PM1 = quo(Siso.PM1),
        Siso.GSAnimacy1 = quo(Siso.GSAnimacy1),
        Siso.GSAnimacy2 = quo(Siso.GSAnimacy2),
        Siso.GSAnimacy3 = quo(Siso.GSAnimacy3),
        Siso.GSAnimacy4 = quo(Siso.GSAnimacy4),
        Siso.GSAnimacy5 = quo(Siso.GSAnimacy5),
        Siso.GSL1 = quo(Siso.GSL1),
        Siso.GSL2 = quo(Siso.GSL2),
        Siso.GSL3 = quo(Siso.GSL3),
        Siso.GSL4 = quo(Siso.GSL4),
        Siso.GSL5 = quo(Siso.GSL5),
        Siso.GSI1 = quo(Siso.GSI1),
        Siso.GSI2 = quo(Siso.GSI2),
        Siso.GSI3 = quo(Siso.GSI3),
        Siso.GSI4 = quo(Siso.GSI4),
        Siso.GSI5 = quo(Siso.GSI5),
        Siso.GSI6 = quo(Siso.GSI6),
        Siso.GSS1 = quo(Siso.GSS1),
        Siso.GSS2 = quo(Siso.GSS2),
        Siso.GSS3 = quo(Siso.GSS3),
        Linear.GSAnthro1 = quo(Linear.GSAnthro1),
        Linear.GSAnthro2 = quo(Linear.GSAnthro2),
        Linear.GSAnthro3 = quo(Linear.GSAnthro3),
        Linear.GSAnthro4 = quo(Linear.GSAnthro4),
        Linear.GSAnthro5 = quo(Linear.GSAnthro5),
        Linear.PM1 = quo(Linear.PM1),
        Linear.GSAnimacy1 = quo(Linear.GSAnimacy1),
        Linear.GSAnimacy2 = quo(Linear.GSAnimacy2),
        Linear.GSAnimacy3 = quo(Linear.GSAnimacy3),
        Linear.GSAnimacy4 = quo(Linear.GSAnimacy4),
        Linear.GSAnimacy5 = quo(Linear.GSAnimacy5),
        Linear.GSL1 = quo(Linear.GSL1),
        Linear.GSL2 = quo(Linear.GSL2),
        Linear.GSL3 = quo(Linear.GSL3),
        Linear.GSL4 = quo(Linear.GSL4),
        Linear.GSL5 = quo(Linear.GSL5),
        Linear.GSI1 = quo(Linear.GSI1),
        Linear.GSI2 = quo(Linear.GSI2),
        Linear.GSI3 = quo(Linear.GSI3),
        Linear.GSI4 = quo(Linear.GSI4),
        Linear.GSI5 = quo(Linear.GSI5),
        Linear.GSI6 = quo(Linear.GSI6),
        Linear.GSS1 = quo(Linear.GSS1),
        Linear.GSS2 = quo(Linear.GSS2),
        Linear.GSS3 = quo(Linear.GSS3))

    tws.Movement0VariableNames <- list(
        GSAnthro1 = quo(GSAnthro1),
        GSAnthro2 = quo(GSAnthro2),
        GSAnthro3 = quo(GSAnthro3),
        GSAnthro4 = quo(GSAnthro4),
        GSAnthro5 = quo(GSAnthro5),

        GSAnimacy1 = quo(GSAnimacy1),
        GSAnimacy2 = quo(GSAnimacy2),
        GSAnimacy3 = quo(GSAnimacy3),
        GSAnimacy4 = quo(GSAnimacy4),
        GSAnimacy5 = quo(GSAnimacy5),

        GSL1 = quo(GSL1),
        GSL2 = quo(GSL2),
        GSL3 = quo(GSL3),
        GSL4 = quo(GSL4),
        GSL5 = quo(GSL5),

        GSI1 = quo(GSI1),
        GSI2 = quo(GSI2),
        GSI3 = quo(GSI3),
        GSI4 = quo(GSI4),
        GSI5 = quo(GSI5),
        GSI6 = quo(GSI6),

        PM1 = quo(PM1),

        GSS1 = quo(GSS1),
        GSS2 = quo(GSS2),
        GSS3 = quo(GSS3))

    tws.Movement1VariableNames <- list(
        GSAnthro1 = quo(GSAnthro1.1),
        GSAnthro2 = quo(GSAnthro1.2),
        GSAnthro3 = quo(GSAnthro1.3),
        GSAnthro4 = quo(GSAnthro1.4),
        GSAnthro5 = quo(GSAnthro1.5),

        GSAnimacy1 = quo(GSAnimacy1.1),
        GSAnimacy2 = quo(GSAnimacy1.2),
        GSAnimacy3 = quo(GSAnimacy1.3),
        GSAnimacy4 = quo(GSAnimacy1.4),
        GSAnimacy5 = quo(GSAnimacy1.5),

        GSL1 = quo(GSL1.1),
        GSL2 = quo(GSL1.2),
        GSL3 = quo(GSL1.3),
        GSL4 = quo(GSL1.4),
        GSL5 = quo(GSL1.5),

        GSI1 = quo(GSI1.1),
        GSI2 = quo(GSI1.2),
        GSI3 = quo(GSI1.3),
        GSI4 = quo(GSI1.4),
        GSI5 = quo(GSI1.5),
        GSI6 = quo(GSI1.6),

        PM1 = quo(PM1.1),

        GSS1 = quo(GSS1.1),
        GSS2 = quo(GSS1.2),
        GSS3 = quo(GSS1.3))

    tws.Movement2VariableNames <- list(
        GSAnthro1 = quo(GSAnthro2.1),
        GSAnthro2 = quo(GSAnthro2.2),
        GSAnthro3 = quo(GSAnthro2.3),
        GSAnthro4 = quo(GSAnthro2.4),
        GSAnthro5 = quo(GSAnthro2.5),

        GSAnimacy1 = quo(GSAnimacy2.1),
        GSAnimacy2 = quo(GSAnimacy2.2),
        GSAnimacy3 = quo(GSAnimacy2.3),
        GSAnimacy4 = quo(GSAnimacy2.4),
        GSAnimacy5 = quo(GSAnimacy2.5),

        GSL1 = quo(GSL2.1),
        GSL2 = quo(GSL2.2),
        GSL3 = quo(GSL2.3),
        GSL4 = quo(GSL2.4),
        GSL5 = quo(GSL2.5),

        GSI1 = quo(GSI2.1),
        GSI2 = quo(GSI2.2),
        GSI3 = quo(GSI2.3),
        GSI4 = quo(GSI2.4),
        GSI5 = quo(GSI2.5),
        GSI6 = quo(GSI2.6),

        PM1 = quo(PM2.1),

        GSS1 = quo(GSS2.1),
        GSS2 = quo(GSS2.2),
        GSS3 = quo(GSS2.3))

    tws.Movement3VariableNames <- list(
        GSAnthro1 = quo(GSAnthro3.1),
        GSAnthro2 = quo(GSAnthro3.2),
        GSAnthro3 = quo(GSAnthro3.3),
        GSAnthro4 = quo(GSAnthro3.4),
        GSAnthro5 = quo(GSAnthro3.5),

        GSAnimacy1 = quo(GSAnimacy3.1),
        GSAnimacy2 = quo(GSAnimacy3.2),
        GSAnimacy3 = quo(GSAnimacy3.3),
        GSAnimacy4 = quo(GSAnimacy3.4),
        GSAnimacy5 = quo(GSAnimacy3.5),

        GSL1 = quo(GSL3.1),
        GSL2 = quo(GSL3.2),
        GSL3 = quo(GSL3.3),
        GSL4 = quo(GSL3.4),
        GSL5 = quo(GSL3.5),

        GSI1 = quo(GSI3.1),
        GSI2 = quo(GSI3.2),
        GSI3 = quo(GSI3.3),
        GSI4 = quo(GSI3.4),
        GSI5 = quo(GSI3.5),
        GSI6 = quo(GSI3.6),

        PM1 = quo(PM3.1),

        GSS1 = quo(GSS3.1),
        GSS2 = quo(GSS3.2),
        GSS3 = quo(GSS3.3))

    tws.Movement4VariableNames <- list(
        GSAnthro1 = quo(GSAnthro4.1),
        GSAnthro2 = quo(GSAnthro4.2),
        GSAnthro3 = quo(GSAnthro4.3),
        GSAnthro4 = quo(GSAnthro4.4),
        GSAnthro5 = quo(GSAnthro4.5),

        GSAnimacy1 = quo(GSAnimacy4.1),
        GSAnimacy2 = quo(GSAnimacy4.2),
        GSAnimacy3 = quo(GSAnimacy4.3),
        GSAnimacy4 = quo(GSAnimacy4.4),
        GSAnimacy5 = quo(GSAnimacy4.5),

        GSL1 = quo(GSL4.1),
        GSL2 = quo(GSL4.2),
        GSL3 = quo(GSL4.3),
        GSL4 = quo(GSL4.4),
        GSL5 = quo(GSL4.5),

        GSI1 = quo(GSI4.1),
        GSI2 = quo(GSI4.2),
        GSI3 = quo(GSI4.3),
        GSI4 = quo(GSI4.4),
        GSI5 = quo(GSI4.5),
        GSI6 = quo(GSI4.6),

        PM1 = quo(PM4.1),

        GSS1 = quo(GSS4.1),
        GSS2 = quo(GSS4.2),
        GSS3 = quo(GSS4.3))
    list(movement.1 = tws.Movement1VariableNames, movement.2 = tws.Movement2VariableNames, movement.3 = tws.Movement3VariableNames, movement.4 = tws.Movement4VariableNames, movement.none = tws.Movement0VariableNames, movement.average = tws.AverageVariableNames)
}

# Return a list of psych alpha results, one for each movement thing
alphaOneEncounter <- function(resultsDataFrame = rawFetchSisoResults(), iteration = 1) {

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

    alphaAnthro <- pysch::alpha(anthroFrame)
    alphaAnimacy <- pysch::alpha(animacyFrame)
    alphaLikeability <- pysch::alpha(likeabilityFrame)
    alphaInt <- pysch::alpha(intelligenceFrame)
    alphaSafety <- pysch::alpha(safetyFrame, keys = c(paste("GSS", iteration, ".1", sep = '')))
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety)
}

alphaAllEncounters <- function(all.encounters.df = allEncounters()) {
    anthroFrame <- all.encounters.df[2:6] # GSAnthro1:GSAnthro6
    animacyFrame <- all.encounters.df[7:11] # GSAnimacy1:GSAnimacy5
    likeabilityFrame <- all.encounters.df[12:16] # GSL1:GSL5
    intelligenceFrame <- all.encounters.df[17:22] # GSI1:GSI6
    safetyFrame <- all.encounters.df[24:26] # GSS1:GSS3
    safetyFrame.plus.prediction <- all.encounters.df[23:26] # PM1:GSS3

    alphaAnthro <- psych::alpha(anthroFrame)
    alphaAnimacy <- psych::alpha(animacyFrame)
    alphaLikeability <- psych::alpha(likeabilityFrame)
    alphaInt <- psych::alpha(intelligenceFrame)
    alphaSafety <- psych::alpha(safetyFrame, keys = c("GSS1"))
    alphaSafety.plus.prediction <- psych::alpha(safetyFrame.plus.prediction, keys = c("GSS1", "PM1"))
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety, safetyPlus=alphaSafety.plus.prediction)
}

filter_movement_for_instance <- function(resultsDataFrame, instance, movementType = 'Siso') {
    quo_instance_var <- enquo(instance)
    dplyr::filter(resultsDataFrame, !!quo_instance_var == movementType)
}

tws.is_movement <- function(x) { grepl("Movement.", x, fixed = TRUE) }

tws.find_variable_names_for_movement <- function(group_var) {
    movement_list <- Filter(f = tws.is_movement, Map(f = quo_name, group_var))
    variable_names = MovementVariableNames()

    if (length(movement_list) > 0) {
         variable_names[[tolower(movement_list[[1]])]]
    } else {
         variable_names[[5]]
    }
}

summary_averages_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    variable_list <- tws.find_variable_names_for_movement(group_var)
    
    resultsDataFrame %>%
        dplyr::group_by(!!!group_var) %>%
        tws.summary(mean, variable_list)
}

summary_medians_for_movement <- function(resultsDataFrame, ...) {
    # All the variables are hard coded for the moment, but this will change.
    group_var <- quos(...)

    movement_list <- Filter(f = is_movement, Map(f = quo_name, group_var))

    variable_list <- tws.find_variable_names_for_movement(group_var)

    resultsDataFrame %>%
        group_by(!!!group_var) %>%
        tws.summary(median, variable_list)
}

tws.summary <- function(resultsDataFrame, func, variable_list) {
    resultsDataFrame %>%
        dpylr::summarize(
            count = n(),
            anthro1 = func(!!variable_list$GSAnthro1, na.rm = TRUE),
            anthro2 = func(!!variable_list$GSAnthro2, na.rm = TRUE),
            anthro3 = func(!!variable_list$GSAnthro3, na.rm = TRUE),
            anthro4 = func(!!variable_list$GSAnthro4, na.rm = TRUE),
            anthro5 = func(!!variable_list$GSAnthro5, na.rm = TRUE),
            pm = func(!!variable_list$PM1, na.rm = TRUE),
            animacy1 = func(!!variable_list$GSAnimacy1, na.rm = TRUE),
            animacy2 = func(!!variable_list$GSAnimacy2, na.rm = TRUE),
            animacy3 = func(!!variable_list$GSAnimacy3, na.rm = TRUE),
            animacy4 = func(!!variable_list$GSAnimacy4, na.rm = TRUE),
            animacy5 = func(!!variable_list$GSAnimacy5, na.rm = TRUE),
            like1 = func(!!variable_list$GSL1, na.rm = TRUE),
            like2 = func(!!variable_list$GSL2, na.rm = TRUE),
            like3 = func(!!variable_list$GSL3, na.rm = TRUE),
            like4 = func(!!variable_list$GSL4, na.rm = TRUE),
            like5 = func(!!variable_list$GSL5, na.rm = TRUE),
            intel1 = func(!!variable_list$GSI1, na.rm = TRUE),
            intel2 = func(!!variable_list$GSI2, na.rm = TRUE),
            intel3 = func(!!variable_list$GSI3, na.rm = TRUE),
            intel4 = func(!!variable_list$GSI4, na.rm = TRUE),
            intel5 = func(!!variable_list$GSI5, na.rm = TRUE),
            intel6 = func(!!variable_list$GSI6, na.rm = TRUE),
            safe1 = func(!!variable_list$GSS1, na.rm = TRUE),
            safe2 = func(!!variable_list$GSS2, na.rm = TRUE),
            safe3 = func(!!variable_list$GSS3, na.rm = TRUE))
}

# Select each variables and put them together.
allEncounters <- function(resultsDataFrame = rawFetchSisoResults()) {
    variable.names <- MovementVariableNames()
    move1 <- dplyr::select(resultsDataFrame, movement = Movement.1, !!!variable.names$movement.1)
    move2 <- dplyr::select(resultsDataFrame, movement = Movement.2, !!!variable.names$movement.2)
    move3 <- dplyr::select(resultsDataFrame, movement = Movement.3, !!!variable.names$movement.3)
    move4 <- dplyr::select(resultsDataFrame, movement = Movement.4, !!!variable.names$movement.4)

    bind_rows(move1, move2, move3, move4)
}

allSisoEncounters <- function(df = allEncounters()) {
    df %>% dplyr::filter(movement == "Siso")
}

allLinearEncounters <- function(df = allEncounters()) {
    df %>% dplyr::filter(movement == "Linear")
}

siso.and.linear.godspeed.compenent.averages <- function(df = rawFetchSisoResults()) {
    df %>% dplyr::mutate(Siso.GSAnthro1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.1, GSAnthro4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.2, GSAnthro4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.3, GSAnthro4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.4, GSAnthro4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnthro5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnthro3.5, GSAnthro4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.PM1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(PM1.1, PM2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(PM1.1, PM3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM1.1, PM4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(PM2.1, PM3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM2.1, PM4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(PM3.1, PM4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.1, GSAnimacy4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.2, GSAnimacy4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.3, GSAnimacy4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.4, GSAnimacy4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSAnimacy5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSAnimacy3.5, GSAnimacy4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.1, GSL4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.1, GSL3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.1, GSL4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.1, GSL4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.4, GSL4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.4, GSL3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.4, GSL4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.4, GSL4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSL5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL1.5, GSL4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSL2.5, GSL3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL2.5, GSL4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSL3.5, GSL4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.1, GSI4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.1, GSI3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.1, GSI4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.1, GSI4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.2, GSI4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.2, GSI3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.2, GSI4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.2, GSI4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.3, GSI4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.3, GSI3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.3, GSI4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.3, GSI4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI4 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI2.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI3.4), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.4, GSI4.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.4, GSI3.4), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.4, GSI4.4), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.4, GSI4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI5 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI2.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI3.5), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.5, GSI4.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.5, GSI3.5), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.5, GSI4.5), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.5, GSI4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSI6 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI2.6), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI3.6), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI1.6, GSI4.6), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSI2.6, GSI3.6), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI2.6, GSI4.6), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSI3.6, GSI4.6), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS1 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS2.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS3.1), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.1, GSS4.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.1, GSS3.1), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.1, GSS4.1), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.1, GSS4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS2 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS2.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS3.2), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.2, GSS4.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.2, GSS3.2), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.2, GSS4.2), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.2, GSS4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Siso.GSS3 =
                             case_when(Movement.1 == "Siso" & Movement.2 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS2.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS3.3), na.rm = TRUE),
                                       Movement.1 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS1.3, GSS4.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.3 == "Siso" ~ rowMeans(data.frame(GSS2.3, GSS3.3), na.rm = TRUE),
                                       Movement.2 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS2.3, GSS4.3), na.rm = TRUE),
                                       Movement.3 == "Siso" & Movement.4 == "Siso" ~ rowMeans(data.frame(GSS3.3, GSS4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.1, GSAnthro4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.1, GSAnthro4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.2, GSAnthro4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.2, GSAnthro4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.3, GSAnthro4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.3, GSAnthro4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.4, GSAnthro4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.4, GSAnthro4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnthro5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro1.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro2.5, GSAnthro4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnthro3.5, GSAnthro4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.PM1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(PM1.1, PM2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(PM1.1, PM3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM1.1, PM4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(PM2.1, PM3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM2.1, PM4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(PM3.1, PM4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.1, GSAnimacy4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.1, GSAnimacy4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.2, GSAnimacy4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.2, GSAnimacy4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.3, GSAnimacy4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.3, GSAnimacy4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.4, GSAnimacy4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.4, GSAnimacy4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSAnimacy5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy1.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy2.5, GSAnimacy4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSAnimacy3.5, GSAnimacy4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.1, GSL4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.1, GSL3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.1, GSL4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.1, GSL4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.2, GSL4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.2, GSL4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.2, GSL4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.4, GSL4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.4, GSL3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.4, GSL4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.4, GSL4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSL5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL1.5, GSL4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSL2.5, GSL3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL2.5, GSL4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSL3.5, GSL4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.1, GSI4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.1, GSI3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.1, GSI4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.1, GSI4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.2, GSI4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.2, GSI3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.2, GSI4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.2, GSI4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.3, GSI4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.3, GSI3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.3, GSI4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.3, GSI4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI4 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI2.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI3.4), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.4, GSI4.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.4, GSI3.4), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.4, GSI4.4), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.4, GSI4.4), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI5 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI2.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI3.5), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.5, GSI4.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.5, GSI3.5), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.5, GSI4.5), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.5, GSI4.5), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSI6 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI2.6), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI3.6), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI1.6, GSI4.6), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSI2.6, GSI3.6), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI2.6, GSI4.6), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSI3.6, GSI4.6), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS1 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS2.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS3.1), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.1, GSS4.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.1, GSS3.1), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.1, GSS4.1), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.1, GSS4.1), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS2 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS2.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS3.2), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.2, GSS4.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.2, GSS3.2), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.2, GSS4.2), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.2, GSS4.2), na.rm = TRUE),
                                       TRUE ~ NA_real_),
                         Linear.GSS3 =
                             case_when(Movement.1 == "Linear" & Movement.2 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS2.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS3.3), na.rm = TRUE),
                                       Movement.1 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS1.3, GSS4.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.3 == "Linear" ~ rowMeans(data.frame(GSS2.3, GSS3.3), na.rm = TRUE),
                                       Movement.2 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS2.3, GSS4.3), na.rm = TRUE),
                                       Movement.3 == "Linear" & Movement.4 == "Linear" ~ rowMeans(data.frame(GSS3.3, GSS4.3), na.rm = TRUE),
                                       TRUE ~ NA_real_)
                         ) %>% dplyr::select(ID, Age, Gender, Experience.Robots, !!!MovementVariableNames()$movement.average)
}

godspeed.nonparamTestByName <- function(name, df, func = c(t.test, wilcox.test), alternative = c("two.sided", "less", "greater")) {
    var.name <- quo_name(enquo(name))
    result <- func(df[[paste("Siso.", var.name, sep = '')]], df[[paste("Linear.", var.name, sep = '')]], paired = TRUE, alternative)
    # Build a dataframe out of this.
    data.frame(Name = var.name,
               method = result$method,
               alternative = result$alternative,
               null.value = result$null.value,
               statistic = result$statistic,
               pvalue = round(result$p.value, digits = 4))
}

godspeed.wilcox.tests.for.components <- function(df = siso.and.linear.godspeed.compenent.averages()) {
    movement.0.variable_names <- MovementVariableNames()$movement.none
    twosided <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "two.sided")
    greater <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "greater")
    less <- lapply(movement.0.variable_names, godspeed.nonparamTestByName, df = df, func = wilcox.test, alternative = "less")
    list(two.sided = twosided, greater = greater, less = less)
}

shapiro.for.name <- function(name, df) {
    var.name <- quo_name(enquo(name))
    shapiro.test(df[[var.name]])
}

                                        # results.shapiro(siso.and.linear.godspeed.compenent.averages, MovementVariableNames()$movement.none)
                                        # results.shapiro(rawFetchSisoResults, unlist(MovementVariableNames()[1:4]))

results.shapiro <- function(df, variable.names) {
    lapply(variable.names, shapiro.for.name, df)
}

godspeed.average.for.series <- function(df = siso.and.linear.godspeed.compenent.averages()) {
    df %>% dplyr::mutate(Siso.GSAnthro.avg =
                             rowMeans(data.frame(Siso.GSAnthro1, Siso.GSAnthro2, Siso.GSAnthro3,
                                                 Siso.GSAnthro4, Siso.GSAnthro5), na.rm = TRUE),
                         Siso.GSAnimacy.avg = 
                             rowMeans(data.frame(Siso.GSAnimacy1, Siso.GSAnimacy2, Siso.GSAnimacy3,
                                                 Siso.GSAnimacy4, Siso.GSAnimacy5), na.rm = TRUE),
                         Siso.GSL.avg = 
                             rowMeans(data.frame(Siso.GSL1, Siso.GSL2, Siso.GSL3,
                                                 Siso.GSL4, Siso.GSL5), na.rm = TRUE),
                         Siso.GSI.avg = 
                             rowMeans(data.frame(Siso.GSI1, Siso.GSI2, Siso.GSI3,
                                                 Siso.GSI4, Siso.GSI5, Siso.GSI6), na.rm = TRUE),
                         Siso.GSS.avg = 
                             rowMeans(data.frame(Siso.GSS1, Siso.GSS2, Siso.GSS3), na.rm = TRUE),
                         Linear.GSAnthro.avg =
                             rowMeans(data.frame(Linear.GSAnthro1, Linear.GSAnthro2, Linear.GSAnthro3,
                                                 Linear.GSAnthro4, Linear.GSAnthro5), na.rm = TRUE),
                         Linear.GSAnimacy.avg = 
                             rowMeans(data.frame(Linear.GSAnimacy1, Linear.GSAnimacy2, Linear.GSAnimacy3,
                                                 Linear.GSAnimacy4, Linear.GSAnimacy5), na.rm = TRUE),
                         Linear.GSL.avg = 
                             rowMeans(data.frame(Linear.GSL1, Linear.GSL2, Linear.GSL3,
                                                 Linear.GSL4, Linear.GSL5), na.rm = TRUE),
                         Linear.GSI.avg = 
                             rowMeans(data.frame(Linear.GSI1, Linear.GSI2, Linear.GSI3,
                                                 Linear.GSI4, Linear.GSI5, Linear.GSI6), na.rm = TRUE),
                         Linear.GSS.avg = 
                             rowMeans(data.frame(Linear.GSS1, Linear.GSS2, Linear.GSS3), na.rm = TRUE))
}
