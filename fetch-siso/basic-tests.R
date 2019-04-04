require(stats)
require(extrafont)
require(psych)
require(tidyverse)

tidyFetchSisoResults <- function() {
    movement <- c("Siso", "Linear")
    results <- read_csv("results.csv", col_types = cols(
                                           ID = col_integer(),
                                           Age = col_integer(),
                                           Gender = col_factor(levels = c("M", "F")),
                                           Experience.Robots = col_factor(levels = c("Yes", "No")),

                                           Movement.1 = col_factor(movement),
                                           Movement.2 = col_factor(movement),
                                           Movement.3 = col_factor(movement),
                                           Movement.4 = col_factor(movement),

                                           GSAnthro1.1 = col_integer(),
                                           GSAnthro1.2 = col_integer(),
                                           GSAnthro1.3 = col_integer(),
                                           GSAnthro1.4 = col_integer(),
                                           GSAnthro1.5 = col_integer(),
                                           PM1.1 = col_integer(),
                                           GSAnimacy1.1 = col_integer(),
                                           GSAnimacy1.2 = col_integer(),
                                           GSAnimacy1.3 = col_integer(),
                                           GSAnimacy1.4 = col_integer(),
                                           GSAnimacy1.5 = col_integer(),
                                           GSL1.1 = col_integer(),
                                           GSL1.2 = col_integer(),
                                           GSL1.3 = col_integer(),
                                           GSL1.4 = col_integer(),
                                           GSL1.5 = col_integer(),
                                           GSI1.1 = col_integer(),
                                           GSI1.2 = col_integer(),
                                           GSI1.3 = col_integer(),
                                           GSI1.4 = col_integer(),
                                           GSI1.5 = col_integer(),
                                           GSI1.6 = col_integer(),
                                           GSS1.1 = col_integer(),
                                           GSS1.2 = col_integer(),
                                           GSS1.3 = col_integer(),

                                           GSAnthro2.1 = col_integer(),
                                           GSAnthro2.2 = col_integer(),
                                           GSAnthro2.3 = col_integer(),
                                           GSAnthro2.4 = col_integer(),
                                           GSAnthro2.5 = col_integer(),
                                           PM2.1 = col_integer(),
                                           GSAnimacy2.1 = col_integer(),
                                           GSAnimacy2.2 = col_integer(),
                                           GSAnimacy2.3 = col_integer(),
                                           GSAnimacy2.4 = col_integer(),
                                           GSAnimacy2.5 = col_integer(),
                                           GSL2.1 = col_integer(),
                                           GSL2.2 = col_integer(),
                                           GSL2.3 = col_integer(),
                                           GSL2.4 = col_integer(),
                                           GSL2.5 = col_integer(),
                                           GSI2.1 = col_integer(),
                                           GSI2.2 = col_integer(),
                                           GSI2.3 = col_integer(),
                                           GSI2.4 = col_integer(),
                                           GSI2.5 = col_integer(),
                                           GSI2.6 = col_integer(),
                                           GSS2.1 = col_integer(),
                                           GSS2.2 = col_integer(),
                                           GSS2.3 = col_integer(),

                                           GSAnthro3.1 = col_integer(),
                                           GSAnthro3.2 = col_integer(),
                                           GSAnthro3.3 = col_integer(),
                                           GSAnthro3.4 = col_integer(),
                                           GSAnthro3.5 = col_integer(),
                                           PM3.1 = col_integer(),
                                           GSAnimacy3.1 = col_integer(),
                                           GSAnimacy3.2 = col_integer(),
                                           GSAnimacy3.3 = col_integer(),
                                           GSAnimacy3.4 = col_integer(),
                                           GSAnimacy3.5 = col_integer(),
                                           GSL3.1 = col_integer(),
                                           GSL3.2 = col_integer(),
                                           GSL3.3 = col_integer(),
                                           GSL3.4 = col_integer(),
                                           GSL3.5 = col_integer(),
                                           GSI3.1 = col_integer(),
                                           GSI3.2 = col_integer(),
                                           GSI3.3 = col_integer(),
                                           GSI3.4 = col_integer(),
                                           GSI3.5 = col_integer(),
                                           GSI3.6 = col_integer(),
                                           GSS3.1 = col_integer(),
                                           GSS3.2 = col_integer(),
                                           GSS3.3 = col_integer(),

                                           GSAnthro4.1 = col_integer(),
                                           GSAnthro4.2 = col_integer(),
                                           GSAnthro4.3 = col_integer(),
                                           GSAnthro4.4 = col_integer(),
                                           GSAnthro4.5 = col_integer(),
                                           PM4.1 = col_integer(),
                                           GSAnimacy4.1 = col_integer(),
                                           GSAnimacy4.2 = col_integer(),
                                           GSAnimacy4.3 = col_integer(),
                                           GSAnimacy4.4 = col_integer(),
                                           GSAnimacy4.5 = col_integer(),
                                           GSL4.1 = col_integer(),
                                           GSL4.2 = col_integer(),
                                           GSL4.3 = col_integer(),
                                           GSL4.4 = col_integer(),
                                           GSL4.5 = col_integer(),
                                           GSI4.1 = col_integer(),
                                           GSI4.2 = col_integer(),
                                           GSI4.3 = col_integer(),
                                           GSI4.4 = col_integer(),
                                           GSI4.5 = col_integer(),
                                           GSI4.6 = col_integer(),
                                           GSS4.1 = col_integer(),
                                           GSS4.2 = col_integer(),
                                           GSS4.3 = col_integer()))
                                        # Now gather the different movements together
    results <- stats::reshape(as.data.frame(results), idvar="ID", direction = "long",
                              varying = list(Movement = c(5,31,57,83),
                                             GSAnthro1 = c(6,32,58,84),
                                             GSAnthro2 = c(7,33,59,85),
                                             GSAnthro3 = c(8,34,60,86),
                                             GSAnthro4 = c(9,35,61,87),
                                             GSAnthro5 = c(10,36,62,88),
                                             PM1 = c(11,37,63,89),
                                             GSAnimacy1 = c(12,38,64,90),
                                             GSAnimacy2 = c(13,39,65,91),
                                             GSAnimacy3 = c(14,40,66,92),
                                             GSAnimacy4 = c(15,41,67,93),
                                             GSAnimacy5 = c(16,42,68,94),
                                             GSL1 = c(17,43,69,95),
                                             GSL2 = c(18,44,70,96),
                                             GSL3 = c(19,45,71,97),
                                             GSL4 = c(20,46,72,98),
                                             GSL5 = c(21,47,73,99),
                                             GSI1 = c(22,48,74,100),
                                             GSI2 = c(23,49,75,101),
                                             GSI3 = c(24,50,76,102),
                                             GSI4 = c(25,51,77,103),
                                             GSI5 = c(26,52,78,104),
                                             GSI6 = c(27,53,79,105),
                                             GSS1 = c(28,54,80,106),
                                             GSS2 = c(29,55,81,107),
                                             GSS3 = c(30,56,82,108)),
                              v.names = c("Movement",
                                          "GSAnthro1",
                                          "GSAnthro2",
                                          "GSAnthro3",
                                          "GSAnthro4",
                                          "GSAnthro5",
                                          "PM1",
                                          "GSAnimacy1",
                                          "GSAnimacy2",
                                          "GSAnimacy3",
                                          "GSAnimacy4",
                                          "GSAnimacy5",
                                          "GSL1",
                                          "GSL2",
                                          "GSL3",
                                          "GSL4",
                                          "GSL5",
                                          "GSI1",
                                          "GSI2",
                                          "GSI3",
                                          "GSI4",
                                          "GSI5",
                                          "GSI6",
                                          "GSS1",
                                          "GSS2",
                                          "GSS3"))
    results <- as_tibble(results)
    flippy <- function(x) abs(x - 6)
    results <- results %>% dplyr::mutate(GSS2.reversed = flippy(GSS2),
                                         GSS3.reversed = flippy(GSS3),
                                         time.ordered = as.ordered(time))
    results %>% dplyr::mutate(GSAnthro.avg = rowMeans(data.frame(GSAnthro1, GSAnthro2, GSAnthro3, GSAnthro4, GSAnthro5)),
                              GSAnimacy.avg = rowMeans(data.frame(GSAnimacy1, GSAnimacy2, GSAnimacy3, GSAnimacy4, GSAnimacy5)),
                              GSL.avg = rowMeans(data.frame(GSL1, GSL2, GSL3, GSL4, GSL5)),
                              GSI.avg = rowMeans(data.frame(GSI1, GSI2, GSI3, GSI4, GSI5, GSI6)),
                              GSS.avg = rowMeans(data.frame(GSS1, GSS2, GSS3)),
                              GSS.reversed.avg = rowMeans(data.frame(GSS1, GSS2.reversed, GSS3.reversed)))
}


tidy.variable.names <- function(df = tidyFetchSisoResults()) {
    # We make an assumption it's the tidyFetchSisoResults
    all.names <- names(df)
    component.names <- all.names[7:33]
    avg.names <- all.names[35:length(all.names)]
    return (c(component.names, avg.names))
}


# This is the old function that just loads items in.
rawFetchSisoResults <- function() {
    utils::read.csv("results.csv", header = TRUE)
}

alphaAllEncounters <- function(df = tidyFetchSisoResults()) {
    anthroFrame <- df %>% dplyr::select(GSAnthro1:GSAnthro5)
    animacyFrame <- df %>% dplyr::select(GSAnimacy1:GSAnimacy5)
    likeabilityFrame <- df %>% dplyr::select(GSL1:GSL5)
    intelligenceFrame <- df %>% dplyr::select(GSI1:GSI6)
    safetyFrame <- df %>% dplyr::select(GSS1:GSS3)
    safetyFrame.plus.prediction <- df %>% dplyr::select(PM1, GSS1:GSS3)

    alphaAnthro <- psych::alpha(anthroFrame)
    alphaAnimacy <- psych::alpha(animacyFrame)
    alphaLikeability <- psych::alpha(likeabilityFrame)
    alphaInt <- psych::alpha(intelligenceFrame)
    alphaSafety <- psych::alpha(safetyFrame, keys = c("GSS2", "GSS3"))
    alphaSafety.plus.prediction <- psych::alpha(safetyFrame.plus.prediction, keys = c("GSS2", "GSS3"))
    list(anthro=alphaAnthro, animacy=alphaAnimacy, likeability=alphaLikeability, int=alphaInt, safety=alphaSafety, safetyPlus=alphaSafety.plus.prediction)
}

siso.and.linear.godspeed.component.averages <- function(df = results.tidy) {
    df %>% dplyr::group_by(Movement, ID) %>%
        dplyr::summarize(
                   GSAnthro1.avg = dplyr::mean(GSAnthro1),
                   GSAnthro2.avg = dplyr::mean(GSAnthro2),
                   GSAnthro3.avg = dplyr::mean(GSAnthro3),
                   GSAnthro4.avg = dplyr::mean(GSAnthro4),
                   GSAnthro5.avg = dplyr::mean(GSAnthro5),
                   PM1.avg = dplyr::mean(PM1),
                   GSAnimacy1.avg = dplyr::mean(GSAnimacy1),
                   GSAnimacy2.avg = dplyr::mean(GSAnimacy2),
                   GSAnimacy3.avg = dplyr::mean(GSAnimacy3),
                   GSAnimacy4.avg = dplyr::mean(GSAnimacy4),
                   GSAnimacy5.avg = dplyr::mean(GSAnimacy5),
                   GSL1.avg = dplyr::mean(GSL1),
                   GSL2.avg = dplyr::mean(GSL2),
                   GSL3.avg = dplyr::mean(GSL3),
                   GSL4.avg = dplyr::mean(GSL4),
                   GSL5.avg = dplyr::mean(GSL5),
                   GSI1.avg = dplyr::mean(GSI1),
                   GSI2.avg = dplyr::mean(GSI2),
                   GSI3.avg = dplyr::mean(GSI3),
                   GSI4.avg = dplyr::mean(GSI4),
                   GSI5.avg = dplyr::mean(GSI5),
                   GSI6.avg = dplyr::mean(GSI6),
                   GSS1.avg = dplyr::mean(GSS1),
                   GSS2.avg = dplyr::mean(GSS2),
                   GSS2.reversed.avg = dplyr::mean(GSS2.reversed),
                   GSS3.avg = dplyr::mean(GSS3),
                   GSS3.reversed.avg = dplyr::mean(GSS3.reversed)) %>%
        dplyr::mutate(GSAnthro.avg = rowMeans(data.frame(GSAnthro1.avg, GSAnthro2.avg, GSAnthro3.avg, GSAnthro4.avg, GSAnthro5.avg)),
                      GSAnimacy.avg = rowMeans(data.frame(GSAnimacy1.avg, GSAnimacy2.avg, GSAnimacy3.avg, GSAnimacy4.avg, GSAnimacy5.avg)),
                      GSL.avg = rowMeans(data.frame(GSL1.avg, GSL2.avg, GSL3.avg, GSL4.avg, GSL5.avg)),
                      GSI.avg = rowMeans(data.frame(GSI1.avg, GSI2.avg, GSI3.avg, GSI4.avg, GSI5.avg, GSI6.avg)),
                      GSS.avg = rowMeans(data.frame(GSS1.avg, GSS2.avg, GSS3.avg)),
                      GSS.reversed.avg = rowMeans(data.frame(GSS1.avg, GSS2.reversed.avg, GSS3.reversed.avg)))
}

siso.and.linear.godspeed.component.averages.gathered <- function(df = siso.and.linear.godspeed.component.averages()) {
    df %>% gather(GSAnthro.avg, GSAnimacy.avg, GSL.avg, GSI.avg, GSS.reversed.avg, key=GS.avg, value = GS.avg.Value) %>%
        gather(GSAnthro1.avg, GSAnthro2.avg, GSAnthro3.avg, GSAnthro4.avg, GSAnthro5.avg, key=GSAnthro, value = GSAnthro.Value) %>%
        gather(GSAnimacy1.avg, GSAnimacy2.avg, GSAnimacy3.avg, GSAnimacy4.avg, GSAnimacy5.avg, key=GSAnimacy, value = GSAnimacy.Value) %>%
        gather(GSL1.avg, GSL2.avg, GSL3.avg, GSL4.avg, GSL5.avg, key=GSL, value = GSL.Value) %>%
        gather(GSI1.avg, GSI2.avg, GSI3.avg, GSI4.avg, GSI5.avg, GSI6.avg, key=GSI, value = GSI.Value) %>%
        gather(GSS1.avg, GSS2.reversed.avg, GSS3.reversed.avg, key=GSS.reversed, value = GSS.reversed.Value)
}

tws.save.plot <- function(plot, filename.without.suffix, suffix = "pdf") {
    filename.unembedded <- paste(filename.without.suffix, "unembedded", suffix, sep = ".")
    filename.embedded <- paste(filename.without.suffix, suffix, sep = ".")
    ggsave(plot = plot, filename.unembedded, device = cairo_pdf)
    embed_fonts(filename.unembedded, outfile=filename.embedded)
    file.remove(filename.unembedded)
}

tws.make.gs.avg.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GS.avg, GS.avg.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Series", y = NULL, title = "Godspeed Averages for Linear and Slow in, Slow out Velocity Profiles") +
        theme_gray() +
        theme(text = font, axis.text.x=xformat,
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Animacy", "Athropomorphism", "Likeability", "Perceived Intelligence★ ", "Perceived Safety"))

    tws.save.plot(plot, "gs-avg")
}

tws.make.gs.anthro.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GSAnthro, GSAnthro.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Anthropomorphism Items", y = NULL,
             title = "Godspeed Anthropomorphism Series") +
        theme_gray() +
        theme(text = font, axis.text.x=xformat,
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Fake—Natural", "Machinelike—Humanlike", "Unconscious—Conscious", "Artificial—Lifelike", "Moving Rigidly—Elegantly"))

    tws.save.plot(plot, "gs-anthro")
}

tws.make.gs.animacy.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GSAnimacy, GSAnimacy.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Animacy Items", y = NULL,
             title = "Godspeed Animacy Series") +
        theme_gray() +
        theme(text = font, axis.text.x=xformat,
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Dead—Alive", "Stagnant—Lively", "Mechanical—Organic", "Artificial—Lifelike", "Inert—Interactive"))

    tws.save.plot(plot, "gs-animacy")
}

tws.make.gs.likeability.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GSL, GSL.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Likeability Items", y = NULL,
             title = "Godspeed Likeability Series") +
        theme_gray() +
        theme(text = font, axis.text.x=xformat,
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Dislike—Like", "Unfriendly—Friendly", "Unkind—Kind", "Unpleasant—Pleasant", "Awful—Nice"))

    tws.save.plot(plot, "gs-likeability")
}

tws.make.gs.intelligence.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GSI, GSI.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Perceived Intelligence Items", y = NULL,
             title = "Godspeed Perceived Intelligence Series") +
        theme_gray() +
        theme(text = font,
              axis.text.x=element_text(color = "black", angle=65, vjust=.8, hjust=0.8),
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Incompetent—Competent", "Ignorant—Knowledgeable", "Irresponsible—Responsible",
                                  "Unintelligent—Intelligent", "Foolish—Sensible", "Unpredictable—Predictable★"))

    tws.save.plot(plot, "gs-intelligence")
}

tws.make.gs.safety.plot <- function(df, xformat, facet.background, font) {
    plot <- ggplot(df, aes(GSS.reversed, GSS.reversed.Value)) + geom_boxplot() +
        facet_wrap(~Movement) +
        labs(x = "Godspeed Perceived Safety Items", y = NULL,
             title = "Godspeed Perceived Safety Series",
             subtitle = "*Items 2 and 3 have been reversed") +
        theme_gray() +
        theme(text = font,
              axis.text.x=xformat,
              strip.text.x = element_text(face = "bold"),
              strip.background = facet.background) +
        scale_x_discrete(labels=c("Anxious - Relaxed", "*Agitated - Calm★", "*Surprised - Quiescent"))

    tws.save.plot(plot, "gs-safety")
}

make.godspeed.graphs <- function() {
    # Make a lot of graphs and then save them.
    # Assumption that extrafonts have been loaded and that you have Aktiv Grotesk installed (likely not).
    df <- siso.and.linear.godspeed.component.averages.gathered()
    levels(df$Movement) <- c("Slow in, Slow out", "Linear")
    df$Movement <- factor(df$Movement, sort(levels(df$Movement)))
    x.axis.text.format <- element_text(color = "black", angle=30, vjust=.8, hjust=0.8)
    facet.background <- element_rect(color = NULL, fill = "white")
    font <- element_text(family = "Aktiv Grotesk")

    # Overall Averages
    tws.make.gs.avg.plot(df, x.axis.text.format, facet.background, font)

    # Anthropomorphism
    tws.make.gs.anthro.plot(df, x.axis.text.format, facet.background, font)

    # Animacy
    tws.make.gs.animacy.plot(df, x.axis.text.format, facet.background, font)

    # Likeability
    tws.make.gs.likeability.plot(df, x.axis.text.format, facet.background, font)

    # Perceived Intelligence
    tws.make.gs.intelligence.plot(df, x.axis.text.format, facet.background, font)

    # Perceived Safety
    tws.make.gs.safety.plot(df, x.axis.text.format, facet.background, font)
}


godspeed.nonparamTestByName <- function(df1, df2, name, func = c(t.test, wilcox.test), paired = TRUE, alternative = c("two.sided", "less", "greater")) {
    func(df1[[name]], df2[[name]], paired = paired, alternative)
}

godspeed.wilcox.tests.for.components <- function(df = siso.and.linear.godspeed.component.averages(), paired = TRUE) {
    df.siso <- df %>% dplyr::filter(Movement == "Siso")
    df.linear <- df %>% dplyr::filter(Movement == "Linear")
    alternatives <- c("two.sided", "greater", "less")
    cols <- names(df)
    variable.names <- cols[3:length(cols)]
    sapply(X = alternatives,
           FUN = function(x) sapply(X = variable.names,
                                    FUN = function(y) godspeed.nonparamTestByName(df1 = df.siso, df2 = df.linear, name = y,
                                                                                  func = wilcox.test, paired = paired, alternative = x),
                                    simplify = FALSE, USE.NAMES = TRUE),
           simplify = FALSE, USE.NAMES = TRUE)
}

# Test if we have a normal distribution for each component.
# example: results.shapiro(results.tidy, names(results.tidy)[7:length(names(results.tidy))])
results.shapiro <- function(df, variable.names) {
    sapply(X=variable.names, FUN=function(x) shapiro.test(df[[x]]), simplify = FALSE, USE.NAMES = TRUE)
}

godspeed.wilcox.for.iterations <- function(df = results.tidy, alternative = c("two.sided", "less", "greater")) {
    df.first <- df %>% dplyr::filter(time.ordered == 1)
    df.second <- df %>% dplyr::filter(time.ordered == 2)
    df.third <- df %>% dplyr::filter(time.ordered == 3)
    df.fourth <- df %>% dplyr::filter(time.ordered == 4)

    comparisons <- list(first.and.second = list(df.first, df.second),
                        first.and.third = list(df.first, df.third),
                        first.and.fourth = list(df.first, df.fourth),
                        second.and.third = list(df.second, df.third),
                        seond.and.fourth = list(df.second, df.fourth),
                        third.and.fourth = list(df.third, df.fourth))

    variable.names <- tidy.variable.names(df)[28:33] # Only look at the averages to maintain sanity.

    sapply(comparisons,
           FUN = function(x) sapply(variable.names,
                                    FUN = function(y) godspeed.nonparamTestByName(x[[1]], x[[2]], name = y,
                                                                                  func = wilcox.test, alternative = alternative),
                                    simplify = FALSE, USE.NAMES = TRUE),
           simplify = FALSE, USE.NAMES = TRUE)
}

godspeed.test.animacy.fixed <- function(df = results.split.averages) {
    df.from.1007 <- df %>%
                       dplyr::filter(ID > 1006) %>%
        dplyr::select(Movement, ID, GSAnimacy1.avg, GSAnimacy2.avg,
                      GSAnimacy3.avg, GSAnimacy4.avg, GSAnimacy5.avg)
    godspeed.wilcox.tests.for.components(df.from.1007)
}

pm1.wilcox.test <- function(df = results.tidy) {
    df.pm1.only <- df %>% dplyr::filter(time.ordered == 1) %>% dplyr::select(ID, Movement, PM1)
    godspeed.wilcox.tests.for.components(df.pm1.only, paired = FALSE)
}

# Objects that we are using.

results.tidy <- tidyFetchSisoResults()
results.raw <- rawFetchSisoResults()
results.split.averages <- siso.and.linear.godspeed.component.averages(results.tidy)
godspeed.alpha <- alphaAllEncounters(results.tidy)
godspeed.avg.shapiro <- results.shapiro(results.tidy, names(results.tidy)[35:40])
godspeed.component.shapiro <- results.shapiro(results.tidy, names(results.tidy)[7:33])



