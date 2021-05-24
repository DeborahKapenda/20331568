# Write your function here

q1 <- function () {

library(readr)
df <- read_csv("D:/20331568/Solution/20331568/Question1/data/Movies.csv")

df <- rename_with(df, ~ tolower(gsub("%", ".", .x, fixed = TRUE)))
df <- rename_with(df, ~ tolower(gsub(".", "", .x, fixed = TRUE)))
df <- rename_with(df, ~ tolower(gsub(" ", "", .x, fixed = TRUE)))
return(df)

}



r <- function (d) {

    p1 <- df %>% group_by(film) %>% filter(rottentomatoes >= 80)

    d <- ggplot(p1, aes(x = genre, y= rottentomatoes, colour = genre))+ geom_point() + labs(
        x = "Film",
        y = "Rotten tomatoes",
        title = "Movie Ratings",
        fill ="#f68060"
    ) + theme_bw()


    return(d)

}


rr <- function (e) {

    p1 <- df %>% group_by(film) %>% filter(audiencescore >= 85)

    e <- ggplot(p1, aes(x = genre, y= rottentomatoes, colour = genre))+ geom_point() + labs(
        x = "Film",
        y = "Rotten tomatoes",
        title = "Movie Ratings",
        fill ="#f68060"
    ) + theme_bw()


    return(e)

}



dis <- function (b) {

    p1 <- df %>% group_by(leadstudio) %>% filter(profitability >= 2)

    b <- ggplot(p1, aes(x = genre, y= profitability, colour = leadstudio))+ geom_point() + labs(
        x = "genre",
        y = "profitability",
        title = "Movie Ratings by Lead Studios",
        fill ="#f68060"
    ) + theme_bw()


    return(b)

}


h <- function (t) {

    p1 <- df %>% group_by(film) %>% filter( worldwidegross >= 100)

    t <- ggplot(p1, aes(x = genre, y= worldwidegross, colour = genre))+ geom_point() + labs(
        x = "genre",
        y = "worldwidegross",
        title = "Movie Ratings by Worldwide gross",
        fill ="#f68060"
    ) + theme_bw()


    return(t)

}




