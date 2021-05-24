##Import the data

data1 <- function () {
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(dplyr))install.packages("dplyr")
library(dplyr)

# Using the file location, import all csv files
tbl <-  list.files("D:/20331568/Solution/20331568/Question2/data/Forbes"  ,
                   pattern = "*.csv",
                   full.names = T) %>%
    map_df(~read_csv(., col_types = cols(.default = "c")))


tbl$NetWorth<- gsub("[a-zA-Z | $]","",tbl$NetWorth)

tbl  <- tbl %>% mutate(NetWorth = as.numeric(NetWorth), Rank  = as.numeric(Rank),
                       Age = as.numeric(Age),
                       Name = as.factor(Name),
                       Industry = as.factor(Industry),
                       Source = as.factor(Source),
                       Country = as.factor(Country))
return(tbl)

}


figure1 <- function (g) {

plot <- tbl %>% group_by(Country) %>% filter(Age <=30)

g <- ggplot(plot, aes(x = Country, y= NetWorth, colour = Country))+ geom_point() + labs(
    x = "Country",
    y = "NetWorth",
    title = "Billionaires under the Age of 30",
    fill ="#f68060"
) + theme_bw()

return(g)

}




figure2 <- function (d) {

    plot <- tbl %>% group_by(Country) %>% filter(Rank <= 10 )

    d <- ggplot(plot, aes(x = Country, y= NetWorth, colour = Country))+ geom_point() + labs(
        x = "Country",
        y = "Rank",
        title = "Top 10 Billionaires",
        fill ="#f68060"
    ) + theme_bw()

    return(d)

}






