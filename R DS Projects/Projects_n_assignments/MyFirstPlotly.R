library(plotly)


library(ggplot2)
## First do a bar plot in ggplot
g <- ggplot(myData, aes(y = enrollment, x = class, fill = offering)) 
g <- g + geom_bar(stat = "identity")
g

## Let's try to get it into plot.ly
py <- plot_ly()
out <- py$ggplotly(g)
out$response$url


set.seed(2016-07-21)
temp <- rnorm(100, mean = 30, sd = 5)
pressue <- rnorm(100)
dtime <- 1:100
plot_ly(x = ~temp, y = ~pressue, z = ~dtime,
        type = "scatter3d", color = ~temp)

library(plotly)
library(tidyr)
library(dplyr)
data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets) %>%
        gather(index, price) %>%
        mutate(time = rep(time(EuStockMarkets), 4))
plot_ly(stocks, x = ~time, y = ~price, color = ~index, type = "candlestick")