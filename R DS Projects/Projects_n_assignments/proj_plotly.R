library(plotly)

Country <- c("Australia", "China","India","United Kingdom","United States")
Year <- c(2008,2009,2010,2011,2012,2013,2014,2015)
GDP_Australia <- c(3.7, 1.8, 2.0, 2.4, 3.6, 2.6, 2.6, 2.4)
GDP_China <- c(9.7, 9.4 ,10.6, 9.5, 7.9, 7.8, 7.3, 6.9)
India <- c(3.9, 8.5, 10.3, 6.6, 5.5, 6.4, 7.5, 8.0)
UK <- c(-0.5, -4.2 ,1.7 ,1.5 ,1.5 ,2.1 ,3.1 ,2.3)
US <- c(-0.3 ,-2.8, 2.6, 1.6,  2.2, 2.7, 2.4, 2.9)
df <- data.frame(c(2008,2009,2010,2011,2012,2013,2014,2015),Australia,China,India,UK,US)

ts_df <- as.ts(df)

plot_ly(gdp_df, x = ~Year, y = ~price, type="scatter")
        

data("EuStockMarkets")
stocks <- as.data.frame(EuStockMarkets) %>%
                gather(index, price) %>%
                mutate(time = rep(time(EuStockMarkets), 4))
plot_ly(stocks, x = ~time, y = ~price, color = ~index, type="scatter")
        
        
        
        