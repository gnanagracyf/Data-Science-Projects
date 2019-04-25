fileUrl ="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
#download.file(fileUrl,destfile = "pid.csv")
install.packages("data.table")
library(data.table)
DT = fread(fileUrl)
#e ways to calculate the average value of the variable
system.time(DT[,mean(pwgtp15),by=SEX])
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
sapply(split(DT$pwgtp15,DT$SEX),mean)
