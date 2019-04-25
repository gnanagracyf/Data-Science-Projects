fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl,destfile="./hid.csv")
list.files()
df <- read.csv("hid.csv",skipNul = TRUE)
result <- lapply(split(df,df$VAL),function(x) nrow(x))
result