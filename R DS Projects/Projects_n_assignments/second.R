##https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx

fileURL="https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(fileUrl,destfile="NGAP.xlsx",mode="wb")
rowindex <- 18:23
colindex <- 7:15
#install.packages("xlsx")
#library("xlsx")
dat <- read.xlsx("NGAP.xlsx",sheetIndex = 1,rowIndex = owindex, colIndex = colindex)
#read.xlsx2("NGAP.xlsx",sheetIndex = 1,rowindex = rowindex, colIndex = colindex,header=TRUE)

openxlsx try this