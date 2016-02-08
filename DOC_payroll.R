library(dplyr);library(ggplot2)

#url <- "http://www2.census.gov/econ/susb/data/2012/us_6digitnaics_2012.xls"
filedir <- "./Personal/us_6digitnaics_2012.csv"
df <- read.csv(filedir, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA"))
row.names(df) <- NULL
colnames(df) <- df[5,]
df <- df[-c(1:7), -1]

df <- cbind(df[,1], as.data.frame(lapply(df[,-1], function(x) gsub(",", "", x))))


df[,3] <- as.numeric(as.character(df[,3]))
df[,4] <- as.numeric(as.character(df[,4]))
df[,5] <- as.numeric(as.character(df[,5]))
df[,8] <- as.numeric(as.character(df[,8]))
df[,10] <- as.numeric(as.character(df[,10]))

df$EMPLOYMENT.RANGE.FLAG <- NULL
df$EMPLOYMENT[df$EMPLOYMENT == 0] <- NA
df <- na.omit(df)

colnames(df) <- c("NAICS.description", "Enterprise.employment.size", "Number.of.firms", "Number.of.establishments", 
                  "Number.of.employment", "Employment.noise flag", "Annual.payroll.1000", "Annual.payroll.noise.flag", 
                  "Estimated.receipt.1000", "Estimated.receipts.noise.flag")

dfTotal <- df[which(as.character(df$Enterprise.employment.size) %in% c("01:  Total")),]
dfTotal <- df[df$Enterprise.employment.size == "01:  Total",]
dfTotal <- dfTotal[!duplicated(dfTotal$`NAICS.description`),]

df.payroll <- dfTotal[order(dfTotal$Annual.payroll.1000, decreasing = T),]

dfTotal$avgpay.emp.1000 <- dfTotal$Annual.payroll.1000/dfTotal$Number.of.employment
df.avgpay <- dfTotal[order(dfTotal$avgpay.emp.1000, decreasing = T),]

ggplot(df.payroll[2:6,], aes(x=NAICS.description, y = Annual.payroll.1000)) + geom_bar(stat = "identity") +
    ylab("Total annual payroll in $1000") + ggtitle("Top Five Industrieis with Highest Payroll")
dev.copy(png, file = "Top Five Industrieis with Highest Payroll.png", width = 1000, height = 500)
dev.off()

plot2 <- ggplot(df.avgpay[2:6,], aes(x=NAICS.description, y = avgpay.emp.1000)) + geom_bar(stat = "identity") +
    ylab("avg payroll per employee in $1000") + 
    scale_x_discrete(labels = c(paste("Commodity Contract","Dealing", sep = "\n"), 
                                "Portfolio Management",
                                paste("Securities and Commodity","Contracts","Intermediation","and Brokerage", sep = "\n"),
                                paste("Securities, Commodity Contracts,","and other Financial Investmets"
                                      ,"and Related Activities", sep = "\n"),
                                "Sports Teams and Clubs")) + 
    ggtitle("Top Five Industrieis with Highest Average Payroll per Employee")
dev.copy(png, file = "Top Five Industrieis with Highest Average Payroll per Employee.png", width = 1000, height = 500)
dev.off()