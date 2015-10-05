Q2_FGDP <- read.csv("Q2_FGDP.csv", skip = 5, stringsAsFactors = FALSE, header = FALSE)[,c(1,2,4,5)]
names(Q2_FGDP) <- c("CountryCode", "Ranking", "CountryName", "GDP")
Q2_FGDP_good <- subset(Q2_FGDP, grepl("^[0-9]", Ranking))
Q2_FGDP_good$GDP <- as.numeric(gsub(",","",Q2_FGDP_good$GDP))
summarize(.data = Q2_FGDP_good, mean(GDP))
FGDP <- Q2_FGDP_good

-------
names(FGDP) <- c("CountryCode", "Ranking", "CountryName", "GDP")
URL_FED <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
FED <- read.csv(URL_FED,stringsAsFactors = FALSE)

data_merge <- inner_join(FGDP, FED, by = "CountryCode")