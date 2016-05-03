## Importing the dplyr library

library (dplyr)

dat <- download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', 'C:/Apps/python/R/data.csv.bz2')

dat <- read.csv('C:/Apps/python/R/data.csv.bz2', stringsAsFactors = F)

dat2 <- dat

names (dat2) <- tolower(names(dat2))

dat3 <- select (tbl_df(dat2), evtype, fatalities:cropdmgexp)
sum(is.na(dat3))

names (dat3)


##Let's look at variables, there are many garbage, need manipulating and cleaning

unique(dat3$propdmgexp)

unique(dat3$cropdmgexp)


dat3$propdmgexp [ (dat3$propdmgexp=='')|(dat3$propdmgexp=='+')|(dat3$propdmgexp=='?')| (dat3$propdmgexp=='-')| (dat3$propdmgexp=='0')| (dat3$propdmgexp=='h') | (dat3$propdmgexp=='H')] <-0
dat3$propdmgexp[(dat3$propdmgexp=='K')] <- 3
dat3$propdmgexp[(dat3$propdmgexp=='M')|(dat3$propdmgexp=='m')] <- 6
dat3$propdmgexp[(dat3$propdmgexp=='B')] <- 9
dat3$cropdmgexp[(dat3$cropdmgexp=='')|(dat3$cropdmgexp=='?')|(dat3$cropdmgexp=='0')] <- 1
dat3$cropdmgexp[(dat3$cropdmgexp=='K')|(dat3$cropdmgexp=='k')] <- 3
dat3$cropdmgexp[(dat3$cropdmgexp=='M')|(dat3$cropdmgexp=='m')] <- 6
dat3$cropdmgexp[(dat3$cropdmgexp=='B')] <- 9
dat3$propdmgexp <- as.numeric(dat3$propdmgexp)
dat3$cropdmgexp <- as.numeric(dat3$cropdmgexp)

dat3$propdmg <- dat3$propdmg*(10^dat3$propdmgexp)
dat3$cropdmg <- dat3$cropdmg*(10^dat3$cropdmgexp)


head(unique(dat3$evtype), 30)

length(unique(dat3$evtype))

dat3$evtype <- gsub ('.*STORM.*', 'STORM', dat3$evtype)
dat3$evtype <- gsub('.*FLOOD.*', 'FLOOD', dat3$evtype)
dat3$evtype <- gsub('.*WIND.*', 'WIND', dat3$evtype)
dat3$evtype <- gsub('.*TORN.*', 'TORNADO', dat3$evtype)
dat3$evtype <- gsub('.*HAIL.*', 'HAIL', dat3$evtype)
dat3$evtype <- gsub('.*HURRICANE.*', 'HURRICANE', dat3$evtype)
dat3$evtype <- gsub('.*RAIN.*', 'RAIN', dat3$evtype)
dat3$evtype <- gsub('.*SNOW.*', 'SNOW', dat3$evtype)
dat3$evtype <- gsub('.*COLD.*', 'COLD', dat3$evtype)
dat3$evtype <- gsub('.*LOW.*TEMPER.*', 'COLD', dat3$evtype)
dat3$evtype <- gsub('.*FROST.*', 'COLD', dat3$evtype)
dat3$evtype <- gsub('.*HIGH.*TEMPER.*', 'HEAT', dat3$evtype)
dat3$evtype <- gsub('.*HEAT.*', 'HEAT', dat3$evtype)
dat3$evtype <- gsub('.*FIRE.*', 'FIRE', dat3$evtype)

length (unique (dat3$evtype))

## Grouping data by event type and looking at fatalities, injuries and economic damage

dat3 <- group_by(dat3, evtype)
dat4 <- summarise(dat3, all_fatalities=sum(fatalities), all_injuries=sum(injuries),
                  all_propdmg=sum(propdmg), all_cropdmg=sum(cropdmg))

table_fatalities <- arrange(select(dat4, evtype, all_fatalities), desc(all_fatalities))[1:10,]
table_fatalities


## Drawing the various charts

par(mar=c(9,5,1,1))
barplot(height = table_fatalities$all_fatalities, names.arg = table_fatalities$evtype, main = 'Fatalities', las=2)