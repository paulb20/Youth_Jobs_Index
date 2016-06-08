#setwd("F:/R&C/Data/123FILES/ASHE")
library(XLConnect)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(stringr)

loadfonts(device="win")
setwd("F:/R&C/Data/123FILES/ASHE")
ashe_files <- list.files(recursive=TRUE)
# 2011 files using soc 2010 Annual Pay
test.one <- readWorksheetFromFile(ashe_files[325], sheet=1, header=TRUE, startRow=5, endCol=17)
#test.one[,17] <- as.numeric(test.one[,17])
test.one[,18] <- factor(test.one[,3])
removecommas <- function(x)(gsub(",","",x))
test.one[,c(3:17)] <- lapply(test.one[,c(3:17)], removecommas)
test.one[,c(3:17)] <- lapply(test.one[,c(3:17)], as.numeric)

one.digit.one <- subset(test.one, nchar(test.one$Code) ==1)



#2010 files using soc 2000
# test.two <- readWorksheetFromFile(ashe_files[94], sheet=1, header=TRUE, startRow=5, endCol=17)
# test.two[,17] <- as.numeric(test.two[,17])
# test.two[,18] <- factor(test.two[,3])
# four.digit.two <- subset(test.two, nchar(test.two$Code) ==4)


#test.three <- test.one[-c(1,2,5,10,16,19,24,27,30,33),] #remove singl-digit SOC levels
# four.digit.one[,19] <- factor(with(four.digit.one, str_replace_all(paste(str_trim(format(as.numeric(X.thousand.)*1000, big.mark=",")),Description))))
# four.digit.one[,19] <- reorder(four.digit[,19], two.digit[,3])

# test.three[,20] <- factor(with(test.three, str_replace_all(paste(str_trim(format(X.thousand.*1000, big.mark=",")),Description))))
#three.digit.one[,19] <- reorder(three.digit[,19], two.digit[,3])

#Four digit occ_qual9 written out to Excel and edited

occ_qual9_2 <- readWorksheetFromFile("four-digit_2011-12.xlsx", sheet=1, header=TRUE)

test.four <- data.frame(four.digit.one, as.numeric(occ_qual9_2$statistic))

#test.three[,c(3:18)] <- lapply(test.three[,c(3:18)], as.numeric)
test.four[,20] <- factor(with(test.four, paste(str_trim(format(X.thousand.*1000, big.mark=",")),str_trim(Description))))
test.four$V20 <- reorder(test.four$V20, test.four[,4])
p <- ggplot(test.four, aes(x=V20, ymin=`Col8`, lower=`Col10`, middle=`Median`,upper=`Col15`, ymax=`Col17`)) + 
  geom_boxplot(stat="identity") +
  coord_flip() + 
  theme_economist(base_size=12, base_family = "Tahoma", horizontal=FALSE) + 
  theme(title = element_text(size = 48)) +
  aes(fill=as.numeric.occ_qual9_2.statistic.) + 
  scale_y_continuous(breaks=c(20000, 40000, 60000, 80000, 100000, 120000), labels = c("20,000", "40,000", "60,000", "80,000", "100,000", "120,000")) +
  labs(list(title= "Pay and qualifications of recruits, by occupation", x= "Occupation group", y= "Annual earnings", fill = "Qualification level (NQF) of recruits"))


#p <- ggplot(test.three, aes(x=Description, ymin=`Col7`, lower=`Col9`, middle=`Median`,upper=`Col14`, ymax=`Col16`)) + geom_boxplot(stat="identity")
#p + aes(fill=occ_qual7.statistic) + coord_flip()
ggsave("occ_quals_fill_entrant.pdf", width=1189, height= 1682, units="mm", family="Tahoma", limitsize = FALSE)
#test.two <- test.one[c(2,5,10,16,19,24,27,30,33),c(1:4,8,10,15,17,18)]

p2 <- p + theme(text= element_text(size=4)) + theme(title = element_text(size = 6))
ggsave("occ_quals_fill_entrant.png", family="Tahoma")

test.one <- data.frame(one.digit.one, occ_qual6$statistic2)
test.one[,20] <- paste(str_trim(format(as.numeric(test.one$X.thousand.)*1000, big.mark=",")),test.one$Description)
test.one[,20] <- reorder(test.one[,20], test.one[,3])
p <- ggplot(test.one, aes(x=V20, ymin=`Col7`, lower=`Col9`, middle=`Median`,upper=`Col14`, ymax=`Col16`)) + geom_boxplot(stat="identity") +
  coord_flip() + theme_economist(base_family = "Tahoma", horizontal=FALSE) + aes(fill=as.factor(occ_qual6.statistic2)) + scale_y_continuous(labels = comma) +
  labs(list(title= "Pay and qualifications of recruits, by occupation \n ordered by number of employees", x= "Occupation group", y= "Annual earnings", fill = "Qualification level (NQF) of recruits"))

test.three <- data.frame(three.digit.one, occ_qual7$statistic)
test.three[,c(3:18)] <- lapply(test.three[,c(3:18)], as.numeric)
test.three[,20] <- factor(with(test.three, paste(str_trim(format(X.thousand.*1000, big.mark=",")),Description)))
test.three$V20 <- reorder(test.three$V20, test.three[,4])
p <- ggplot(test.three, aes(x=V20, ymin=`Col7`, lower=`Col9`, middle=`Median`,upper=`Col14`, ymax=`Col16`)) + geom_boxplot(stat="identity") +
  coord_flip() + theme_economist(base_family = "Tahoma", horizontal=FALSE) + aes(fill=occ_qual7.statistic) + scale_y_continuous(breaks=c(20000, 40000, 60000, 80000, 100000, 120000), labels = c("?20,000", "?40,000", "?60,000", "?80,000", "?100,000", "?120,000")) +
  labs(list(title= "Pay and qualifications of recruits, by occupation", x= "Occupation group", y= "Annual earnings", fill = "Qualification level (NQF) of recruits"))


#p <- ggplot(test.three, aes(x=Description, ymin=`Col7`, lower=`Col9`, middle=`Median`,upper=`Col14`, ymax=`Col16`)) + geom_boxplot(stat="identity")
#p + aes(fill=occ_qual7.statistic) + coord_flip()
ggsave("occ_quals_fill_entrant.pdf", width=420, height= 594, units="mm", family="Tahoma")
#test.two <- test.one[c(2,5,10,16,19,24,27,30,33),c(1:4,8,10,15,17,18)]

#ggplot(test.two, aes(x=V9, ymin=`Col7`, lower=`Col9`, middle=`Median`,upper=`Col14`, ymax=`Col16`)) + geom_boxplot(stat="identity")
test.large <- readWorksheetFromFile(ashe_files[117], sheet=1, header=TRUE, startRow=5, endCol=17)
# does not work # test.large[,3:17] <- as.numeric(test.large[,3:17])
test.large[,18] <- factor(test.large[,3])
single.digit <- subset(test.large, nchar(test.large$Code) ==1)
two.digit <- subset(test.large, nchar(test.large$Code) ==2)
four.digit <- subset(test.large, nchar(test.large$Code) ==4)

#start to separate regions
Northeast <- grep("^North East", with(four.digit.one, str_trim(Description)))
Northwest <- grep("^North West", with(four.digit.one, str_trim(Description)))
Yorkshire <- grep("^Yorkshire", with(four.digit.one, str_trim(Description)))
Eastmidlands <- grep("^East Midlands", with(four.digit.one, str_trim(Description)))
Westmidlands <- grep("^West Midlands", with(four.digit.one, str_trim(Description)))
East <- grep("^East,", with(four.digit.one, str_trim(Description)))
London <- grep("^London", with(four.digit.one, str_trim(Description)))
Southeast <- grep("^South East", with(four.digit.one, str_trim(Description)))
Southwest <- grep("^South West", with(four.digit.one, str_trim(Description)))
Wales <- grep("^Wales", with(four.digit.one, str_trim(Description)))
Scotland <- grep("^Scotland", with(four.digit.one, str_trim(Description)))
Northernireland <- grep("^Northern Ireland", with(four.digit.one, str_trim(Description)))

Northeasttwo <- grep("^North East", with(four.digit.two, str_trim(Description)))

national <- four.digit.one[1:(Northeast[1]-1),]
national.two <- four.digit.two[1:(Northeasttwo[1]-1),]
national[,3] <- as.numeric(national[,3])
national[,4] <- as.numeric(national[,4])
national[,6] <- as.numeric(national[,6])

national.two[,3] <- as.numeric(national.two[,3])
national.two[,4] <- as.numeric(national.two[,4])
national.two[,6] <- as.numeric(national.two[,6])

national.merged <- merge(national, national.two, by="Code", suffixes=c(".2011", ".2010"))
national.merged$volume.change <- national.merged[,3]/national.merged[,20]
national.merged$pay.change <- national.merged[,4]/national.merged[,21]
p <- qplot(national.merged[,36], national.merged[,37], size=national.merged[,3])
p + scale_area()




