library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)
library(plyr)
library(lubridate)

###################################################################################################
##Operations on osha.DBF
#read the dbf files
osha <- read.dbf("MA615-midterm-spring-17-master/osha.dbf")
scc <- read.dbf("MA615-midterm-spring-17-master/lookups/SCC.DBF")
sic <- read.dbf("MA615-midterm-spring-17-master/lookups/SIC.DBF")
#First, I create a new dataframe after reading the txt about osha
#select the potential variables that I think is helpful to the study
osha <- select(osha,OWNERCODE,ACTIVITYNO, OSHA1MOD, SITECITY, SITECNTY,SITEZIP, SIC, PAPREP,
               SITEADD, ESTABNAME)
#Looking at the osha1 table, I found the variable OWNERCODE may not be helpful
sum(osha$OWNERCODE==0)
sum(osha$OWNERCODE!=0)
#80223 data points do not have a ownercode, so I drop the variable OWNERCODE
osha <- select(osha, -OWNERCODE)
#Arrange the data by ACTIVITYNO
osha <- arrange(osha, ACTIVITYNO)
#Check whether there are 80445 different activity numbers
sum(duplicated(osha))
#Since we get 0, we do not need to worry about duplicated activity numbers
#rename the SITECITY and SITECNTY
colnames(osha) <- c("ACTIVITYNO", "OSHA1MOD", "CITY", "COUNTY", "SITEZIP", "SIC", "PAPREP",
                     "SITEADD", "ESTABNAME")
#Select the rows that have State name MA
MA <- scc[scc$STATE=="MA",]
#select only 3 variables
MA <- select(MA, COUNTY, CITY, NAME)
#Left join the osha1 and MA data
osha3 <- left_join(osha,MA, by = c("CITY","COUNTY"))
#Left join osha3 and sic
osha3<- left_join(osha3 ,sic, by = "SIC")
#drop the city, county and SIC number columns
osha3 <-select(osha3, -CITY, -COUNTY, -SIC)
#Arange the order of the columns
osha3 <- osha3[c("NAME","ACTIVITYNO",  "SITEZIP","SITEADD","OSHA1MOD","PAPREP","INDUSTRY","ESTABNAME")]
#rename the columns
colnames(osha3) <- c("name","ACTIVITYNO", "Zipcode","Address","Date","Preptime","Industry","EstabName")
#arrange the rows
osha3 <-arrange(osha3,name)


###################################################################################################
##Operations on viol.dbf
#read the Viol file
viol <- read.dbf("MA615-midterm-spring-17-master/viol.dbf")
#After reading the txt for viol I only choose ACTIVITYNO and VIOLTYPE
viol <- select(viol, ACTIVITYNO, VIOLTYPE)
#rename the factor level names for VIOLTYPE
levels(viol$VIOLTYPE) <- c("other","repeat","serious","unclassified","willful")
#arrange the data by ACTIVITYNO
viol <- arrange(viol, ACTIVITYNO)
#Count the frequency of ACTIVITYNO base on the VIOLTYPE
viol1 <- count(viol,vars = c("ACTIVITYNO", "VIOLTYPE"))

#Extract the factor level "other" and rename the columns
other1 <- viol1[(viol1$VIOLTYPE=="other"),]
colnames(other1) <- c("ACTIVITYNO","viol_other","viol_freq")
#combine the columns into one column
other1 %<>% unite(violType, viol_other, viol_freq)
#Do the same operations on factor level "repeat"
repeated1 <-viol1[(viol1$VIOLTYPE=="repeat"),]
colnames(repeated1) <- c("ACTIVITYNO","viol_repeat","viol_freq")
repeated1 %<>% unite(violType, viol_repeat, viol_freq)
#Do the same operations on factor level "repeat"
serious1 <-viol1[(viol1$VIOLTYPE=="serious"),]
colnames(serious1) <- c("ACTIVITYNO","viol_serious","viol_freq")
serious1 %<>% unite(violType, viol_serious, viol_freq)
#Do the same operations on the factor level "serious"
unclassified1 <-viol1[(viol1$VIOLTYPE=="unclassified"),]
colnames(unclassified1) <- c("ACTIVITYNO","viol_unclass","viol_freq")
unclassified1 %<>% unite(violType, viol_unclass, viol_freq)
#Do the same operations on the factor level "willful"
willful1 <-viol1[(viol1$VIOLTYPE=="willful"),]
colnames(willful1) <- c("ACTIVITYNO","viol_will","viol_freq")
willful1 %<>% unite(violType, viol_will, viol_freq)
#row bind all the 5 dataframe
violations <- rbind(other1,repeated1,serious1,unclassified1,willful1)
#seperate the second column into two, one is the violation type, the other is the frequency count
violations <- violations %>% separate(violType, c("violType", "viol_Freq"), remove = TRUE)
violations <- arrange(violations, ACTIVITYNO)



###################################################################################################
##operations on accid.dbf
#read the DBF files
ac <- read.dbf("MA615-midterm-spring-17-master/accid.dbf")
acc <- read.dbf("MA615-midterm-spring-17-master/lookups/acc.dbf")
#check if they are all from MA, if they are, drop the variable SITESTATE
if(sum(ac$SITESTATE == "MA")==dim(ac)[1]){ac %<>% select(-SITESTATE)}
#Bodypart and event type are not directly related to safety issues so I drop them
ac <- select(ac,-BODYPART,-EVENT)
#Get the rows for ENVIRON
enviro <- acc[(acc$CATEGORY=="ENVIR-FAC"),] 
#Since they are all in ENVIR-FAC, we do not need the first column
enviro <- select(enviro, -CATEGORY)
#rename the columns
colnames(enviro) <- c("ENVIRON","ENVIR_FAC")
#we want to joint enviro and ac by the variable"ENVIRON" but the factor levels for "ENVIRON"in ac
#are not equal to the factor levels for "ENVIRON" in enviro, we need to add one row to enviro
row0 <- as.data.frame(t(as.data.frame(c("00",NA))))
colnames(row0) <- c("ENVIRON","ENVIR_FAC")
rownames(row0) <- "50"
enviro <- rbind(enviro, row0)
#join the column for ENVIRON and the big accident table
ac1 <- left_join(ac,enviro, by="ENVIRON")
#Do the same thing for Human 
human <- acc[acc$CATEGORY=="HUMAN-FAC",]
human <- select(human, -CATEGORY)
colnames(human) <- c("HUMAN","HUMAN_FAC")
colnames(row0) <- c("HUMAN","HUMAN_FAC")
human <- rbind(human, row0)
ac1 <- left_join(ac1,human, by="HUMAN")
#Do the same thing for Source
sourc <- acc[acc$CATEGORY=="SOURC-INJ",]
sourc <- select(sourc, -CATEGORY)
colnames(sourc) <- c("SOURCE","SOURC_INJ") 
colnames(row0) <- c("SOURCE","SOURC_INJ")
sourc <- rbind(sourc, row0)
ac1 <- left_join(ac1,sourc, by="SOURCE")
#Do the same thing for Nature
nature <- acc[acc$CATEGORY=="NATUR-INJ",]
nature <- select(nature, -CATEGORY)
colnames(nature) <- c("NATURE","NATUR_INJ") 
colnames(row0) <- c("NATURE","NATUR_INJ")
nature <- rbind(nature, row0)
ac1 <- left_join(ac1,nature, by="NATURE")

#choose the variables I decide to use after reading the loyout for ACCID
acfinal <- select(ac1, ACTIVITYNO, RELINSP, ENVIR_FAC,HUMAN_FAC,SOURC_INJ,NATUR_INJ)
#Change each variable name into lower case
colnames(acfinal) <- c("ACTIVITYNO", "relinsp","envir_fac","human_fac",
                       "source","nature")
#Check whether the ACTIVITYNO and the relinsp columns contain the same information
all(acfinal$ACTIVITYNO == acfinal$relinsp)
#Since ACTIVITYNO and Relationship are the same, we drop the relinsp column
acfinal <- select(acfinal, -relinsp)
#Since we only care about the factor type that cause the event, I remove duplicated rows
accident1 <- acfinal[!duplicated(acfinal), ]



####################################################################################################
#Combine all the table I get together
final1 <- left_join(osha3, violations, by = "ACTIVITYNO")
final1 <- left_join(final1,  accident1, by = "ACTIVITYNO")
#I find that the last 4 columns have many NAs, which may effect the data analysis as outliers
#So I do not use the accident1, and only left_join the osha3 and violations
final <- left_join(osha3, violations, by = "ACTIVITYNO")
#Change the column viol_Freq from character to values
final$viol_Freq <- as.numeric(final$viol_Freq)
#Change the date format
final[ ,5] <- ymd(final[, 5])
#View the table
View(final)




####################################################################################################
#Graphs
library(ggplot2)
#Make the plot based on area name and the frequency of event occur
area <- count(final, vars = "name")
#rearrange the data by frequency, because we need to know the place that most activities happens  
area2 <- arrange(area,desc(freq))
#Since there are too many observations, I devide them into 8 parts with 60 places in each part.
P1 <- area2[1:60,]
P2 <- area2[61:120,]
P3 <- area2[121:180,]
P4 <- area2[181:240,]
P5 <- area2[241:300,]
P6 <- area2[301:360,]
P7 <- area2[361:420,]
P8 <- area2[421:468,]
#Plot the Bar graphs for all the place
#Since 8 plots take too much time, I only draw 2 of them
ggplot(P1, aes(x = name, y = freq, fill=name)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + ggtitle("Top 60 Places") + ylab("Number of Dangerous Activities")
ggplot(P8, aes(x = name, y = freq, fill=name)) + geom_bar(stat = "identity") + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + ggtitle("421-468th Places") + ylab("Number of Dangerous Activities")

#I choose plot the violation types in Boston by zipcode
Boston <- final[final$name=="BOSTON",]
Boston <- arrange(select(Boston,Zipcode, violType, viol_Freq), Zipcode)
#Obmit the NA rows
Boston <- na.omit(Boston)
#find the frequency of each violation type for each Zipcode
Boston <- ddply(Boston, .(Zipcode,violType), summarise, viol_Freq = sum(viol_Freq))

#Since the data is too large, I separate it into 2 parts
P1 <- Boston[1:100,]
P2 <- Boston[101:276,]
#Plot the data
require(ggrepel)
ggplot(P1, aes(Zipcode, viol_Freq, label=Zipcode))+ geom_label_repel(aes(fill=violType), colour="white", fontface="bold",segment.color='black') + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + ggtitle("Violation Types and Frequency For Boston (Part1)") + ylab("Frequency")
ggplot(P2, aes(Zipcode, viol_Freq, label=Zipcode))+ geom_label_repel(aes(fill=violType), colour="white", fontface="bold",segment.color='black') + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle=45,hjust=1,vjust=0.5)) + ggtitle("Violation Types and Frequency For Boston (Part2)") + ylab("Frequency")
