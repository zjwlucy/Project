library(data.table)
library(foreign)
library(dplyr)
library(magrittr)
library(tidyr)

dd <- read.dbf("accid.dbf")
# head(dd)
# ddt <- data.table(dd)
label1 <- read.dbf("lookups/acc.dbf")
# levels(dd$DEGREE)
# levels(dd$DEGREE) <- C("no injury","fatality","hospitalized","non-hospita")

#d1 <- select(dd, -SITESTATE)


if(sum(dd$SITESTATE == "MA")==dim(dd)[1]){dd %<>% select(-SITESTATE)}

dim(label1)
sum(label1$CATEGORY=="PART~BODY")

parts <- label1[(label1$CATEGORY=="PART~BODY"),]
dim(parts)

parts <- select(parts, CODE, VALUE)


colnames(parts) <- c("BODYPART", "VALUE")
str(parts)

d1 <- left_join(dd, parts, by="BODYPART")

head(parts)

print(parts)
# save(parts, file = "parts.txt")


########################################################################
ac <- read.dbf("accid.dbf")
ad <- read.dbf("admpay.dbf")
de <- read.dbf("debt.dbf")
ha <- read.dbf("hazsub.dbf")
hi <- read.dbf("history.dbf")
op <- read.dbf("optinfo.dbf")
osha <- read.dbf("osha.dbf")
prog <- read.dbf("prog.dbf")
relact <- read.dbf("relact.dbf")
viol <- read.dbf("viol.dbf")
head(ac)
head(ad)    #variable can be delete: filter 
head(de)
head(ha)
head(hi)
head(op)
head(osha)
head(prog)
head(relact)
head(viol)