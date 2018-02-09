############################################################################
library(reshape2)
library(plyr)
library(dbConnect)
library(ggplot2)
library(ggmap)
library(lubridate)
library('splitstackshape')


#############################################################################
con <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="XXXXX",
                 dbname="genbank", 
                 host="metadatalab.syr.edu")


con_write <- dbConnect(MySQL(),
                       user="GenBankUser", 
                       password="XXXXX",
                       host="metadatalab.syr.edu",
                       dbname="test")

#dbListTables(con)

ReferencePatent<-dbGetQuery(con,'select * from genbank.ReferencePatent')

tmp<-refdf[1:1000,]

View(tmp)
colnames(tmp)

#[1] "id"         "reference"  "authors"    "consortium" "title"      "journal"    "pubmed"    
#[8] "remark"    

#what sarah wants:
#The final DF should have these attributes: id, authors (original field), title, 
#patentType (e.g. EP, US, GB), PatentNumber (e.g. 0238993), version (e.g. -A2 1), 
#patentDay, PatentMonth, PatentYear, extra_journal 
#(the leftover data that comes after the semi-colon or that is 
#not any of the previously indicated patent data), 
#and then the parsed author names (one per column (e.g. Author 1, Author 2, Author 3). 
                 


######################################
#PARSING THE ACTUAL DATASET, REAL DEAL
###########################
#Split Author names of the real set
#eliminate "\t"
authorsplit<-gsub("\t", " ", ReferencePatent$authors, fixed=TRUE)
#eliminate "and"
authorsplit<-gsub(" and ", ", ", authorsplit, fixed=TRUE)
#split the split
authorsplit<-strsplit(authorsplit,", ")
#check the max number of authors from a patent
max(unlist(lapply(authorsplit, function(x) length(x))))
#the max author is 50, does that mean that I have to create 50 columns for them authors?

#Populate this df
refdf<-as.data.frame(ReferencePatent$authors)
#Write a for loop to populate the author columns
for (i in 1:max(unlist(lapply(authorsplit, function(x) length(x))))){
  eval(parse(text = paste0('refdf$Author', i, ' <- sapply(authorsplit,function(x) x[i])')))
}

#How many unique authors
length(unique(unlist(authorsplit)))

nrow(refdf[Author1=='.',])
#1664192

refdf[Author1=='.',"Author1"]

View(head(refdf))
View(tmpdf)
table(tmpdf$patenttype)

#1. parse patenttype
#1.1 sub "\t" and split the journal column by ' '
journal<-strsplit(gsub("\t", " ", ReferencePatent$journal, fixed=TRUE)," ")
refdf$patenttype<-sapply(journal,function(x) x[2])

#2. parse patent number
patentNumber<-strsplit(sapply(journal,function(x) x[3]), "-")
refdf$patentNumber<-sapply(patentNumber,function(x) x[[1]])


#3. parse version (somethin' like -A2 1)
version1<-sapply(patentNumber,function(x) x[2])
version2<-sapply(journal,function(x) x[4])

version3<-mapply(c, version1, version2, SIMPLIFY=FALSE)
refdf$patentVersion<-as.vector(sapply(version3,function(x) paste(x[1],x[2])))

#4. patentDay, patentmonth, patent year
fulldate<-dmy(sapply(journal,function(x) x[5]))
refdf$patentYear<-year(fulldate)
refdf$patentMonth<-month(fulldate)
refdf$patentDay<-day(fulldate)

#5. split the journal column on the first occrence of ";\t" to get extra portion of the journal
refdf$patentExtra <- as.character(colsplit(ReferencePatent$journal,";\t",c("a","b"))[2])
##########convert the column to char type
tmp1<-refdf$patentExtra[1:7000000,]
tmp2<-refdf$patentExtra[7000001:14000000,]
tmp3<-refdf$patentExtra[14000001:21000000,]
tmp4<-refdf$patentExtra[21000001:26003035,]
refdf$patentExtra<-c(tmp1,tmp2,tmp3,tmp4)



###################################
#TEST WITH A TMP SET
##############################s

#get 1000 rows from the db to play around
tmp<-dbGetQuery(con,'select * from genbank.ReferencePatent Limit 1000;')



#0. parse id, author original field, title
tmpid<-tmp$id
tmpauthor<-tmp$authors
tmptitle<-tmp$title
#1. parse patenttype
#1.1 split the journal column by ' '
journalsplit<-strsplit(gsub("\t", " ", tmp$journal, fixed=TRUE)," ")
tmppatenttype<-sapply(journalsplit,function(x) x[2])

#2. parse patent number
sapply(journalsplit,function(x) x[3])
splitpatentnumber<-strsplit(sapply(journalsplit,function(x) x[3]), "-")
tmppatentnumber<-sapply(splitpatentnumber,function(x) x[[1]])

#3. parse version (somethin' like -A2 1)
versionpart1<-sapply(splitpatentnumber,function(x) x[2])
versionpart2<-sapply(journalsplit,function(x) x[4])

versioncombine<-mapply(c, versionpart1, versionpart2, SIMPLIFY=FALSE)
tmpversion<-as.vector(sapply(versioncombine,function(x) paste(x[1],x[2])))

#4. patentDay, patentmonth, patent year
tmpfulldate2<-sapply(journalsplit,function(x) x[5])
tmpfulldate<-dmy(tmpfulldate2)
tmppatentyear<-year(tmpfulldate)
tmppatentmonth<-month(tmpfulldate)
tmppatentday<-day(tmpfulldate)

#5. extra_journal 
tmpextrajournal <- list()
for (i in 1:length(journalsplit)){
  a<-paste(journalsplit[[i]][-1:-5],collapse=' ')
  tmpextrajournal[i]<-a
}
tmpextrajournal<-unlist(tmpextrajournal)

#6. Deal with the authors
#6.1 Author 1
#Split Author Names of the temp set
#eliminate "\t"
tmpauthorsplit<-gsub("\t", " ", tmpauthors, fixed=TRUE)
#eliminate "and"
tmpauthorsplit<-gsub(" and ", ", ", tmpauthorsplit, fixed=TRUE)
#split the split
tmpauthorsplit<-strsplit(tmpauthorsplit,", ")


#find the max number of authors for a patent...which is 13
max(unlist(lapply(tmpauthorsplit, function(x) length(x))))

#they have 13 authors,max...


#populate a tmpdf
tmpdf<-as.data.frame(tmpauthors)
tmpdf$numofauthors<-unlist(lapply(tmpauthorsplit, function(x) length(x)))
tmpdf$id<-tmpid
tmpdf$title<-tmptitle
tmpdf$patenttype<-tmppatenttype
tmpdf$patentnumber<-tmppatentnumber
tmpdf$patentversion<-tmpversion
tmpdf$patentyear<-tmppatentyear
tmpdf$patentMonth<-tmppatentmonth
tmpdf$patentDay<-tmppatentday
tmpdf$extra_journal<-tmpextrajournal
tmpdf$tmpauthors<-NULL
tmpdf$AuthorsOriginal<-tmpauthors

View(tmpdf)
#Write a for loop to populate the author columns
for (i in 1:max(unlist(lapply(tmpauthorsplit, function(x) length(x))))){
  eval(parse(text = paste0('tmpdf$Author', i, ' <- sapply(tmpauthorsplit,function(x) x[i])')))
}


#get a list of all the authors names
tmpallauthors<-unlist(tmpauthorsplit)
#Visualize it
barplot(sort(table(tmpallauthors)),main="Freq of the Author names in all them patents")

View(tmpdf)

###Save the work:
saveRDS(refdf,'abc.rds')

#the highest number of authors on a patent(distribution)
View(tmpdf)

#obj1. eliminate the annoying "and" in the author strings (gsub all the " and " to " " i guess?)

pctmp1<-tmp$authors
pctmp1[1]

journal_field <- dbGetQuery(con,"select * from ReferencePatent LIMIT 200;")
newdf = data.frame(journal_field$journal,journal_field$authors)
ref <- dbGetQuery(con,"select * from Reference WHERE LOWER(authors) LIKE '%kennedy%';")

#To write data in a MySql table:

dbWriteTable(con_write, "US_data", US_data)

