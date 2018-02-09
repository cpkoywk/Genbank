library(reshape2)
library(plyr)
library(dbConnect)
library(ggplot2)
library(ggmap)
library(lubridate)
library('splitstackshape')
library(stringi)
##############################################################################################
#############################################################
#4. Format intl patents (patentreference) in to disambiguous format (PATENTNO. LN,FN,SEQofAuthor)
#separate it

head(par_PatentRefence)

#Split Author names of the real set
#eliminate "\t"

authorsplit<-gsub("\t", " ", ReferencePatent$authors, fixed=TRUE)
#eliminate "and"
authorsplit<-gsub(" and ", ", ", authorsplit, fixed=TRUE)
#sub",," with ","
authorsplit<-gsub(",,", ",", authorsplit, fixed=TRUE)
#eliminate et,al

#split the split
authorsplit<-strsplit(authorsplit,", ")
#check the max number of authors from a patent
max(unlist(lapply(authorsplit, function(x) length(x))))
#the max author is 50, does that mean that I have to create 50 columns for them authors?

####par_PatentReference->disambigintlpatref
par_PatentReference<-dbGetQuery(con,'SELECT * FROM test.par_PatentReference')
View(head(par_PatentReference,100))
colnames(par_PatentReference)
par_PatentReference$row_names<-NULL

#get all intlpatref
intlpatref$row_names<-NULL
intlpatref<-par_PatentReference[par_PatentReference$patenttype!="US",]

colnames(intlpatref)
#How many unique authors are there in the intlpatref?
#uniqueauthors<-as.vector(as.matrix(intlpatref[,2:51]))
#sort(table(uniqueauthors),decreasing = TRUE)
#length(unique(uniqueauthors))
#There are 124829 unique authors
#length(unique(intlpatref$patentNumber))
#108872 unique patentNumbers
#length(unique(intlpatref$id))
#18006057 unique ids


#write.csv(head(intlpatref,1000),file="sample.csv")

#assume authors across multiple patentnumbers' are the same
intlpatref2<-intlpatref[,2:53]
intlpatref2$patentExtra<-intlpatref$patentExtra

patentExtra<-intlpatref2$patentExtra
colnames(intlpatref2)
#
intlpatref3<-intlpatref2[,1:52]
colnames(intlpatref3)
length(unique(intlpatref3$patentNumber))
#108872 unique patentNumbers

intlpatref_unique<-intlpatref3[!duplicated(intlpatref3),]
#reorganize columns
#intlpatref_unique <- subset(uniqueintlpatref, select=c(52,1:52))


#intlpatref_unique2<-intlpatref2[!duplicated(intlpatref2),]
#delete the blank author rows
#52 patentNumbers don't have an author
length(intlpatref_unique[intlpatref_unique$Author1==".",])
intlpatref_unique2<-intlpatref_unique[intlpatref_unique$Author1!=".",]
intlPatWoAuthor<-intlpatref_unique[intlpatref_unique$Author1==".",]

#this has 107954 entries, 


#further clean:s Find not unique author rows
tmp<-as.data.frame(sort(table(uniqueintlpatref2$patentNumber),decreasing=TRUE))
tmp2<-tmp[tmp$`sort(table(uniqueintlpatref2$patentNumber), decreasing = TRUE)`>1,]
#patents that appears more than once:
tmp3<-names(tmp2)
tmp4<-uniqueintlpatref2[uniqueintlpatref2$patentNumber %in% tmp3, ]
tmp4<-tmp4[order(tmp4$patentNumber),]

write.csv(tmp4,'notuniquepatentnumbers.csv')

#Write a very stupid loop
uniqueintlpatref3<-intlpatref_unique2
for (i in 1:50){
  eval(parse(text = paste0('supertmp',i,'<- data.frame(Author=uniqueintlpatref3[,',i,'],
                           patentNo=uniqueintlpatref3$patentNumber,
                           patentType=uniqueintlpatref3$patenttype,Seq=',i, ',stringsAsFactors=FALSE)')))
  eval(parse(text = paste0('supertmp',i,'<-na.omit(supertmp',i,')')))
}


disambintlpatref<-rbind(supertmp1,supertmp10,supertmp11,supertmp12,supertmp13,supertmp14,supertmp15,supertmp16,supertmp17,supertmp18,supertmp19,supertmp2,supertmp20,supertmp21,supertmp22,supertmp23,supertmp24,supertmp25,supertmp26,supertmp27,supertmp28,supertmp29,supertmp3,supertmp30,supertmp31,supertmp32,supertmp33,supertmp34,supertmp35,supertmp36,supertmp37,supertmp38,supertmp39,supertmp4,supertmp40,supertmp41,supertmp42,supertmp43,supertmp44,supertmp45,supertmp46,supertmp47,supertmp48,supertmp49,supertmp5,supertmp50,supertmp6,supertmp7,supertmp8,supertmp9)


View(head(disambintlpatref))
rm(supertmp1,supertmp10,supertmp11,supertmp12,supertmp13,supertmp14,supertmp15,supertmp16,supertmp17,supertmp18,supertmp19,supertmp2,supertmp20,supertmp21,supertmp22,supertmp23,supertmp24,supertmp25,supertmp26,supertmp27,supertmp28,supertmp29,supertmp3,supertmp30,supertmp31,supertmp32,supertmp33,supertmp34,supertmp35,supertmp36,supertmp37,supertmp38,supertmp39,supertmp4,supertmp40,supertmp41,supertmp42,supertmp43,supertmp44,supertmp45,supertmp46,supertmp47,supertmp48,supertmp49,supertmp5,supertmp50,supertmp6,supertmp7,supertmp8,supertmp9)

View(disambintlpatref)

#######Parse the first name and last name
splitfn <- function(df) {
  df<-cSplit(df,"Author",",",drop=FALSE)
  df$Author_1<-as.character(df$Author_1)
  df$Author_2<-as.character(df$Author_2)
  fn<-as.data.frame(stri_split_fixed(str = df$Author_2, pattern = ".", n = 2,simplify=TRUE))
  df$Author_2<-fn$V1
  df$MI<-fn$V2
  return(df)
}

disambintlpatref<-splitfn(disambintlpatref)
colnames(disambintlpatref)
View(disambReference)

colnames(disambintlpatref)[5]<-'Lname'
colnames(disambintlpatref)[6]<-'Fname'

#sort the df based on patentNo
disambintlpatref<-disambintlpatref[order(disambintlpatref$patentNo),]

View(head(disambintlpatref))


#tolower disambintlpatref
str(disambintlpatref)
disambintlpatref<-as.data.frame(sapply(disambintlpatref,tolower))

disambintlpatref$Author<-as.character(disambintlpatref$Author)
disambintlpatref$patentNo<-as.character(disambintlpatref$patentNo)
disambintlpatref$patentType<-as.character(disambintlpatref$patentType)
disambintlpatref$Lname<-as.character(disambintlpatref$Lname)
disambintlpatref$Fname<-as.character(disambintlpatref$Fname)
disambintlpatref$MI<-as.character(disambintlpatref$MI)
disambintlpatref$Seq<-as.integer(as.character(disambintlpatref$Seq))

colnames(disambintlpatref)[2]<-"patent_no"

#Push them to db
con <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="123GenBank456",
                 dbname="test", 
                 host="metadatalab.syr.edu")

dbWriteTable(con, value = disambintlpatref, name = "disamb_IntlPatentReference", append = TRUE )
dbWriteTable(con, value = disambReference, name = "disamb_Reference", append = TRUE )


write.csv2(disambintlpatref,"disambintlpatref.csv")


#Move onto Week 7


