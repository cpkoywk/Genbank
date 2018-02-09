library(reshape2)
library(plyr)
library(dbConnect)
library(ggplot2)
library(ggmap)
library(lubridate)
library('splitstackshape')
library(stringi)


#0.Subset the US data from patref
US_data <- refdf[which(refdf$patenttype=='US'), ]

saveRDS(US_data, "US_data.rds") 

#1. push the df(refdf to mysql)
con <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="XXXXXX",
                 dbname="test", 
                 host="metadatalab.syr.edu")
dbWriteTable(con, value = refdf, name = "par_PatentRefence", append = TRUE )


#2. Parse the genbank.reference table in the same way I did refdf, 
#keep the id, title, journal, authororginal, author1,2,3,4,pubmed,year,reference

Reference<-dbGetQuery(con,'select * from genbank.Reference')
colnames(Reference)
colnames(parsedReference)



######################################
View(head(Reference))
parsedReference<-as.data.frame(Reference$id)
parsedReference$title<-Reference$title
parsedReference$reference<-Reference$reference
parsedReference$origauthors<-Reference$authors
parsedReference$consortium<-Reference$consortium
parsedReference$journal<-Reference$journal
parsedReference$pubmed<-Reference$pubmed
parsedReference$remark<-Reference$remark
parsedReference$year<-Reference$year
parsedReference$new_id<-Reference$new_id


#Parse authors
#eliminate"\t"
authorsplit<-gsub("\t", " ", parsedReference$origauthors, fixed=TRUE)
#eliminate "and"
authorsplit<-gsub(" and ", ", ", authorsplit, fixed=TRUE)
#split the split
authorsplit<-strsplit(authorsplit,", ")

#check the max number of authors...417???
max(unlist(lapply(authorsplit, function(x) length(x))))

#parsedReference$numofauthors<-unlist(lapply(authorsplit, function(x) length(x)))

#Write a for loop to populate the author columns
for (i in 1:max(unlist(lapply(authorsplit, function(x) length(x))))){
  eval(parse(text = paste0('parsedReference$Author', i, ' <- sapply(authorsplit,function(x) x[i])')))
}


#Push it to mysql
dbRemoveTable(con, name="par_Reference")

dbWriteTable(con, value = parsedReference, name = "par_Reference", append = TRUE )

###Finished Parsing#######################

#3. Format reference table in to disambiguous format (Reference$id,LN,FN,SEQ) (https://docs.google.com/document/d/1eknPTdimFJXg8Ezrkic3A3BOkbXkRQ83RCfs7HeLTYw/edit)
colnames(parsedReference)
parsedReference2<-parsedReference[,c(11:427)]
parsedReference2$id<-parsedReference$`Reference$id`
colnames(parsedReference2)

#stupid for loop strikes again
for (i in 1:417){
  eval(parse(text = paste0('dupertmp',i,'<- data.frame(Author=parsedReference2[,',i,'],
                           patentNo=parsedReference2$id, 
                           Seq=',i, ',stringsAsFactors=FALSE)')))
  eval(parse(text = paste0('dupertmp',i,'<-na.omit(dupertmp',i,')')))
}

#disambReference<-rbind
rm(dupertmp1,dupertmp10,dupertmp100,dupertmp101,dupertmp102,dupertmp103,
   dupertmp104,dupertmp105,dupertmp106,dupertmp107,dupertmp108,dupertmp109,
   dupertmp11,dupertmp110,dupertmp111,dupertmp112,dupertmp113,dupertmp114,
   dupertmp115,dupertmp116,dupertmp117,dupertmp118,dupertmp119,dupertmp12,
   dupertmp120,dupertmp121,dupertmp122,dupertmp123,dupertmp124,dupertmp125,
   dupertmp126,dupertmp127,dupertmp128,dupertmp129,dupertmp13,dupertmp130
   ,dupertmp131,dupertmp132,dupertmp133,dupertmp134,dupertmp135,dupertmp136,
   dupertmp137,dupertmp138,dupertmp139,dupertmp14,dupertmp140,dupertmp141,
   dupertmp142,dupertmp143,dupertmp144,dupertmp145,dupertmp146,dupertmp147,
   dupertmp148,dupertmp149,dupertmp15,dupertmp150,dupertmp151,dupertmp152,
   dupertmp153,dupertmp154,dupertmp155,dupertmp156,dupertmp157,dupertmp158,
   dupertmp159,dupertmp16,dupertmp160,dupertmp161,dupertmp162,dupertmp163,
   dupertmp164,dupertmp165,dupertmp166,dupertmp167,dupertmp168,dupertmp169,
   dupertmp17,dupertmp170,dupertmp171,dupertmp172,dupertmp173,dupertmp174,
   dupertmp175,dupertmp176,dupertmp177,dupertmp178,dupertmp179,dupertmp18,
   dupertmp180,dupertmp181,dupertmp182,dupertmp183,dupertmp184,dupertmp185,
   dupertmp186,dupertmp187,dupertmp188,dupertmp189,dupertmp19,dupertmp190,
   dupertmp191,dupertmp192,dupertmp193,dupertmp194,dupertmp195,dupertmp196,
   dupertmp197,dupertmp198,dupertmp199,dupertmp2,dupertmp20,dupertmp200,
   dupertmp201,dupertmp202,dupertmp203,dupertmp204,dupertmp205,dupertmp206,
   dupertmp207,dupertmp208,dupertmp209,dupertmp21,dupertmp210,dupertmp211,
   dupertmp212,dupertmp213,dupertmp214,dupertmp215,dupertmp216,dupertmp217,
   dupertmp218,dupertmp219,dupertmp22,dupertmp220,dupertmp221,dupertmp222,
   dupertmp223,dupertmp224,dupertmp225,dupertmp226,dupertmp227,dupertmp228,
   dupertmp229,dupertmp23,dupertmp230,dupertmp231,dupertmp232,dupertmp233,
   dupertmp234,dupertmp235,dupertmp236,dupertmp237,dupertmp238,dupertmp239,
   dupertmp24,dupertmp240,dupertmp241,dupertmp242,dupertmp243,dupertmp244,
   dupertmp245,dupertmp246,dupertmp247,dupertmp248,dupertmp249,dupertmp25,
   dupertmp250,dupertmp251,dupertmp252,dupertmp253,dupertmp254,dupertmp255,
   dupertmp256,dupertmp257,dupertmp258,dupertmp259,dupertmp26,dupertmp260,
   dupertmp261,dupertmp262,dupertmp263,dupertmp264,dupertmp265,dupertmp266,
   dupertmp267,dupertmp268,dupertmp269,dupertmp27,dupertmp270,dupertmp271,
   dupertmp272,dupertmp273,dupertmp274,dupertmp275,dupertmp276,dupertmp277,
   dupertmp278,dupertmp279,dupertmp28,dupertmp280,dupertmp281,dupertmp282,
   dupertmp283,dupertmp284,dupertmp285,dupertmp286,dupertmp287,dupertmp288,
   dupertmp289,dupertmp29,dupertmp290,dupertmp291,dupertmp292,dupertmp293,
   dupertmp294,dupertmp295,dupertmp296,dupertmp297,dupertmp298,dupertmp299,
   dupertmp3,dupertmp30,dupertmp300,dupertmp301,dupertmp302,dupertmp303,
   dupertmp304,dupertmp305,dupertmp306,dupertmp307,dupertmp308,dupertmp309,
   dupertmp31,dupertmp310,dupertmp311,dupertmp312,dupertmp313,dupertmp314,
   dupertmp315,dupertmp316,dupertmp317,dupertmp318,dupertmp319,dupertmp32,
   dupertmp320,dupertmp321,dupertmp322,dupertmp323,dupertmp324,dupertmp325,
   dupertmp326,dupertmp327,dupertmp328,dupertmp329,dupertmp33,dupertmp330,
   dupertmp331,dupertmp332,dupertmp333,dupertmp334,dupertmp335,dupertmp336,
   dupertmp337,dupertmp338,dupertmp339,dupertmp34,dupertmp340,dupertmp341,
   dupertmp342,dupertmp343,dupertmp344,dupertmp345,dupertmp346,dupertmp347,
   dupertmp348,dupertmp349,dupertmp35,dupertmp350,dupertmp351,dupertmp352,
   dupertmp353,dupertmp354,dupertmp355,dupertmp356,dupertmp357,dupertmp358,
   dupertmp359,dupertmp36,dupertmp360,dupertmp361,dupertmp362,dupertmp363,
   dupertmp364,dupertmp365,dupertmp366,dupertmp367,dupertmp368,dupertmp369,
   dupertmp37,dupertmp370,dupertmp371,dupertmp372,dupertmp373,dupertmp374,
   dupertmp375,dupertmp376,dupertmp377,dupertmp378,dupertmp379,dupertmp38,
   dupertmp380,dupertmp381,dupertmp382,dupertmp383,dupertmp384,dupertmp385,
   dupertmp386,dupertmp387,dupertmp388,dupertmp389,dupertmp39,dupertmp390,
   dupertmp391,dupertmp392,dupertmp393,dupertmp394,dupertmp395,dupertmp396,
   dupertmp397,dupertmp398,dupertmp399,dupertmp4,dupertmp40,dupertmp400,
   dupertmp401,dupertmp402,dupertmp403,dupertmp404,dupertmp405,dupertmp406,
   dupertmp407,dupertmp408,dupertmp409,dupertmp41,dupertmp410,dupertmp411,
   dupertmp412,dupertmp413,dupertmp414,dupertmp415,dupertmp416,dupertmp417,
   dupertmp42,dupertmp43,dupertmp44,dupertmp45,dupertmp46,dupertmp47,
   dupertmp48,dupertmp49,dupertmp5,dupertmp50,dupertmp51,dupertmp52,
   dupertmp53,dupertmp54,dupertmp55,dupertmp56,dupertmp57,dupertmp58,
   dupertmp59,dupertmp6,dupertmp60,dupertmp61,dupertmp62,dupertmp63,
   dupertmp64,dupertmp65,dupertmp66,dupertmp67,dupertmp68,dupertmp69,
   dupertmp7,dupertmp70,dupertmp71,dupertmp72,dupertmp73,dupertmp74,
   dupertmp75,dupertmp76,dupertmp77,dupertmp78,dupertmp79,dupertmp8,
   dupertmp80,dupertmp81,dupertmp82,dupertmp83,dupertmp84,dupertmp85,
   dupertmp86,dupertmp87,dupertmp88,dupertmp89,dupertmp9,dupertmp90,
   dupertmp91,dupertmp92,dupertmp93,dupertmp94,dupertmp95,dupertmp96,
   dupertmp97,dupertmp98,dupertmp99)

head(disambReference)

#Split the Author column
disambReference<-cSplit(disambReference,"Author",",",drop=FALSE)


colnames(disambReference)


# ##Why 83 of them have an Author_3 column? This is weird
# #Get rid of them!
# troubles<-disambReference[!is.na(disambReference$Author_3),]
# View(troubles[order(troubles$patentNo)])
# 
# Reference[Reference$id==troubles$patentNo,]
# 
# #split the original Reference table into troubled ones and normal ones
# troubledReference<-Reference[Reference$id %in% troubles$patentNo, ]
# normalReference<-Reference[!Reference$id %in% troubles$patentNo, ]
# 
# #Manually edit those troubled guys in Excel, it aint much
# write.csv(troubledReference,"troubledReference.csv")
# ?write.csv
# TroubleSolvedReference<-read.csv("troubledReferencesolved.csv",row.names = "X")
# 
# #Put it back
# Reference<-rbind(normalReference,TroubleSolvedReference)
# ReferenceFixed<-rbind(normalReference,TroubleSolvedReference)
# 
# #Run the whole parsing again (line31-157)



####Continue parsing the first name
View(head(disambReference))
#split on the first dot of author_2 column
disambReference$Author_2<-as.character(disambReference$Author_2)
fn<-as.data.frame(stri_split_fixed(str = disambReference$Author_2, pattern = ".", n = 2,simplify=TRUE))
disambReference$Author_2<-fn$V1
disambReference$MI<-fn$V2


###Finish Parsing
write.csv2(disambReference,"disambReference.csv")


#tolower disambReference
head(disambReference)
disambReference<-as.data.frame(sapply(disambReference,tolower))
disambReference$Author<-as.character(disambReference$Author)
disambReference$Lname<-as.character(disambReference$Lname)
disambReference$Fname<-as.character(disambReference$Fname)
disambReference$MI<-as.character(disambReference$MI)
disambReference$unique_id<-as.character(disambReference$unique_id)
disambReference$seq<-as.integer(as.character(disambReference$seq))

disambReference<-disambReference[order(disambReference$unique_id),]

dbWriteTable(con, value = disambReference, name = "disamb_Reference", append = TRUE )
