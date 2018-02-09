#Find all patents with only one author
soloparreflist1<-table(disambintlpatref$patent_no)[table(disambintlpatref$patent_no)==1]

#Find all the authors of patents with only one author
soloparrefauthor<-subset(disambintlpatref, patent_no%in% names(soloparreflist1))
soloparrefauthor<-as.data.frame(soloparrefauthor)

View(soloparrefauthor)

#Find all patents with more than one author
soloparreflist2<-table(disambintlpatref$patent_no)[table(disambintlpatref$patent_no)>1]
soloparrefauthor2<-as.data.frame(subset(disambintlpatref, patent_no%in% names(soloparreflist2)))

#Find solo authors
finalsololist<-subset(soloparrefauthor, !Author %in% soloparrefauthor2$Author)
View(finalsololist)
nrow(finalsololist)
#3495 (rows) in disambintlpatref have worked on solo projects only (with absolutely no co-authors)

write.csv(finalsololist,file="soloAuthors.csv")

###Make a disambigintlref dataset without the solo authors
disambintlpatrefnosolo<-subset(disambintlpatref, !disambintlpatref$Author %in% finalsololist$Author)
dbWriteTable(con, value = disambintlpatrefnosolo, name = "disamb_IntlPatentReferenceNOSolo", append = TRUE )








#X is disambReference, Y is finalsololist
#Match Fname and Lname
#All the subsets should add up to 18898 after all the subsetting to ensure nothing is lost
Solomatch1<-merge(disambReference, finalsololist, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
                  all.x = FALSE, all.y = TRUE)
View(Solomatch1)

head(finalsololist)

View(Solomatch1)
write.csv(Solomatch1,"Pan_Stuff/Disambisolo/Solomatch1.csv")

solomatchna<-subset(Solomatch1, is.na(Solomatch1$Author.x))
solomatchfl<-na.omit(Solomatch1)
View(solomatchna)
#Match Fname Lname and MI
#solomatchlfm<-merge(disambReference, finalsololist, by.x = c("Fname","Lname","MI"), by.y = c("Fname","Lname","MI"), all.x = FALSE, all.y = FALSE)
solomatchfml<-Solomatch1[Solomatch1$MI.x==Solomatch1$MI.y,]
solomatchfml<-na.omit(solomatchfml)
View(solomatchfml)

#Fname and Lname match but not MI: These are the guys that LName and FName match, but the MI does not match
#Sometimes it blank in X but not blank in Y, sometimes vice versa, sometimes they both have values, they just don't match
#solomatchdisamb<-solomatchlf[!solomatchlf$unique_id %in% solomatchlfm$unique_id,]
solomatchdisamb<-Solomatch1[Solomatch1$MI.x!=Solomatch1$MI.y,]
solomatchdisamb<-na.omit(solomatchdisamb)
Solomatch1
View(solomatchdisamb)
View(Solomatch1)

#MI absolutely don't match (both have MI values, but they are different values):
solomatchdisamb[solomatchdisamb==""] <- NA
solomatchflmatchminomatch<-na.omit(solomatchdisamb)

solomatchdisamb2<-solomatchdisamb[rowSums(is.na(solomatchdisamb)) > 0,]


#Final results:
#Either Ref or PatRef has NA in MI
View(solomatchdisamb2) #126509
write.csv(solomatchdisamb2,"Pan_Stuff/Disambisolo/solomatchMINA.csv")
#Lname, Fname match, but MI don't (no NA)
View(solomatchflmatchminomatch) #58950
write.csv(solomatchflmatchminomatch,"Pan_Stuff/Disambisolo/solomatchflmatchminomatch.csv")

#Lname, Fname, MI all match

View(solomatchfml) #25237
nrow(solomatchflm[solomatchflm$MI.x!="",])
#solo authors in Patref that don't have a match in Reference table
View(solomatchna) #1430
write.csv(solomatchna,"Pan_Stuff/Disambisolo/solomatchna.csv")

length(unique(solomatchflm$unique_id))
nrow(solomatchflm)
View(solomatchflm)

#CHECK IF THE ROW NUMBERS MATCH
nrow(solomatchdisamb2)+nrow(solomatchflmatchminomatch)+nrow(solomatchflm)+nrow(solomatchna)

#test.SoloAuthorIntPat write table
dbWriteTable(con_write, "solomatchfmlintl", solomatchflm)

dbWriteTable(con_write, "US_data", US_data)

#update the google doc scenario ad bullet point pointing out the weakness of each scenario (XXX/YYY)
#coauthor->take a subset of ppl who have coauthors who have lNAME,fName,MI (I don't understand)


#maybe try to find a third variable for validation for the FML solo matches 
tmp1<-dbGetQuery(con,'SELECT * FROM test.par_PatentRefence LIMIT 1000')
tmp2<-dbGetQuery(con,'SELECT * FROM test.par_Reference LIMIT 1000')
#It doesn't look like we have a third variables from these tables
View(tmp2)

#isolating the names that have at least one ref with a co-author
#with a fml match.
disambintlpatrefAll<-read.csv('Pan_Stuff/Disambigintlpatref/disambintlpatrefAll.csv',stringsAsFactors = FALSE)


#result is weird

disambReference[disambReference$unique_id=="333573001",]
disambReference
tmp[tmp$patent_no=="9002557",]
disambintlpatrefwosolowona[disambintlpatrefwosolowona$patent_no=="9002557",]


tmp[duplicated(tmp),]
xxx<-disambintlpatrefAll[!duplicated(disambintlpatrefAll),]
View(head(xxx))
nrow(xxx)
View(head(disambintlpatrefAll,1000))
nrow(tmp[duplicated(tmp),])
View(tmp)

View(head(disambintlpatrefAll,1000))
colnames(disambintlpatrefwosolowona)
colnames(disambReference)




#intldisambig without those 324 guys
head(disambintlpatref)

finalsololist$seq<-as.integer(as.character(finalsololist$seq))

disambintlpatref2<-disambintlpatref[ !(disambintlpatref$Author %in% finalsololist$Author), ]
#Find all the rows without authors
noauthors<-disambintlpatref2[disambintlpatref2$Author==".", ]
write.csv(noauthors,file="noauthors.csv")


#Find all the rows with authors
disambintlpatref2<-disambintlpatref2[disambintlpatref2$Author!=".", ]
#Make a backup of it
write.csv(disambintlpatref2,file="disambintlpatrefwosolowona.csv")



disambintlpatref
#######Write big function################

#take first 100000 rows for experiment
tmp<-disambintlpatref2[1:100000,]
View(tmp)

str(finalsololist)
str(disambintlpatref)

View(head(finalsololist))
finalsololist[finalsololist$Author=="gall,j.",]
head(tmp)
nrow(tmp)
View(head(tmp,100))
View(finalsololist)

  
  
finalsololist[i,1]
disambReference

class(tmp$patent_no)
names(tmp1)

intlpatref[intlpatref$patentNumber=="9005142",]

#Filter these authors who are in the Patents with more than one authors

tmp2<-table(tmp$patent_no)[table(tmp$patent_no)>1]
