
#coauthor matching function
f <- function(x) {
  b<-sapply(X = x[[9]], FUN = function (X) { grep(X, x[[16]], ignore.case = TRUE, fixed = TRUE, perl=FALSE) }, simplify = "array")
  return (length(unlist(b)))
}
##function to combine coauthors of the same author in different patents
unioncoauthor1<-function(x) {
  return (union(x[[9]],x[[16]]))
}

#Table 1
#Sarah, Pan (this row can only be disambiguated if we run the within-table coauthor match)
#Sarah, Jeff
#Sarah, Jeff, Pan

#Table 2
#Sarah, Jeff


#within disambintlpatref table coauthor match 12/5/17
#tmp run
tmp<-disambintlpatref[1:50000,]
match1<-merge(tmp, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
match11<-merge(tmp, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
View(head(match11))
match1<-match1[match1$patent_no.x!=match1$patent_no.y,]
match1$nummatch<-apply(match1, 1, f)
View(match1)

tmp<-match1[1:16,]
tmp<-tmp[tmp$nummatch!=0,]
tmp

melt(tmp, id.vars=c("Author.x"))
dcast(tmp,"Author.x",value.var="")
melt()
View(tmp)











#these are the records that have both FL match and coauthor match, 
match1withmatch<-match1[match1$nummatch!=0,]

#to make union coauthor
union(match1withmatch$coauthor.x[[1]],match1withmatch$coauthor.y[[1]])
#to make union patent number
a<-c(match1withmatch$patent_no.x[1],match1withmatch$patent_no.y[1])

match1withmatch$coauthor<-apply(match1withmatch, 1, unioncoauthor1)

View(match1withmatch)


##############Chunk2
start_time <- Sys.time()
tmp<-disambintlpatref[50001:100000,]
match1<-merge(disambReference, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
match1$nummatch<-apply(match1, 1, f)
#these are the records that have both FL match and coauthor match, 
match2withmatch<-match1[match1$nummatch!=0,]
#these are the records that have both FML match but no coauthor match, 
#excluding those w/o mi (all of them have MI)
match2withoutcomatchbutwithfml<- match1[!(is.na(match1$MI.x) |is.na(match1$MI.y)| match1$MI.x=="" |match1$MI.y==""| match1$nummatch!=0), ]
step2_time-start_time
rm(match1)


##############Chunk3
start_time <- Sys.time()
tmp<-disambintlpatref[100001:200000,]
match1<-merge(disambReference, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
match1$nummatch<-apply(match1, 1, f)
#these are the records that have both FL match and coauthor match, 
match3withmatch<-match1[match1$nummatch!=0,]
#these are the records that have both FML match but no coauthor match, 
#excluding those w/o mi (all of them have MI)
match3withoutcomatchbutwithfml<- match1[!(is.na(match1$MI.x) |is.na(match1$MI.y)| match1$MI.x=="" |match1$MI.y==""| match1$nummatch!=0), ]
step2_time-start_time
rm(match1)

##############Chunk4
start_time <- Sys.time()
tmp<-disambintlpatref[200001:300000,]
match1<-merge(disambReference, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
match1$nummatch<-apply(match1, 1, f)
#these are the records that have both FL match and coauthor match, 
match4withmatch<-match1[match1$nummatch!=0,]
#these are the records that have both FML match but no coauthor match, 
#excluding those w/o mi (all of them have MI)
match4withoutcomatchbutwithfml<- match1[!(is.na(match1$MI.x) |is.na(match1$MI.y)| match1$MI.x=="" |match1$MI.y==""| match1$nummatch!=0), ]
step2_time-start_time
rm(match1)

##############Chunk5
start_time <- Sys.time()
tmp<-disambintlpatref[300001:397547,]
match1<-merge(disambReference, tmp, by.x = c("Fname","Lname"), by.y = c("Fname","Lname"), 
              all.x = FALSE, all.y = FALSE)
match1$nummatch<-apply(match1, 1, f)
#these are the records that have both FL match and coauthor match, 
match5withmatch<-match1[match1$nummatch!=0,]
#these are the records that have both FML match but no coauthor match, 
#excluding those w/o mi (all of them have MI)
match5withoutcomatchbutwithfml<- match1[!(is.na(match1$MI.x) |is.na(match1$MI.y)| match1$MI.x=="" |match1$MI.y==""| match1$nummatch!=0), ]
step2_time-start_time
rm(match1)

###put the chunks together
coauthormatch<-rbind(match1withmatch,match2withmatch,match3withmatch,match4withmatch,match5withmatch)
fmlmatchnocoauthor<-rbind(match1withoutcomatchbutwithfml,match2withoutcomatchbutwithfml,match3withoutcomatchbutwithfml,match4withoutcomatchbutwithfml,match5withoutcomatchbutwithfml)
View(head(fmlmatchnocoauthor))
View(head(coauthormatch))
nrow(coauthormatch)


#disambiguated patref records #141980 
tmp<-unique(coauthormatch[c('Author.y', "patent_no")])
#disambiguated Reference records #683789
tmp2<-unique(coauthormatch[c('Author.x', "unique_id")])

#fml patref records #72803
tmp3<-unique(fmlmatchnocoauthor[c('Author.y', "patent_no")])
#disambiguated Reference records #757904
tmp4<-unique(fmlmatchnocoauthor[c('Author.x', "unique_id")])


#####12/4 meeting

#send this to sarah
# match1withmatchforsarah<-match1withmatch
# match1withmatchforsarah$coauthor.x<-unlist(lapply(match1withmatchforsarah$coauthor.x, function(x) paste(shQuote(x,type="csh"), collapse=", ")))
# match1withmatchforsarah$coauthor.y<-unlist(lapply(match1withmatchforsarah$coauthor.y, function(x) paste(shQuote(x,type="csh"), collapse=", ")))
# 
# tmpdf<-match1withmatch[1:100,]
# tmplist<-match1withmatch$coauthor.x[1:100]
# tmplist2<-match1withmatch$coauthor.y[1:100]
# 
# tmpvector<-unlist(lapply(tmplist, function(x) paste(shQuote(x,type="csh"), collapse=", ")))
# tmpvector2<-unlist(lapply(tmplist2, function(x) paste(shQuote(x,type="csh"), collapse=", ")))
# 
# tmpdf$coauthor.x<-tmpvector
# tmpdf$coauthor.y<-tmpvector2
# write.csv(match1withmatchforsarah,"match1withmatchforsarah.csv")

tmp<-disambintlpatref[100001:200000,]
match2<-merge(disambReference, tmp, by.x = c("Fname","Lname","MI"), by.y = c("Fname","Lname","MI"), 
              all.x = FALSE, all.y = FALSE)
match2$nummatch<-apply(match2, 1, f)
match2withmatch<-match2[match2$nummatch!=0,]
match2withoutcomatch<- match2[!(is.na(match2$MI) | match2$MI=="" | match2$nummatch!=0), ]

View(head(match2))
tmp<-disambintlpatref[200001:300000,]
match3<-merge(disambReference, tmp, by.x = c("Fname","Lname","MI"), by.y = c("Fname","Lname","MI"), 
              all.x = FALSE, all.y = FALSE)
tmp<-disambintlpatref[300000:397547,]
match4<-merge(disambReference, tmp, by.x = c("Fname","Lname","MI"), by.y = c("Fname","Lname","MI"), 
              all.x = FALSE, all.y = FALSE)

View(match1)

tmp$
View(head(disambintlpatref))
View(head(disambReference))


#function to match the coauthor columns
tmp3<-match1[1:100,]



f2 <- function(x) {
  b<-sapply(X = x[[8]], FUN = function (X) { grep(X, x[[14]], ignore.case = TRUE, fixed = TRUE, perl=FALSE) }, simplify = "array")
  return (length(unlist(b)))
}

match1$nummatch<-apply(match1, 1, f)


View(match1[order(match1$nummatch,decreasing=TRUE),])
sum(match1$nummatch !=0)
#4059 fl matches found coauthor matches, out of 10000


#Apply this to every rows  
colnames(match1)

a<-
Reduce("+",unlist(sapply(X = tmp3[8], FUN = function (X) { grep(X, tmp3[15], ignore.case = TRUE) }, simplify = "array")))

tmp3[[1,8]]
class(tmp3$authorfl.x[1])
class(tmp3$authorfl.x[[1]])


tmp3[3,8]


colnames(match1)
x[8],x[15]

colnames(tmp)

#tmp2<-sapply(X = list.co.auths1[[1]], FUN = function (X) { grep(X, list.co.auths2[3], ignore.case = TRUE) }, simplify = "array")
#Reduce("+",unlist(tmp2))


tmplist

#Create coauthor list1
fifty.names <- c("Georgina","Gustavo","Simonne","Birgit","Santos","Jaymie","Jed","Stephaine","Felisha","Linn","Eddy","Sherwood","Katrice"
                 ,"Luella","Loreta","Verlie","Dirk","Candi","Pattie","Tomeka","Karry","Phebe","My","Roseline","Mae","Adelia","Audie"
                 ,"Yolanda","Charissa","Lupita","Neely","Mireya","Erich","Marcus","Melvina","Diana","Saturnina","Efrain","Mickie","Abram"
                 ,"Verda","Carmelina","Barb","Deon","Faustina","Nilsa","Geralyn","Heidy","John","Mercedes")


list.co.auths1[[1]]<-c("Melvina","Faustina","Luella")

list.co.auths2<-list()
list.co.auths2[[1]]<-"Melvina"
list.co.auths2[[2]]<-c("Melvina","Luella")
list.co.auths2[[3]]<-c("Melvina","Luella","Faustina")
list.co.auths2[[4]]<-c("Melvina","Lebron","Luella")
list.co.auths2[[5]]<-c("Lebron")

tmp<-sapply(X = list.co.auths1[[1]], FUN = function (X) { grep(X, list.co.auths2[[3]], ignore.case = TRUE) }, simplify = "array")

tmp<-sapply(X = list.co.auths1[[1]], FUN = function (X) { grep(X, list.co.auths2[[2]], ignore.case = TRUE) }, simplify = "array")
length(unlist(tmp))


#for documentation purposes
write.csv(head(disambReference)




