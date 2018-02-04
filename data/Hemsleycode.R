
fifty.names <- c("Georgina","Gustavo","Simonne","Birgit","Santos","Jaymie","Jed","Stephaine","Felisha","Linn","Eddy","Sherwood","Katrice"
                 ,"Luella","Loreta","Verlie","Dirk","Candi","Pattie","Tomeka","Karry","Phebe","My","Roseline","Mae","Adelia","Audie"
                 ,"Yolanda","Charissa","Lupita","Neely","Mireya","Erich","Marcus","Melvina","Diana","Saturnina","Efrain","Mickie","Abram"
                 ,"Verda","Carmelina","Barb","Deon","Faustina","Nilsa","Geralyn","Heidy","John","Mercedes")

# first, lets make a random list of vectors of names
num.list.items <- 10
list.auths <- sample(fifty.names, size = num.list.items)
list.auths
list.co.auths <- lapply(1:num.list.items, function(x) sample(fifty.names, size = sample(2:8, size = 1)))
list.co.auths

list.auths1 <- c("Georgina","Georgina","Georgina","Georgina","Georgina","Georgina","Georgina","Georgina","Georgina","Georgina")

list.tmp<-list()
list.tmp[[1]]<-c("Jaymie", "Deon")
list.tmp[[2]]<-"Jaymie"
list.tmp[[3]]<-"Lebron"


#If two list has matchjupyj
grep(list.tmp[[1]],list.co.auths[[1]])
tmp2<-sapply(X = list.co.auths[[1]], FUN = function (X) { grep(X, list.tmp[1], ignore.case = TRUE) }, simplify = "array")

Reduce("+",unlist(tmp2))

grep(list.tmp[[2]],list.co.auths[[1]])



for (i in 1:num.list.items) {
  # now find those co-auths in a bigger list and get the index of their positions
  indexes <- sapply(X = list.co.auths[[i]], FUN = function (X) { grep(X, fifty.names, ignore.case = TRUE) }, simplify = "array")
  print(paste0("Auth (", i, "): ", list.auths[i], ":: co-Auths: ", paste(list.co.auths[[i]], collapse = ","), ":: Matched Names: ", paste(fifty.names[indexes], collapse = ",")))
  # now we might do something with the matched values
}  


# make a function that does some work. 
# this could all fit on one line, but it would be hard to read
# the function finds all co-auths for a given author and 
# returns them as a list after removing the author
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(df$unique_id == df$unique_id[x])
  authors <- df$authorfl[index]
  authors <- authors[-which(authors == gsub("\\..*","",df$Author[x]) )]
  l <- list(authors)
  l
}

df<-read.csv("sample.csv",stringsAsFactors = FALSE,row.names =1)


# make a function that does some work. 
# this could all fit on one line, but it would be hard to read
# the function finds all co-auths for a given author and 
# returns them as a list after removing the author
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(df$unique_id == df$unique_id[x])
  authors <- df$authorfl[index]
  authors <- authors[-which(authors == df$authorfl[x] )]
  l <- list(authors)
  l
}


# this line calls the function for each line in the file
co.auth.list <- lapply(1:dim(df)[1], FUN.co.auth.list)
co.auth.list2<-lapply(co.auth.list,unlist)

df$coauthor<-co.auth.list2
View(df)

gsub("\\..*","",df$Author[1])
df[6,]

class(df$unique_id)

df$Author[4]
gsub("\\..*","",df$Author[4])

