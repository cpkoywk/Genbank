#####Make coauthor column
########TEST RUN###########################
#grab 10 rows from disambintlpatrefnosolo(ordered by patent_no)
tmp<-head(disambintlpatref[order(disambintlpatref$patent_no),],100)
tmptmp<-head(disambReference[order(disambReference),],100)

View(tmp)

######Hemsley's super code to generate the coauthor column for disambReference table
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(disambReference$unique_id == disambReference$unique_id[x])
  authors <- disambReference$authorfl[index]
  authors <- authors[-which(authors == disambReference$authorfl[x] )]
  l <- list(authors)
  l
}

# this line calls the function for each line in the file
co.auth.list <- lapply(1:dim(disambReference)[1], FUN.co.auth.list)

#how much time this takes?
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time


#1-999999
co.auth.list1<-co.auth.list[1:999999]

#1000000-1999999
tmp<-disambReference[1000000:1999999,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list2 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time


#2000000-2999997
tmp<-disambReference[2000000:2999997,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list3 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time

#2999998-4000001
tmp<-disambReference[2999998:4000001,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list4 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time

#4000002-5000007
tmp<-disambReference[4000002:5000007,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list5 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time


#5000008-6000010
tmp<-disambReference[5000008:6000010,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list6 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time

#6000011-7000196
tmp<-disambReference[6000011:7000196,]
FUN.co.auth.list <- function(x) {
  # x <- 83 + 1
  index <- which(tmp$unique_id == tmp$unique_id[x])
  authors <- tmp$authorfl[index]
  authors <- authors[-which(authors == tmp$authorfl[x] )]
  l <- list(authors)
  l
}
start_time <- Sys.time()
co.auth.list7 <- lapply(1:dim(tmp)[1], FUN.co.auth.list)
end_time <- Sys.time()
end_time - start_time

View(head(disambintlpatref))

#combine all these co.auth.lists
co.auth.list<-c(co.auth.list1,co.auth.list2,co.auth.list3,co.auth.list4,co.auth.list5,co.auth.list6,co.auth.list7)
co.auth.list<-unlist(co.auth.list, recursive=FALSE)
head(co.auth.list)
#append the list to where it belongs
colnames(disambintlpatref)
disambReference$coauthor<-co.auth.list
colnames(disambReference)


