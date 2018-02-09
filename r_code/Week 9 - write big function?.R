#Author: Shrutik
library(stringdist)
library(reshape2)
library(data.table)

#using the first 100000 rows of disambintlpatref experiment
tmp
#in case tmp got messed up
tmp2<-tmp

a<-tmp
b<-disambReference

# This code works without data since we are defining a dummy dataset

# Packages:


fuzzy1(a,b)

a
b
###Fuzzy Function:
fuzzy1 <- function(a,b)
{
  #a1 = as.character(a$authors[1])
  #b1 = as.character(b$authors[grep("Talmadge",b$authors)][2])
  a1 = as.character(a)
  b1 = as.character(b)
  dist.a1b1<-adist(a1,b1, partial = TRUE, ignore.case = TRUE)
  distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
  dist.methods<-list()
  
  #Fuzz 1###########
  for(m in 1:length(distance.methods))
  {
    dist.name.enh<-matrix(NA, ncol = length(b1),nrow = length(a1))
    for(i in 1:length(b1)) {
      for(j in 1:length(a1)) { 
        dist.name.enh[j,i]<-stringdist(tolower(b1[i]),tolower(a1[j]),method = distance.methods[m])      
        #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
      }  
    }
    dist.methods[[distance.methods[m]]]<-dist.name.enh
  }
  
  match.s1.s2.enh<-NULL
  for(m in 1:length(dist.methods))
  {
    
    dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
    min.name.enh<-apply(dist.matrix, 1, base::min)
    for(i in 1:nrow(dist.matrix))
    {
      s2.i<-match(min.name.enh[i],dist.matrix[i,])
      s1.i<-i
      match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=b1[s2.i],s1name=a1[s1.i], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
    }
  }
  
  # Let's have a look at the results
  matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
  Fuzzy1 = matched.names.matrix
  
  return(Fuzzy1)
}

# Creating a dummy df:######
# Note: you can pass any dataframe here too, just make sure to use the same structure as these ones. Dataframe a should have the columns patent_no, Lname, Fname and seq. Dataframe b should have unique_id, Lname, Fname and seq. Also it is very important to use the same name nomenclature for both the dataframes. Either use Full Lname, Full Fname in both or Full Lname, Fname initial on both, this will define the accuracy of the algorithm.

a = data.frame( patent_no = c(1,1,1,2,3,4,5,6)
                ,Lname = c("roberts","evans","george","yu","bahl","stevens","Alyssia","Cecelia")
                ,Fname = c("A","L","A","F","G","M","A","A")
                ,seq = c(1,2,3,1,1,1,1,1))

b = data.frame( unique_id = c(1,1,2,3,4,5,6,7)
                ,Lname = c("Robert","Evans","micheal","watson","baxter","stevens","Elissa","Cecily")
                ,Fname = c("A","L","A","F","G","M","A","A")
                ,seq = c(1,2,1,1,1,1,1,1))

# Then convert the dataframes to tables since tables have better processing speeds in R and have a better memory utilization. It may not matter with a small dataset but with over 10k rows, this little trick will come in very handy.
a = data.table(a)
b = data.table(b)

# Structure for final author list:
merged_author_list_temp = data.frame(   patentAuthorLname = as.character(a$Lname[1])
                                        ,patentAuthorFname = as.character(a$Fname[1])
                                        ,refAuthorLname = as.character(b$Lname[1])
                                        ,refentAuthorFname = as.character(b$Fname[1])
                                        ,patentNumber = a$patent_no[1]
                                        ,unique_id = b$unique_id[1]
)
# Here we define the final list where we will put all the matched authors and this will form a connection bridge between the two tables. 
final_list = merged_author_list_temp[0,]
final_list = data.table(final_list)

# The main loop:
# This loop controls the row flow of the main dataset. Suppose dataframe a has 100 rows then this loop will run 100 times.
for(i in 1:length(a$patent_no))
{
  print(i) # Necessary only if you want to keep track of the progress.
  
  # First, we will take the main author of the Patent in dataframe a, and match it’s last name with all the last names pn dataframe b.
  argument1 = a$Lname[i]
  argument2 = b$Lname[i]
  result = fuzzy1(argument1,argument2) # First result
  df = result[0,] # For copying the df structure
  
  
  # For loop to check the matches between L_name in 1st table with all L_names in 2nd table
  # Here, we will loop through the entire length of dataframe b and try to match the last names with the current last name in dataframe a.
  for(j in 1:length(b$unique_id)) # Loops through b
  {
    argument2 = b$Lname[j]
    result = fuzzy1(argument1,argument2) # Stores all the cosine values of all the matches
    df = rbind(df,result)
  }
  
  # Now, since we only want similar last names:
  # Once we have all the values, we will filter them out to keep only the similar last names. Due to the errors in names and other problems we will set a threshold of 0.2 in this case. This means that we will pass all the author names which are somewhat similar too. For example, a threshold value of 0.2 will pass George and georg as same authors. You can change the value depending on the requirement.
  current_df = df[df$cosine<0.2,] # Filtering process.
  # Now, lets check if the first initial matches:
  # once we know that the last names are similar, we can move on to the first names/initials. We follow the same process to match the first initials.
  firstInitial_table1 = a$Fname[a$Lname == as.character(current_df$s1name[1])]
  firstInitial_table2 = b$Fname[b$Lname == as.character(current_df$s2name[1])]
  # Fuzzy match for Fnames having the same Lnames
  result_firstInitial_1 = fuzzy1(firstInitial_table1,firstInitial_table2) 
  
  # Now, if the match is good enough and there is just one entry in the table, we can say that the two authors from diff table are the same. Only one entry in the table means that there is an unique match and we do not need to match for co-authors. Here’s how we specify that in R:
  if(!is.na(result_firstInitial_1$cosine[1])&result_firstInitial_1$cosine[1]<0.2&  length(result_firstInitial_1$cosine)==1)
  {
    # Let's create a final merged author list, all the final matched authors will go into this list
    # Make sure to get the patent number and unique_id directly from the original dataframes. In fact, that is the only way to retrieve the ids, since we are matching just the strings, we need to use that to trace back and get the unique_id and patent number.
    
    merged_author_list = data.frame(  patentAuthorLname = as.character(current_df$s1name[1])
                                      ,patentAuthorFname = as.character(result_firstInitial_1$s1name[1])
                                      ,refAuthorLname = as.character(current_df$s2name[1])
                                      ,refentAuthorFname = as.character(result_firstInitial_1$s2name[1])
                                      ,patentNumber = a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                    a$Lname==as.character(current_df$s1name[1])]
                                      ,unique_id = b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                 b$Lname==as.character(current_df$s2name[1])]
    )
    l = list(final_list,merged_author_list)
    final_list = rbindlist(l)
    # Another important trick. Be sure to use list and rbindlist() for joining the rows since it is much faster than the original rbind(). Also, we are producing the final_list on the go using the rbindlist(), this means that we can reuse the same variables for each loop. For example, in the previous lines we use merged_author_list, which we can use for all the iterations, just reallocating the same memory for every use, this saves a ton of space.
  }
  # Now we get into the co-authors. This else represents that there are more than one author(s) with a similar lastname/first initial combination, so we move on to the co-authors.
  else #else if the same combination appears twice, we check for co-authors
  {
    # To get the co-author list, we simply take the authors with the same patent numbers or unique_ids
    # To get all the authors from table1 with same patent number
    currentPatentNumber = a$patent_no[a$Lname == as.character(argument1)]
    argument1_df = a[a$patent_no == currentPatentNumber,]
    argument1 = argument1_df$Lname[argument1_df$Lname != as.character(argument1)][1]
    # Same with table 2
    currentUnique_id = b$unique_id[b$Lname == as.character(argument2)]
    argument2_df = b[b$unique_id == currentUnique_id,]
    argument2 = argument2_df$Lname[argument2_df$Lname != as.character(argument2)][1]
    result1 = fuzzy1(argument1,argument2)
    df1 = result1[0,] # Again, For copying the df structure
    
    # Next step is similar to the previous ones. We match the lastnames, then firstnames and if those are similar and have more than one entry then we move onto the third author match and so on
    for(k in 1:length(argument2_df$unique_id))
    {
      argument2 = argument2_df$Lname[k]
      result1 = fuzzy1(argument1,argument2)
      df1 = rbind(df1,result1)
    }
    
    current_df = df1[df1$cosine<0.1,]
    firstInitial_table1=argument1_df$Fname[argument1_df$Lname==as.character(current_df$s1name[1])]
    firstInitial_table2=argument2_df$Fname[argument2_df$Lname==as.character(current_df$s2name[1])]
    result_firstInitial_1 = fuzzy1(firstInitial_table1,firstInitial_table2) 
    
    
    if(!is.na(result_firstInitial_1$cosine[1])&result_firstInitial_1$cosine[1]<0.1 &
       length(result_firstInitial_1$cosine) == 1)
    {
      #print ("SECOND IF!!!") # for checking purposes
      merged_author_list1 = data.frame(  patentAuthorLname = as.character(current_df$s1name[1])
                                         ,patentAuthorFname = as.character(result_firstInitial_1$s1name[1])
                                         ,refAuthorLname = as.character(current_df$s2name[1])
                                         ,refentAuthorFname = as.character(result_firstInitial_1$s2name[1])
                                         ,patentNumber = a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                       a$Lname==as.character(current_df$s1name[1])]
                                         ,unique_id = b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                    b$Lname==as.character(current_df$s2name[1])]
      )
      l1 = list(final_list,merged_author_list1)
      final_list = rbindlist(l1)
      
    }
    else
    {
      print("TBD") # this is where the third author match is done, which was included in the later versions.
    }
    
  }#else end
  
} # Main for loop end

tmp<-head(disambintlpatref,1000)
unique(c(tmp$Lname,tmp$Fname))

class(table(tmp$patent_no))
as.vector(table(tmp$patent_no))




View(tmp)
