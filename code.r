##Group Members:
##Changliang Wei S2345097
##Simin Wang S2417531
##Zheng Wang S2283040

##Contribution
##The contribution to this project is roughly equivalent
##Changliang Wei contributs more in Q10, Simin Wang and Zheng Wang contribute more on the comments

setwd("/Users/wangzheng/Desktop/project1/group_project_1")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers


###########################
##4
#separate the punctuation marks ","
split_punct <- function (x,a) {
  punct_loc <- grep(x, a, fixed=TRUE)
  delet_list <- gsub(x,"", a, fixed=TRUE)
  empty_vec<-rep("",length(punct_loc)+length(a))
  i<-punct_loc+1:length(punct_loc)
  empty_vec[i]<-x
  empty_vec[-i]<-delet_list
  empty_vec
}


##5
#separate the punctuation marks ","
a1 <- split_punct(",",a)

#separate the punctuation marks "."
a2 <- split_punct(".",a1)

#separate the punctuation marks ";"
a3 <- split_punct(";",a2)

#separate the punctuation marks ":"
a4 <- split_punct(":",a3)

#separate the punctuation marks "!"
a5 <- split_punct("!",a4)

#separate the punctuation marks "?"
a6 <- split_punct("?",a5)


##6
#6(a)
#find the vector of unique words
lower_a<-tolower(a6) #replace capital letters in words with lower case letters
b<-unique(lower_a)

#6(b)
#find Bible text indicies in the unique word vector b
index_a_in_b <- match(lower_a,b)

#6(c)
#find the vector of frequency of unique words from b in Bible text
unique_frequence <- tabulate(index_a_in_b)

#6(d)
#find the 500 most common words
unique_order<-order(-unique_frequence) #sort the unique_frequence in decresing order
m<-500
unique_order_m<-unique_order[1:m] # vector of indicies of the 500 most common words

#6(e)
#create the 500 most commonly occurring words
b<-b[unique_order_m]


##7
#7(a)
#find Bible text indicies in the 500 most common words b
d<-match(lower_a,b)

#7(b)
#create a matrix with the first column of the index of common words, second column of the index of following word, and the third coloumn of the index of the following word shift one place
len_lower_a <- length(lower_a) # length of lower_a
matrix<-cbind(d[1:(len_lower_a-2)],d[1:(len_lower_a-1)][-1],d[-1][-1]) #remove the last two rows

#7(c)
#drop word triplets that contain an NA
sum_matrix<-rowSums(is.na(matrix)) # identify NA
nona_row_index<-which(sum_matrix==0) # find the index of row without NA
matrix<-matrix[nona_row_index,] # set the matrix by droping the row with NA

#7(d)
#create maritx T
T<-array(0,dim=c(m,m,m)) # (initialize T) create a zero matrix with dimension of m*m*m

#adding a 1 to T[i,k,j] every time the jth common word follows the pair i,k
for (i in (1:dim(matrix)[1])){
  T[matrix[i,1],matrix[i,2],matrix[i,3]] <- T[matrix[i,1],matrix[i,2],matrix[i,3]] +1
}

#7(e)

#7(f)
#create a matrix A with the first column of the index of common words, second column of the index of following word
matrix2<-cbind(d[1:(length(lower_a)-1)],d[-1])
sum_matrix2<-rowSums(is.na(matrix2)) # identify NA
nona_row_index2<-which(sum_matrix2==0) # find the index of row without NA
matrix2<-matrix2[nona_row_index2,] # set the matrix2 by droping the row with NA
A <- array(0,dim = c(m,m)) # (initialize A) create a zero matrix with dimension of m*m

# adding a 1 to A[i,j] every time the jth common word follow i
for (i in (1:dim(matrix2)[1])){
  A[matrix2[i,1],matrix2[i,2]] <- A[matrix2[i,1],matrix2[i,2]]+1
}

# create a matrix S with the index of common words
matrix3<-d
nona_row_index3<-which(is.na(matrix3)==FALSE) # identify NA and find the index of row without NA
matrix3<-matrix3[nona_row_index3] # set the matrix3 by droping the row with NA
S<-rep(0,m) # (initialize S) create a zero matrix with dimension of m

# adding a 1 to S[i] every time the ith common word is never followed by a word in b
for (i in (1:length(matrix3))){
  S[matrix3[i]] <- S[matrix3[i]]+1
}


##8
# simulate 50 words using full model
first_word_index <- sample(m,1,prob=S) # find the index of first word 

# find the index of second word
if (sum(A[first_word_index,]) == 0){ # check if the first word has a following word
  second_word_index <- sample(m,1,prob=S) # no following word, choose a new word with probability S
}else{
  second_word_index <- sample(m,1,prob=A[first_word_index,]) # has a following word, choose the next word with probability A
} 

# find the rest of 50 words' indecies
pairs_index <- rep(0,50)
pairs_index[1] <- first_word_index
pairs_index[2] <- second_word_index
for (i in (3:50)){
  if (sum(T[pairs_index[i-2],pairs_index[i-1],]) == 0){ 
    if (sum(A[pairs_index[i-2],]) == 0){ 
      pairs_index[i] <- sample(m,1,prob=S) 
    }else{ 
      pairs_index[i] <- sample(m,1,prob=A[pairs_index[i-2],]) 
    }
  }else{
    pairs_index[i] <- sample(m,1,prob=T[pairs_index[i-2],pairs_index[i-1],])
  }
}  

# print the 50 words
words <- cat(b[pairs_index])


##9
# simulate 50 words with probability is S
new_pairs_index <- rep(0,50)
for (i in (1:50)){
  new_pairs_index[i] <- sample(m,1,prob=S)
}
words_from_s <- cat(b[new_pairs_index])


##10
# modify the version of b and repeat the process of Q8
upper_words <- a6[which(is.na(match(a6,lower_a)))] # find the capital word in the main text
lower_upper_words <- tolower(upper_words) # transform upper case to lower case
new_b <- b
for (i in (1:m)){
  if (length(which(match(lower_upper_words,b[i])>0)) >= S[i]/2) { # jusify if the word most often start with a capital letter in the main text
    substr(new_b[i],1,1) <- toupper(substr(new_b[i],1,1)) # transform the first letter to capital
  }
}

# repeat the process of Q8
first_word_index <- sample(m,1,prob=S)

if (sum(A[first_word_index,]) == 0){
  second_word_index <- sample(m,1,prob=S)
}else{
  second_word_index <- sample(m,1,prob=A[first_word_index,])
} 

pairs_index_ten <- rep(0,50)
pairs_index_ten[1] <- first_word_index
pairs_index_ten[2] <- second_word_index
for (i in (3:50)){
  if (sum(T[pairs_index_ten[i-2],pairs_index_ten[i-1],]) == 0){
    if (sum(A[pairs_index_ten[i-2],]) == 0){
      pairs_index_ten[i] <- sample(m,1,prob=S)
    }else{
      pairs_index_ten[i] <- sample(m,1,prob=A[pairs_index_ten[i-2],])
    }
  }else{
    pairs_index_ten[i] <- sample(m,1,prob=T[pairs_index_ten[i-2],pairs_index_ten[i-1],])
  }
}  

words_ten <- cat(new_b[pairs_index_ten])






