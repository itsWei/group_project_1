setwd("/Users/wangzheng/Desktop/project1/group_project_1")
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers


###########################
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
a1<-split_punct(",",a)

#separate the punctuation marks "."
a2<-split_punct(".",a1)

#separate the punctuation marks ";"
a3<-split_punct(";",a2)

#separate the punctuation marks ":"
a4<-split_punct(":",a3)

#separate the punctuation marks "!"
a5<-split_punct("!",a4)

#separate the punctuation marks "?"
a6<-empty_vec <- split_punct("?",a5)

#6(a)
lower_a<-tolower(a6)
b<-unique(lower_a)
#6(b)
match(lower_a,b)
#6(c)
tabulate(match(lower_a,b))
#6(d)
unique_order<-order(-tabulate(match(lower_a,b)))
m<-500
unique_order_m<-unique_order[1:m]
#6(e)
b<-b[unique_order_m]
#7(a)
d<-match(lower_a,b)
#7(b)
matrix<-cbind(d[1:(length(lower_a)-2)],d[1:(length(lower_a)-1)][-1],d[-1][-1])
#7(c)
sum_matrix<-rowSums(is.na(matrix))
nona_row_index<-which(sum_matrix==0)
matrix<-matrix[nona_row_index,]
#7(d)
matrix_t<-array(0,dim=c(m,m,m))
matrix_t <- array(0,dim = c(m,m,m))

for (i in (1:dim(matrix)[1])){
  matrix_t[matrix[i,1],matrix[i,2],matrix[i,3]] <- matrix_t[matrix[i,1],matrix[i,2],matrix[i,3]] +1
}
#7(e)
#7(f)
matrix2<-cbind(d[1:(length(lower_a)-1)],d[-1])
sum_matrix2<-rowSums(is.na(matrix2))
nona_row_index2<-which(sum_matrix2==0)
matrix2<-matrix2[nona_row_index2,]
matrix_a<-array(0,dim=c(m,m))
matrix_a <- array(0,dim = c(m,m))

for (i in (1:dim(matrix2)[1])){
  matrix_a[matrix2[i,1],matrix2[i,2]] <- matrix_a[matrix2[i,1],matrix2[i,2]]+1
}
#####
matrix3<-d
nona_row_index3<-which(is.na(matrix3)==FALSE)
matrix3<-matrix3[nona_row_index3]
matrix_s<-rep(0,m)


for (i in (1:length(matrix3))){
  matrix_s[matrix3[i]] <- matrix_s[matrix3[i]]+1
}




