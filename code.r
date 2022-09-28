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
unique_order_m<-unique_order[1:500]
#6(e)
b<-b[unique_order_m]

