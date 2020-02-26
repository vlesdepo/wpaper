#библиотеки
library(imager)

#конструктор
wall.create <- function(m,n){
     r <- cimg(array(1,c(m,n,3,1)))
     attr(r,"class") <- c("wall",class(r))
     return(r)
}

wall.clean <- function(w){
     
     
}
######ТЕСТЫ#########
ww <- wall.create(100,200)
