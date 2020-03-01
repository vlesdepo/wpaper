#библиотеки
library(imager)

#конструктор
wall.create <- function(m,n){
     r <- cimg(array(1,c(m,n,1,3)))
     attr(r,"class") <- c("wall",class(r))
     return(r)
}

wall.clean <- function(w){
     w[,,,] <- 1
     attr(w,"class") <- c("wall",class(w))
     return(w)
}

wall.addBar <- function(wall,x,y,w,h,col){
        r <- draw_rect(wall,x,y,x+w,y+h,color=col,filled = T)
        return(r)
}

wall.addRect <- function(wall,dx,dy,dw,dh,db,col){
        cat(dx,dy,"\n")
        cat(dx + db,dx + dw - db,"\n")
        cat(dy + db, dy + dh - db,"\n")
        
        t <- imsub(wall, x > (dx + db), x < (dx + dw - db),
                        y > (dy + db), y < (dy + dh - db))
        cat(dim(t))
        cat(dx,dy,"\n")
        r <- draw_rect(wall,dx,dy,dx+dw,dy+dh,color=col,filled = T)
        r <- imdraw(r,t,x = (dx + db),
                        y = (dy + db))
        
        return(r)
} 
        
######ТЕСТЫ#########
ww <- wall.create(100,200)
ww <- wall.clean(ww)
ww <- wall.addBar(ww,12,12,70,70,col="red")
ww <- wall.addRect(ww,12,12,70,70,3,col="red")
save.image(ww,"test.png")
ww1 <- imsub(ww, x > 10, x < 75, y > 10, y < 75)
