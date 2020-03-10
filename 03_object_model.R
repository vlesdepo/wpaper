#библиотеки
library(imager)

#параметры
fone <- "Data//a38606.jpg"
pict <- "Data//a38505.jpg"



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

        t <- wall[(dx+db):(dx + dw - db),(dy + db):(dy + dh - db),,]
        r <- draw_rect(wall,dx,dy,dx+dw,dy+dh,color=col,filled = T)
        r[(dx+db):(dx + dw - db),(dy + db):(dy + dh - db),,] <- t
        
        return(r)
} 

wall.grid.mm <- function(sc) {
        # sc - scale, масштаб
        stX <- 100 #отступ слева
        stY <- 100 #отступ сверху
        s <- 100 #ширина между картинами
        Xgen <- stX+(297+s)*(0:3)
        Ygen <- stY+(210+s)*(0:3)
        m <- expand.grid(Xgen,Ygen)
        m <- sapply(m,function(x){x*sc})
        m <- round(m,0)
        m <- as.data.frame(m)
        m[c(8,11,12,14,15,16),] <- NA
        m <- na.omit(m)
        return(m)
} #исходные данные

wall.scale <-function(x,k=800/2500){
        r <- round(x*k,0)
        return(r)
        
}

wall.addImg <- function(wall,img,x,y,wx,hy){
        imt <- resize(im = img,size_x = wx,size_y = hy)
        r <- imdraw(wall,imt,x,y)
        return(r)
}

######скрипт########
ww <- wall.create(800,600)
coords <- wall.grid.mm(800/2500)
flw <- load.image(pict)
ww <- load.image(fone)
        for (i in (1:nrow(coords))){
                ww <- wall.addRect(ww,coords[i,1],coords[i,2],wall.scale(297),wall.scale(210),3,"brown")
                ww <- wall.addImg(ww,flw,coords[i,1],coords[i,2],wall.scale(297),wall.scale(210))
        }
                                 
                                 

save.image(ww,"test.png")

        
######ТЕСТЫ#########
ww <- wall.create(800,600)
ww <- wall.clean(ww)
ww <- wall.addBar(ww,12,15,70,45,col="red")
ww <- wall.addRect(ww,12,35,70,45,3,col="green")
save.image(ww,"test.png")
ww1 <- imsub(ww, x > 10, x < 75, y > 10, y < 75)
apply(coords,1,function(x){cat(x[1], x[2],"; \n")})
