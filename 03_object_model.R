#библиотеки
library(imager)

#параметры
fone <- "Data//a38606.jpg"
pict <- "Data//a38505 - 2.jpg"
Hroom <- 2700 #mm



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

wall.addTexture <- function(wall,pic,sc=1){
        #заполняет wall картинкой pic
        
        r <- resize(im = pic,
                    size_x = round(dim(pic)[1]*sc,0),
                    size_y = round(dim(pic)[2]*sc,0))
        newdim <- dim(r)
        
        for (m in (1:(round(dim(wall)[1]/newdim[1],0)))) {
              for (n in (1:(round(dim(wall)[2]/newdim[2],0))))  {
                      wall <- imdraw(wall,r,(m-1)*newdim[1]+1,(n-1)*newdim[2]+1)
                      
              }
                
        }
        return(wall)
        
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

######скрипт 1  вариант -> простое добавление цветных обоев########
flw <- load.image(pict)
back <- load.image(fone)
ww <- wall.create(800,600)
ww <- wall.addTexture(ww,back,640/Hroom)
coords <- wall.grid.mm(800/2500)

A4px <- c(round(29.7*(923/106),0),round(21.0*(923/106),0))
ww <- load.image(fone)
        for (i in (1:nrow(coords))){
                #выбираем случайные координаты с цветных обоев
                coord_temp <- c(sample(1:(dim(flw)[1]-A4px[1]),1),sample(1:(dim(flw)[2]-A4px[2]),1)) 
                flw_temp <- as.cimg(flw[coord_temp[1]:(coord_temp[1]+A4px[1]),coord_temp[2]:(coord_temp[2]+A4px[2]),,])
                #ставим их на стену
                ww <- wall.addImg(ww,flw_temp,coords[i,1],coords[i,2],wall.scale(297),wall.scale(210))
                ww <- wall.addRect(ww,coords[i,1],coords[i,2],wall.scale(297),wall.scale(210),3,"brown")
                        }
save.image(ww,"test.png")

 