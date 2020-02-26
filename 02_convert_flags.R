#конвертируем картинки флагов в маски
#
#библиотеки
library(OpenImageR)

#параметры
folderInput <- "Data//flags//" #входная папка
folderOutput <- "Data//flags_mask//"

# читаем список файлов
fls <- list.files(folderInput,recursive = T,include.dirs = T,pattern = "*.png*",full.names = F)


#обработка
for (f in fls) {
     cat("Обработка ", f,"\n")
     img <- readImage(paste0(folderInput,f),info=F)
     img <- rgb_2gray(img)
     d <- dim(img)
     img[which(img < 0.7)] <- 0
     img[which(img > 0)] <- 1
     dim(img) <- d
     writeImage(img,paste0(folderOutput,f))
}

rm(d,f,fls,folderInput,folderOutput,img)
