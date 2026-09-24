
#------------------------------------------------#
# Carrega e retorna todas as imagens             #
#------------------------------------------------#
read_images <- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = TRUE)
  all_im <- lapply(name_imgs, load.image)
  names(all_im) <- name_imgs
  return(all_im)
}

#------------------------------------------------#
# Exibe a classe de cada imagem (time)           #
#------------------------------------------------#
get_classes<- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = FALSE)
  classes<-NULL
  for(name in name_imgs){
    name<-strsplit(name, '_')
    classes<-cbind(classes,name[[1]][1])
  }
  return(classes)
}

#------------------------------------------------#
# Retorna ground_truth escolhida classe relevante#
#------------------------------------------------#
get_ground_truth<- function(path_soccer, classes, classe_relevante){
  ground_truth <- integer(length(classes))
  ground_truth[which(classes %in% classe_relevante)] <-1
  names(ground_truth) <- list.files(path_soccer, full.names = TRUE)
  return(ground_truth)
}

#------------------------------------------------#
# Mostra imagem                                  #
#------------------------------------------------#
mostrarImagemColorida <- function(path_img, nome=''){
  path_img <- as.character(path_img)
  img <- load.image(path_img)
  return(plot(img, axes = FALSE, main = nome))
}