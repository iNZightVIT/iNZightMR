
prototype <- function(obj,...) {
  
  UseMethod("prototype")
  
}

prototype.mrocalc <- function(obj,...){

    stop("No longer supported.")
  
  ## if(!"package:ggplot2" %in% search()){
    
  ##   stop("require ggplot2 package")
    
  ## }
  ## else{
    
    
  ##   s1 <- switcher(obj)
  ##   print(s1)
  ##   s1$ErrBars <- validateRange2(s1$ErrBar)
  ##   s1$confL <- validateRange2(s1$confL)
  ##   s1$confU <- validateRange2(s1$confU)
  ##   s1$compL <- validateRange2(s1$compL)
  ##   s1$compU <- validateRange2(s1$compU)
  ##   s1 <- validateRange1(s1)
  ##   ggplot(s1, aes(x=var,y=bars)) + 
  ##     geom_bar(fill="Alice Blue",stat="identity",colour="black") + 
  ##     geom_bar(fill="red",stat="identity",data=s1, aes(y=est))+
  ##     theme_classic() + 
  ##     geom_linerange(aes(ymax=confL,ymin=confU), data=s1)+
  ##     geom_linerange(aes(ymax=compL,ymin=compU), data=s1,colour="green",size=1.5) +
  ##     geom_hline(aes(yintercept=compL),linetype=3,colour="Dark Khaki")+
  ##     geom_hline(aes(yintercept=compU),linetype=3,colour="Dark Khaki")+
  ##     labs(list(title = "Profile distribution", x = "MRO", y = "Proportion"))
    
    
  ## }
  
  
}



prototype.bymrocalc <- function(obj,...){

        stop("No longer supported.")
  
  
  ## if(!"package:ggplot2" %in% search(## )){
    
  ##   stop("require ggplot2 package")
    
  ## }
  ## else{
  ##   s1 <- switcher(obj)
  ##   print(s1)
  ##   s1$ErrBars <- validateRange2(s1$ErrBar)
  ##   s1$confL <- validateRange2(s1$confL)
  ##   s1$confU <- validateRange2(s1$confU)
  ##   s1$compL <- validateRange2(s1$compL)
  ##   s1$compU <- validateRange2(s1$compU)
  ##   s1 <- validateRange1(s1)
  ##   if (which((names(s1) == "var"))<3){
  ##     TYPE = names(s1)[1]
  ##     names(s1)[1]="type"
  ##     ggplot(s1, aes(x=var,y=bars)) + 
  ##       geom_bar(fill="Alice Blue",stat="identity",colour="black") + 
  ##       geom_bar(fill="red",stat="identity",data=s1, aes(y=Est))+
  ##       theme_classic() + 
  ##       geom_linerange(aes(ymax=confL,ymin=confU), data=s1)+
  ##       geom_linerange(aes(ymax=compL,ymin=compU), data=s1,colour="green",size=1.5) +
  ##       geom_hline(aes(yintercept=compL),linetype=3,colour="Dark Khaki")+
  ##       geom_hline(aes(yintercept=compU),linetype=3,colour="Dark Khaki")+
  ##       facet_grid(. ~ type) + 
  ##       labs(list(title = "Profile distribution", x = "MRO", y = "Proportion"))
  ##   }
  ##   else{
  ##     TYPE1 = names(s1)[2]
  ##     names(s1)[2]="type1"
  ##     TYPE2 = names(s1)[1]
  ##     names(s1)[1]="type2"
  ##     ggplot(s1, aes(x=var,y=bars)) + 
  ##       geom_bar(fill="Alice Blue",stat="identity",colour="black") + 
  ##       geom_bar(fill="red",stat="identity",data=s1, aes(y=Est))+
  ##       theme_classic() + 
  ##       geom_linerange(aes(ymax=confL,ymin=confU), data=s1)+
  ##       geom_linerange(aes(ymax=compL,ymin=compU), data=s1,colour="green",size=1.5) +
  ##       geom_hline(aes(yintercept=compL),linetype=3,colour="Dark Khaki")+
  ##       geom_hline(aes(yintercept=compU),linetype=3,colour="Dark Khaki")+
  ##       facet_grid(type2 ~ type1) + 
  ##       labs(list(title = "Profile distribution", x = "MRO", y = "Proportion"))
      
      
  ##   }
      
    
  ## }
  
  
}



prototype.between <- function(obj,...){

        stop("No longer supported.")
  
  ## if(!"package:ggplot2" %in% search()){
    
  ##   stop("require ggplot2 package")
    
  ## }
  ## else{
    
  ##   s2 <- switcher(obj)
  ##   print(s2)
  ##   s2$ErrBars <- validateRange2(s2$ErrBars)
  ##   s2$confL <- validateRange2(s2$confL)
  ##   s2$confU <- validateRange2(s2$confU)
  ##   s2$compL <- validateRange2(s2$compL)
  ##   s2$compU <- validateRange2(s2$compU)
  ##   s2 <- validateRange1(s2)
  ##   TYPE = names(s2)[1]
  ##   names(s2)[1]="type"
   
  ##   p <- ggplot(s2, aes(x=var,y=est,fill=type))+
  ##     geom_bar(stat="identity",position="dodge")+
  ##     theme_classic() + 
  ##     geom_linerange(aes(ymax=confL,ymin=confU), data=s2,position = position_dodge(width=0.9))+
  ##     geom_linerange(aes(ymax=compL,ymin=compU), data=s2,colour="green",size=1.5, position = position_dodge(width=0.9)) +
  ##     labs(list(title = "Profile distribution of Inco Family", x = "MRO", y = "Proportion"))
  ##   p
    
    
  ## }
  
  
}


prototype.b2  <- function(obj,...){
  
      stop("No longer supported.")
  ## if(!"package:ggplot2" %in% search()){
    
  ##   stop("require ggplot2 package")
    
  ## }
  ## else{
    
  ##   s2 <- switcher(obj)
  ##   print(s2)
  ##   s2$ErrBars <- validateRange2(s2$ErrBars)
  ##   s2$confL <- validateRange2(s2$confL)
  ##   s2$confU <- validateRange2(s2$confU)
  ##   s2$compL <- validateRange2(s2$compL)
  ##   s2$compU <- validateRange2(s2$compU)
  ##   s2 <- validateRange1(s2)
  ##   TYPE1 = names(s2)[2]
  ##   names(s2)[2]="type1"
  ##   TYPE2 = names(s2)[1]
  ##   names(s2)[1]="type2"
  ##   p <- ggplot(s2, aes(x=var,y=est,fill=type1))+
  ##     geom_bar(stat="identity",position="dodge")+
  ##     theme_classic() + 
  ##     geom_linerange(aes(ymax=confL,ymin=confU), data=s2,position = position_dodge(width=0.9))+
  ##     geom_linerange(aes(ymax=compL,ymin=compU), data=s2,colour="green",size=1.5, position = position_dodge(width=0.9)) +
  ##     labs(list(title = "Profile distribution of Inco Family", x = "MRO", y = "Proportion"))
  ##   if (nlevels(s2$type2)>4)
  ##     return(p + facet_grid(. ~ type2))
  ##   else
  ##     return(p + facet_grid(type2 ~ .))
    
  ## }
  
  
}
