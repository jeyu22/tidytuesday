options(scipen = 999)
library(tidyverse)
library(paramtest)
library(purrr)


test <- c(22,2)
test1 <- c(2,3)

testt <- list(test,test1)
length(testt)


decision <- function(bac,tt,choicea1,choicea2,choiceb1,choiceb2,
                     choicec1,choicec2,choicee1,choicee2,choiced1,choiced2, p_ab,
                     p_ac,p_ad,p_ae,p_bc,p_bd,p_be,p_cd,p_ce,p_de,...){
  
  choicea <- c(choicea1,choicea2)
  choiceb <- c(choiceb1,choiceb2)
  choicec <- c(choicec1,choicec2)
  choiced <- c(choiced1,choiced2)
  choicee <- c(choicee1,choicee2)
  
  choicex <- list(choicea,choicea,choicea,choicea,choiceb,choiceb,choiceb,choicec,choicec,choiced)
  choicey <- list(choiceb,choicec,choiced,choicee,choicec,choiced,choicee,choiced,choicee,choicee)
  probs <- c(p_ab,p_ac,p_ad,p_ae,p_bc,p_bd,p_be,p_cd,p_ce,p_de)
  
  skn <- function(e,w,a,bac){
    
    
    y <- (2/w)*dnorm((bac-e)/w, 0 , .04) * pnorm(a*((bac-e)/w),0,.04)
    y <- y * .1
    
    return(y)
  }
  
  m <- skn(.087,1.66,2.5,bac)
  v <- 1+(tt*bac)
  
  dec <- function(choicex,choicey,myopia = m, var = v){
    resultx <- 0
    resulty <- 0
    
    for(i in 1:length(choicex)){
      resultx <- resultx +sum(((1-myopia)^(length(choicex)-i))*choicex[i])
    }
    for(i in 1:length(choicey)){
      resulty <- resulty +sum(((1-myopia)^(length(choicey)-i))*choicey[i])
    }
    return( pnorm((resultx-resulty)/(var*sqrt((2*((1-myopia)^(2*length(choicex)))-2)/(myopia*(myopia-2))
    ))))
  }
  
   all <- map2_dbl(.x = choicex, .y= choicey, .f = dec)
   
   #err <- 0 
   #for(i in 1:length(probs)){
   #  err <- err + ((all[i]-probs[i])^2)
   #}
   
   return(all)
 
}

decision(bac = .12, tt = 5, choicea1 = choicea1_, choicea2 = choicea2_,
         choiceb1 = choiceb1_, choiceb2 = choiceb2_,
         choicec1 = choicec1_,choicec2 = choicec2_,
         choiced1 = choiced2_,choiced2 = choiced1_,
         choicee1 = choicee2_, choicee2 = choicee1_,
         p_ab = .8,
         p_ac = .4,p_ad = 0,p_ae = 0,p_bc = .4 ,p_bd = .4 ,p_be = 0 ,p_cd = .2 ,p_ce = .2,p_de =.4
         )

ok <- grid_search(func = decision, params=list(tt = seq(0,10,1),bac = seq(0,.12,.02),choicea1= c(choicea1_,choicea2_),
                                               choicea2 = c(choicea1_,choicea2_),choiceb1= c(choiceb1_,choiceb2_),
                                               choiceb2 = c(choiceb1_,choiceb2_),choicec1= c(choicec1_,choicec2_),
                                               choicec2 = c(choicec1_,choicec2_),choiced1= c(choiced1_,choiced2_),
                                               choiced2 = c(choiced1_,choiced2_),
                                               choicee1= c(choicee1_,choicee2_),
                                               choicee2 = c(choicee1_,choicee2_)),output = "data.frame",  p_ab = .8,
                  p_ac = .4,p_ad = 0,p_ae = 0,p_bc = .4 ,p_bd = .4 ,p_be = 0 ,p_cd = .2 ,p_ce = .2,p_de =.4, parallel = "multicore")

ok1 <- grid_search(func = decision, params=list(tt = seq(0,10,1),bac = seq(0,.12,.02),choicea1= c(choicea1_,choicea2_),
                                               choicea2 = c(choicea1_,choicea2_),choiceb1= c(choiceb1_,choiceb2_),
                                               choiceb2 = c(choiceb1_,choiceb2_),choicec1= c(choicec1_,choicec2_),
                                               choicec2 = c(choicec1_,choicec2_),choiced1= c(choiced1_,choiced2_),
                                               choiced2 = c(choiced1_,choiced2_),
                                               choicee1= c(choicee1_,choicee2_),
                                               choicee2 = c(choicee1_,choicee2_)),output = "data.frame",  p_ab = 0,
                  p_ac = 0,p_ad = 0,p_ae = 0,p_bc = 0 ,p_bd = .2 ,p_be = 0 ,p_cd = 0 ,p_ce = 0,p_de =0,parallel = "snow")


# SOBER
View(ok$results %>% filter(choicea1.test != choicea2.test,
                           choiceb1.test != choiceb2.test,
                           choicec1.test != choicec2.test,
                           choiced1.test != choiced2.test,
                           choicee1.test != choicee2.test,) %>%arrange((output))) %>%head(20)

# DRUNK 
View(ok1$results %>% filter(choicea1.test != choicea2.test,
                           choiceb1.test != choiceb2.test,
                           choicec1.test != choicec2.test,
                           choiced1.test != choiced2.test,
                           choicee1.test != choicee2.test,) %>%arrange((output))) %>% head(20)


# things that can be changed:

# a and r in the cummulative transform
# what values should t take on 






