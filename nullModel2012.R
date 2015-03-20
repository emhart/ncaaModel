

########Null model#####
## run a null and compare it to 
windat <- read.csv("regwinners.csv")
nboot <- 10000
nullmod <- vector()
for(z in 1:nboot){
  winners <- vector()
  n.right <- vector()
  ff4 <- vector()
  ffwinners <- c(1,63,1)
  count <- 1
  for(i in 1:4){
    
    tmp.dat <- subset(windat,windat$region==i)
    win.dat <- tmp.dat[,5:8]
    teams <- tmp.dat$index
    for(k in 1:4){
      n.games <- length(teams)/2
      position <- sort(rep(1:n.games,2))
      
      for(j in 1:n.games){
        winners[j] <- sample(teams[position==j],1)
      }
      n.right[count] <- sum(winners%in%win.dat[,k])
      teams <- winners
      winners <- vector()
      count <- count  + 1
      
      
    }
    ff4[i] <- teams[1]
    
  }
  
  w1 <- sample(ff4[1:2],1)
  w2 <- sample(ff4[3:4],1)
  ch <- sample(c(w1,w2),1)
  ch.cor <- sum(c(w1,w2,ch)%in%ffwinners)
  
  nullmod[z] <- sum(n.right)+ch.cor
}

ranks <- c(8,9,13,8,2)

library(ggplot2)
nullmod <- data.frame(nullmod)
ncaa.fig <- ggplot(nullmod,aes(x=nullmod))+geom_density(fill="red",alpha=.3)+ylab("Density")+xlab("Number of games correct")
ncaa.fig + geom_vline(xintercept=31,linetype=2)+geom_vline(xintercept=40)+geom_vline(xintercept=43,colour="blue")+geom_vline(xintercept=51,colour="red")+opts(axis.text.y=theme_text(size=12))+opts(axis.text.x=theme_text(size=12))

