boot.test<-function(x,y, reps=10000)
{
  b_x<-boot(x, sum,R=reps)
  b_y<-boot(y, sum,R=reps)
  z<-(b_x$t0-b_y$t0)/sqrt(var(b_x$t[,1])+var(b_y$t[,1]))
  cat(pnorm(z))
}


