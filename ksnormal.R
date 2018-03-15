ksnormal<-function(ksdata)
{
  ksout<-ks.test(ksdata,'pnorm',mean(ksdata,na.rm=T),sd(ksdata,na.rm=T),exact=F)
  return(ksout)
}
