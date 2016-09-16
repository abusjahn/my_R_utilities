smartround <- function(roundin,smooth=F,level=2, textout=T)
{
  if (!is.matrix(roundin))
  {
    roundin<-matrix(roundin)
  }
  smartround<-roundin
  roundlevel<-0
  if(min(roundin,na.rm=T)!=0 | max(roundin,na.rm=T)!=0)
  {
    roundlevel<-round(max(0,level-log10(max(abs(roundin),
                                          na.rm=T))))
  }
  smartround[which(!is.na(smartround))]<-
    round(roundin[which(!is.na(roundin))],roundlevel)
  if (smooth&max(abs(smartround),na.rm=T)!=0)
  {
    smartround[which(!is.na(smartround))]<-
      round(
        smartround[which(!is.na(smartround))]/
           10^round(log10(max(abs(smartround),na.rm=T))-level))*
       10^round(log10(max(abs(smartround),na.rm=T))-level)
  }
  if(textout==T)
  {
    smartround[which(!is.na(smartround))]<-
      formatC(smartround[which(!is.na(smartround))],format='f',
              digits=roundlevel,drop0trailing=F)
  }
  return(smartround)

}
