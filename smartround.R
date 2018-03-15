roundR <- function(roundin,smooth=F,level=2, textout=T,drop0=F)
{
  if (!is.matrix(roundin))
  {
    roundin<-matrix(roundin)
  }
  roundout<-roundin
  roundlevel<-0
  if(min(roundin,na.rm=T)!=0 | max(roundin,na.rm=T)!=0)
  {
    roundlevel<-round(max(0,level-log10(max(abs(roundin),
                                          na.rm=T))))
  }
  roundout[which(!is.na(roundout))]<-
    round(roundin[which(!is.na(roundin))],roundlevel)
  # if(all((roundin[which(!is.na(roundin))]-roundout[which(!is.na(roundout))])==0))
  # {
  #   roundlevel<-0
  #   roundout[which(!is.na(roundout))]<-
  #     round(roundin[which(!is.na(roundin))],roundlevel)
  # }
  if (smooth&max(abs(roundout),na.rm=T)!=0)
  {
      roundout[which(!is.na(roundout))]<-
      round(
        roundout[which(!is.na(roundout))]/
           10^round(log10(max(abs(roundout),na.rm=T))-level))*
       10^round(log10(max(abs(roundout),na.rm=T))-level)
  }
  if(textout==T)
  {
    roundout[which(!is.na(roundout))]<-
      formatC(roundout[which(!is.na(roundout))],format='f',
              digits=roundlevel,drop0trailing=drop0)
  }
  return(roundout)

}
