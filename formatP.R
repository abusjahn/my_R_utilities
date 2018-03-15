formatP<-function(pIn,ndigits=3,text=T,pretext=F,mark=F)
{
  formatp<-''
  if(is.numeric(pIn))
  {
    if (!is.matrix(pIn))
    {
      pIn<-matrix(pIn);
    }
    formatp<-apply(X=pIn,MARGIN=c(1,2),max,
                   10**(-ndigits),na.rm=F);
    formatp<-apply(X=formatp,MARGIN=c(1,2),round,ndigits);
    formatp<-apply(formatp,MARGIN=c(1,2),
                   formatC,format="f",
                   digits=ndigits,drop0trailing =F);
    if(pretext)
    {
      for (row_i in 1:nrow(pIn))
      {
        for (col_i in 1:ncol(pIn))
        {
          formatp[row_i,col_i]<-paste0(
            ifelse(pIn[row_i,col_i]<10**(-ndigits),
                   '<','='),
            formatp[row_i,col_i])
        }
      }
    }
    if(mark)
    {
      formatp<-matrix(
        paste(formatp,
              apply(gsub('[\\<\\=]','',formatp),c(1,2),markSign)),
        ncol=ncol(pIn))
    }
    if (text==F & pretext==F)
    {
      formatp<-apply(formatp,MARGIN=c(1,2),as.numeric)
    }
  }
  return(formatp);
}
