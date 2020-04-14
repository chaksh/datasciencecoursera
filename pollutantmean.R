pollutantmean<-function(directory,pollutant,id=1:332)
{
  sums<-numeric()
  lengths<-numeric()
  wd<-getwd()
  i<-1
  while(i<=length(id))
  {
    if (id[i]<10)
      ch<-"00"
    else if (id[i]>=10&id[i]<100)
      ch<-"0"
    else
      ch<-""
    x<-read.csv(file.path(wd,directory,paste(ch,id[i],".csv",sep="")))
    sums[i]<-sum(x[[pollutant]],na.rm="TRUE")
    y<-is.na(x[[pollutant]])
    lengths[i]<-sum(!y)
    i<-i+1
  }
  mean<-sum(sums)/sum(lengths)
  mean
}