complete<-function(directory,id=1:332)
{
  cases<-numeric()
  wd=getwd()
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
    y<-is.na(x[["sulfate"]])
    z<-is.na(x[["nitrate"]])
    cases[i]<-sum(!y&!z)
    i<-i+1
  }
  result<-data.frame(id=id,nobs=cases)
  result
}