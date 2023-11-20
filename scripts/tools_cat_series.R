### number_segments(c("A","A","B","B","B","B","A","A","B","B","B"))
### transforms series "A-A-B-B-B-B-A-A-B-B-B" 
### into new series   "1-1-2-2-2-2-3-3-4-4-4"
number_segments=function(series){
  series=as.factor(series)
  seriesnum=as.numeric(
    as.vector(
      factor(series, labels=1:nlevels(series))
    ))
  diffseriesnum=abs(diff(seriesnum))
  seg=rep(0,length(diffseriesnum))
  seg[diffseriesnum>0]=1
  seg=c(1,seg)
  seg=cumsum(seg)
  return(seg)
}


### levels_to_colors(c("A","A","B","B","B"), c("red","blue"))
### transforms series "A-A-B-B-B"
### into new series "red-red-blue-blue-blue"
levels_to_colors=function(series,lcolors=1:nlevels(series)){
  series=as.factor(series)
  seriescol=as.vector(factor(series,labels=lcolors))
  return(seriescol)
}

### plot_cat_series(...)
### produces a barcode-type graph of a series 
plot_cat_series=function(series,
                         x=1:length(series),
                         col=1:nlevels(as.factor(series)),...){
  seg=number_segments(series)
  vcol=levels_to_colors(series,col)
  vcol=as.vector(tapply(vcol,seg,"unique"))
  mins=as.vector(tapply(x,seg,"min"))
  maxs=c(mins[2:length(mins)],max(x))
  plot(range(x),c(0,1),
       col="white",
       xaxs="i",yaxs="i",
       xlab="", ylab="", yaxt="n",...)
  for (i in 1:length(mins)){
    myx=c(mins[i],maxs[i],maxs[i],mins[i])
    myy=c(0,0,1,1)
    polygon(myx,myy,col=vcol[i],border=vcol[i])
  }
}