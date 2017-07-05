#'plot a scatter3d
#'
#'This function plots in 3d given coordinates
#'@param jsonFilePath
#'@export
#'

scatterPlot <- function(jsonFilePath)
{
  library("car")
  library("rgl")
  library("jsonlite")

  data.json <- jsonlite::read_json(path=jsonFilePath,simplifyVector = TRUE)
  rgl::plot3d(x=data.json$nodes$x,y=data.json$nodes$y,z=data.json$nodes$z,xlab="x",ylab="y",zlab="z",size=10,col="blue",box=FALSE)
  rgl::planes3d(a=0,b=0,c=1,d=(-1)*data.json$nodes$z,alpha=0.5)

  count=1
  while(count<=nrow(data.json$links)){
    rgl::lines3d(x=c(data.json$links$source$x[c(count)],data.json$links$target$x[c(count)]),
                 y=c(data.json$links$source$y[c(count)],data.json$links$target$y[c(count)]),
                 z=c(data.json$links$source$z[c(count)],data.json$links$target$z[c(count)]),
                 add=TRUE)
    count=count+1
    }
}

