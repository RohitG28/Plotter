#'plot a scatter3d
#'jsonString of the form '{"nodes": [{"x" : 1, "y" : 1, "z" : 1},{"x":1,"y":2,"z":3}]
#'                         "links" : [{"source" : {"x" : 1, "y" : 1, "z" : 1},
#'                                  "target" : {"x" : 1, "y" : 1, "z" : 1}}
#'                                 ,{"source" : {"x" : 1, "y" : 1, "z" : 1},
#'                                  "target" : {"x" : 1, "y" : 1, "z" : 1}}]
#'                        }'
#'This function plots in 3d given coordinates
#'@param jsonString
#'@export
#'

scatterPlot <- function(jsonString)
{
  library("car")
  library("rgl")
  library("jsonlite")

  data.json <- jsonlite::fromJSON(jsonString,simplifyVector = TRUE)
  rgl::plot3d(x=data.json$nodes$x,y=data.json$nodes$y,z=data.json$nodes$z,xlab="x",ylab="y",zlab="z",size=10,col="blue",box=FALSE)
  rgl::planes3d(a=0,b=0,c=1,d=(-1)*data.json$nodes$z,alpha=0.5)

  count=1
  while(count<=length(data.json$links$source$x)){
    rgl::lines3d(x=c(data.json$links$source$x[c(count)],data.json$links$target$x[c(count)]),
                 y=c(data.json$links$source$y[c(count)],data.json$links$target$y[c(count)]),
                 z=c(data.json$links$source$z[c(count)],data.json$links$target$z[c(count)]),
                 add=TRUE)
    count=count+1
    }
}
