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
  data.nodes=as.data.frame(data.json$nodes)
  data.links=as.data.frame(data.json$links)
  rgl::plot3d(x=data.nodes$x,y=data.nodes$y,z=data.nodes$z,xlab="x",ylab="y",zlab="z",size=10,col="blue",box=FALSE)
  rgl::planes3d(a=0,b=0,c=1,d=(-1)*data.nodes$z,alpha=0.5)

  count=1
  while(count<=nrow(data.links$source)){
    rgl::lines3d(x=c(data.links$source$x[c(count)],data.links$target$x[c(count)]),
                 y=c(data.links$source$y[c(count)],data.links$target$y[c(count)]),
                 z=c(data.links$source$z[c(count)],data.links$target$z[c(count)]),
                 add=TRUE)
    count=count+1
    }
}
