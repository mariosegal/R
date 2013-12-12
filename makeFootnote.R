makeFootnote_left <- function(footnoteText=
                                format(Sys.time(), "%d %b %Y"),
                              size= .7, color= "black")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(0.02,"npc"),
            y= unit(0.02,"npc"),
            just=c("left", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


makeFootnote_right <- function(footnoteText=
                                 format(Sys.time(), "%d %b %Y"),
                               size= .7, color= "black")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(0.98,"npc"),
            y= unit(0.02,"npc"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}


makeTitle <- function(text=
                                 format(Sys.time(), "%d %b %Y"),
                               size= 1, color= "blue")
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= text ,
            x = unit(0.5,"npc"),
            y= unit(0.98,"npc"),
            just=c("center", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}
