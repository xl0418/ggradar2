#' ggradar2
#' @param plot.data The input data should be in the data.frame format with columns named. The values in the data are supposed to be numeric. The ‘group’ column is suggested to included in your data. ggradar2 now can smartly detect if ‘group’ is correctly provided. If not, you will be asked if the first column is allowed to be defined as the group column. If you want to plot multiple plots against some subgroups, please specify it in the column data$facet1.
#' @param base.size The size of radar chart. The default value is 20.
#' @param axis.labels The label of each column in your data is plotted around the radar.
#' @param axis.label.offset The offset of axis labels.
#' @param axis.label.size The size of axis labels.
#' @param axis.line.colour The color of axis labels.
#' @param grid.min,grid.max Rescale your values in this range.
#' @param centre.y The radius of inner circle.
#' @param label.centre.y TRUE prints the central value. FALSE turns it off.
#' @param grid.line.width The width of grid lines.
#' @param grid.line.trend "classic" sets equal width of the grid lines. "increase" sets an outward-increasing width of the grid lines. "decrease" sets an outward-decreasing width of the grid lines.
#' @param gridline.min.linetype,gridline.mid.linetype,gridline.max.linetype Set the grid line type for the inner, middle and outer circles. The default setting is "longdash".
#' @param gridline.min.colour,gridline.mid.colour,gridline.max.colour Set the colors for the inner, middle and outer circles. The default settings are "grey", "#007A87", "grey".
#' @param grid.label.size Set the size of grid labels.
#' @param gridline.label.offset The offset of grid labels.
#' @param gridline.label The default setting is the percentage. Replace it with your labels for circle lines.
#' @param gridline.label.type The type of labels of the circle lines, i.e. percentage or numeric.
#' @param group.line.width The width of group lines.
#' @param group.point.size The size of the point in each axis for group lines.
#' @param group.colours Set colors for the group lines.
#' @param polygonfill Turn on/off the polygon fill. TRUE/FALSE.
#' @param polygonfill.transparency The transparency of polygon fills.
#' @param group.fill.colours The colors of polygon fills.
#' @param background.circle.colour The background color for the radar.
#' @param background.circle.transparency The transparency of the background.
#' @param radarshape "round" gives you a round radar. "sharp" gives you a sharp radar.
#' @param multiplots Turn on/off multi-plotting function. If on, data$facet1 column should be included in your data. TRUE/FALSE
#' @param fullscore Set full scores to your values.
#' @param stripbackground Turn on/off the background for the panels of multiple plots.
#' @param legend.title Legend title.
#' @param plot.legend TRUE/FALSE.
#' @param plot.title Plot title.
#' @param legend.text.size The size of the legend text.
#' @param label.gridline.opener Turn on/off the labels at the inner grid, middle grid and outer grid.
#' @param axis.label.color,grid.label.color Change the colors of the axis labels and grid labels.
#' @param ci The confidence interval indicates the higher and lower boundaries.
#' @author Liang Xu
#' @examples
#' library(ggradar2)
#' library(ggplot2)
#' data(mtcars)
#' ggradar2(mtcars)
#' @export ggradar2
#' @import ggplot2
#' @import graphics
#' @import utils
#' @export
ggradar2 <- function(plot.data,
                    base.size=20,
                    axis.labels="",
                    grid.min=0,  #10,
                    grid.max=1,  #100,
                    centre.y=grid.min - ((1/9)*(grid.max-grid.min)),
                    label.centre.y=FALSE,
                    grid.line.width=0.5,
                    grid.line.trend = "classic",
                    gridline.min.linetype="longdash",
                    gridline.mid.linetype="longdash",
                    gridline.max.linetype="longdash",
                    gridline.min.colour="grey",
                    gridline.mid.colour="#007A87",
                    gridline.max.colour="grey",
                    grid.label.size=6,
                    gridline.label.offset=-0.1*(grid.max-centre.y),
                    label.gridline.opener=TRUE,
                    gridline.label=seq(0, 100, 20),
                    gridline.label.type="percentage",
                    axis.label.offset=1.15,
                    axis.label.size=5,
                    axis.line.colour="grey",
                    group.line.width=1.5,
                    group.point.size=3,
                    group.colours=NULL,
                    group.fill.colours=NULL,
                    background.circle.colour="#D7D6D1",
                    background.circle.transparency=0.2,
                    legend.title="",
                    plot.legend=TRUE,
                    plot.title="",
                    legend.text.size=14,
                    radarshape = "round",
                    polygonfill = TRUE,
                    polygonfill.transparency = 0.2,
                    multiplots = FALSE,
                    stripbackground = TRUE,
                    fullscore = NULL,
                    axis.labels.color = "black",
                    grid.labels.color = "black",
                    ci = FALSE) {

  # Default settings
  plot.extent.x.sf=1
  plot.extent.y.sf=1.2
  x.centre.range=0.02*(grid.max-centre.y)
  num.circle <- length(gridline.label)
  # Load the data and check if subgroup is given
  if(multiplots){
    if(length(which(colnames(plot.data) == 'facet1'))==0){
      return('Error: no facet is applied.')
    }else{
      plot.data <- as.data.frame(plot.data)
      facet1ind <- which(colnames(plot.data) == 'facet1')
      facet1df <- plot.data$facet1
      facet1df <- factor(facet1df,levels = as.vector(unique(facet1df)))
      plot.data <- plot.data[,-facet1ind]
    }
  }else if(multiplots == FALSE) {
    plot.data <- as.data.frame(plot.data)
  }else{
    return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
  }


  # Check if the group names are given. If not, choose the first column as the
  # group name.
  if(!is.null(plot.data$group)){
    plot.data$group <- as.factor(as.character(plot.data$group))
  }else if(!is.null(rownames(plot.data))){
    plot.data$group <- rownames(plot.data)
    print("Row names are chosen as the group names.")
  }else{
    groupcheck <- readline(" WARNING: 'group' column and row names are not detected. The first column will be chosen as the group name. Yes/no? (y/n)")
    if(groupcheck == 'y'){
      plot.data[,1] <- as.factor(as.character(plot.data[,1]))
      names(plot.data)[1] <- "group"
    }else{
      print(" Abort! Data check failed! ")
      return(" Abort! Data check failed! ")
    }

  }


  col_group = which(colnames(plot.data)=='group')
  # Extract names of the variables from the data frame except the group names.
  var.names <- colnames(plot.data)[-col_group]
  df_variables <-  plot.data[,-col_group]
  valid.data.nrow <- nrow(df_variables)
  # Full scores are considered if specified
  if(!is.null(fullscore)){
    if(length(fullscore) == ncol(df_variables)){
      df_variables <- rbind(df_variables, fullscore)
    }else{
      return(print("Error: please provide the same length of 'fullscore' as of the variables."))
    }
  }



  # process the confidence data
  if(class(ci) == "data.frame" ) {

    hl_group <- which(colnames(ci) == "type")
    df_h_variables <- subset(ci, type == "h")[,-hl_group]
    df_l_variables <- subset(ci, type == "l")[,-hl_group]

    # Full scores are considered if specified
    if(!is.null(fullscore)){
      if(length(fullscore) == ncol(df_variables)){
        max.value <- apply(rbind(df_h_variables, fullscore), 2, max)
      }else{
        return("Error: please provide the same length of 'fullscore' as of the variables.")
      }
    } else {
      max.value <- apply(df_h_variables, 2, max)
    }

    df_h_variables <- data.frame(lapply(rbind(df_h_variables, max.value),
                                      function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/grid.max)))[1:valid.data.nrow,]
    df_l_variables <- data.frame(lapply(rbind(df_l_variables, max.value),
                                        function(x) scale(x, center = FALSE,
                                                          scale = max(x, na.rm = TRUE)/grid.max)))[1:valid.data.nrow,]
    df_variables <- data.frame(lapply(rbind(df_variables, max.value),
                                      function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/grid.max)))[1:valid.data.nrow,]
  } else {
    df_variables <- data.frame(lapply(df_variables,
                                      function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/grid.max)))
  }

  # Get rid of the full scores as we don't want to plot them
  if(!is.null(fullscore)){
    plot.data <-  cbind(plot.data$group,df_variables[1:valid.data.nrow,]) # bug fixed remove [-nrow(df_variables),] -- 2.1.2025
  } else {
    plot.data <-  cbind(plot.data$group,df_variables)
  }
  names(plot.data)[1] <- 'group'

  # process the confidence data
  if(class(ci) == "data.frame" ) {
    h.data <- cbind(plot.data$group,df_h_variables)
    l.data <- cbind(plot.data$group,df_l_variables)
    names(h.data)[1] <- 'group'
    names(l.data)[1] <- 'group'
  }

  # Check if the axis labels are properly set up.
  if(length(axis.labels) == 1 && axis.labels == ""){
    axis.labels <- var.names
  }else{
    if (length(axis.labels) != ncol(plot.data)-1)
      return("Error: 'axis.labels' contains the wrong number of axis labels")
  }


  #calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x=(grid.max+abs(centre.y))*plot.extent.x.sf
  plot.extent.y=(grid.max+abs(centre.y))*plot.extent.y.sf

  # Set grid.line.trend type
  if(grid.line.trend == 'increase'){
    grid.line.width <- seq(from = grid.line.width, to = grid.line.width+num.circle*0.2,by=0.2)
  }else if(grid.line.trend == 'classic'){
    grid.line.width <- rep(grid.line.width,num.circle)
  }else if(grid.line.trend == 'decrease'){
    grid.line.width <- rev(seq(from = grid.line.width, to = grid.line.width+num.circle*0.2,by=0.2))
  }else{
    return("Error: 'grid.line.trend' so far only contains two types, e.g. 'classic' and 'increase' ")
  }

  #Declare required internal functions

  CalculateGroupPath <- function(df) {
    #Converts variable values into a set of radial x-y coordinates
    #Code adapted from a solution posted by Tony M to
    #http://stackoverflow.com/questions/9614433/creating-radar-chart-a-k-a-star-plot-spider-plot-using-ggplot2-in-r
    #Args:
    #  df: Col 1 -  group ('unique' cluster / group ID of entity)
    #      Col 2-n:  v1.value to vn.value - values (e.g. group/cluser mean or median) of variables v1 to v.n

    path <- df[,1]
    path <- factor(path,levels = as.vector(path))

    ##find increment
    angles = seq(from=0, to=2*pi, by=(2*pi)/(ncol(df)-1))
    ##create graph data frame
    graphData= data.frame(seg="", x=0,y=0)
    graphData=graphData[-1,]

    for(i in levels(path)){
      pathData = subset(df, df[,1]==i)
      for(j in c(2:ncol(df))){
        #pathData[,j]= pathData[,j]


        graphData=rbind(graphData, data.frame(group=i,
                                              x=pathData[,j]*sin(angles[j-1]),
                                              y=pathData[,j]*cos(angles[j-1])))
      }
      ##complete the path by repeating first pair of coords in the path
      graphData=rbind(graphData, data.frame(group=i,
                                            x=pathData[,2]*sin(angles[1]),
                                            y=pathData[,2]*cos(angles[1])))
    }
    #Make sure that name of first column matches that of input data (in case !="group")
    colnames(graphData)[1] <- colnames(df)[1]
    graphData #data frame returned by function
  }
  CaclulateAxisPath = function(var.names,min,max) {
    #Caculates x-y coordinates for a set of radial axes (one per variable being plotted in radar plot)
    #Args:
    #var.names - list of variables to be plotted on radar plot
    #min - MININUM value required for the plotted axes (same value will be applied to all axes)
    #max - MAXIMUM value required for the plotted axes (same value will be applied to all axes)
    #var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    #Cacluate required number of angles (in radians)
    angles <- seq(from=0, to=2*pi, by=(2*pi)/n.vars)
    #calculate vectors of min and max x+y coords
    min.x <- min*sin(angles)
    min.y <- min*cos(angles)
    max.x <- max*sin(angles)
    max.y <- max*cos(angles)
    #Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i,min.x[i],min.y[i])
      b <- c(i,max.x[i],max.y[i])
      axisData <- rbind(axisData,a,b)
    }
    #Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no","x","y")
    rownames(axisData) <- seq(1:nrow(axisData))
    #Return calculated axis paths
    as.data.frame(axisData)
  }
  funcCircleCoords <- function(center = c(0,0), r = 1, npoints = 100){
    #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  #[creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[,2:ncol(plot.data)]<- plot.data[,2:ncol(plot.data)]+abs(centre.y)
  #print(plot.data.offset)
  # (b) convert into radial coords
  group <-NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  # If the ci data is provided, calculate the offset for the ci data
  if(class(ci) == "data.frame" ) {
    h.offset <- h.data
    h.offset[,2:ncol(h.data)]<- h.data[,2:ncol(h.data)]+abs(centre.y)
    #print(plot.data.offset)
    # (b) convert into radial coords
    h.group <-NULL
    h.group$path <- CalculateGroupPath(h.offset)

    l.offset <- l.data
    l.offset[,2:ncol(l.data)]<- l.data[,2:ncol(l.data)]+abs(centre.y)
    #print(plot.data.offset)
    # (b) convert into radial coords
    l.group <-NULL
    l.group$path <- CalculateGroupPath(l.offset)
  }

  #print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CaclulateAxisPath(var.names,grid.min+abs(centre.y),grid.max+abs(centre.y))
  #print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  #Labels
  axis$label <- data.frame(
    text=axis.labels,
    x=NA,
    y=NA )
  #print(axis$label)
  #axis label coordinates
  n.vars <- length(var.names)
  angles = seq(from=0, to=2*pi, by=(2*pi)/n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*sin(angles[i])})
  axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.max+abs(centre.y))*axis.label.offset)*cos(angles[i])})
  #print(axis$label)

  # mini type for web plotting

  if(gridline.label.type == "percentage"){
    values.radar <- paste0(gridline.label, "%")
  } else if (gridline.label.type == "numeric") {
    values.radar <- gridline.label
  }
  grid.mids <- seq(grid.min, grid.max, (grid.max - grid.min)/(num.circle -1) )



  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  #[default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size=base.size) +
    theme(axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          legend.key=element_rect(linetype="blank"))


  #Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  if(multiplots){
    facet_vec <- factor(unique(facet1df),levels = as.vector(unique(facet1df)))
    no.facet <- length(facet_vec)
    multiaxislabel <- cbind(axis$label[rep(seq_len(nrow(axis$label)), no.facet),],rep(facet_vec,each = nrow(axis$label)))
    names(multiaxislabel)[4] <- 'facet1'
    base <- ggplot2::ggplot(multiaxislabel) + xlab(NULL) + ylab(NULL) + coord_equal() +
      geom_text(data=subset(multiaxislabel,multiaxislabel$x < (-x.centre.range)),
                aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, color = axis.labels.color) +
      scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
      scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))+facet_wrap(~facet1)
  }else if(multiplots == FALSE){
    base <- ggplot2::ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
      geom_text(data=subset(axis$label,axis$label$x < (-x.centre.range)),
                aes(x=x,y=y,label=text),size=axis.label.size,hjust=1, color = axis.labels.color) +
      scale_x_continuous(limits=c(-1.5*plot.extent.x,1.5*plot.extent.x)) +
      scale_y_continuous(limits=c(-plot.extent.y,plot.extent.y))
  }else{
    return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
  }


  if(radarshape == "round"){
    gridline <- NULL
    for (gridline.level in c(1: (num.circle) )) {
      gridline[[paste0("level", gridline.level)]][["path"]] <- funcCircleCoords(c(0,0),grid.mids[gridline.level]+abs(centre.y),npoints = 360)
      gridline[[paste0("level", gridline.level)]][["label"]] <- data.frame(x=gridline.label.offset,y=grid.mids[gridline.level]+abs(centre.y),
                                                                           text=as.character(grid.mids[gridline.level]))
    }

    for (gridline.level in c(1: (num.circle) )) {
      if (gridline.level == 1){
        gridline.linetype <- gridline.min.linetype
        gridline.colour <- gridline.min.colour
      } else if (gridline.level == num.circle) {
        gridline.linetype <- gridline.max.linetype
        gridline.colour <- gridline.max.colour
      } else{
        gridline.linetype <- gridline.mid.linetype
        gridline.colour <- gridline.mid.colour
      }

      base <- base +  geom_path(data=gridline[[gridline.level]]$path,aes(x=x,y=y),
                                lty=gridline.linetype,colour=gridline.colour,size=grid.line.width[gridline.level])
    }

  }else if(radarshape == "sharp"){
      # ... + sharp grid-lines at 'min', 'mid' and 'max' y-axis values
      # Extract the coordinates of the inner points and the outer points
      oddindex <-  seq(1,nrow(axis$path),2)
      evenindex <- seq(2,nrow(axis$path),2)
      axis$innerpath <- axis$path[oddindex,]
      axis$outerpath <- axis$path[evenindex,]
      axis$innerpath <- rbind(axis$innerpath,head(axis$innerpath,1))
      axis$outerpath <- rbind(axis$outerpath,head(axis$outerpath,1))
      gridline <- NULL
      gridline[[paste0("level", 1)]][["path"]] <- axis$innerpath
      gridline[[paste0("level", 1)]][["label"]] <- data.frame(x=gridline.label.offset,y=grid.mids[1]+abs(centre.y),
                                                                           text=as.character(grid.mids[1]))
      for(line.count in c(2:(num.circle - 1))) {
        gridline[[paste0("level", line.count)]][["path"]] <- (-axis$innerpath+axis$outerpath) * line.count/num.circle+axis$innerpath
        gridline[[paste0("level", line.count)]][["label"]] <- data.frame(x=gridline.label.offset,y=grid.mids[line.count]+abs(centre.y),
                                          text=as.character(grid.mids[line.count]))
      }
      gridline[[paste0("level", num.circle)]][["path"]] <- axis$outerpath
      gridline[[paste0("level", num.circle)]][["label"]] <- data.frame(x=gridline.label.offset,y=grid.mids[num.circle]+abs(centre.y),
                                                 text=as.character(grid.mids[num.circle]))

      for(line.count in c(1:num.circle)) {
        if (line.count == 1){
          gridline.linetype <- gridline.min.linetype
          gridline.colour <- gridline.min.colour
        } else if (line.count == num.circle) {
          gridline.linetype <- gridline.max.linetype
          gridline.colour <- gridline.max.colour
        } else{
          gridline.linetype <- gridline.mid.linetype
          gridline.colour <- gridline.mid.colour
        }
        base <- base + geom_path(data = gridline[[line.count]]$path,aes(x=x,y=y),
                                lty=gridline.linetype,colour=gridline.colour,size=grid.line.width[line.count])
      }

    }else{
      return("Error: 'radarshape' should be specified...")
    }


    # + axis labels for any vertical axes [abs(x)<=x.centre.range]
    base <- base + geom_text(data=subset(axis$label,abs(axis$label$x)<=x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0.5, color = axis.labels.color)
    # + axis labels for any vertical axes [x>x.centre.range]
    base <- base + geom_text(data=subset(axis$label,axis$label$x>x.centre.range),
                             aes(x=x,y=y,label=text),size=axis.label.size,hjust=0, color = axis.labels.color)
    # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
    base <- base + theme_clear

    if(radarshape == 'round'){
      #  + background circle against which to plot radar data
      base <- base + geom_polygon(data=gridline[[num.circle]]$path,aes(x=x,y=y),
                                  fill=background.circle.colour,
                                  alpha=background.circle.transparency)
    }else if(radarshape == 'sharp'){
      #  + background polygon  against which to plot radar data
      base <- base + geom_polygon(data=gridline[[num.circle]]$path,aes(x=x,y=y),
                                  fill=background.circle.colour,
                                  alpha=background.circle.transparency)
    }else{
      return("Error: 'radarshape' should be specified...")
    }


    # + radial axes
    base <- base + geom_path(data=axis$path,aes(x=x,y=y,group=axis.no),
                             colour=axis.line.colour)

    # Draw path for different facets.
    if(multiplots){

      multigrouppath <- cbind(group$path,rep(facet1df,each=nrow(group$path)/nrow(plot.data)))
      names(multigrouppath)[4] <- 'facet1'
      # fill different facets
      if(polygonfill){
        base <- base + geom_polygon(data=multigrouppath,aes(x=x,y=y,col = factor(group), fill = factor(group)),
                                    alpha=polygonfill.transparency,show.legend = F)+
          facet_wrap(~facet1)
      }

      # ... + group (cluster) 'paths'
      base <- base + geom_path(data=multigrouppath,aes(x=x,y=y,group=group,colour=group),
                               size=group.line.width)+
        facet_wrap(~facet1)

      # ... + group points (cluster data)
      base <- base + geom_point(data=multigrouppath,aes(x=x,y=y,group=group,colour=group),size=group.point.size)+
              facet_wrap(~facet1)
    }else if(multiplots == FALSE){
      if(polygonfill){
        if(class(ci) == "data.frame") {
          h.group$subid <- 1L
          l.group$subid <- 2L
          hl.group.path <- rbind(h.group$path, l.group$path)
          hl.group.path$subid <- rep(c(1L, 2L), each = nrow(h.group$path))
          base <- base + geom_polygon(data=hl.group.path,aes(x=x,y=y,  fill = factor(group), subgroup = subid),
                                      alpha=polygonfill.transparency,show.legend = F)
        } else {
          base <- base + geom_polygon(data=group$path,aes(x=x,y=y,col = factor(group), fill = factor(group)),
                                      alpha=polygonfill.transparency,show.legend = F)
        }

      }

      # ... + group (cluster) 'paths'
      base <- base + geom_path(data=group$path,aes(x=x,y=y,group=group,colour=group),
                               size=group.line.width)

      # ... + group points (cluster data)
      base <- base + geom_point(data=group$path,aes(x=x,y=y,group=group,colour=group),size=group.point.size)

    }else{
      return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
    }




    #... + amend Legend title
    if (plot.legend){
      if(multiplots== FALSE){
        base  <- base + labs(colour=legend.title,size=legend.text.size) +
          theme(legend.text = element_text(size = legend.text.size), legend.position="left") +
          theme(legend.key.height=unit(2,"line"))
      }else if(multiplots){
        base  <- base + labs(colour=legend.title,size=legend.text.size) +
          theme(legend.text = element_text(size = legend.text.size), legend.position="bottom") +
          theme(legend.key.height=unit(2,"line"))
      }else{
        return("Error: 'multiplots' can be either '1D' for facets plotting or 'none' for single plotting. ")
      }

    }else{
      base <- base + theme(legend.position = "none")
    }

    # ... + grid-line labels (max; mid; min)

    if (label.gridline.opener) {
      for(line.count in c(1:(num.circle))){
        base <- base + geom_text(aes(x=x,y=y),label=values.radar[line.count],
                                 data=gridline[[line.count]]$label,size=grid.label.size*0.8, hjust=1, color = grid.labels.color)

      }
    }

    # ... + centre.y label if required [i.e. value of y at centre of plot circle]
    if (label.centre.y==TRUE) {
      centre.y.label <- data.frame(x=0, y=0, text=as.character(centre.y))
      base <- base + geom_text(aes(x=x,y=y,label=text),data=centre.y.label,size=grid.label.size, hjust=0.5, color = grid.labels.color)
      }

    if (!is.null(group.colours)){
      colour_values <- rep(group.colours,100)
      if (!is.null(group.fill.colours)){
        fill_values <- rep(group.fill.colours,100)
      }else{
        fill_values <- colour_values
      }
    } else {
      colour_values <- rep(c("#FF5A5F", "#FFB400", "#007A87",  "#8CE071", "#7B0051",
                          "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)
      fill_values <- colour_values
    }

    base <- base + theme(legend.key.width=unit(3,"line")) + theme(text = element_text(size = 20)) +
      scale_colour_manual(values=colour_values) +
      scale_fill_manual(values = fill_values) +
      theme(legend.title=element_blank())

    if (plot.title != "") {
      base <- base + ggtitle(plot.title)
    }
    if (stripbackground==FALSE){
      base <- base + theme(strip.background = element_blank())
    }
  #base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]


  return(base)

}
