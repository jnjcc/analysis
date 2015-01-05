# Multiple plot function
#
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to PLOTLIST (as a list of ggplot
# objects)
# - COLS:   Number of columns in layout
# - LAYOUT: A matrix specifying the layout. If present, 'COLS' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and PLOTLIST
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If LAYOUT is NULL, then use 'COLS' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### plot for kernel density estimation, with shading area
kde_shading <- function(x, alternative = c("less", "greater", "two.sided"),
                        xfun = identity, prob = .025,
                        ## curve color, shading color, shading alpha
                        ccolor = "steelblue", scolor = "red", salpha = .5,
                        ...) {
    alternative <- match.arg(alternative)
    x <- xfun(x)
    ## kernel density estimation
    kde <- with(density(x), data.frame(x, y))
    probs <- sort(c(prob, 1 - prob))
    quants <- quantile(x, probs = probs)
    local_env <- environment()
    ## NOTICE: by default, ggplot2 searches the global environment
    p <- ggplot(as.data.frame(x), environment = local_env) +
        stat_density(aes(x = x), geom = "line", color = ccolor, ...)
    if (alternative == "less") {
        probs <- probs[1]
        quants <- quants[1]
        shading_data <- subset(kde, x < quants)
    } else if (alternative == "greater") {
        probs <- probs[2]
        quants <- quants[2]
        shading_data <- subset(kde, x > quants)
    } else { ## two.sided
        lower_data <- subset(kde, x < quants[1])
        p <- p + geom_ribbon(data = lower_data, aes(x = x, ymax = y), ymin = 0,
                             fill = scolor, alpha = salpha)
        shading_data <- subset(kde, x > quants[2])
    }
    p + geom_ribbon(data = shading_data, aes(x = x, ymax = y), ymin = 0,
                    fill = scolor, alpha = salpha) +
        annotate("text", x = quants, y = 0,
                 label = sprintf("%.2f (%.2f%%)", quants, probs * 100))
}
