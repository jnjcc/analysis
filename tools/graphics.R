## Panel plots: panel.smooth(), panel.cor(), panel.hist(), panel.lm()
##   pairs(upper.panel = xx, lower.panel = xx, diag.panel = xx)
##
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    ousr <- par("usr"); on.exit(par(ousr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, use = "complete.obs"))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste(prefix, txt, sep = "")
    if (missing(cex.cor)) {
        cex.cor <- 0.8 / strwidth(txt)
    }
    text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}

panel.hist <- function(x, ...) {
    ousr <- par("usr"); on.exit(par(ousr))
    par(usr = c(ousr[1:2], 0, 1.5))
    h <- hist(x, plot = FALSE);
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y / max(y)
    rect(breaks[-nB], 0, breaks[-1], y, ...)
}

panel.lm <- function(x, y, col = par("col"), bg = NA, pch = par("pch"),
                     cex = 1, col.smooth = "black", ...) {
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    abline(stats::lm(y ~ x), col = col.smooth, ...)
}
