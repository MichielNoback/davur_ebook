def.par <- par(no.readonly = TRUE)
xhist <- hist(faithful$waiting, breaks = 15, plot = F)
yhist <- hist(faithful$eruptions, breaks = 15, plot = F)
top <- max(c(xhist$counts, yhist$counts))
xrange <- c(40, 100)
yrange <- c(1, 6)
nf <- layout(matrix(c(2,0,1,3),2,2,byrow = TRUE), c(3,1), c(1,3), TRUE)
layout.show(nf)
par(mar = c(3,3,1,1))
plot(faithful$waiting,
     faithful$eruptions,
     xlim = xrange,
     ylim = yrange,
     xlab = "",
     ylab = "",
     pch = 20,
     col=rgb(0.5, 0.1, 0.5, 0.5))
par(mar = c(0,3,1,1))
barplot(xhist$counts,
        axes = FALSE,
        ylim = c(0, top),
        space = 0,
        col=rgb(0.7, 0.2, 0, 1))
par(mar = c(3,0,1,1))
barplot(yhist$counts,
        axes = FALSE,
        xlim = c(0, top),
        space = 0,
        horiz = TRUE,
        col=rgb(0, 0.2, 0.7, 1))
par <- def.par


