library(ggplot2)
library(scales)

# set up an example dataset (see ?lmFit)
sd <- 0.3*sqrt(4/rchisq(100,df=4))
y <- matrix(rnorm(100*6,sd=sd),100,6)
rownames(y) <- paste("Gene",1:100)
y[1:2,4:6] <- y[1:2,4:6] + 2
design <- cbind(Grp1=1,Grp2vs1=c(0,0,0,1,1,1))
options(digits=3)

# Ordinary fit
fit <- lmFit(y,design)
fit <- eBayes(fit)
tt <- topTable(fit,coef=2, n = Inf)

# transformation function for reverse log1p axis
revlog_trans <- function(base = exp(1)) {
    trans <- function(x) -log1p(x)
    inv <- function(x) expm1(-x)
    scales::trans_new("revlog1p", trans, inv, domain = c(0, Inf))
}

ggplot(tt, aes(x = logFC, y = P.Value)) +
    scale_fill_gradient(low = "lightgray", high = "navy") +
    scale_color_gradient(low = "lightgray", high = "navy") +
    scale_y_continuous(trans = revlog_trans(), expand = c(0.005, 0.005)) +
    expand_limits(y = c(0, 1)) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon",
                    show.legend = FALSE) +
    geom_point(data = subset(tt, P.Value < 0.05), 
               color = "red", alpha = 0.5) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 0.05, linetype = "dashed") +
    geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
    theme_linedraw() + 
    theme(panel.grid = element_blank()) +
    xlab("Fold change (log2)") +
    ylab("P-Value") +
    annotate("text", x = min(tt$logFC), y = 1,
             label = "Nominally significant",
             color = "black", hjust = 0) +
    annotate("point", x = min(tt$logFC) - 0.05, y = 1, color = "red")
