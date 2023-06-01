
library(ggplot2)
show_palette <- function(pal, cols = 4) {
  pal <- rev(pal)
  series <- 0 : (length(pal)-1)
  df <- data.frame(my_pal = pal, x=(series+1)%%cols, y=series%/%cols, stringsAsFactors = F)
  #print(df)
  p <- ggplot() +
    scale_x_continuous(name="", breaks=NULL, expand=c(0, 0)) +
    scale_y_continuous(name="", breaks=NULL, expand=c(0, 0)) +
    scale_fill_identity() +
    geom_rect(data = df, mapping=aes(xmin=x, xmax=x+1, ymin=y, ymax=y+1), fill="white") +
    geom_rect(data = df, mapping=aes(xmin=x+0.05, xmax=x+0.95, ymin=y, ymax=y+1, fill=my_pal)) +
    geom_text(data = df, mapping=aes(x=x+0.5, y=y+0.5, label=my_pal), colour="white", hjust=0.5, vjust=1.3, size=4)
    print(p)
}

#custom_col <- c("#FFDB6D", "#C4961A", "#F4EDCA",
#                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")
#show_palette(custom_col)
