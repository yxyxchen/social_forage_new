library('ggplot2')


symnum.args <- list(cutpoints = c(0,0.001, 0.01, 0.05, 1),
                    symbols = c("***", "**", "*", "ns"))

myTheme = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), text=element_text(face = "bold", size = 18),
                axis.line = element_line(size = 1),
                panel.background = element_blank())