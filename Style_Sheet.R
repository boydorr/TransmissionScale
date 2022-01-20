pnas.c <- list(blue1="#CEDBEC", blue2="#3483B1", blue3="#0464A4", blue4="#155A88", blue5="#013667", red="#A21122", purple5="#39348A", purple2="#B8B6D6",
               grey1="#F5F5F5", grey2="#EFEFEF", grey3="#C6C6C6", grey4="#6A6A6A", grey5="#292A2A", green1="#8EA446", green2="#567613")
sc.colours <- list(dark.navy = "#132936", blue = "#36588A", bright.blue = "#5080CC", dark.red = "#6E110B", bright.red = "#CA2015", grey = "#666666", bg.grey = "#E6E6E6", orange = "#FD9826", black = "#000000")


sf <- 2 # scale factor
ss <- theme_classic() + theme(axis.line.x = element_line(colour=pnas.c$grey5, size = sf*0.25),
                              axis.line.y= element_line(colour=pnas.c$grey5, size = sf*0.25),
                              axis.ticks = element_line(size = sf*0.25, colour=pnas.c$grey5),
                              axis.text = element_text(size = sf*9, colour=pnas.c$grey5), # axis tick labels
                              axis.title.x = element_text(size = sf*11, margin = margin(t = 10)),
                              axis.title.y = element_text(size = sf*11),
                              legend.text = element_text(size = sf*9, colour=pnas.c$grey5),
                              legend.title = element_text(size = sf*11),
                              strip.background = element_rect(colour="white", fill="white"),
                              strip.text = element_text(size = sf*9))
# PNAS has three figure widths (1, column, two-column narrow with caption to side, two-column wide with caption below)
pnas.1col.w <- sf*8.7
pnas.2col.w <- sf*11.4
pnas.2col.ww <- sf*17.8

sc.1col.w <- sf*8.7
sc.2col.w <- sf*11.4
sc.2col.ww <- sf*17.8
