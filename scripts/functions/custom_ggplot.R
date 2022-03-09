
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# customized ggplot themes  ----

my_theme1 <- theme_minimal() + 
  theme(strip.background = 
          element_rect(fill="lightgoldenrod"), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white",colour=NA), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text.x=element_text(size= 12),  # vjust is distance to axis; angle=90 changes text direction
        axis.title.x=element_text(size= 12),
        axis.text.y=element_text(size= 14, margin = margin(t=0, r=20, b=0, l=0)), 
        axis.title.y = element_text(size= 12),
        legend.position = "right",
        legend.text = element_text(size=11),
        legend.title = element_text(size=12))


my_theme2 <- theme_minimal() + 
  theme(strip.background = 
          element_rect(fill="lightgoldenrod"), 
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill="white",colour=NA), 
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text.x=element_text(size= 14),  # vjust is distance to axis; angle=90 changes text direction
        axis.title.x=element_text(size= 14),
        axis.text.y=element_text(size= 14, margin = margin(t=0, r=10, b=0, l=0)), # hjust=-0.1 (not working to move closer to axis)
        #axis.title.y = element_blank(),
        legend.position = "right",
        legend.text = element_text(size=11),
        legend.title = element_text(size=12))


#' Instead of defining the size of each element, we can also specify the size relative to the default size
#' Some info on using colors
#' https://www.r-bloggers.com/2013/09/how-to-expand-color-palette-with-ggplot-and-rcolorbrewer/

my_theme3 <- list(
              theme(text=element_text(size=11), #default text size
                   panel.border = element_blank(), # delete panel border
                   panel.background = element_rect(fill = NA), # fill in background 
                   panel.grid.major = element_line(colour = "grey90"), # color of grid
                   axis.line = element_line(),  # add axis lines
                   plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
                   plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
                   axis.title.x = element_text(size = rel(1)),
                   axis.title.y = element_text(size = rel(1)),
                   axis.text.x = element_text(size = rel(1)),
                   axis.text.y = element_text(size = rel(1)),
                   legend.text = element_text(size = rel(1)),
                   #legend.title = element_text(size = rel(1.1)),
                   legend.title = element_blank(),
                   legend.position="right"),
              
              #' Colors used for bar charts
              #scale_fill_manual(values = c("cornflowerblue","royalblue","mediumblue","navy","black","orange","red")),
              scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
              #scale_fill_brewer(palette="Set2"),
              
              #' Colors used in line charts: 
              #scale_color_manual(values = c("blue", "red", "yellow", "green"))
              scale_color_brewer(palette="Set2")
)
              

my_themeNoTtl <-   list(
          theme(text=element_text(size=11), #default text size
          panel.border = element_blank(), # delete panel border
          panel.background = element_rect(fill = NA), # fill in background 
          panel.grid.major = element_line(colour = "grey90"), # color of grid
          axis.line = element_line(),  # add axis lines
#          plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
          plot.title = element_blank(),
#          plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
          plot.subtitle =element_blank(),
          axis.title.x = element_text(size = rel(1)),
          axis.title.y = element_text(size = rel(1)),
          axis.text.x = element_text(size = rel(1)),
          axis.text.y = element_text(size = rel(1)),
          legend.text = element_text(size = rel(1)),
          #legend.title = element_text(size = rel(1.1)),
          legend.title = element_blank(),
          legend.position="right"),
    
    #' Colors used for bar charts
    scale_fill_manual(values = c("cornflowerblue","royalblue","mediumblue","navy","black","orange","red")),
    #scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
    #scale_fill_brewer(palette="Set2"),
    
    #' Colors used in line charts: 
    #scale_color_manual(values = c("blue", "red", "yellow", "green"))
    scale_color_brewer(palette="Set2")
)


themeNT2 <-   list(
  theme(text=element_text(size=11), #default text size
        panel.border = element_blank(), # delete panel border
        panel.background = element_rect(fill = NA), # fill in background 
        panel.grid.major = element_line(colour = "grey90"), # color of grid
        axis.line = element_line(),  # add axis lines
        #          plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
        plot.title = element_blank(),
        #          plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
        plot.subtitle =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1)),
        #legend.title = element_text(size = rel(1.1)),
        legend.title = element_blank(),
        legend.position="right"),
  
  #' Colors used for bar charts
  scale_fill_manual(values = c("cornflowerblue","royalblue","mediumblue","navy","black","orange","red")),
  #scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
  #scale_fill_brewer(palette="Set2"),
  
  #' Colors used in line charts: 
  #scale_color_manual(values = c("blue", "red", "yellow", "green"))
  scale_color_brewer(palette="Set2")
)

themeNT3 <-   list(
  theme(text=element_text(size=11), #default text size
        panel.border = element_blank(), # delete panel border
        panel.background = element_rect(fill = NA), # fill in background 
        panel.grid.major = element_line(colour = "grey90"), # color of grid
        axis.line = element_line(),  # add axis lines
        #          plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
        plot.title = element_blank(),
        #          plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
        plot.subtitle =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), #element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1)),
        #legend.title = element_text(size = rel(1.1)),
        legend.title = element_blank(),
        legend.position="right"),
  
  #' Colors used for bar charts
  scale_fill_manual(values = c("lightskyblue1","cornflowerblue","mediumblue","navy","black","lightseagreen","royalblue","lightskyblue1","orange","red")),
  #scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
  #scale_fill_brewer(palette="Set2"),
  
  #' Colors used in line charts: 
  #scale_color_manual(values = c("blue", "red", "yellow", "green"))
  scale_color_brewer(palette="Set2")
)

themeNT4 <-   list(
  theme(text=element_text(size=11), #default text size
        panel.border = element_blank(), # delete panel border
        panel.background = element_rect(fill = NA), # fill in background 
        panel.grid.major = element_line(colour = "grey90"), # color of grid
        axis.line = element_line(),  # add axis lines
        #          plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
        plot.title = element_blank(),
        #          plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
        plot.subtitle =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y =element_blank(),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_blank(),
        #legend.title = element_text(size = rel(1.1)),
        legend.title = element_blank(),
        legend.position="bottom"),
#        legend.position=element_blank(),
  #' Colors used for bar charts
  scale_fill_manual(values = c("cornflowerblue","royalblue","mediumblue","navy","black","orange","red")),
  #scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
  #scale_fill_brewer(palette="Set2"),
  
  #' Colors used in line charts: 
  scale_color_manual(values = c("black","blue","green","red","yellow","green"))
  #scale_color_brewer(palette="Set2")
)


themeNT5 <-   list(
  theme(text=element_text(size=11), #default text size
        panel.border = element_blank(), # delete panel border
        panel.background = element_rect(fill = NA), # fill in background 
        panel.grid.major = element_line(colour = "grey90"), # color of grid
        axis.line = element_line(),  # add axis lines
        #          plot.title = element_text(hjust = 0.5, size = rel(1.2)), #, size = 14, face = "bold"),
        plot.title = element_blank(),
        #          plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)), #, size = 12, face = "bold"),
        plot.subtitle =element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), #element_text(size = rel(1.5)),
        axis.text.x = element_text(size = rel(1.5)),
        axis.text.y = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(0.85)),
        #legend.title = element_text(size = rel(1.1)),
        legend.title = element_blank(),
        legend.position="right"),
  
  #' Colors used for bar charts
  scale_fill_manual(values = c("lightsteelblue1","lightskyblue1","cornflowerblue","royalblue","mediumblue","navy","black","lightseagreen","orange","red")),
  #scale_fill_manual(values = brewer.pal(9, "Blues")[1:9]),
  #scale_fill_brewer(palette="Set2"),
  
  #' Colors used in line charts: 
  #scale_color_manual(values = c("blue", "red", "yellow", "green"))
  scale_color_brewer(palette="Set2")
)


#' we can make the rotation of the axis ticks conditional on the number of levels. Here we define uni_factor
#' This function is exectuted at the end of each chart to check whether to rotate the x axis labels 
# xaxis <- theme(axis.title.y=element_text(size= 12,angle=90), margin = margin(t=0, r=10, b=0, l=0))
rot.axis <- function(uni_factors){ if(uni_factors>7){theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=0.9))}}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# customized save function  ----

#' we want to adjust the dimensions of the graph depending on:
#' - position of the legend
#' - how much data we want to plot

# this is one is used in plots where the year is on the x axis (plots dimension 2)
custom.save.t <- function(my_theme, filename, data){
  #debug: my_theme = my_theme3
  ggsave( filename = filename,
          units = "in",
          scale = 0.8,
          height = 6 + ifelse(grepl("top|bottom", my_theme[[1]]$legend.position), 2, 0),
          width = 0.01 +
                  8* (length(unique(data$t))/16)^0.2 +
                  ifelse( grepl("right|left", my_theme[[1]]$legend.position), 2, 0)
              ) 
}

# this is one is used in plots where factors are on the x axis (plots dimension 3 and 4)
custom.save.f <- function(my_theme, filename, factors){
  #debug: my_theme = my_theme3
  ggsave( filename = filename,
          units = "in",
          scale = 0.8,
          height = 6 + ifelse(grepl("top|bottom", my_theme[[1]]$legend.position), 2, 0),
          width = 0.01 +
            0.01 + 8* (factors/4)^0.2 +
            ifelse( grepl("right|left", my_theme[[1]]$legend.position), 2, 0)
  ) 
}




