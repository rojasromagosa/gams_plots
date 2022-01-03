


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

xaxis <- theme(axis.title.y=element_text(size= 12,angle=90), margin = margin(t=0, r=10, b=0, l=0))


#' Instead of defining the size of each element, we can also specify the size relative to the default size
my_theme3 <- theme(text=element_text(size=11), #default text size
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
                   legend.title = element_text(size = rel(1.1))
)



