#Introduction to R

#Data visualization
library(ggplot2)
participants_data<- read.csv("participants_data.csv")

#Create barplot
participants_barplot <- table(participants_data$academic_parents)
barplot(participants_barplot)

#ggplot2: names and email

ggplot(data = participants_data,
       aes(x = age,
           y = number_of_siblings)) + geom_point()

#Create a scatterplot of days to email response (y)
#as a function of the letters in your first name (x)
ggplot(data = participants_data,
       aes(x = letters_in_first_name,
           y = days_to_email_response)) + geom_point()

###############################

#ggplot2: add color and size

ggplot(data = participants_data,
       aes(x = age,
           y = ppbatch,
           color = gender,
           size = number_of_siblings)) + geom_point()

#Create a scatterplot of days to email response (y)
#as a function of the letters in your first name (x)
#with colors representing binary data
#related to academic parents (color)
#and working hours per day as bubble size (size).
ggplot(data = participants_data,
       aes(x = letters_in_first_name,
           y = days_to_email_response,
           color = academic_parents,
           size = working_hours_per_day)) + geom_point()

#Exercise
ggplot(participants_data,
       aes(x = research_continent,
           y = number_of_publications,
           color = gender,
           size = years_of_study)) + geom_point()

###############################

#ggplot2: iris data

# Create a scatterplot of iris petal length (y)
# as a function of sepal length (x)
# with color representing iris species (color)
# and petal width as bubble sizes (size).
ggplot(data = iris,
       aes(x = Sepal.Width,
           y = Sepal.Length,
           color = Species,
           size = Petal.Width)) + 
  geom_point()

###############################

#ggplot2: diamond size

plot1<- ggplot(data = diamonds,
                aes(x = cut,
                    y = clarity,
                    alpha = 0.2)) + 
  #Adjust the level of opacity of the points with the alpha argument
  geom_point()


# Create a plot with diamonds data of carat (x) and the price (y)
plot2<- ggplot(data = diamonds,
                aes(x = carat,
                    y = price,
                    alpha = 0.2)) +
  geom_point()

#Use formula arguments such as the log function
ggplot(data = diamonds,
       aes(x = log(depth),
           y = log(table),
           alpha = 0.2)) +
  geom_point()

###############################

# ggplot2: Colors and shapes

# Using color with ggplot geom_point
# Use top_n function to select the top few rows of the data 
library(dplyr)
dsmall <- top_n(diamonds, n = 10)

ggplot(data = dsmall, aes(x = depth,
                          y = price,
                          color = cut)) +
  geom_point()

# Create a smaller diamond data set (top 100 rows),
# create a scatterplot with carat on the x-axis,
# and price on the y-axis and
# with the color of the diamonds as the color of the points
dsmaller <- top_n(diamonds, n = 100)

ggplot(data = dsmaller, aes(x = carat,
                            y = price,
                            color = color)) +
  geom_point()

# Using shape with ggplot geom_point
dsmall <- top_n(diamonds, n = 10)

ggplot(data = dsmall, aes(x = carat,
                          y = depth,
                          shape = clarity)) +
  geom_point()

# Create a smaller diamond data set (top 40 rows),
# create a scatterplot with carat on the x-axis,
# and price on the y-axis and 
# with the cut of diamond as the shapes for points.
dsmaller <- top_n(diamonds, n = 40)

ggplot(data = dsmaller, aes(x = carat,
                            y = price,
                            shape = cut)) + 
  # warning message: using shape for ordinal variable is not advised 
  # call (str (diamonds) to see that cut is an ordinal factor)
  geom_point()

###############################

# ggplot2: set parameters

# I() Inhibit Interpretation/ Conversion of Objects
# The I function inhibits the conversion of character vectors to factors and 
# the dropping of names, and ensures that matrices are inserted as single columns.
# Here it will allow us to use a single argument for coloring and
# providing opacity to the points.
ggplot(data = diamonds,
       aes(x = depth,
           y = price,
           alpha = I(0.4),
           color = I("green"))) +
  geom_point()

# Create a plot of the diamonds data 
# with carat on the x-axis, price on the y-axis.
# Use the inhibit function to set alpha to 0.1 and color to blue. 
ggplot(data = diamonds, 
       aes(x = carat,
           y = price,
           alpha = I(0.1),
           color = I("blue"))) +
  geom_point()

###############################

# ggplot2: geom options

# With geom different types of plots can be defined e.g. points, line,
# boxplot, path, smooth.
dsmall <- top_n(diamonds, n = 10)

ggplot(data = dsmall,
       aes(x = depth,
           y = price)) +
  geom_point()+
  geom_smooth()

# Create a smaller data set of diamonds with 50 rows.
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
dsmaller <- top_n(diamonds, 50)

ggplot(data = dsmaller,
       aes(x = carat,
           y = price)) +
  geom_point()+
  geom_smooth()

###############################

# ggplot2: smooth function

# geom_smooth() selects a smoothing method based on the data.
# Use method = to specify your preferred smoothing method.
dsmall <- top_n(diamonds, n = 10)

ggplot(data = dsmall,
       aes(x = depth,
           y = price)) +
  geom_point()+
  geom_smooth(method = 'glm')

# Create a smaller data set of diamonds with 50 rows. 
# Create a scatterplot and smoothed conditional 
# means overlay with carat on the x-axis 
# and price on the y-axis.
# Use 'glm' as the option for the smoothing
dsmaller <- top_n(diamonds, 50)

ggplot(data = dsmaller, 
       aes(x = carat,
           y = price)) +
  geom_point() +
  geom_smooth(method = 'glm')

###############################

# ggplot2: boxplots

# Boxplots can be displayed through geom_boxplot().
ggplot(data = diamonds,
       aes(x = color,
           y = carat)) +
  geom_boxplot()

# Change the boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds,
       aes(x = cut,
           y = price/carat)) +
  geom_boxplot()

###############################

# ggplot2: jitter points

# Jittered plots geom_jitter() show all points.
ggplot(data = diamonds,
       aes(x = color,
           y = carat)) +
  geom_point()+
  geom_jitter()

# Change the jittered boxplot so that the x-axis is cut and
#  the y-axis is price divided by carat
ggplot(data = diamonds,
       aes(x = cut,
           y = price/carat)) +
  geom_point()+
  geom_jitter()

###############################

# ggplot2: adding alpha

# In case of overplotting changing alpha can help.
ggplot(data = diamonds,
       aes(x = color,
           y = price/ carat,
           alpha = I(0.1))) +
  geom_point()+
  geom_jitter()

# Change the alpha to 0.4 to make 
# the scatter less transparent
ggplot(data = diamonds,
       aes(x = color,
           y = price/carat,
           alpha = I(0.4))) +
  geom_point()+
  geom_jitter()

###############################

# ggplot2: geom_histogram

# Use geom_histogram to create a density plot
ggplot(data = diamonds,
       aes(x = carat)) +
  geom_density()

# Change the density plot so that the x-axis is carat 
# and the color is the diamond color
ggplot(data = diamonds,
       aes(x = carat,
           color = color)) +
  geom_density()

# Adding alpha
ggplot(data = diamonds,
       aes(x = price,
           color = cut,
           alpha = I(0.5))) +
  geom_density()

# Change the density plot so that the x-axis is carat 
# the color is the diamond color
# and the alpha is set to 0.3 using the inhibit function
ggplot(data = diamonds,
       aes(x = carat,
           color = color,
           alpha = I(0.3))) +
  geom_density()

###############################

# ggplot2: subset

# Use factor to subset the built in mpg data.
ggplot(data = mpg,
       aes(x = displ,
           y = hwy,
           color = class)) +
  geom_point()+
  geom_smooth(method = 'glm')

# Create a plot of the mpg data with 
# manufacturer as the color and a linear model 'lm'
# as the smooth method
ggplot(data = mpg,
       aes(x = displ,
           y = hwy,
           color = manufacturer)) +
  geom_point()+
  geom_smooth(method = 'lm')

###############################

# ggplot2: "slow ggplotting"

# for aes() in ggplot()
# using fewer functions; example - using labs() to add a title instead of ggtitle()
# using functions multiple times; example aes(x = var1) + aes(y = var2) 
# rather than aes(x = var1, y = var2)
# using base R functions and tidyverse functions. 
# For other packages, the :: style to call them
# write out arguments (no shortcuts) aes(x = gdppercap) not aes(gdppercap)

###############################################################################

# ggplot flip book: slow ggplot 
#(https://evamaerey.github.io/ggplot_flipbook/ggplot_flipbook_xaringan.html#3)
library(tidyverse)
ggplot(data = participants_data) +
  aes(x = age) +
  aes(y = `number_of_publications`) +
  geom_point() +
  aes(col = fct_rev(`gender`)) +
  geom_smooth(method = "lm", se = F) +
  scale_color_manual(values = 
                       c("blue", "pink")) +
  ylim(c(0, 15)) + #crop the plot
  labs(col = "",
       x = "Age",
       y = "Number of publications") + #delete legend title fct_rev
  labs(title = "Is there a correlation between researchers' age and number of publications?") +
  labs(subtitle = "Male and female researchers' age and number of publications ") +
  # Break lines will show in output
  theme_bw()

###############################

# Create multiple plots

ggplot(participants_data) +
  aes(x = age, y = number_of_publications) +
  facet_wrap(~ research_continent) + #create multiple plots
  geom_area(alpha = .2,
            position = "dodge") +
  aes(fill = gender) +
  labs(fill = "") +
  labs(x = "Age") +
  labs(y = "Number of publications")+
  labs(title = "Information on Researchers") +
  labs(subtitle = "Male and female researchers' age and number of publications in different continents")+
  theme_light()

###############################

# Create multiple plots with dashed area

ggplot(participants_data) +
  aes(x = age) +
  aes(y = `working_hours_per_day`) +
  aes(xmin = 0) +
  aes(xmax = age) +
  aes(ymin = 0) +
  aes(ymax = `working_hours_per_day`) +
  facet_wrap(fct_inorder(continent_of_origin) ~ .) +
  geom_rect(fill = "blue", alpha = .2) +
  aes(yend = 0) +
  aes(xend = 0) +
  geom_segment(
    aes(yend = `working_hours_per_day`), 
    lty = "dashed") +
  geom_segment(aes(xend = age), 
               lty = "dashed") +
  scale_y_continuous(limits = c(0, 20), 
                     expand = c(0, 0), 
                     breaks = c(0, 5, 15, 20)) +
  scale_x_continuous(limits = c(0, 50), 
                     expand = c(0, 0),
                     breaks = c(0, 10, 20, 30, 40, 50)) +
  labs(x = "Age (years)") +
  labs(y = "Working hours per day (hours)") +
  labs(title = "Daily working hours based on Age") +
  labs(subtitle = "Daily working hours of participants in different continents") +
  theme_bw(base_size = 12)

###############################

# Create colored grid horizontal barplot

ggplot(data = Scentdata) + 
  aes(x = Scent) +
  aes(y = Percent) +
  aes(fill = `Rank (test)`) +
  facet_grid(Type ~ .) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = colorRampPalette(
      RColorBrewer::brewer.pal(n = 9, name = "Reds"))(7), 
    guide = guide_legend(reverse = TRUE)) +
  labs(fill = "") +
  xlab("") +
  labs(title = "Ranks of perfumes based on scent and color") +
  labs(subtitle = "Data Source: @Me")

###############################

# Create boxplot comparisons

ggplot(participants_data) +
  aes(x = gender) + 
  aes(y = working_hours_per_day) +
  geom_jitter(alpha = .5, 
              height = 0, 
              width = .25) +
  aes(col = gender) +
  geom_boxplot(alpha = .25) +
  aes(fill = gender) +
  scale_colour_manual(values = 
                        c("pink", "blue")) +
  scale_fill_manual(values = 
                      c("pink", "blue")) +
  theme_bw() +
  labs(title = "Daily working hours of male and female participants") +
  labs(subtitle = "Data source: participants_data") +
  labs(caption = "Visualization: Me")

###############################

# Create multiple line graphs

ggplot(data = participants_data) + 
  aes(x = ppbatch) +
  aes(y = number_of_publications) +
  geom_line() + 
  facet_wrap(~ continent_of_origin) +
  labs(y = "Number of publications") +
  labs(title = "Number of publications among seven research continents, 2017-2021") +
  labs(caption = "Vis: @Me| Source: participants_data") +
  theme_minimal()

###############################

# Create multiple jitter plot with smooth overlay

ggplot(participants_data) +
  aes(x = ppbatch) +
  aes(y = `age`) +
  facet_wrap(~ continent_of_origin, scales = "free_y", nrow = 2) +
  geom_jitter(mapping = aes(col = fct_inorder(gender)), 
              width = 1, height = .5, size = 1) +
  geom_smooth(col = "grey30") +
  annotate(geom = "ribbon", x = c(2017,2021),
           ymin = 20, ymax = 50, 
           alpha = .1, fill = "yellow") +
  geom_hline(yintercept = c(20, 50), lty = "dotted") +
  geom_hline(yintercept = c(35), lty = "dashed") +
  scale_color_manual(values = c("pink", "blue")) +
  labs(x = "", col = "") +
  labs(title = "Age of researchers from batch 2017 to 2021") + 
  labs(subtitle = "Includes gender") +
  labs(caption = "Source: participants_data | Vis: Me \nValues 'jittered' to reduce overplotting") +
  theme_bw(base_size = 13)

###############################

# Create multiple line graph with x- and y- intercepts

ggplot(data = participants_data) +
  aes(x = `ppbatch`) +
  aes(y = `number_of_publications`) +
  facet_wrap(~ continent_of_origin) +
  geom_line() +
  geom_hline(yintercept = 5, col = "grey") +
  geom_vline(xintercept = as.numeric("2018"), lty = "dashed") +
  #use as.POSIXct for the chosen date e.g. 
  #geom_vline(xintercept = as.numeric(as.POSIXct("2016-06-23")), lty = "dashed")
  labs(title = "Researchers data") +
  labs(subtitle = "Source: Me")

###############################

# Create ribbon with multiple line charts and x-, y- intercepts

ggplot(data = flipbookdataset20) +
  aes(x = `Date (start of quarter)`) +
  aes(y = `Percentage change from previous period`) +
  aes(ymin = min_) +
  aes(ymax = max_) +
  geom_hline(yintercept = 2, col = "grey") +
  geom_ribbon(alpha = .5) +
  geom_line(aes(col = Country), alpha = .5)# +
  geom_line(data = flipbookdataset20, col = "black") +
  geom_vline(xintercept = 
               as.numeric(as.POSIXct("2011-01-07")), 
             lty = 2) +
  annotate(geom = "text", 
           x = as.POSIXct("2011-01-07"), y = 1.5, 
           label = "Brexit Vote", angle = 90) +
  labs(title = "Quarterly GDP Growth of G7 in Relation to Brexit Vote", 
       subtitle = "Source: OECD", col = "") +
  theme_bw()






















################################################################################

# ggplot2: not slow example

# Add a title and axis labels in ggplot.
ggplot(mtcars,
       aes(mpg,
           y = hp,
           col = gear)) +
  geom_point() +
  ggtitle("Cars") +
  labs(x = "mpg",
       y = "hp",
       col = "Gear")

# ggplot2: slow ggplotting example

# This is an example of a ‘slow ggplotting’ version for the same plot that
#we created above. This format can be easier to read later
#(more kind to other readers and future you) but can be more laborious to write.
ggplot(data = mtcars) +
  aes(x = mpg) +
  labs(x = "mpg") + #"the x label"
  aes(y = hp) +
  labs(y = "hp") + #"the y label"
  geom_point() +
  aes(col = gear) +
  labs(col = "Gear") + #"the legend title"
  labs(title = "Cars")

###############################

# ggplot2: geom_tile

# Use dplyr, and ggplot2 together

#Perform Pearson correlation study
# subset the data to numeric only with select_if
part_data <- select_if(participants_data, 
                       is.numeric)
# use 'cor' to perform pearson correlation
# use 'round' to reduce correlation 
# results to 1 decimal
cormat <- round(cor(part_data), 
                digits = 1)
# use 'as.data.frame.table' to build a table
# with correlation values
#The results of this, i.e. names(melted_cormat), are “Var1”, “Var2” (x and y) 
#and “value” (the correlation result). 
melted_cormat <- as.data.frame.table(cormat, 
                                     responseName = "value")
# plot the result with 'geom-tile'
ggplot(data = melted_cormat, 
       aes(x = Var1,
           y = Var2,
           fill = value)) + 
  geom_tile()

###############################

# Export figures

#To export figures we can use the png function from the grDevices library. 
#Call the png() function and tell it your specifications for the plot 
#(consider the requirements of the targeted publication). 
#Then run the code that generates the plot before calling dev.off() 
#to reset the ‘active’ device.
png(file = "cortile.png", width = 7, height = 6, units = "in", res = 300)

ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()




