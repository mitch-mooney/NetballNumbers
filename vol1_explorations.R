# load necissary libraries
library(tidyverse)
library(patchwork)
library(rpart)
library(rpart.plot)
library(ggdendro)
library(ggimage)

# read in data from .csv file
vol1_data <- readr::read_csv("netball-numbers-challenge/datasets/vol1/resultsFromTheTimeMachine.csv")

###########################################################################################################
####                                         Custom theme for plots                                   #####
###########################################################################################################

# create function to customise plotting
theme_netball <- function(base_size = 15, 
                          base_family = "serif",
                          base_line_size = base_size/22, 
                          base_rect_size = base_size/22,
                          background_hex = "#8cbaa3",
                          text_color = "#575958"){
  theme_grey(
    base_size = base_size, 
    base_family = base_family, 
    base_line_size = base_line_size, 
    base_rect_size = base_rect_size) +
    theme(panel.background = element_rect(fill = "#404241", colour = "#404241"), 
          # added the same color argument to apply to the entire plot
          plot.background = element_rect(fill = background_hex, colour = background_hex),
          panel.border = element_rect(fill = NA, colour = NA), 
          panel.grid = element_line(colour = background_hex, size = 0.3), 
          panel.grid.minor = element_line(size = rel(0.5)), 
          strip.background = element_rect(fill = background_hex, colour = background_hex), 
          legend.key = element_rect(fill = background_hex, colour = NA),
          legend.background = element_rect(fill = background_hex),
          legend.text=element_text(colour = text_color, family = base_family),
          legend.title=element_text(colour = text_color,size = base_size/1.25, family = base_family),
          # remove the x title: might not want to make this a global setting!!
          axis.line = element_line(size = 1, colour = background_hex),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(color = text_color, size = base_size*1.5, family = base_family, face = "bold"),
          plot.subtitle = element_text(color = text_color, size = base_size, family = base_family, face = "italic"),
          plot.caption = element_text(color = text_color, size = base_size/1.5, family = base_family, face = "italic"),
          axis.title.x = element_text(color = text_color, size = base_size/1.5, family = base_family, face = "bold"),
          # add an accent color for the plot text and increase size
          axis.title.y = element_text(color = text_color, size = base_size/1.5, angle = 90, family = base_family, face = "bold"),
          axis.text.x = element_text(color = text_color, size = base_size, family = base_family),
          axis.text.y = element_text(color = text_color, size = base_size, family = base_family),
          # Facet labels
          strip.text.x = element_text(color = text_color, size = base_size, family = base_family, angle = 90),
          strip.text.y = element_text(color = text_color, size = base_size, family = base_family, angle = 90),
          complete = TRUE
    )
}

###########################################################################################################
####                                         Data wrangling                                           #####
###########################################################################################################

# add a few more columns for coaches
coaching.data <- vol1_data %>% 
  group_by(coach) %>% 
  arrange(year) %>% 
  mutate(goal.diff.coach = cumsum(Gdiff),
         years.coached = row_number(),
         wins.coached = cumsum(win),
         loss.coached = cumsum(loss),
         draw.coached = cumsum(draw),
         win.prop = wins.coached/(wins.coached+loss.coached+draw.coached),
         coach.next.year = ifelse(year - lead(year, n=1) < 2, 'YES', 'NO'),
         coach.next.year = ifelse(is.na(coach.next.year), 'NO', coach.next.year),
         wins_last_year = lag(win)) %>% 
  ungroup() 

# getting total number of coaches per team in the data period
team.coached <- coaching.data %>% 
  group_by(team) %>% 
  distinct(coach) %>% 
  count(coach) %>% 
  summarise(coaches = sum(n)) %>% 
  ungroup() 
# get total number of wins and seasons played
# then join with the total number of coaches data frame above.
win.record <- coaching.data %>% 
  group_by(team) %>% 
  summarise(years.played = n(), win = sum(win)) %>% 
  mutate(wins.per.year = win/years.played) %>% 
  left_join(team.coached) %>% 
  mutate(coaches.per.year = coaches/years.played)

# Gather logos of teams for visualisation
# call in images using png package
filenames <- list.files("images/", pattern="*.png", full.names=TRUE) # this should give you a character vector, with each file name represented by an entry
teams <- str_split_fixed(filenames, '/', 2)[,2]
teams <- str_split_fixed(teams, '.png', 2)[,1]
# create a tibble with the teams and file paths to the images
image.ref <- tibble(label = filenames,
                    team = teams)

###########################################################################################################
####                                         Plot1                                                    #####
###########################################################################################################

# run statistical test to determine the non-parametric correlation between wins per season and coaching churn per season
correlation <- cor.test(win.record$wins.per.year, win.record$coaches.per.year, method = 'spearman')
# plot the outcome with a linear regression line.
# Add logos of the teams as the points.
(plot.stability <- win.record %>% 
    left_join(image.ref, by = c('team')) %>% # join with images to plot
  ggplot(aes(x = wins.per.year, y = coaches.per.year)) +
  geom_point(aes(color = team)) +
  ggimage::geom_image(aes(x = wins.per.year, y = coaches.per.year, image = label), size=.1, by='height')+
  geom_smooth(method = 'lm', se=FALSE) +
  labs(subtitle = 'Average number of coaches per season per team \nby average wins per season per team\n', 
       y = 'average number of coaches per year',
       x = 'average wins per year',
       color = 'Team') +
  theme_netball(background_hex = "#DAF7A6")+
  theme(legend.position = 'none')
)

# description as a single string
description.1 <- paste0('To understand the stability of coaches at a team, above, is a plot of average \n',
                        'wins per year over the average number of coaches per year. The correlation test, \n', 
                        'indicates that the relationship is moderate r = ', round(correlation$estimate,2), ', p = ', round(correlation$p.value,3))

(plot1.describe <- ggplot() +
    geom_label(aes(x = 1, y =1, label = description.1, family = "serif"),label.padding = unit(1, "lines"), color = '#575958', fill= '#DAF7A6',size = 4) +
    theme_void()+
    theme(plot.background = element_rect(fill = "#DAF7A6", colour = "#DAF7A6"))
  
)

###########################################################################################################
####                                         Plot2                                                    #####
###########################################################################################################

# plot coaching years over win percentage highlighting if they coach the following year.
(coaching.plot<-coaching.data %>% 
    filter(year < 2021) %>% 
    ggplot(aes(x = years.coached, y = win.prop, group= coach)) +
    geom_point(aes(color = coach.next.year), alpha = 0.5, size = 3)+
    geom_line(aes(x = years.coached, y = win.prop), color = 'grey') +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values=c("#F67844", "#8DF644")) +
    #geom_hline(yintercept = 0, size = 2, color = 'grey', alpha = 0.5) +
    labs(subtitle = 'Win percentage of coaches across their career,\n Coaches who have coached less than 3 years were excluded.\n',
         color = 'coached next year',
         y = 'Accumulated Win',
         x = 'Seasons Coached (since 2008)') +
    theme_netball(background_hex = "#DAF7A6")+
    theme(legend.position = 'bottom')
)

# description as a single string
description.2 <- paste0('Mapping the coaching journeys the below plot shows\n ', 
                        'at what stage coaches stop coaching at the elite level \n ', 
                        'and what their corresponding accumulative win percentage had been.')

(plot2.describe <- ggplot() +
    geom_label(aes(x = 1, y =1, label = description.2, family = "serif"),label.padding = unit(1, "lines"),color = '#575958',  fill= '#DAF7A6',size = 4) +
    theme_void()+
    theme(plot.background = element_rect(fill = "#DAF7A6", colour = "#DAF7A6"))
  
)


###########################################################################################################
####                                         Plot3                                                    #####
###########################################################################################################


# gather variables of interest
model.data <- coaching.data %>% 
  filter(year < 2021) %>% 
  select(coach.next.year, win, Gdiff, years.coached, win.prop) %>% 
  drop_na() # ensure no NAs 

# Run classification decision tree using rpart
fit <- rpart(coach.next.year~., data = model.data, method = 'class')

# plot tree using rpart.plot
# this is what is should look like
par(bg="#DAF7A6", family = 'serif', col = "#575958",col.axis = "#575958", col.main = "#575958", col.sub = "#575958")
rpart.plot(fit, extra = 104, box.palette = c('#F67844', '#8DF644'), 
           main = 'Decision Tree predicting coaches not coaching next season')

# Turns out that rpart.plots are very difficult to turn into a grob object to put into a broader ggplot layout
# So we had to use a package ggdendro to grab some of the split infomation to great a graphic in ggplot
# trying to create a ggplot object from the decision tree r.part model

fitr <- dendro_data(fit) # gather the split information from the model
# gather the probability values from the model to add as text to the plot
prob.splits <- as.data.frame(fit[["frame"]][["yval2"]])[4:7,]
# bind with dendro_data for the plot
prob.splits <- cbind(prob.splits, fitr$leaf_labels)
prob.splits$V4 <- round(prob.splits$V4, 2) # round values for display for 'No' classication 
prob.splits$V5 <- round(prob.splits$V5, 2)   # round values for display for 'Yes' classification
# Add labels for segments in tree
seg.labs <- fitr$segments %>% 
  group_by(x) %>% 
  filter(y == max(y)) %>% 
  ungroup() %>% 
  group_by(xend) %>% 
  filter(yend == max(yend)) %>% 
  ungroup()
# rebuild key parts of decision tree visualisation in ggplot2 platform
(dec.tree <- ggplot() +
  # add tree segments
  geom_segment(data = fitr$segments, 
               aes(x = x, y = y, xend = xend, yend = yend), size =2) +
  # add decision labels for segments
  geom_text(data = seg.labs, aes(x = x, y = y+0.0075, label = 'yes'), size = 3, fill = "#DAF7A6") +
  geom_text(data = seg.labs, aes(x = xend, y = y+0.0075, label = 'no'),  size = 3,fill = "#DAF7A6") +
  # add condition labels to segements
  geom_label(data = fitr$labels, aes(x = x, y = y, label = label), size = 5, color = '#575958') +
  geom_label(data = prob.splits, aes(x = x, y = y, label = label, fill = label), size = 5, color = '#575958') +
  # add probability splits for end decisions
  geom_text(data = prob.splits, aes(x = x, y = y-0.025, label = paste0('No:', V4, '\nYes:', V5)), size =3, color = '#575958')+
  scale_color_manual(values=c("#F67844", "#8DF644")) +
  scale_fill_manual(values=c("#F67844", "#8DF644")) +
  # make theme consistent with other plots
  theme(legend.position = 'none',
        plot.background = element_rect(fill = "#DAF7A6", colour = "#DAF7A6"),
        plot.subtitle = element_text(hjust = 0.5, color = '#575958', size = 12, family = "serif", face = "italic")) +
  # add themes for tree in ggplot
  theme_dendro() +
  labs(subtitle = 'Decision Tree predicting coaches not coaching next season, \nDecision probabilities at the bottom of the tree')
)

# description as a single string
description.3 <- paste0('Looking for a pattern in the data of coaches not coaching at the elite level the \n',
                        'following year. Follow the conditions in the decision tree to find the corresponding \n', 
                        'probability the coach will remaining a coach the next year at the elite level with \n', 
                        'the best guess from the algorithm.')


(plot3.describe <- ggplot() +
    geom_label(aes(x = 1, y =1, label = description.3, family = "serif"),label.padding = unit(1, "lines"),color = '#575958',  fill= '#DAF7A6',size = 4) +
    theme_void()+
    theme(plot.background = element_rect(fill = "#DAF7A6", colour = "#DAF7A6"))
  
)

###########################################################################################################
####                                    Putting the plots together                                    #####
###########################################################################################################


# using layout from patchwork to organize plots in a single page
# t = 1st row reference, l = first column reference, b = final row reference, r = final column reference.
layout <- c(
  area(t = 1, l = 1, b = 5, r = 4 # plot 1
  ), 
  area(t = 6, l = 1, b = 8, r = 4 # description of plot
  ), 
  area(t = 3, l = 5, b = 8, r = 8 # plot 2 
  ), 
  area(t = 1, l = 5, b = 2, r = 8 # plot 2 description
  ), 
  area(t = 1, l = 9, b = 5, r = 12 # plot 3
  ), 
  area(t = 6, l = 9, b = 8, r = 12 # description of plot 3
  )) 

# layout plots using patchwork
plot.stability + plot1.describe + coaching.plot + plot2.describe + dec.tree + plot3.describe +
  plot_layout(design = layout) +
  plot_annotation(title = 'Ever wondered about coaching churn in elite netball?',
                  subtitle = "Analysing the influence of win/loss on the coaching journey in ANZ Championships and Suncorp Super Netball leagues from 2008",
                  caption = 'Twitter: @mitch_mooney | Data: github.com/aaronsfox/netball-numbers-challenge',
                  theme = theme(panel.background = element_rect(fill = "#DAF7A6"),
                                plot.background = element_rect(fill = "#DAF7A6", colour = "#DAF7A6"),
                                plot.title = element_text(size = 32,hjust = 0.5,
                                                          colour = "darkblue", family = "serif",
                                                          face = "bold"),
                                plot.subtitle = element_text(family = "serif", size = 18, hjust = 0.5,
                                                                 colour = "darkgrey",
                                                                 lineheight = 1.2),
                                plot.caption = element_text(family = "serif", size = 12,
                                                            colour = "darkblue")))

# Save plots as single image
ggsave(paste0("netball_numbers_", format(Sys.time(), "%d%m%Y"), ".png"),
       dpi = 720,
       width = 19, 
       height = 10)
