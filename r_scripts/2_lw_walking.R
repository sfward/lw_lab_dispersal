


# Load packages -----------------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
source("r_scripts/1_packages.R")
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




# Load data -----------------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
lw_crawler_dispersal <- read_excel("data/lw_dispersal_data.xlsx", na=".", range="A1:G993")
head(lw_crawler_dispersal)
tail(lw_crawler_dispersal)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







# Data cleaning -----------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#duplicating data, fixing transcription errors, zeroing data for cardboard/paper data 
#duplicate data 
lw_dispersal <- lw_crawler_dispersal 

#zero data out using starting coordinates
unique_lw_trials <- unique(lw_dispersal$trial)
i <- unique_lw_trials[1] 
for (i in unique_lw_trials){
  curr_trial <- lw_dispersal[which(lw_dispersal$trial == i),]
  
  # get starting location
  x_start <- curr_trial[1,"dist_from_left_CM"] 
  y_start <- curr_trial[1,"dist_from_top_CM"] 

  # apply correction to "zero out" coordinates
  lw_dispersal[which(lw_dispersal$trial == i), "dist_from_left_CM"] <- lw_dispersal[which(lw_dispersal$trial == i), "dist_from_left_CM"] - x_start$dist_from_left_CM  
  lw_dispersal[which(lw_dispersal$trial == i), "dist_from_top_CM"] <- lw_dispersal[which(lw_dispersal$trial == i), "dist_from_top_CM"] - y_start$dist_from_top_CM  
  
}  

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




# Create time stamps   --------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#creating time stamp for cardboard versus paper - need lubridate and tidyverse
timestamp <- lw_dispersal %>% 
  dplyr::mutate(Hours = hour(seconds_to_period(time_sec)),
         Minutes = minute(seconds_to_period(time_sec)),
         Seconds = second(seconds_to_period(time_sec))) %>%
  dplyr::select(Hours, Minutes, Seconds) %>%
  unite(HMS_time, Hours:Seconds, sep=":")

lw_dispersal$HMS_time <- timestamp$HMS_time
#convert timestamp from above to appropriate format - done in the bayes move code 
#as.POSIXct(concat_date$timestamp, format=("%H:%M:%S"))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# Using bayesmove to quantify movement ---------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#USING BAYES MOVE PACKAGE 
#create the data offrame for prep_data function of bayes move (may not be necessary - but it is necessary to name the time column "date")
bayesmove_lm <- data.frame(id = lw_dispersal$trial,x = lw_dispersal$dist_from_left_CM,y = lw_dispersal$dist_from_top_CM,date = as.POSIXct(lw_dispersal$HMS_time, format=("%H:%M:%S")))

#
results_lw <- prep_data(dat = bayesmove_lm,coord.names = c("x", "y"), id = "id")
results_lw$time_sec <- lw_dispersal$time_sec+10
results_lw$time_sec_w_NA <- ifelse(is.na(results_lw$step), NA, results_lw$time_sec)
results_lw$x_m <- results_lw$x/100
results_lw$y_m <- results_lw$y/100
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Movement summary ------------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#summarizing results from bayes move 
#sum steps by animal (trial ID) for cardboard versus paper 

summ_stats_lw <- results_lw %>%
  group_by(id) %>%
  summarise(
    total_distance_m = sum(step, na.rm = TRUE)/100, # convert to CM
    time_sec = max(time_sec_w_NA, na.rm = TRUE),
    time_min = time_sec/60,
    total_distance_m_per_minute = total_distance_m / ( max(time_min, na.rm = TRUE) ),
    correction_ratio <- 300/max(time_sec, na.rm = TRUE) ,
    total_distance_m_per_5minute = total_distance_m * (300/max(time_sec, na.rm = TRUE) )
    
  )

View(summ_stats_lw)


summary(summ_stats_lw)
summary(summ_stats_lw$total_distance_m_per_5minute)
t.test(summ_stats_lw$total_distance_m_per_5minute)


# how many crawled off the page?
page_leavers <- results_lw[which(is.na(results_lw$x)),]
unique(page_leavers$id)
length(unique(page_leavers$id))
table(page_leavers$id)
summary(as.numeric(t(table(page_leavers$id))))







# Figure - lacewings ---------------------------------------------------------------------------
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot(1,1)

axis_vals <- 12/100 
alpha_path <- 0.5 # transparency of lines
my_linewidth <- 1
summary(results_lw)

my_colors_gp = carto_pal(9, "Antique")[6:7]

g_1 <- 1:4
g_2 <- 5:8
g_3 <- 9:12
g_4 <- 13:16
g_5 <- 17:20
g_6 <- 21:24
g_7 <- 25:28
g_8 <- 29:32

unique(parse_number(page_leavers$id))
p_1 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_1, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("y")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_2 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_2, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_3 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_3, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("y")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_4 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_4, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_5 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_5, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("y")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_6 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_6, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("")+
  ylab("")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_7 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_7, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("x")+
  ylab("y")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

p_8 <- ggplot(results_lw[which(results_lw$id %in% c(paste("LW_", g_8, sep=""))),], aes(x = x_m, y = y_m, group = id, color=id))+
  geom_path(alpha=alpha_path, linewidth=my_linewidth,   linejoin = "round")+ 
  geom_point(size=1, alpha=0.5)+
  xlim(-axis_vals,axis_vals) + ylim(-axis_vals,axis_vals)+
  theme_classic() + theme(legend.position="none")+
  xlab("x")+
  ylab("")+
  scale_color_carto_d(palette = 2)+
  coord_fixed()

resize.win(5.5,9.44) # warnings are for insects that left the page (two per graph)
  (p_1|p_2)/
  (p_3|p_4)/
  (p_5|p_6)/
  (p_7|p_8)

 # theme(plot.tag.position  = c(.2, .96))) +
 #  plot_annotation(tag_levels = "A") 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




