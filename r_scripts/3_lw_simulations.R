

# Dispersal capacity -------------------------------------
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
distance_crawled_per_minute_lw <- summ_stats_lw$total_distance_m_per_minute

iterations <- 200
total_movement_m_1hr <- rep(NA,iterations)
total_movement_m_6hr <- rep(NA,iterations)
total_movement_m_12hr <- rep(NA,iterations)


set.seed(123)
for(i in 1:iterations){

  # walk consistently for one hour
  movement_m_per_minute1 <-  sample(distance_crawled_per_minute_lw,1)
  min_minutes_walked1 <- 1 # 
  max_minutes_walked1 <- 60 #
  minutes_walked_hour <- runif(1, min=min_minutes_walked1, max=max_minutes_walked1)
  total_movement_m_1hr[i] <- movement_m_per_minute1*minutes_walked_hour #
  
  # walk up to 6 hours
  movement_m_per_minute6 <-  sample(distance_crawled_per_minute_lw,1)
  min_minutes_walked6 <- 1 # 
  max_minutes_walked6 <- 60*6 #
  minutes_walked_6hour <- runif(1, min=min_minutes_walked6, max=max_minutes_walked6)
  total_movement_m_6hr[i] <- movement_m_per_minute6*minutes_walked_6hour
  
  
  # walk up to 12 hours
  movement_m_per_minute12 <-  sample(distance_crawled_per_minute_lw,1)
  min_minutes_walked12 <- 1 # 
  max_minutes_walked12 <- 60*12 #
  minutes_walked_12hour <- runif(1, min=min_minutes_walked12, max=max_minutes_walked12)
  total_movement_m_12hr[i] <- movement_m_per_minute12*minutes_walked_12hour
  

}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# Create graph -----------------------------------------
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sort(total_movement_m_1hr)[c(100)]
sort(total_movement_m_1hr)[c(5,195)]

sort(total_movement_m_6hr)[c(100)]
sort(total_movement_m_6hr)[c(5,195)]

sort(total_movement_m_12hr)[c(100)]
sort(total_movement_m_12hr)[c(5,195)]


move_df <- data.frame(
  dist_m_1hr_sim = total_movement_m_1hr,
  dist_m_6hr_sim = total_movement_m_6hr,
  dist_m_12hr_sim = total_movement_m_12hr)


max_y <-0.15
bins_n <- 40
my_colors_gp = carto_pal(6, "Vivid")

hist_raw_d_5 <- 
  ggplot(summ_stats_lw, aes(x = total_distance_m_per_5minute))+
  geom_histogram(aes(y = after_stat(count / sum(count))), width=c(0.5,0.8), fill=my_colors_gp[2], color="black")+
  scale_y_continuous(labels = scales::percent, limits=c(0,max_y))+
  theme_classic() + theme(legend.position=c(0.8,0.8), legend.title=element_blank())+
  xlab("") +
  ylab("Percent")+
  ggtitle('A. 5 min (observed)')

p_1hr_sim <- ggplot(move_df, aes(x = dist_m_1hr_sim)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = bins_n, color="black",fill=my_colors_gp[3]) +
  scale_y_continuous(labels = scales::percent, limits=c(0,max_y))+
  ylab("")+
  xlab("")+
  theme_classic()+
  ggtitle('B. <1 hr (simulated)')

p_6hr_sim <- ggplot(move_df, aes(x = dist_m_6hr_sim)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = bins_n, color="black",fill=my_colors_gp[3]) +
  scale_y_continuous(labels = scales::percent, limits=c(0,max_y))+
  ylab("Percent")+
  xlab("Dispersal (m)")+
  theme_classic()+
  ggtitle('C. <6 hr (simulated)')

p_12hr_sim <- ggplot(move_df, aes(x = dist_m_12hr_sim)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), bins = bins_n, color="black",fill=my_colors_gp[3]) +
  scale_y_continuous(labels = scales::percent, limits=c(0,max_y))+
  ylab("")+
  xlab("Dispersal (m)")+
  theme_classic()+
  ggtitle('D. <12 hr (simulated)')


resize.win(6.7125984,4.5)
(hist_raw_d_5|p_1hr_sim)/
(p_6hr_sim|p_12hr_sim)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

