# Run 3
# Pre-processing script

##################################################################
#Video stats pre-processing 
sum(is.na(cyber.security.3_video.stats))

############## DATA CONSTRUCTION##################################3
video_views1 = select(cyber.security.3_video.stats, c(1:15))
Video_devices1 = select(cyber.security.3_video.stats, c(16:21))
video_location1 = select(cyber.security.3_video.stats, c(22:28))
