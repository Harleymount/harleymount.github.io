#before starting: 
#note that my experiments are setup so that each plate is exactly identical and a technical replicate of each other

#read in necessary packages
library(growthcurver)
library(ggplot2)
library(Cairo)
#tell the script how long your time interval and reading time is
time_interval=15
number_of_hours=48
#-----read the results file that was converted using the robot computer spconversion tool
file<-read.csv('results.txt', header=F)
#note that my experiments are setup so that each palte is exactly identical and a technical replicate of each other
#split plates based on location in the robot
#tech replicate 1
rep_1<-subset(file, file$V1=='S3L1')
#tech replicate 2
rep_2<-subset(file, file$V1=='S3L2')
#---------------------Time formatting
#data formatting
#Convert 15:01:28 format to total minutes 
#Note I calculate this time for one of my technical replicates, but the intervals should be the same for all 
#plates so it doesnt really matter
#---get the time values
time<-rep_1$V3
new_time=c()#where I will store minute convered values


#time conversion loop to put everything in number of minutes
for (i in time){ # loop over old time format and convert to total minutes format
    values<-unlist(strsplit(as.character(i), ':'))
    time_in_minutes=as.numeric(values[1])*60 + as.numeric(values[2]) + as.numeric(values[3])/60
    new_time<-c(new_time, time_in_minutes)
}
#With total minutes determined we can then begin to calculate differences between time points
start_point=1 #used as a place holder as we iterate over our new time and need to calculate the different from the first measurement
difference_time<-c() # here the difference between intervals is stored
#differences between time points
for (i in new_time){ # loop over the minute wise time and then calculate the difference between points
    calculated_difference<-new_time[start_point+1]-new_time[start_point]
    if (is.na(calculated_difference)){
        calculated_difference=time_interval # because we are finding differences the last time point has no difference and therefore I just set it to 15, it shouldn't matter too much, but with intervals that are not 15 minutes would require altering this code
    }
    else if (!is.na(calculated_difference) & calculated_difference < 0){
        calculated_difference<-1440+calculated_difference # if we go into the next day account for this with adding the negative value to the total number of minutes in a day 
    }
    difference_time<-c(difference_time, calculated_difference)
    start_point=start_point+1 # move one more item down the set 
}
#Now that we know the difference between each time point we can sum them across each interval to get cumulative time in munutes 
cumulative_time<-c()
growth_time=0
for (i in difference_time){ # determine the total time passed at each point by adding its time with the previous value
    growth_time<-i + growth_time
    cumulative_time<-c(cumulative_time, growth_time)
    
}
# add the time values we calculated as row names because it looks nicer than having it as a column 
rownames(rep_1)<-cumulative_time
#this cumulative time can be used for both replicates, there is slight variation between total times but the differences should be maintained between all samples 
#--------------------------

#------------------------Calculate average and SD between plate replicates-----------------------
#do some math on the plate technical replicates 
#here we take only the read values for the plate and put them in a numeric matrix to be used for matrix math
#read values start in column 5
matrix_rep1<-as.matrix(rep_1[,5:length(rep_1[,])])
matrix_rep2<-as.matrix(rep_2[,5:length(rep_2[,])])
#put two technical replicate matrices in a list so we can do math 
matrix_list<-list(matrix_rep1, matrix_rep2)

#calculate mean and SD
matrix_mean<-apply(simplify2array(matrix_list), 1:2, mean) # calculate mean of two matrices
matrix_SD<-apply(simplify2array(matrix_list), 1:2, sd) # calculate SD of two matrices

#add column names and row names to our data frames 
col_names_output_from_robot<-c('A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8', 'A9', 'A10', 'A11', 'A12','B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B9', 'B10', 'B11', 'B12','C1', 'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', 'C9', 'C10', 'C11', 'C12','D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10', 'D11', 'D12','E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7', 'E8', 'E9', 'E10', 'E11', 'E12','F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', 'F9', 'F10', 'F11', 'F12','G1', 'G2', 'G3', 'G4', 'G5', 'G6', 'G7', 'G8', 'G9', 'G10', 'G11', 'G12','H1', 'H2', 'H3', 'H4', 'H5', 'H6', 'H7', 'H8', 'H9', 'H10', 'H11', 'H12')
colnames(matrix_mean)<-col_names_output_from_robot
colnames(matrix_SD)<-col_names_output_from_robot

#robot column names are in this format above, so we label the columns appropriately
rownames(matrix_mean)<-cumulative_time # add cumulative time we calculated as a row name in teh matrix
rownames(matrix_SD)<-cumulative_time #do this for the SD matrix also
#col_names<-c('A1', 'B1','C1','D1','E1','F1','G1','H1','A2', 'B2','C2','D2','E2','F2','G2','H2','A3', 'B3','C3','D3','E3','F3','G3','H3','A4', 'B4','C4','D4','E4','F4','G4','H4','A5', 'B5','C5','D5','E5','F5','G5','H5','A6', 'B6','C6','D6','E6','F6','G6','H6','A7', 'B7','C7','D7','E7','F7','G7','H7','A8', 'B8','C8','D8','E8','F8','G8','H8','A9', 'B9','C9','D9','E9','F9','G9','H9','A10', 'B10','C10','D10','E10','F10','G10','H10','A11', 'B11','C11','D11','E11','F11','G11','H11','A12', 'B12','C12','D12','E12','F12','G12','H12')


#---------------format the mean values into a dataframe that can be used by growth curver
mean_data_frame<-as.data.frame(matrix_mean)
SD_data_frame<-as.data.frame(matrix_SD)
#reorder the dataframe to a format that can be used by growth_curver
mean_data_frame<-mean_data_frame[c('A1', 'B1','C1','D1','E1','F1','G1','H1','A2', 'B2','C2','D2','E2','F2','G2','H2','A3', 'B3','C3','D3','E3','F3','G3','H3','A4', 'B4','C4','D4','E4','F4','G4','H4','A5', 'B5','C5','D5','E5','F5','G5','H5','A6', 'B6','C6','D6','E6','F6','G6','H6','A7', 'B7','C7','D7','E7','F7','G7','H7','A8', 'B8','C8','D8','E8','F8','G8','H8','A9', 'B9','C9','D9','E9','F9','G9','H9','A10', 'B10','C10','D10','E10','F10','G10','H10','A11', 'B11','C11','D11','E11','F11','G11','H11','A12', 'B12','C12','D12','E12','F12','G12','H12')]
SD_data_frame<-SD_data_frame[c('A1', 'B1','C1','D1','E1','F1','G1','H1','A2', 'B2','C2','D2','E2','F2','G2','H2','A3', 'B3','C3','D3','E3','F3','G3','H3','A4', 'B4','C4','D4','E4','F4','G4','H4','A5', 'B5','C5','D5','E5','F5','G5','H5','A6', 'B6','C6','D6','E6','F6','G6','H6','A7', 'B7','C7','D7','E7','F7','G7','H7','A8', 'B8','C8','D8','E8','F8','G8','H8','A9', 'B9','C9','D9','E9','F9','G9','H9','A10', 'B10','C10','D10','E10','F10','G10','H10','A11', 'B11','C11','D11','E11','F11','G11','H11','A12', 'B12','C12','D12','E12','F12','G12','H12')]
#add a time column to the dataframe, growthcurver needs this present to do its plotting as the first column
time<-as.numeric(rownames(matrix_mean))
mean_data_frame<-cbind(time,mean_data_frame)
SD_data_frame<-cbind(time,SD_data_frame)

#---------------------plot the growth curver plate
SummarizeGrowthByPlate(mean_data_frame, plot_fit=T, plot_file=paste0(drug_name,"_",concentration,"full_plate.pdf"))
#---------------------plotting individual wells

#aftre reviwing the output from growthcurver you can determine how far along the growth curve to want to plot and choose
#the maximum number of time points
#about 96 time points per day
time_limit<-(number_of_hours*60)/time_interval-2
mean_data_frame<-mean_data_frame[1:time_limit,]
SD_data_frame<-SD_data_frame[1:time_limit,]
time<-time[1:time_limit]
#convert time to hours
time<-time/60

#choose concentration for growth curve plots
well_to_plot=10

#isolate only desired wells for entire plate
standard<-data.frame(mean_data_frame[,paste0('A',well_to_plot)],mean_data_frame[,paste0('B',well_to_plot)],mean_data_frame[,paste0('C',well_to_plot)],mean_data_frame[,paste0('D',well_to_plot)],mean_data_frame[,paste0('E',well_to_plot)],mean_data_frame[,paste0('F',well_to_plot)],mean_data_frame[,paste0('G',well_to_plot)],mean_data_frame[,paste0('H',well_to_plot)])
colnames(standard)<-c('A','B','C','D','E','F','G','H')
standard<-cbind(standard, time)
standard_SD<-data.frame(SD_data_frame[,paste0('A',well_to_plot)],SD_data_frame[,paste0('B',well_to_plot)],SD_data_frame[,paste0('C',well_to_plot)],SD_data_frame[,paste0('D',well_to_plot)],SD_data_frame[,paste0('E',well_to_plot)],SD_data_frame[,paste0('F',well_to_plot)],SD_data_frame[,paste0('G',well_to_plot)],SD_data_frame[,paste0('H',well_to_plot)])
colnames(standard_SD)<-c('A_SD','B_SD','C_SD','D_SD','E_SD','F_SD','G_SD','H_SD')

#combine the mean and SD for measurements into one dataframe to plot
merge_mean_and_sd<-cbind(standard, standard_SD)
merge_mean_and_sd = merge_mean_and_sd[seq(1, nrow(merge_mean_and_sd), 4), ]
merge_mean_and_sd = merge_mean_and_sd[seq(1, nrow(merge_mean_and_sd), 4), ]

#plotting starts here-------------------------------------------------------
Cleanup<- theme(panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                panel.background=element_blank(), 
                axis.line=element_line(color="black"))



drug_name='Amphotericin B'
concentration='2'

CairoPDF(file=paste0(drug_name,"_",concentration,".pdf"),width = 10, height=10)
ggplot() +
  Cleanup+
  #makes a line plot for desired well
  geom_line(data=merge_mean_and_sd, aes(x=time, y=B, linetype=supp), linetype=1,color='gray0', size=rel(2), alpha=0.5)+
  #adds points to the line plot
  geom_point(data=merge_mean_and_sd,aes(x=time, y=B), size=2, color='gray0', stat='identity')+
  #adds error bars to the points
  geom_errorbar(data=merge_mean_and_sd,aes(ymax = B + B_SD, ymin=B - B_SD, x=time), position = "dodge", width = 0.5, color='gray0')+
  
  labs(title=paste0(drug_name,' Growth Curve', '\n (',concentration,')'))+
  ylab("OD620") + 
  xlab("Time (hours)")+
  
theme(axis.line.x= element_line(colour="black"),
      axis.line.y=element_line(colour='black'),
      axis.title.y = element_text(face="bold", size=rel(3), color='black'), 
      axis.text.y = element_text(face="bold", size=rel(3), color='black'),
      axis.title.x = element_text(face="bold", size=rel(3), color='black'), 
      axis.text.x = element_text(face='bold', size=rel(3), color='black'),
      plot.title = element_text(face='bold', size=rel(3), hjust=0.5, color='black'),
      plot.margin = unit(c(1,1,1,1), units = 'cm'),
      legend.position = 'right')+
scale_x_continuous(breaks=seq(0,48,6), limits=c(0,48))+
scale_y_continuous(breaks=seq(0,1.5,0.2), limits=c(0,1.5))
dev.off()


#theme modifications for a black background plot are below
#--------------White Lines
#theme(axis.line.x= element_line(colour="white"),
#     axis.line.y=element_line(colour='white'),
#      axis.title.y = element_text(face="bold", size=rel(3), color='white'), 
#      axis.text.y = element_text(face="bold", size=rel(3), color='white'),
#     axis.title.x = element_text(face="bold", size=rel(3), color='white'), 
#    axis.text.x = element_text(face='bold', size=rel(3), color='white'),
#   plot.title = element_text(face='bold', size=rel(3), hjust=0.5, color='white'),
#  legend.position = 'right',
# plot.background = element_rect(fill = "transparent", color = NA),panel.background = element_rect(fill = "transparent"))