# Queuing Model M/G/1 FCFS
#-------------------------------- Description ----------------------------------
# 
#-------------------------------------------------------------------------------

#------------------------------- Preleminary Data ------------------------------
n_jobs = 500 # number of jobs

#-------------------------------------------------------------------------------
dp_size = rlnorm(n_jobs,10)# size of data packets vector DO NOT CHANGE!
#-------------------------------------------------------------------------------

n_serv = 1 # Number of Servers in the queue
t_tot = 10000 # total time in seconds the queue wil function for data collection
int_times_rate = 0.5 #interarrival times parameter
int_times_vec = ceiling(rexp(n_jobs,int_times_rate))# interarival times vector
#------------------------------------------------------------------------------

#----------------------------- Running the Queue -------------------------------
#------------------------ This is a M/G/1 FCFS Queue ---------------------------
in_queue = c()
finished_jobs = c()
waiting_time_fcfs = c()
time = 0
time_int = 0 # keeps track of the interarrival times
index = 1 # Index for adding jobs to queue
job = 1
prog = 1 # Index for job number
idle_time = 0
inst_queue_length_fcfs = c() # This vector records the instantaneous queue length

while(time < t_tot){ # while current time is less than total time
  for(i in 1:t_tot){
#-------------------------- Adds new jobs to queue -----------------------------
    if( sum(in_queue) == 0 | length(in_queue == 0)){ # Adds Idle time for queue
      idle_time = idle_time+1
    }
    if(time_int == int_times_vec[job]){ # if the time i is equal to an interarrival time
      job = job + 1
      in_queue = append(in_queue,int_times_vec[index]) # append the queue vector
      print(paste("There has been a new job added to the queue",i))
      #print(in_queue)
      print(paste("Intermentent Time: ",time_int))
      index = index + 1
      time = time + 1
      waiting_time_fcfs = append(waiting_time_fcfs,time_int)
      time_int = 0
      #print("---------------------------------------------------------")
    }else{
      print(paste("No Job has been added. ",i,time_int))
      time = time + 1
      time_int = time_int + 1
      #print("---------------------------------------------------------")
    }
    if(length(in_queue) != 0){ # Work on the item in the queue
      print(paste("We are on job number: ",prog))
      print(paste("Job Progress: ",in_queue[prog]))
      print(in_queue)
      in_queue[prog] = in_queue[prog] - 1
      print(paste("Time: ",time))
      print("----------------------------------------------------------")
      if(is.na(in_queue[prog])){ # Delete NA from inqueue Vector
        idle_time = idle_time+1
        print("we have an NA")
        #prog = prog - 1
        #print(paste("Prog: " ,prog))
        in_queue = in_queue[!is.na(in_queue)]
        prog = prog -1
      }
      
      if(in_queue[prog] == 0){ # Move to the next job when done
        finished_jobs = append(finished_jobs,int_times_vec[prog])
        prog = prog + 1
        print("prog added")
      }else if(sum(in_queue == 0)){
        idle_time = idle_time + 1
      }else{
        idle_time = idle_time + 1
      }
        
    }
#------------------------ Instantaneous Queue Length----------------------------
    inst_queue_length_fcfs = append(inst_queue_length_fcfs, length(which(in_queue != 0)))
  }
}
# -------------------------- Metrics for Queue:---------------------------------
print(paste("Average Waiting Time: ",round(mean(waiting_time))))
print(paste("Sojourn Time: ", round(mean(2*waiting_time))))
print(paste("Mean Queue Length",mean(inst_queue_length_fcfs)))

#--------------------- Plot Graphs for M/G/1 FCFS ------------------------------
plot(inst_queue_length_fcfs,type='l', # Inst. Queue Length
     main=paste('Instantaneous Queue Length Graph with Lambda =',int_times_rate),
     xlab='Time',
     ylab='Instantaneous Queue Length')
     #ylim=c(0,max(inst_queue_length)+1))

plot(waiting_time_fcfs,type='l', # Waiting Times
     main='Waiting Time Graph',
     xlab='Time',
     ylab='Waiting Time (Seconds)')


# Queue Efficiency:
eff_fcfs = length(which(in_queue==0))/t_tot
print(paste("FCFS Efficiency: ",eff_fcfs))

x_new = inst_queue_length_fcfs[1:500]
y_new = c(1:500)#c(1:length(inst_queue_length_fcfs))
ggplot(data=as.data.frame(x_new),aes(x=y_new,y=x_new))+
  geom_line()+
  ggtitle(paste("Queue Length, Frame = 500, Lambda = ",int_times_rate))+
  xlab("Time")+
  ylab("Queue Length")
