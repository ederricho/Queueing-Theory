# Queueing Model M/G/1 PS
#-------------------------------- Description ----------------------------------
# 
#-------------------------------------------------------------------------------

#------------------------------- Preleminary Data ------------------------------
n_jobs = 100 # number of jobs

#-------------------------------------------------------------------------------
dp_size = rlnorm(n_jobs,10)# size of data packets vector DO NOT CHANGE!
#-------------------------------------------------------------------------------

n_serv = 1 # Number of Servers in the queue
t_tot = 1000 # total time in seconds the queue will function for data collection
int_times_rate = 0.025 #interarrival times parameter
int_times_vec = ceiling(rexp(n_jobs,int_times_rate))# interarival times vector
#------------------------------------------------------------------------------

#----------------------------- Running the Queue -------------------------------
#------------------------ This is a M/G/1 PS Queue ---------------------------
in_queue = c()
finished_jobs = c()
#waiting_time = c()
time = 0
time_int = 0 # keeps track of the interarrival times
index = 1 # Index for adding jobs to queue
job = 1
prog = 1 # Index for job number
idle_time = 0
inst_queue_length_ps = c() # This vector records the instantaneous queue length

while(time < t_tot){ # while current time is less than total time
  for(i in 1:t_tot){
    #-------------------------- Adds new jobs to queue -----------------------------
    if(sum(in_queue) == 0 | length(in_queue == 0)){ # Adds Idle time for queue
      idle_time = idle_time+1
    }
    if(time_int == int_times_vec[job]){ # if the time i is equal to an interarrival time
      job = job + 1
      in_queue = append(in_queue,int_times_vec[index]) # append the queue vector
      print(paste("There has been a new job added to the queue",i))
      index = index + 1
      time = time + 1
      #waiting_time = append(waiting_time,time_int)
      time_int = 0
      #print("---------------------------------------------------------")
    }else{
      print(paste("No Job has been added. ",i,time_int))
      time = time + 1
      time_int = time_int + 1
      #print("---------------------------------------------------------")
    }
    if(length(in_queue) != 0 & (time %% length(in_queue) == 0)){ # Work on the items in the queue
      #num = length(which(in_queue != 0)) # rate of jobs
      print(paste("Number of NonZero Jobs: ",num))
      print(in_queue)
      in_queue = in_queue[which(in_queue!=0)] - 1
      print(paste("Time: ",time))
      print("----------------------------------------------------------")
    }
    inst_queue_length_ps = append(inst_queue_length_ps, length(which(in_queue != 0)))
    #print(inst_queue_length)
    }
#------------------------ Instantaneous Queue Length----------------------------
    
  }

# -------------------------- Metrics for Queue:---------------------------------
#print(paste("Average Waiting Time: ",round(mean(waiting_time))))
print(paste("Sojourn Time: ", round(mean(2*waiting_time))))
print(paste("Mean Queue Length",mean(inst_queue_length_ps)))

#--------------------- Plot Graphs for M/G/1 FCFS ------------------------------
plot(inst_queue_length_ps,type='l', # Inst. Queue Length
     main='Instantaneous Queue Length Graph',
     xlab='Time',
     ylab='Instantaneous Queue Length')
     #ylim=c(0,max(inst_queue_length)+1))

plot(waiting_time,type='l', # Waiting Times
     main='Waiting Time Graph',
     xlab='Time',
     ylab='Waiting Time (Seconds)')

# Queue Efficiency:
eff_ps = (job-length(in_queue))/t_tot
print(paste("PS Efficiency: ",eff_ps))
