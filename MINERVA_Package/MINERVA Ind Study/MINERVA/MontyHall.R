no_move_win <-0
move_win <-0
times<-100
for (i in 1:times){
  doors<-c(1,2,3)
  prize<-sample(doors,1)
  pick<-sample(doors,1)
  doors<-doors[!doors == pick]
  host_remove<-sample(doors,1)
  doors<-doors[!doors == host_remove]
  if (host_remove == prize){doors<-host_remove}
  if (prize == pick){no_move_win<-no_move_win+1}
  if (prize == doors){move_win<-move_win+1}}
no_move_win/times
move_win/times