Rotmat2Euler <- function(Rotmat, rad=T, norm_yaw=T){
  # Rotmat --- n^R_b
  # rad --- output unit is rad or degree
  # norm_yaw --- True: + if north->east; False: + if north -> west
  
  pitch=asin(Rotmat[3,2])
  roll=atan2(-Rotmat[3,1],Rotmat[3,3])
  yaw=atan2(-Rotmat[1,2],Rotmat[2,2])
  
  if(rad==F){
    pitch=pitch/pi*180.0
    roll=roll/pi*180.0
    yaw=yaw/pi*180.0
  }
  
  if(norm_yaw==T){
    yaw=-yaw
  }
  
  return(c(pitch,roll,yaw))
}