QxyzwHam_Diff2Euler <- function(QxyzwHam, QxyzwHam_GT, rad=T, norm_yaw=T){
  
  # QxyzwHam --- Hamilton Quat in xyzw order
  
  Rotmat=QxyzwHam2Rotmat(QxyzwHam)
  Rotmat_GT=QxyzwHam2Rotmat(QxyzwHam_GT)
  
  Rotmat_Diff=Rotmat %*% t(Rotmat_GT)
  return (Rotmat2Euler(Rotmat_Diff,rad,norm_yaw))
}