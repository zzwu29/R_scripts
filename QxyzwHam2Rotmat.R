QxyzwHam2Rotmat <- function(QxyzwHam){
  
  # QxyzwHam --- Hamilton Quat in xyzw order
  
  Qxx<-QxyzwHam[1]*QxyzwHam[1]
  Qyy<-QxyzwHam[2]*QxyzwHam[2]
  Qzz<-QxyzwHam[3]*QxyzwHam[3]
  Qww<-QxyzwHam[4]*QxyzwHam[4]
  
  Qxy<-QxyzwHam[1]*QxyzwHam[2]
  Qxz<-QxyzwHam[1]*QxyzwHam[3]
  Qyz<-QxyzwHam[2]*QxyzwHam[3]
  Qwx<-QxyzwHam[4]*QxyzwHam[1]
  Qwy<-QxyzwHam[4]*QxyzwHam[2]
  Qwz<-QxyzwHam[4]*QxyzwHam[3]
  
  Rotmat<-c(Qww+Qxx-Qyy-Qzz,2*(Qxy-Qwz),2*(Qxz+Qwy),
            2*(Qxy+Qwz),Qww-Qxx+Qyy-Qzz,2*(Qyz-Qwx),
            2*(Qxz-Qwy),2*(Qyz+Qwx),Qww-Qxx-Qyy+Qzz)
  Rotmat<-matrix(data=Rotmat,nrow=3,ncol=3,byrow=T)
  
  return(Rotmat)
}