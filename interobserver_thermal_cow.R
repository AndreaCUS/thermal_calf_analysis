library(irr) #inter-rater reliability

interobserver<- read.csv (file.choose())
names(interobserver)

#----Front images

interobserver.ears.front.max <- matrix(c(interobserver$L_ear_max, interobserver$R_ear_max, interobserver$MARL_ear_max, interobserver$MARR_ear_max), nrow=20, ncol=2, byrow=FALSE)
interobserver.ears.front.max
icc(interobserver.ears.front.max, model=c("oneway"))
#     ICC(1) = 0.987, p = 1.45e-17,  0.968 < ICC < 0.995

interobserver.left.ears.front.max <- matrix(c(interobserver$L_ear_max, interobserver$MARL_ear_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.ears.front.max
icc(interobserver.left.ears.front.max, model=c("oneway"))
# ICC(1) = 0.99, p = 4.89e-10 ,  0.962 < ICC < 0.997

interobserver.right.ears.front.max <- matrix(c(interobserver$R_ear_max, interobserver$MARR_ear_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.ears.front.max
icc(interobserver.right.ears.front.max, model=c("oneway"))
#  ICC(1) = 0.986, p = 2.44e-09,  0.948 < ICC < 0.996

interobserver.ears.front.avg <- matrix(c(interobserver$L_ear_average, interobserver$R_ear_average, interobserver$MARL_ear_average, interobserver$MARR_ear_average), nrow=20, ncol=2, byrow=FALSE)
interobserver.ears.front.avg
icc(interobserver.ears.front.avg, model=c("oneway"))
#ICC(1) = 0.984, p = 1.39e-16,   0.96 < ICC < 0.993

interobserver.left.ears.front.avg <- matrix(c(interobserver$L_ear_average, interobserver$MARL_ear_average), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.ears.front.avg
icc(interobserver.left.ears.front.avg, model=c("oneway"))
#  ICC(1) = 0.985, p = 3.32e-09 , 0.945 < ICC < 0.996

interobserver.right.ears.front.avg <- matrix(c(interobserver$L_ear_average, interobserver$MARL_ear_average), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.ears.front.avg
icc(interobserver.right.ears.front.avg, model=c("oneway"))
#     ICC(1) = 0.985, p = 3.32e-09 , 0.945 < ICC < 0.996

interobserver.ears.front.LR.max <- matrix(c(interobserver$L.R_ear_max, interobserver$MARL.R_ear_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.ears.front.LR.max
icc(interobserver.ears.front.LR.max, model=c("oneway"))
#ICC(1) = 0.875, p = 0.00011,   0.597 < ICC < 0.967

interobserver.ears.front.LR.ave <- matrix(c(interobserver$L.R_ear_ave, interobserver$MARL.R_ear_ave), nrow=10, ncol=2, byrow=FALSE)
interobserver.ears.front.LR.ave
icc(interobserver.ears.front.LR.ave, model=c("oneway"))
# ICC(1) = 0.712, p = 0.00505, 0.223 < ICC < 0.919
plot(interobserver.ears.front.LR.ave[,1], interobserver.ears.front.LR.ave[,2])

interobserver.nostrils.max <- matrix(c(interobserver$L_nostril_max, interobserver$R_nostril_max, interobserver$MARL_nostril_max, interobserver$MARR_nostril_max), nrow=20, ncol=2, byrow=FALSE)
interobserver.nostrils.max
icc(interobserver.nostrils.max, model=c("oneway"))
#ICC(1) = 0.98, p = 7.79e-16 ,  0.952 < ICC < 0.992

interobserver.left.nostrils.max <- matrix(c(interobserver$L_nostril_max, interobserver$MARL_nostril_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.nostrils.max
icc(interobserver.left.nostrils.max, model=c("oneway"))
#ICC(1) = 0.988, p = 1.04e-09 ,   0.956 < ICC < 0.997

interobserver.right.nostrils.max <- matrix(c(interobserver$R_nostril_max, interobserver$MARR_nostril_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.nostrils.max
icc(interobserver.right.nostrils.max, model=c("oneway"))
#ICC(1) = 0.975, p = 4.27e-08,  0.908 < ICC < 0.994

interobserver.nostrils.avg <- matrix(c(interobserver$L_nostril_average, interobserver$R_nostril_average, interobserver$MARL_nostril_average, interobserver$MARR_nostril_average), nrow=20, ncol=2, byrow=FALSE)
interobserver.nostrils.avg
icc(interobserver.nostrils.avg, model=c("oneway"))
# ICC(1) = 0.989, p = 3.78e-18 , 0.972 < ICC < 0.995

interobserver.nostril.front.LR.max <- matrix(c(interobserver$L.R_NOSTRIL_MAX, interobserver$MARL.R.NOSTRIL_MAX), nrow=10, ncol=2, byrow=FALSE)
interobserver.nostril.front.LR.max
icc(interobserver.nostril.front.LR.max, model=c("oneway"))
#ICC(1) = 0.756, p = 0.00241 ,  0.312 < ICC < 0.932

interobserver.nostril.front.LR.ave <- matrix(c(interobserver$L.R_NOSTRIL_AVE, interobserver$MARL.R_NOSTRIL_AVE), nrow=10, ncol=2, byrow=FALSE)
interobserver.nostril.front.LR.ave
icc(interobserver.nostril.front.LR.ave, model=c("oneway"))
#ICC(1) = 0.95, p = 1.24e-06 ,  0.824 < ICC < 0.987

interobserver.nasal.airway.max <- matrix(c(interobserver$L_airway_max, interobserver$R_airway_max, interobserver$MARL_airway_max, interobserver$MARR_airway_max), nrow=20, ncol=2, byrow=FALSE)
interobserver.nasal.airway.max
icc(interobserver.nasal.airway.max, model=c("oneway"))
#ICC(1) = 0.925,  p = 4.11e-10,  0.824 < ICC < 0.97

interobserver.left.nasal.airway.max <- matrix(c(interobserver$L_airway_max, interobserver$MARL_airway_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.nasal.airway.max
icc(interobserver.left.nasal.airway.max, model=c("oneway"))
#ICC(1) = 0.919, p = 1.38e-05 , 0.724 < ICC < 0.979

interobserver.right.nasal.airway.max <- matrix(c(interobserver$R_airway_max, interobserver$MARR_airway_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.nasal.airway.max
icc(interobserver.right.nasal.airway.max, model=c("oneway"))
#ICC(1) = 0.939,  p = 3.48e-06 , 0.787 < ICC < 0.984

interobserver.nasal.airway.ave <- matrix(c(interobserver$L_airway_average, interobserver$R_airway_average, interobserver$MARR_airway_average, interobserver$MARR_airway_average), nrow=20, ncol=2, byrow=FALSE)
interobserver.nasal.airway.ave
icc(interobserver.nasal.airway.ave, model=c("oneway"))
# ICC(1) = 0.991, p = 5.74e-19 ,  0.977 < ICC < 0.996

interobserver.left.nasal.airway.ave <- matrix(c(interobserver$L_airway_average, interobserver$MARL_airway_average), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.nasal.airway.ave
icc(interobserver.left.nasal.airway.ave, model=c("oneway"))
#ICC(1) = 0.992, p = 1.27e-10 , 0.971 < ICC < 0.998

interobserver.right.nasal.airway.ave <- matrix(c(interobserver$R_airway_average, interobserver$MARR_airway_average), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.nasal.airway.ave
icc(interobserver.right.nasal.airway.ave, model=c("oneway"))
#ICC(1) = 0.997, p = 2.33e-12 , 0.987 < ICC < 0.999

interobserver.LR.nasal.airway.ave <- matrix(c(interobserver$L.R_AIRWAY_AVE, interobserver$MARL.R_AIRWAY_AVE), nrow=10, ncol=2, byrow=FALSE)
interobserver.LR.nasal.airway.ave
icc(interobserver.LR.nasal.airway.ave, model=c("oneway"))
#ICC(1) = 0.841, p = 0.000341 , 0.507 < ICC < 0.957

interobserver.LR.nasal.airway.max <- matrix(c((interobserver$L_airway_max-interobserver$R_airway_max), interobserver$MARL.R_AIRWAY_MAX), nrow=10, ncol=2, byrow=FALSE)
interobserver.LR.nasal.airway.max
icc(interobserver.LR.nasal.airway.max, model=c("oneway"))
# ICC(1) = 0.741, p = 0.00316 , 0.281 < ICC < 0.928

interobserver.muzzle.max <- matrix(c(interobserver$muzzle_max, interobserver$MARmuzzle_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.muzzle.max
icc(interobserver.muzzle.max, model=c("oneway"))
#ICC(1) = 1, p = 1.43e-20,  1 < ICC < 1

interobserver.muzzle.ave <- matrix(c(interobserver$muzzle_average, interobserver$MARmuzzle_average), nrow=10, ncol=2, byrow=FALSE)
interobserver.muzzle.ave
icc(interobserver.muzzle.ave, model=c("oneway"))
#ICC(1) = 0.89, p = 5.97e-05,  0.639 < ICC < 0.971

interobserver.whorl <- matrix(c(interobserver$whorl_max, interobserver$MARwhorl_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.whorl
icc(interobserver.whorl, model=c("oneway"))
#ICC(1) = 0.969, p = 1.27e-07 , 0.887 < ICC < 0.992


#----Back images

interobserver.ears.back.max <- matrix(c(interobserver$L_earbasemax, interobserver$R_earbasemax, interobserver$MARL_earbasemax, interobserver$MARR_earbasemax), nrow=20, ncol=2, byrow=FALSE)
interobserver.ears.back.max
icc(interobserver.ears.back.max, model=c("oneway"))
#ICC(1) = 0.988, p = 6.29e-18,  0.97 < ICC < 0.995

interobserver.left.ears.back.max <- matrix(c(interobserver$L_earbasemax, interobserver$MARL_earbasemax), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.ears.back.max
icc(interobserver.left.ears.back.max, model=c("oneway"))
#ICC(1) = 0.99, p = 4.16e-10 ,  0.963 < ICC < 0.998

interobserver.right.ears.back.max <- matrix(c(interobserver$R_earbasemax, interobserver$MARR_earbasemax), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.ears.back.max
icc(interobserver.right.ears.back.max, model=c("oneway"))
#ICC(1) = 0.987, p = 1.81e-09 ,   0.951 < ICC < 0.997

interobserver.ears.back.avg <- matrix(c(interobserver$L_earbaseaverage, interobserver$R_earbaseaverage, interobserver$MARL_earbaseaverage, interobserver$MARR_earbaseaverage), nrow=20, ncol=2, byrow=FALSE)
interobserver.ears.back.avg
icc(interobserver.ears.back.avg, model=c("oneway"))
#ICC(1) = 0.995, p = 4.42e-22 , 0.989 < ICC < 0.998

interobserver.left.ears.back.avg <- matrix(c(interobserver$L_earbaseaverage, interobserver$MARL_earbaseaverage), nrow=10, ncol=2, byrow=FALSE)
interobserver.left.ears.back.avg
icc(interobserver.left.ears.back.avg, model=c("oneway"))
#ICC(1) = 0.995, p = 1.69e-11 , 0.981 < ICC < 0.999

interobserver.right.ears.back.avg <- matrix(c(interobserver$R_earbaseaverage, interobserver$MARR_earbaseaverage), nrow=10, ncol=2, byrow=FALSE)
interobserver.right.ears.back.avg
icc(interobserver.right.ears.back.avg, model=c("oneway"))
#ICC(1) = 0.997, p = 2.16e-12 , 0.987 < ICC < 0.999

interobserver.ears.back.LR.max <- matrix(c(interobserver$L.R_earbase_max, interobserver$MARL.R_earbase_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.ears.back.LR.max
icc(interobserver.ears.back.LR.max, model=c("oneway"))
# ICC(1) = 0.736, p = 0.00343 , 0.271 < ICC < 0.926

interobserver.ears.back.LR.ave <- matrix(c(interobserver$L.R_earbase_ave, interobserver$MARL.R_earbase_ave), nrow=10, ncol=2, byrow=FALSE)
interobserver.ears.back.LR.ave
icc(interobserver.ears.back.LR.ave, model=c("oneway"))
#ICC(1) = 0.843, p = 0.000318 , 0.513 < ICC < 0.958


#----Side images
interobserver.eyeball <- matrix(c(interobserver$eye_ball._average, interobserver$MAReyeball._average), nrow=10, ncol=2, byrow=FALSE)
interobserver.eyeball
icc(interobserver.eyeball, model=c("oneway"))
plot(interobserver.eyeball[,1], interobserver.eyeball[,2])
identify(interobserver.eyeball[,1], interobserver.eyeball[,2])
#ICC(1) = 0.912, p = 1.97e-05, 0.705 < ICC < 0.977


interobserver.eye.inner.corner.max <- matrix(c(interobserver$eye_inner_corner_max, interobserver$MAReye_inner_corner_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.eye.inner.corner.max
icc(interobserver.eye.inner.corner.max, model=c("oneway"))
plot()
#ICC(1) = 0.996, p = 3.24e-12 , 0.986 < ICC < 0.999

interobserver.eye.rostral <- matrix(c(interobserver$rostral_eyesurrounding_max,interobserver$MARcaudal_eyesurrounding_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.eye.rostral
icc(interobserver.eye.rostral, model=c("oneway"))
plot(interobserver.eye.rostral[,1], interobserver.eye.rostral[,2])
identify(interobserver.eye.rostral[,1], interobserver.eye.rostral[,2])
# ICC(1) = 0.914, p = 1.78e-05 ,   0.71 < ICC < 0.978

interobserver.eye.caudal <- matrix(c(interobserver$caudal_eyesurrounding_max, interobserver$MARrostral_eyesurrounding_max), nrow=10, ncol=2, byrow=FALSE)
interobserver.eye.caudal
icc(interobserver.eye.caudal, model=c("oneway"))
plot(interobserver.eye.caudal[,1], interobserver.eye.caudal[,2])
identify(interobserver.eye.caudal[,1], interobserver.eye.caudal[,2])

