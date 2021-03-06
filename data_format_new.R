##################################################
## Project:   Mouse data import into R
## Date:      Feb 17, 2020
## Author:    Arnie Seong
##################################################


# 
id_wk4 <- c(2041, 2042, 2043, 2044, 2045, 2046, 2047, 2048, 2049, 2050, 2051, 2052, 2053, 2054, 2055)
trt_wk4 <- c("Air", "Air", "E-cig", "E-cig", "Control", "Air", "Air", "Air", "Air", "Control", "E-cig", "E-cig", "E-cig", "E-cig", "Control")

#wk4
perce_45.1_wk4 <- c(31.3,32.3,33.8,32.1,30.4,29.1,34.9,35.6,36.5,29,34.9,37,31.4,25.6,26.5)
perce_45.2_wk4 <- c(43.3,41.7,37.1,46.2,49.5,55.1,49.4,46.3,44.8,52.4,45.6,37.6,50.8,52.7,55.5)
perce_45.h_wk4 <- c(24.8,25.1,28.1,21.2,19.3,14.8,15.1,17.5,18.3,17.7,18.6,24.1,15.8,20.8,17.4)

noRBC_45.1_wk4 <- c(1088, 2600, 1106, 1073, 1828, 1130, 1320, 823, 404, 1101, 1015, 1740, 699, 886, 1024)
noRBC_45.2_wk4 <- c(1503, 3356, 1214, 1547, 2975, 2144, 1868, 1070, 495, 1993, 1325, 1768, 1130, 1824, 2146)
noRBC_45.h_wk4 <- c(862, 2019, 918, 709, 1157, 574, 572, 405, 202, 675, 541, 1133, 352, 720, 672)



#wk8
perce_45.1_wk8 <- c(32.9,24.8,28.3,28.6,38.2,21.6,31.9,34.6,31.5,29.7,32.1,34.5,27.1,38.2,22.8)
perce_45.2_wk8 <- c(52,62,54.1,58.9,46,68.4,57.1,55.3,55.2,55.2,57.5,51.6,57.9,50.1,68.5)
perce_45.h_wk8 <- c(14.3,12.6,17,12.1,15.3,9.6,10.7,9.7,12.9,14.8,10.1,13.2,14.4,11.2,8.5)

noRBC_45.1_wk8 <- c(1116, 1604, 1064, 1086, 2676, 783, 1009, 916, 1015, 867, 918, 1014, 719, 1254, 651)
noRBC_45.2_wk8 <- c(1767, 4005, 2034, 2200, 3217, 2474, 1804, 1463, 1779, 1611, 1644, 1516, 1534, 1644, 1957)
noRBC_45.h_wk8 <- c(487, 813, 641, 453, 1072, 349, 338, 257, 416, 432, 289, 387, 381, 366, 242)

#wk12
perce_45.1_wk12 <- c(38.4,33.9,30.7,32.7,43.4,18.6,36.2,32.7,31.7,28,34.6,30.1,25.2,49.7,17.3)
perce_45.2_wk12 <- c(51.8,56.6,58,59.4,43.7,74.7,56.4,60.7,61.5,56.7,56.4,60.6,62.1,40.8,76.5)
perce_45.h_wk12 <- c(9.5,9.4,11.2,7.9,12.7,6.7,7.4,6.5,6.7,15.1,8.8,9.1,12.5,9.3,5.9)

noRBC_45.1_wk12 <- c(1455, 867, 1370, 1175, 2404, 762, 1472, 1524, 1898, 1509, 940, 1057, 990, 1407, 647)
noRBC_45.2_wk12 <- c(1962, 1449, 2590, 2135, 2419, 3066, 2294, 2832, 3690, 3062, 1532, 2123, 2443, 1155, 2860)
noRBC_45.h_wk12 <- c(360, 240, 502, 283, 706, 273, 300, 301, 399, 814, 238, 318, 491, 262, 222)



#wk18
perce_45.1_wk18 <- c(40.7,51.3,30.6,27,41,14.1,38.9,35.4,27,27.2,30.6,24.3,19.6,53.3,15.2)
perce_45.2_wk18 <- c(50.2,41.7,56.7,64.5,45.5,79.58,52.9,58.1,65.7,55.1,59.8,68.6,66.5,37.7,80.1)
perce_45.h_wk18 <- c(8.7,6.1,12.2,7.8,13,5.3,7.9,6,7.1,17.2,9.1,6.8,13.6,8.4,4.2)

noRBC_45.1_wk18 <- c(1075, 3771, 2082, 1919, 3602, 990, 2870, 1118, 1568, 1415, 1338, 1274, 1369, 1081, 312)
noRBC_45.2_wk18 <- c(1326, 3053, 3833, 4559, 3985, 5563, 3905, 1833, 3814, 2869, 2614, 3591, 4648, 765, 1643)
noRBC_45.h_wk18 <- c(230, 447, 822, 554, 1138, 372, 580, 188, 410, 895, 400, 357, 948, 170, 86)

#wk20
perce_45.1_wk20 <- c(36.6,39.8,22.4,24.9,33.1,10.9,33,29.6,21.9,21,27.3,17.3,14.5,43,11.5)
perce_45.2_wk20 <- c(54.8,52.6,64.2,66.6,52.6,83.6,58.5,64.2,71.3,61.7,64.8,75.9,74.3,46,4.8)
perce_45.h_wk20 <- c(8,6.9,12.6,8.3,13.9,5.2,8.2,5.6,6.5,16.9,7.6,6.5,10.8,10,83.4)

noRBC_total_wk20 <- c(1075, 3771, 2082, 1919, 3602, 990, 2870, 1118, 1568, 1415, 1338, 1274, 1369, 1081, 312)

wks <- rep(c(4, 8, 12, 18, 20), each=15)
id <- rep(id_wk4, 5)
trt <- rep(trt_wk4, 5)

percent_45.1 <- c(perce_45.1_wk4, 
                  perce_45.1_wk8,
                  perce_45.1_wk12,
                  perce_45.1_wk18,
                  perce_45.1_wk20)
percent_45.2 <- c(perce_45.2_wk4,
                  perce_45.2_wk8,
                  perce_45.2_wk12,
                  perce_45.2_wk18,
                  perce_45.2_wk20)
percent_45.12 <- c(perce_45.h_wk4,
                   perce_45.h_wk8,
                   perce_45.h_wk12,
                   perce_45.h_wk18,
                   perce_45.h_wk20)


noRBC_total <- c(noRBC_45.1_wk4+noRBC_45.2_wk4+noRBC_45.h_wk4,
                 noRBC_45.1_wk8+noRBC_45.2_wk8+noRBC_45.h_wk8,
                 noRBC_45.1_wk12+noRBC_45.2_wk12+noRBC_45.h_wk12,
                 noRBC_45.1_wk18+noRBC_45.2_wk18+noRBC_45.h_wk18,
                 noRBC_total_wk20)

mouse <- data.frame(id, trt, wks, percent_45.1, percent_45.2, percent_45.12, noRBC_total) 

save(mouse, file="mouse_new.Rdata")









