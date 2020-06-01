set terminal postscript enhanced color
####################
set output "mml.ps"
p "alpha_mml" u 1:3 t "low mass inside Rt" w l, "alpha_mml" u 1:5 t "low mass inside Rh" w l,"alpha_mml" u 1:4 t "high mass inside Rt" w l , "alpha_mml" u 1:6 t "high mass inside Rh" w l 

####################
set output "constbin.ps"
p "alpha_constbin" u 1:3 t "low mass inside Rt" w l, "alpha_constbin" u 1:5 t "low mass inside Rh" w l,"alpha_constbin" u 1:4 t "high mass inside Rt" w l , "alpha_constbin" u 1:6 t "high mass inside Rh" w l 

####################
#pause -1
