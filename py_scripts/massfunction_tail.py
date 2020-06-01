# -*- coding: utf-8 -*-

import os
from contextlib import contextmanager
import subprocess as sp

def mkdir(path):
    try:
        os.makedirs(path)
    except FileExistsError:
        # directory", path, "already exists, ignoring making it.
        pass

@contextmanager
def cd(newdir):
    prevdir = os.getcwd()
    os.chdir(os.path.expanduser(newdir))
    try:
        yield
    finally:
        os.chdir(prevdir)

def filterfunc(variable):
    first_character_in_filename = variable[0]
    if first_character_in_filename == "0":
        return True
    elif first_character_in_filename == "1":
        return True
    else:
        return False
    
def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1
    
## ======================================
## Read total mass and tidal radius at t = 0 and desired snapshot
#%%
Nbin = "70"
overfile = '../N50R0.5d20S1/overview.txt'
statploutput = 'out_' + Nbin
snappath = "../N50R0.5d20S1/tail/"
print(file_len(overfile))
with open(overfile) as f:
    lines = f.readlines()
    line1 = lines[1].split() ## second line of overview.txt
    Mtot = float(line1[3]) ## initial total mass
    rtide0 = float(line1[6]) ## initial tidal radius
    for i in range(1, file_len(overfile)):
        linenow = lines[i].split() ## ith snapshot (line i+1 in overview.txt)
        t = float(linenow[1]) ## current time (second snapshot)
        MNow = float(linenow[3]) ## current mass (second snapshot)
        rh = float(linenow[5]) ## current halfmass radius
        rt = float(linenow[6]) ## current tidal radius
        print("current time and mass = ", t, MNow)
        snapname = "{0:07.1f}".format(t)
        print(snapname)
        ## ======================================
        ## prepare inputstatpl
        with cd(snappath):
            try:
#                sp.call('rm ./alpha*', shell=True)
#                for root, dirs, files in os.walk("."):
#                    files.sort()
#                    files = list(filter(filterfunc, files))
#                    print(files)
                with open('inputstatpl', 'w') as f:
                    f.write(snapname + "\n")
                    f.write('low_'+ snapname + "\n")
                    f.write('high_'+ snapname + "\n")
                    f.write(Nbin + " ! number of bin, -1 for auto\n")
                    f.write(str(Mtot) + " ! Mtot \n")
                    f.write(str(MNow) + " ! MNow \n") # MNow
                    f.write( str(rt) + " ! Rt\n") # Rt
                    f.write(str(rh) + " ! Rh\n") # Rh
                    f.write(str(t) + " ! t\n") 
                    f.close(); print("")
        #            inputstatplContent = sp.check_output('cat inputstatpl', shell=True)
        #            print(inputstatplContent)
                statpl_exit = sp.call('statpl_tail < ./inputstatpl >> ' + statploutput, shell=True)
                print('statpl exit with ', statpl_exit)
            except FileExistsError:
                pass
#print(snappath)
#%%
## ======================================
## prepare inputstatpl
#with cd(snappath):
#    try:
#        sp.call('rm ./alpha*', shell=True)
#        for root, dirs, files in os.walk("."):
#            files.sort()
#            files = list(filter(filterfunc, files))
##            print(files)
#        with open('inputstatpl', 'w') as f:
#            f.write(snapname + "\n")
#            f.write('low_'+ snapname + "\n")
#            f.write('high_'+ snapname + "\n")
#            f.write(Nbin + " ! number of bin, -1 for auto\n")
#            f.write(str(Mtot) + " ! Mtot \n")
#            f.write(str(MNow) + " ! MNow \n") # MNow
#            f.write( str(rt) + " ! Rt\n") # Rt
#            f.write(str(rh) + " ! Rh\n") # Rh
#            f.write(str(t) + " ! t\n") 
#            f.close(); print("")
##            inputstatplContent = sp.check_output('cat inputstatpl', shell=True)
##            print(inputstatplContent)
#        statpl_exit = sp.call('./statpl < ./inputstatpl >> ' + statploutput, shell=True)
#        print('statpl exit with ', statpl_exit)
#    except FileExistsError:
#        pass