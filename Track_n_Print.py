# -*- coding: utf-8 -*-
"""
Timesheet logging Script

Created on Mon Oct  8 16:47:29 2018

@author: j_osborne
"""
##Head
#required imports
from datetime import datetime as dt
from datetime import timedelta as tdel
#functional definitions

##Body
#Sets up day specific variables
path = "C:/Users/j_osborne/Documents/JDO-Logs/"+(dt.now() - tdel(days=dt.now().weekday())).strftime("%d-%m-%y") + ".txt"
morn = input("Day Begin? y for yes ")
if morn == "y":
    with open(path, "a") as f:
        f.write('\n'+'\n'+'--'+dt.now().strftime("%A")+'--'+'\n')

#Sets up defaults
jn = 'JN00000'
inline = "Ready"

#Takes input from user
while inline != "x":
    inline = input("Entry: ")
    if inline == "x":
        break
    timedline = dt.now().strftime("%H:%M")+ ' ' + inline
    jn = input("Ref: {} ".format(jn)) or jn
    formjn = ' ' + '({})'.format(str(jn)) 
    billableline = timedline + formjn + '\n'
    
#Saves the input as an append
    with open(path, "a") as f:
        f.write(billableline )