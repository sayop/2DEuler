#!/usr/bin/env python
import os
import sys
import numpy as np
import matplotlib.pyplot as plt

dataDir0 = './CFL0.01'
dataDir1 = './CFL0.1'
dataDir2 = './CFL0.3'
dataDir3 = './CFL0.5'

# setup for RMS log data
File = 'ErrorLog.dat'

File0 = os.path.join(dataDir0,File)
data0 = np.loadtxt(File0)

File1 = os.path.join(dataDir1,File)
data1 = np.loadtxt(File1)

File2 = os.path.join(dataDir2,File)
data2 = np.loadtxt(File2)

File3 = os.path.join(dataDir3,File)
data3 = np.loadtxt(File3)

# Plot for RMS log
x0 = data0[:,0]
rms0 = data0[:,1]

x1 = data1[:,0]
rms1 = data1[:,1]

x2 = data2[:,0]
rms2 = data2[:,1]

x3 = data3[:,0]
rms3 = data3[:,1]

MinX = 1.0 #min(x1)
MaxX = max(x0)
MinY = 1.0E-3 #min(rms1)
MaxY = 1.0 #max(rms1)

p = plt.plot(x0,rms0, 'c-', label='CFL = 0.01')
p = plt.plot(x1,rms1, 'g-', label='CFL = 0.1')
p = plt.plot(x2,rms2, 'b--', label='CFL = 0.3')
p = plt.plot(x3,rms3, 'r--', label='CFL = 0.5')
plt.setp(p, linewidth='2.0')
plt.axis([MinX,MaxX, MinY, MaxY])
plt.xscale('log')
plt.yscale('log')
plt.xlabel('Iteration #', fontsize=22)
plt.ylabel('Normalized RMS error', fontsize=22)
plt.grid(True)
ax = plt.gca()
xlabels = plt.getp(ax, 'xticklabels')
ylabels = plt.getp(ax, 'yticklabels')
plt.setp(xlabels, fontsize=18)
plt.setp(ylabels, fontsize=18)
plt.legend(
          loc='upper right',
          borderpad=0.25,
          handletextpad=0.25,
          borderaxespad=0.25,
          labelspacing=0.0,
          handlelength=2.0,
          numpoints=1)
legendText = plt.gca().get_legend().get_texts()
plt.setp(legendText, fontsize=18)
legend = plt.gca().get_legend()
legend.draw_frame(False)

pltFile = 'CFLeffectRMS.png'
fig = plt.gcf()
fig.set_size_inches(8,5)
plt.tight_layout()
plt.savefig(pltFile, format='png')
plt.close()

print "%s DONE!!" % (pltFile)

