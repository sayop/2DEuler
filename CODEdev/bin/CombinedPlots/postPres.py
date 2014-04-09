#!/usr/bin/env python
import os
import sys
import numpy as np
import matplotlib.pyplot as plt

dataDir1 = '../CASE01'
dataDir2 = '../CASE02'
dataDir3 = '../CASE03'

# setup for DATA file
File = 'WallData.dat'

File1 = os.path.join(dataDir1,File)
data1 = np.loadtxt(File1)

File2 = os.path.join(dataDir2,File)
data2 = np.loadtxt(File2)

File3 = os.path.join(dataDir3,File)
data3 = np.loadtxt(File3)

FileExac = 'ExacP.dat'
FileExac = os.path.join(dataDir1,FileExac)
dataExac = np.loadtxt(FileExac)

xExac = dataExac[:,0]
pExac = dataExac[:,1]

# Plot for pressure
x1 = data1[:,0]
pres1 = data1[:,4]

x2 = data2[:,0]
pres2 = data2[:,4]

x3 = data3[:,0]
pres3 = data3[:,4]

MinX = min(x1)
MaxX = max(x1)
MinY = 0.3#min(pres)
MaxY = 1.3#max(pres)

p = plt.plot(xExac,pExac, 'k-', label='Exact Solution')
p = plt.plot(x1,pres1, 'g-', label='CASE #1')
p = plt.plot(x2,pres2, 'b--', label='CASE #2')
p = plt.plot(x3,pres3, 'r--', label='CASE #3')
plt.setp(p, linewidth='2.0')
plt.axis([MinX,MaxX, MinY, MaxY])
plt.xscale('linear')
plt.yscale('linear')
plt.xlabel('x', fontsize=22)
plt.ylabel('Pressure', fontsize=22)
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

pltFile = 'CombinedPressure.png'
fig = plt.gcf()
fig.set_size_inches(8,5)
plt.tight_layout()
plt.savefig(pltFile, format='png')
plt.close()

print "%s DONE!!" % (pltFile)

