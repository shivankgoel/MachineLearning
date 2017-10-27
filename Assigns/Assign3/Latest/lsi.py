import scipy.sparse as sp
import re
import sys

z = int(sys.argv[2])
kn = int(sys.argv[4])
direcloc = sys.argv[6]
docinname = sys.argv[8]
docoutname = sys.argv[10]
terminname = sys.argv[12]
termoutname = sys.argv[14] 
queryinname = sys.argv[16]
queryoutname = sys.argv[18]

#print(z)
#print(kn)

import os
currentdir = os.getcwd()
path = currentdir+'/'+direcloc
os.chdir(path)

pattern = re.compile(r'\W+')
words = []
xindex = []
yindex = []
wordloc = {}
docloc = {}
wcount = 0
for i in range(5000) :
	f1 = open( (str(i+1)+".txt"), "r")
        headinglen = 0
	while f1.read(1) != '\n':
		headinglen += 1
	f1.seek(0,0)
	docloc[((f1.read(headinglen)).rstrip('\n')).strip(' ')] = i
	f1.seek(0,0)
	allwords = pattern.split(f1.read().rstrip('\n').strip(' '))
	for wordi in allwords:
		word = wordi.lower()
		if(word not in wordloc):
			wordloc[word]=wcount
			wcount+=1
	#print(i)
	f1.close()


wordfreq = {}
for i in range(5000) :
	wordfreq={}
	f1 = open( (str(i+1)+".txt"), "r")
	allwords = pattern.split(f1.read().rstrip('\n').strip(' '))
	for wordi in allwords:
		word = wordi.lower()
		if(word not in wordfreq):
			wordfreq[word]=1
		else:
			wordfreq[word]+=1
	for key in wordfreq:
		words.append(wordfreq[key])
		xindex.append(wordloc[key])
		yindex.append(i)
	 
	#print(i)
	f1.close()

numtotalwords = len(wordloc)

wordvsdoc = sp.csc_matrix((words, (xindex, yindex)), shape=(numtotalwords, 5000))
wordvsdoc = wordvsdoc.multiply(1.0)
from scipy.sparse.linalg import svds
#z = 250
u1, s1, vt1 = svds(wordvsdoc,z,which = 'LM')

import numpy as np
termcomp = np.dot(u1,np.diag(s1))
doccomp = np.dot(np.transpose(vt1),np.diag(s1))

###################################################################################################################################

os.chdir(currentdir)

from scipy import spatial
#result = 1 - spatial.distance.cosine(doccomp[0],doccomp[0])

#kn = 9
doclines = [(line.rstrip('\n')).strip(' ') for line in open(docinname)]
# topdocs = [{ str(x):-1 for x in range(kn) } for y in range(len(doclines))]
# ind = 0
# for i in doclines:
# 	docindex = docloc[i]
# 	#print(docindex)
# 	for j in docloc:
# 		cmpindex = docloc[j]
# 		sim = 1 - spatial.distance.cosine(doccomp[docindex],doccomp[cmpindex])
# 		minkey = min(topdocs[ind],key=topdocs[ind].get)
# 		if(topdocs[ind][minkey] < sim):
# 			del topdocs[ind][minkey]
# 			topdocs[ind][j] = sim
# 	ind+=1

fout = open(docoutname,"a")

sim = [0 for x in xrange(5000)]
for i in doclines:
    docs_names = []
    doc_ind = docloc[i]
    for j in xrange(5000):
        sim[j] = spatial.distance.cosine(doccomp[doc_ind],doccomp[j,:])
    indices = np.array(sim).argsort()
    for k in indices[0:kn]:
        docs_names.append(docloc.keys()[docloc.values().index(k)])
    for p in xrange(kn):
		fout.write(docs_names[p]+";"+"\t")
    fout.write('\n')

#print(topdocs)

#import operator


# for i in range(len(doclines)):
# 	sorttopterms = sorted(topdocs[i].items(), key=operator.itemgetter(1) , reverse = True)
# 	for j in range(kn):
# 		fout.write(sorttopterms[j][0]+";")
# 		if j!=kn-1:
# 			fout.write("\t")
# 	fout.write("\n")
	

fout.close()

################################################################################################################################

#kn = 9
termlines = [(line.rstrip('\n')).strip(' ') for line in open(terminname)]
# topterms = [{ str(x):-1 for x in range(kn) } for y in range(len(termlines))]
# ind = 0
# for i in termlines:
# 	termindex = wordloc[i.lower()]
# 	#print(termindex)
# 	for j in wordloc:
# 		cmpindex = wordloc[j]
# 		sim = 1 - spatial.distance.cosine(termcomp[termindex],termcomp[cmpindex])
# 		minkey = min(topterms[ind],key=topterms[ind].get)
# 		if(topterms[ind][minkey] < sim):
# 			del topterms[ind][minkey]
# 			topterms[ind][j] = sim
# 	ind+=1


# import operator
fout = open(termoutname,"w")

# for i in range(len(termlines)):
# 	sorttopterms = sorted(topterms[i].items(), key=operator.itemgetter(1) , reverse = True)
# 	for j in range(kn):
# 		fout.write(sorttopterms[j][0]+";")
# 		if j!=kn-1:
# 			fout.write("\t")
# 	fout.write("\n")

sim = [0 for x in xrange(len(wordloc))]
for i in termlines:
    docs_names = []
    doc_ind = wordloc[i]
    for j in xrange(len(wordloc)):
        sim[j] = spatial.distance.cosine(termcomp[doc_ind],termcomp[j,:])
    indices = np.array(sim).argsort()
    for k in indices[0:kn]:
        docs_names.append(wordloc.keys()[wordloc.values().index(k)])
    for p in xrange(kn):
		fout.write(docs_names[p]+";"+"\t")
    fout.write('\n')

fout.close()

#print(topterms)

################################################################################################################################
							
#z = 200		
#kn = 9
tsinv = np.dot(u1,np.linalg.inv(np.diag(s1)))
querylines = [(line.rstrip('\n')).strip(' ') for line in open(queryinname)]
topquery = [{ str(x):-1 for x in range(kn) } for y in range(len(querylines))]
#wordvsdocarr = wordvsdoc.toarray()
queries = []
for i in querylines:
	Acol = np.array([0 for x in range(len(wordloc))])
	wordsquery = pattern.split(i)
	for j in wordsquery:
		if j.lower() in wordloc:
			loc = wordloc[j.lower()]
			Acol[loc] += 1 
	x = np.dot(Acol,tsinv)
	queries.append(x)

ind = 0
for i in queries:
	Dq = i
	#print(termindex)
	for j in docloc:
		cmpindex = docloc[j]
		sim = 1 - spatial.distance.cosine(Dq,doccomp[cmpindex])
		minkey = min(topquery[ind],key=topquery[ind].get)
		if(topquery[ind][minkey] < sim):
			del topquery[ind][minkey]
			topquery[ind][j] = sim
	ind+=1


import operator
fout = open(queryoutname,"w")

for i in range(len(querylines)):
	sorttopterms = sorted(topquery[i].items(), key=operator.itemgetter(1) , reverse = True)
	for j in range(kn):
		fout.write(sorttopterms[j][0]+";")
		if j!=kn-1:
			fout.write("\t")
	fout.write("\n")

fout.close()

#print(topterms)


