#!/usr/bin/python

import sys

if __name__ == "__main__":
	if len(sys.argv) != 2:
		print "Usage: %s n" % sys.argv[0]
		sys.exit(1)
	
	nBuckets = int(sys.argv[1])
	buckets = [[] for x in range(nBuckets)] 

	curBucket = 0
	for line in sys.stdin:
		line = line.strip();
		buckets[curBucket].append(line)

		curBucket += 1
		if curBucket == nBuckets:
			curBucket = 0
	
	for i in range(nBuckets):
		print "%d %s" % (i, ' '.join(buckets[i]))
