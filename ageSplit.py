import sys 

uppers = {}
lowers = {}

for line in sys.stdin:
	columns = line.split["\t"]
	countyName = columns[0]
	lowerBoundAge = int(columns[1].split["-"][0])
	pop = columns[-6]
	if lowerBoundAge < 55: 
		if countyName not in lowers:
			lowers[countyName] = 0 
		lowers[countyName] = lowers[countyName] + pop
	else:
		if countyName not in uppers:
			uppers[countyName] = 0 
		uppers[countyName] = uppers[countyName] + pop 

for county in lowers.keys():
	print("{},{},{}".format(county, uppers[county], lowers[county]))