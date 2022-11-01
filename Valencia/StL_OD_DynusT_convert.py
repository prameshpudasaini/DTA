# Convert OD demand to demand.DAT

# parameters
input_od = r"D:\GitHub\DTA\ignore\Valencia\StreetLight\1202751_2021_ODD_Truck.DAT"
output_od = r"D:\GitHub\DTA\ignore\Valencia\StreetLight\demand_002.DAT"
intervals = 24
zone_num = 1104
from_zone = [i for i in range(1, zone_num + 1)]

# open file
f = open(input_od, 'r')
next(f)
g = open(output_od, 'w')

hour_seq = '    ' # four spaces in front of hour sequence
for i in range(intervals + 1):
    hr = i * 60
    hour_seq += '  %0.1f' % hr

constant_st = '   %s 1.000' % intervals # first line, three spaces in front
constant_st += '\n' 
constant_st += hour_seq 
constant_st += '\n'

g.write(constant_st)

i = 1
trips = ''
k = 1
with open(input_od, 'r') as od_file:
    f = od_file.readlines()
    hour_time = 0
    for line in f[1:]:
        if i % (zone_num ** 2) == 1: # new start time
            time_st = 'Start time ='
            len_time = 5 - len(str(hour_time * 60))
            time_st += ' ' * len_time + str(hour_time * 60) + '.0' + '\n'
            hour_time += 1
            g.write(time_st)
        
        od_traf = float(line.split()[3])
        intpart = int(od_traf)
        
        od_traf = str(format(od_traf, '.4f')) # od traffic
        
        len_int = len(str(intpart))
        len_space = 5 - len_int
        trips += ' ' * len_space + od_traf # four spaces
        
        if k % 10 == 0: # new line
            trips += '\n'
            g.write(trips)
            trips = ''
        elif i % zone_num == 0: # new origin
            trips = str(trips) + '\n'
            g.write(trips)
            trips = ''
            k = 0
        k += 1
        i += 1
        
g.close()