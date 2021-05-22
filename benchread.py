import re

re_bench = re.compile(r'^BENCH (?P<phase>\S+) (?P<name>\S+)( (?P<id>\S+))?$')
re_time = re.compile(r'\s*(?P<time>\d+\.\d+)s elapsed cpu time')

all_phases = ['staging', 'staged', 'run-staged', 'unstaged']
all_times = {}
all_names = []
all_ids = []
cur_phase = None
cur_name = None
cur_id = None

for line in open('bench-log-ex.txt'):
    m = re_bench.match(line)
    if m:
        assert cur_phase is None and cur_name is None and cur_id is None, "no time found for (%s, %s, %s)" % (cur_name, cur_phase, cur_id)
        cur_phase = m['phase']
        cur_name = m['name']
        cur_id = m['id']
        if cur_name not in all_names:
            all_names.append(cur_name)
        if cur_id not in all_ids:
            all_ids.append(cur_id)
        continue
    if cur_phase is None and cur_name is None:
        continue
    m = re_time.match(line)
    if m:
        time = float(m['time'])
        key = (cur_name, cur_phase, cur_id)
        assert key not in all_times, "two times for (%s, %s, %s)" % (cur_name, cur_phase, cur_id)
        all_times[key] = time
        cur_phase = None
        cur_name = None
        cur_id = None

for name in all_names:
    for id in all_ids:
        s = name
        if id:
            s += ' (%s)' % id
        times = []
        for phase in all_phases:
            s += ' & '
            key = (name, phase, id)
            if key in all_times:
                time = all_times[key]
                times.append(time)
                s += '$%.5f$s' % time
        if times != []:
            s += ' & '
            if len(times) >= 2:
                gain = times[-1]/times[-2]
                s += '$%.2f$' % gain
            s += '\\\\'
            print(s)
            print('\\hline')
        
    
        