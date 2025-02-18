
import re

header = """
\\begin{figure}[htbp]
\\begin{adjustbox}{max width=\\textwidth}
\\small
\\begin{tabular}{@{}p{1.5cm} l c c c c@{}}
\\toprule
{} &
\\textbf{Name} &
\\rotatebox{90}{\\textbf{Staging}} &
\\rotatebox{90}{\\textbf{Staged}} &
\\rotatebox{90}{\\textbf{Unstaged}} &
\\textbf{Description} \\\\
\\midrule

"""

footer = """
\\end{tabular}
\\end{adjustbox}
\\caption{Performance Chart with Vertical Subdivision Labels}
\\end{figure}
"""

re_bench = re.compile(r'^BENCH (?P<phase>\S+) (?P<name>\S+)( (?P<id>\S+))?$')
#re_time = re.compile(r'\s*(?P<time>\d+\.\d+)s elapsed cpu time')
re_time = re.compile(r'\s*cpu time:\s*(?P<time>\d+)')
re_int_count = re.compile(r'^generated code u-eval-expo count: (?P<count>\d+)')
#MAX_TIME = 100000.0
MAX_TIME = 100000

all_phases = ['staging', 'staged', 'unstaged']
all_times = {}
all_int_counts = {}
all_names = []
all_ids = []
cur_phase = None
cur_name = None
cur_id = None

print(header)

for line in open('bench-log-ex.txt'):
    m = re_bench.match(line)
    if m:
        cur_phase = m['phase']
        assert cur_phase in all_phases, "invalid phase %s" % (cur_phase)
        cur_name = m['name']
        cur_id = m['id']
        if cur_name not in all_names:
            all_names.append(cur_name)
        if cur_id not in all_ids:
            all_ids.append(cur_id)
        continue
    key = (cur_name, cur_phase, cur_id)
    m = re_time.match(line)
    if m:
        time = int(m['time'])
        all_times[key] = time
    m = re_int_count.match(line)
    if m:
        int_count = int(m['count'])
        all_int_counts[key] = int_count

for name in all_names:
    for id in all_ids:
        s = name
        if id:
            s += ' (%s)' % id
        times = {}
        for phase in all_phases:
            s += ' & '
            key = (name, phase, id)
            if key in all_times:
                time = all_times[key]
                times[phase] = time
                s += '$%d$' % time
            elif (phase == 'unstaged' and (
                  (name, 'staged', id) in all_times or
                  (name, 'run-staged', id) in all_times)):
                s += '\\timeout{$>5$ min}'
            if key in all_int_counts:
                s += ' ($%d$)' % all_int_counts[key]
        s += ' & '
        if times:
            min_time = times.get('staged', MAX_TIME)
            if 0 < min_time:
                if min_time < MAX_TIME:
                    if 'unstaged' in times:
                        gain = (1.0*times['unstaged']) / min_time
                        s += '$%.3f$' % gain
                    else:
                        gain = 5*60*1000 / min_time
                        s += '\\timeout{>$%.3f$}' % gain

            s += '\\\\'
            print(s)
            print('\\hline')

print(footer)
