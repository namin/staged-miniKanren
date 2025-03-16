
import re
from collections import defaultdict
from decimal import Decimal

header = """
\\begin{figure}[htbp]
\\begin{adjustbox}{max width=\\textwidth}

\\begin{tabular}{ l l c c c c l }
\\toprule
 &
\\textbf{Name} &
\\rotatebox{90}{\\textbf{Staging}} &
\\rotatebox{90}{\\textbf{Staged}} &
\\rotatebox{90}{\\textbf{Unstaged}} &
\\rotatebox{90}{\\textbf{Gain}} &
\\textbf{Description} \\\\
"""

footer = """
\\bottomrule
\\end{tabular}
\\end{adjustbox}
\\caption{Performance Chart with Vertical Subdivision Labels}
\\end{figure}
"""
def nested_dict():
    return defaultdict(nested_dict)

re_bench = re.compile(r'^BENCH (?P<category>\S+) (?P<phase>\S+) (?P<name>\S+) (?P<id>\S+) "(?P<description>[^"]*)"$')
#re_time = re.compile(r'\s*(?P<time>\d+\.\d+)s elapsed cpu time')
#re_time = re.compile(r'\s*cpu time:\s*(?P<time>\d+)')
re_time = re.compile(r"\s*cpu time:\s*(?P<time>-?\d+)")
re_int_count = re.compile(r'^generated code u-eval-expo count: (?P<count>\d+)')
#MAX_TIME = 100000.0
MAX_TIME = 100000

all_categories_internal_keys = ['simple', 'eval/program', 'eval-eval', 'synth/ground-context']
all_categories_print_names = ['simple', 'functions', 'interpreters', 'ground context']

all_phases = ['staging', 'staged', 'unstaged']
all_times = nested_dict()
cur_phase = None
cur_name = None
cur_id = None
cur_desc = None

for line in open('bench-log-ex.txt'):
    m = re_bench.match(line)
    if m:
        cur_category = m['category']
        assert cur_category in all_categories_internal_keys, "invalid category %s" % (cur_category)
        cur_phase = m['phase']
        assert cur_phase in all_phases, "invalid phase %s" % (cur_phase)
        cur_name = m['name']
        cur_id = None if m['id'] == '#f' else m['id']
        cur_desc = m['description']
        if ((not 'description' in all_times[cur_category][cur_name][cur_id]) or
            all_times[cur_category][cur_name][cur_id]['description'] == ""):
            all_times[cur_category][cur_name][cur_id]['description'] = cur_desc
        elif cur_desc != "":
            raise f"duplicate descriptions found"
        continue
    m = re_time.match(line)
    if m:
        time = int(m['time'])
        maybe_already_recorded_time = all_times[cur_category][cur_name][cur_id][cur_phase]
        if isinstance(maybe_already_recorded_time, int):
            print(f"{cur_category} {cur_name} {cur_id} {cur_phase}")
            print('recorded time: %s, new time: %s ' % (maybe_already_recorded_time, time))
        assert not isinstance(maybe_already_recorded_time, int) # we do not overwrite
        all_times[cur_category][cur_name][cur_id][cur_phase] = time

def lines_in_cat(category_dict):
    num = 0
    for name in category_dict:
        num += len(category_dict[name])
    return num

for category in all_categories_internal_keys:
    assert lines_in_cat(all_times[category]) > 0, "category missing data %s" % (category)

print(header)

for category, category_name in zip(all_categories_internal_keys, all_categories_print_names):
   category_dict = all_times[category]
   print('\\midrule')
   print('\\multirow{%d}{*}{\\rotatebox{90}{%s}}' % (lines_in_cat(category_dict),category_name))
   for name in all_times[category]:
       for id in all_times[category][name]:
           if id is None:
               s = f" & {name} "
           else:
               s = f" & {name} {id} "
           times = {}
           for phase in all_phases:
               s += ' & '
               if phase in all_times[category][name][id]:
                   time = all_times[category][name][id][phase]
                   times[phase] = time
                   if (Decimal(time) == -1):
                       s += '\\timeout{$>5m$}'
                   else:
                       s += '$%d$' % time
           s += ' & '
           if times:
               min_time = times.get('staged', MAX_TIME)
               if 0 < min_time:
                   if min_time < MAX_TIME:
                       if 'unstaged' in times:
                           time = times['unstaged']
                           if Decimal(time) == -1:
                               s += '$\\infty{}$ '
                           else:
                               gain = (1.0*time) / min_time
                               s += '$%.3f$' % gain
                       else:
                           #gain = 5*60*1000 / min_time
                           #s += '\\timeout{>$%.3f$}' % gain
                           s += '$\\bot{}$ '
               s += f" & {all_times[category][name][id]['description']} "
               s += '\\\\'
               print(s)

print(footer)
