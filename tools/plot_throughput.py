#!/usr/bin/env python
# pylint: disable=missing-docstring

import sys
from time import time
import xml.etree.ElementTree as ET
import matplotlib
import matplotlib.pyplot as plt
from typing import List, Tuple

matplotlib.use('Agg')

class Bytes:

    def __init__(self, value: int) -> None:
        self.value = value

    def __repr__(self) -> str:
        return str(self.value)

    def __str__(self) -> str:
        if self.value < 10 * 1024:
            return str(self.value) + " B"
        elif self.value < 10 * 1024 ** 2:
            return str(int(self.value / 1024)) + " kB"
        elif self.value < 10 * 1024 ** 3:
            return str(int(self.value / 1024 ** 2)) + " MB"
        elif self.value < 10 * 1024 ** 4:
            return str(int(self.value / 1024 ** 3)) + " GB"
        else:
            return str(int(self.value / 1024 ** 4)) + " TB"

def plot(source: str) -> None:
    print('Parsing XML...')
    tree = ET.parse(source)
    test = tree.getroot()

    req_size: Bytes = Bytes(int(test.attrib['request_size']))
    buf_size: Bytes = Bytes(int(test.attrib['buffer_size']))
    cch_size: int = int(test.attrib['cache_size'])

    fig_thr, ax_thr = plt.subplots(figsize=(12, 6), dpi=120)

    requests: Dict[float, int] = {0.0: 0}

    for sample in test:
        requests[float(sample.attrib['time'])] = int(sample.attrib['reqs'])

    times: List[float] = sorted(requests)
    avg_reqs: List[float] = []
    for t in times[1:]:
        t_delta: float = t - times[times.index(t) - 1]
        r_delta: int = requests[t] - requests[times[times.index(t) - 1]]
        avg_reqs.append(r_delta / t_delta)

    ax_thr.plot(times[1:], avg_reqs, '.', markersize=1,
                label=f'Request={req_size}\nBuffer={buf_size}\nCache={cch_size}')
    ax_thr.set(xlabel='Time (s)', ylabel='Requests/s',
               title=f'Request rate')
    ax_thr.set_xlim(left=0, right=times[-1])
    ax_thr.set_ylim(bottom=0, top=max(avg_reqs) * 1.1)
    ax_thr.legend(loc='best')

    save_figure(fig_thr, 'throughput.png')
    ax_thr.clear()

def get_axis_limits(ax, scale=.9):
    return ax.get_xlim()[1]*scale, ax.get_ylim()[1]*scale

def save_figure(fig: matplotlib.figure.Figure, name: str) -> None:
    t_start = time()
    print(f'Saving figure {name}...', end='')
    fig.savefig(name)
    print(f' ({time() - t_start:.2f} s)')

if __name__ == '__main__':
    plot(sys.argv[1])
