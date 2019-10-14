#!/usr/bin/env python
# pylint: disable=missing-docstring

import xml.etree.ElementTree as ET
from statistics import mean
from time import time
from typing import Dict, List

import matplotlib
import matplotlib.pyplot as plt

matplotlib.use('Agg')


def plot() -> None:  # pylint: disable=too-many-statements, too-many-locals
    print('Parsing XML...')
    tree = ET.parse('latency.xml')
    test = tree.getroot()

    block_size = int(test.attrib['block_size'])

    fig_lat, ax_lat = plt.subplots(figsize=(12, 6), dpi=120)
    fig_jit, ax_jit = plt.subplots(figsize=(12, 6), dpi=120)
    fig_ord, ax_ord = plt.subplots(figsize=(12, 6), dpi=120)

    burst_sizes: Dict[str, List[int]] = {}
    avg_latency: Dict[str, List[float]] = {}
    avg_jitter: Dict[str, List[float]] = {}
    throughput: Dict[str, List[float]] = {}

    x_max = 0
    burst_size = None

    for run in test:
        if burst_size and burst_size != int(run.attrib['burst_size']):
            ax_lat.set(xlabel='Request', ylabel='Latency (ms)',
                       title=f'Latency (burst size: {burst_size})')
            ax_lat.set_xlim(left=0, right=x_max)
            ax_lat.set_ylim(bottom=0)
            ax_lat.legend(loc='upper right')
            save_figure(fig_lat, f'latency_{burst_size}.png')
            ax_lat.clear()

            ax_jit.set(xlabel='Request', ylabel='Jitter (ms)',
                       title=f'Jitter (burst size: {burst_size})')
            ax_jit.set_xlim(left=0, right=x_max)
            ax_jit.set_ylim(bottom=0)
            ax_jit.legend(loc='upper right')
            save_figure(fig_jit, f'jitter_{burst_size}.png')
            ax_jit.clear()

            ax_ord.set(xlabel='Request', ylabel='Received (ms)',
                       title=f'Received order (burst size: {burst_size})')
            ax_ord.set_xlim(left=0, right=x_max)
            ax_ord.legend(loc='upper right')
            save_figure(fig_ord, f'ordering_{burst_size}.png')
            ax_ord.clear()

            x_max = 0

        burst_size = int(run.attrib['burst_size'])
        iterations = int(run.attrib['iterations'])
        if iterations * burst_size > x_max:
            x_max = iterations * burst_size

        requests = []
        latency = []
        sent = []
        received = []
        for iteration in run:
            for request in iteration:
                requests.append(int(request.attrib['id']))
                latency.append((float(request.attrib['received']) - float(request.attrib['sent']))
                               * 1000)
                sent.append(float(request.attrib['sent']))
                received.append(float(request.attrib['received']))
        jitter = [abs(j - i) for i, j in zip(latency[:-1], latency[1:])]
        order = [(j - i) * 1000 for i, j in zip(received[:-1], received[1:])]

        operation = str(run.attrib['operation'])
        name = operation.lower()
        if 'cold' in run.attrib:
            operation += ' (cold)' if run.attrib['cold'] == 'True' else ' (hot)'
            name += '_cold' if run.attrib['cold'] == 'True' else '_hot'

        ax_lat.plot(requests, latency, '.', label=operation, markersize=1)
        ax_jit.plot(requests[1:], jitter, '.', label=operation, markersize=1)
        ax_ord.plot(requests[1:], order, '.', label=operation, markersize=1)

        fig_req, ax_req = plt.subplots(figsize=(12, 6), dpi=120)
        first_sent = sent[0]
        fixed_sent = map(lambda x: x - first_sent, sent)  # pylint: disable=cell-var-from-loop
        fixed_received = map(lambda x: x - first_sent, received)  # pylint: disable=cell-var-from-loop
        limited_sent = [x for x in fixed_sent if x < 0.5]
        limited_received = list(fixed_received)[:len(limited_sent)]
        limited_requests = requests[:len(limited_sent)]
        ax_req.plot(limited_sent, limited_requests, '.', label='sent',
                    markersize=1)
        ax_req.plot(limited_received, limited_requests, '.', label='received',
                    markersize=1)
        ax_req.set(xlabel='Time (s)', ylabel='Requests',
                   title=f'Requests (burst size: {burst_size}, operation: {operation})')
        ax_req.set_xlim(left=0)
        ax_req.set_ylim(bottom=0)
        ax_req.legend(loc='upper left')
        save_figure(fig_req, f'requests_{burst_size}_{name}.png')
        ax_req.clear()

        if name not in burst_sizes:
            burst_sizes[name] = []
            avg_latency[name] = []
            avg_jitter[name] = []
            throughput[name] = []

        burst_sizes[name].append(burst_size)
        avg_latency[name].append(mean(latency))
        avg_jitter[name].append(mean(jitter))
        throughput[name].append(requests[-1] * block_size / float(run[-1][-1].attrib['received'])
                                / 1024 / 1024)

    fig, ax = plt.subplots(figsize=(12, 6), dpi=120)
    for label, x in burst_sizes.items():
        ax.plot(x, avg_latency[label], '.', label=label)
    ax.set(xlabel='Burst size', ylabel='Latency (ms)', title=f'Latency')
    ax.set_xscale('log')
    ax.set_ylim(bottom=0)
    ax.legend(loc='upper left')
    save_figure(fig, f'avg_latency_{burst_size}.png')

    fig, ax = plt.subplots(figsize=(12, 6), dpi=120)
    for label, x in burst_sizes.items():
        ax.plot(x, avg_jitter[label], '.', label=label)
    ax.set(xlabel='Burst size', ylabel='Jitter (ms)', title=f'Jitter')
    ax.set_xscale('log')
    ax.set_ylim(bottom=0)
    ax.legend(loc='upper left')
    save_figure(fig, f'avg_jitter_{burst_size}.png')

    fig, ax = plt.subplots(figsize=(12, 6), dpi=120)
    for label, x in burst_sizes.items():
        ax.plot(x, throughput[label], '.', label=label)
    ax.set(xlabel='Burst size', ylabel='Throughput (MiB/s)', title=f'Throughput')
    ax.set_xscale('log')
    ax.set_ylim(bottom=0)
    ax.legend(loc='upper left')
    save_figure(fig, f'throughput_{burst_size}.png')


def save_figure(fig: matplotlib.figure.Figure, name: str) -> None:
    t_start = time()
    print(f'Saving figure {name}...', end='')
    fig.savefig(name)
    print(f' ({time() - t_start:.2f} s)')


if __name__ == '__main__':
    plot()
