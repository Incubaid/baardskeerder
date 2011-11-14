import os
import os.path
import sys

import re
import errno
import fcntl
import urllib
import urllib2
import subprocess

try:
    import cPickle as pickle
except ImportError:
    import pickle

try:
    import json
except ImportError:
    import simplejson as json

def run_benchmark(target_path, executable, count, size):
    file_ = os.path.join(target_path, 'benchmark.db')

    if os.path.isfile(file_):
        os.unlink(file_)

    arguments = [
        executable,
        '--bench-size', str(count),
        '--value-size', str(size),
        '--file', file_
    ]

    subprocess.check_call(['sync'])
    output = subprocess.check_output(arguments)

    lines = output.strip().splitlines()

    iterations = int(re.match(r'iterations = (\d+)', lines[0]).groups()[0])
    value_size = int(re.match(r'value_size = (\d+)', lines[1]).groups()[0])

    assert iterations == count
    assert value_size == size
    set_time = None
    get_time = None
    delete_time = None
    def value(l):
        index = l.find(':') + 1
        vs = l[index:-2].strip()
        v = float(vs)
        return v

    for l in lines:
        if l.find("sets:") >= 0 and l.find('starting') < 0 :
            set_time = value(l)         
        if l.find("gets:") >= 0  and l.find('starting') < 0: 
            get_time = value(l) 
        if l.find("deletes:") >=0 and l.find('starting') < 0:
            delete_time = value(l)

    return (
        {'name': 'set', 'time': set_time},
        {'name': 'get', 'time': get_time},
        {'name': 'delete', 'time': delete_time},
    )

def main(config, commitid):
    fd = open(config, 'r')

    try:
        config = json.load(fd)
    finally:
        fd.close()

    max_int_len = lambda is_: max(len(str(i)) for i in is_)
    max_count_len = max_int_len(a for (a, _) in config['benchmarks'])
    max_size_len = max_int_len(a for (_, a) in config['benchmarks'])

    base = config['storage']['base']
    subdir = config['storage']['subdir']

    base_dev = os.stat(base).st_dev
    dirs = (os.path.join(base, name) for name in os.listdir(base)
        if os.path.isdir(os.path.join(base, name)))

    results = list()

    base_data = {
        'commitid': commitid,
        'project': config['codespeed']['project'],
        'branch': config['codespeed']['branch'],
        'environment': config['codespeed']['environment'],
    }

    for dir_ in dirs:
        if os.stat(dir_).st_dev == base_dev:
            continue

        target = os.path.join(dir_, subdir)
        if not os.path.isdir(target):
            os.makedirs(target, 0755)

        for (count, size) in config['benchmarks']:
            count_ = '%s%d' % ('0' * (max_count_len - len(str(count))), count)
            size_ = '%s%d' % ('0' * (max_size_len - len(str(size))), size)

            for executable in config['executables']:
                bench_results = run_benchmark(target, executable, count, size)

                for r in bench_results:
                    data = base_data.copy()
                    data['executable'] = os.path.basename(executable)
                    data['benchmark'] = '%s_%s_%s_%s' % \
                        (os.path.basename(dir_), r['name'], count_, size_)
                    data['result_value'] = r['time']

                    results.append(data)

    data = {
        'json': json.dumps(results)
    }

    try:
        response = urllib2.urlopen(
            '%s/result/add/json/' % config['codespeed']['url'],
            urllib.urlencode(data))
    except urllib2.HTTPError, exc:
        sys.stderr.write(exc.read())
        sys.stderr.flush()

        raise

    response_ = response.read()
    response.close()

    print 'Server responded: %s\n' % response_


def try_lock(f):
    fd = os.open(f, os.O_CREAT | os.O_WRONLY, 0644)
    fd = os.fdopen(fd, 'w')

    try:
        fcntl.lockf(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except IOError, exc:
        fd.close()

        if exc.errno in (errno.EACCES, errno.EAGAIN):
            return None
        else:
            raise
    except:
        fd.close()

        raise

    return fd

def unlock(fd):
    try:
        fcntl.lockf(fd, fcntl.LOCK_UN)
    finally:
        fd.close()

if __name__ == '__main__':
    lockfile = sys.argv[1]
    config = sys.argv[2]
    commitid = sys.argv[3]

    lock = try_lock(lockfile)

    if lock is not None:
        try:
            main(config, commitid)
        finally:
            unlock(lock)
