import sublime
import logging
import os
import platform
import subprocess
import threading
import ErlSmart.core.global_vars as gv
from concurrent.futures import ThreadPoolExecutor
from .monitor import Monitor
from .index import init_index
from .job import add_index_job, del_index_job
from .utils import get_folders, adjust_path


def startup():
    init_log()
    init_index()
    start_parserv()
    init_pool()
    start_monitor()
    scan_file()
    del_outdated_index()


def shutdown():
    gv.monitor().shutdown()
    gv.pool().shutdown()


def init_log():
    logging.basicConfig(level=logging.DEBUG,
                        format='%(asctime)s - %(levelname)s : %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


def start_parserv():
    t = threading.Thread(target=start_parserv2, name='ParserServer')
    t.start()


def start_parserv2():
    if platform.system() == "Windows":
        subprocess.Popen(['erl', '-boot', 'start_sasl', '-noshell', '-noinput',
                          '-pa', 'parserv/_build/default/lib/parserv/ebin',
                          '-pa', 'parserv/_build/default/lib/cowboy/ebin',
                          '-pa', 'parserv/_build/default/lib/cowlib/ebin',
                          '-pa', 'parserv/_build/default/lib/jsx/ebin',
                          '-pa', 'parserv/_build/default/lib/ranch/ebin',
                          '-s', 'parserv_main'], stdout=subprocess.DEVNULL,
                         stderr=subprocess.DEVNULL, stdin=subprocess.DEVNULL, shell=True).communicate()
    else:
        subprocess.Popen(['erl', '-boot', 'start_sasl', '-noshell', '-noinput',
                          '-pa', 'parserv/_build/default/lib/parserv/ebin',
                          '-pa', 'parserv/_build/default/lib/cowboy/ebin',
                          '-pa', 'parserv/_build/default/lib/cowlib/ebin',
                          '-pa', 'parserv/_build/default/lib/jsx/ebin',
                          '-pa', 'parserv/_build/default/lib/ranch/ebin',
                          '-s', 'parserv_main']).communicate()


def start_monitor():
    monitor = Monitor()
    monitor.start()
    gv.set_monitor(monitor)


def init_pool():
    gv.set_pool(ThreadPoolExecutor(1))


def scan_file():
    erl_lib = adjust_path(subprocess.getoutput("escript core/erl_lib.erl"))
    gv.set_erl_lib(erl_lib)
    all_folders = [erl_lib] + get_folders()
    sublime.set_timeout_async(lambda: scan(all_folders), 100)


def scan(all_folders: list):
    for path in all_folders:
        for file_path, dirs, files in os.walk(path):
            for f in files:
                filename = os.path.join(file_path, f)
                add_index_job(filename)


def del_outdated_index():
    sublime.set_timeout_async(lambda: del_outdated_index2(), 50)


def del_outdated_index2():
    paths = gv.index_reader().get_paths()
    for path in paths:
        if not os.path.exists(path):
            del_index_job(path, False)
