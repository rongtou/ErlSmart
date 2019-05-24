import sublime
import logging
import os
import subprocess
import threading
import ErlSmart.core.global_vars as gv
from concurrent.futures import ThreadPoolExecutor
from .monitor import Monitor
from .db import init_db
from .job import add_index_job
from .utils import get_folders, adjust_path


def startup():
    init_log()
    init_db()
    start_parserv()
    start_monitor()
    init_pool()
    scan_file()


def shutdown():
    gv.get('monitor').shutdown()
    gv.get('pool').shutdown()


def init_log():
    logging.basicConfig(level=logging.ERROR,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


def start_parserv():
    t = threading.Thread(target=start_parserv2, name='ParserServer')
    t.start()


def start_parserv2():
    logging.error("start")
    subprocess.Popen(r'''erl -boot start_sasl -noshell -noinput -pa parserv/_build/default/lib/parserv/ebin -pa parserv/_build/default/lib/cowboy/ebin -pa parserv/_build/default/lib/cowlib/ebin -pa parserv/_build/default/lib/jsx/ebin -pa parserv/_build/default/lib/ranch/ebin -s parserv_main''', stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, stdin=subprocess.DEVNULL, shell=True).communicate()


def start_monitor():
    monitor = Monitor()
    monitor.start()
    gv.put('monitor', monitor)


def init_pool():
    gv.put('pool', ThreadPoolExecutor(1))


def scan_file():
    erl_lib = subprocess.getoutput("escript core/erl_lib.erl")
    all_folders = [adjust_path(erl_lib)] + get_folders()
    sublime.set_timeout_async(lambda: scan(all_folders), 100)


def scan(all_folders: list):
    for path in all_folders:
        for file_path, dirs, files in os.walk(path):
            for f in files:
                filename = os.path.join(file_path, f)
                add_index_job(filename)
