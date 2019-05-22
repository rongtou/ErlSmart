import sublime
import logging
import os
import platform
import subprocess
import ErlSmart.core.global_vars as gv
from .monitor import Monitor
from .db import init_db
from .job import add_index_job
from concurrent.futures import ThreadPoolExecutor


def startup():
    print("======= ErlSmart plugin load ")
    init_log()
    init_db()
    init_monitor()
    init_pool()
    scan_file()


def shutdown():
    gv.get('monitor').shutdown()
    gv.get('pool').shutdown()


def init_log():
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


def init_monitor():
    monitor = Monitor()
    monitor.run()
    gv.put('monitor', monitor)


def init_pool():
    gv.put('pool', ThreadPoolExecutor(5))


def scan_file():
    # sublime.set_timeout_async(lambda: scan(pool, "e:\\Work\\xw01\\server"), 100)
    erl_lib = subprocess.getoutput("escript core/erl_lib.erl")
    if platform.system() == "Windows":
        erl_lib = erl_lib.replace("/", "\\").capitalize()
    sublime.set_timeout_async(lambda: scan(erl_lib), 100)


def scan(path: str):
    for file_path, dirs, files in os.walk(path):
        for f in files:
            filename = os.path.join(file_path, f)
            add_index_job(filename)
