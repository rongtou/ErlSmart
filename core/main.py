import sublime
import threading
import time
import logging
import os
import platform
import subprocess
from .monitor import Monitor
from .scan import scan
from concurrent.futures import ThreadPoolExecutor

pool = None
monitor = None


def startup():
    print("======= ErlSmart plugin load ")
    print(os.getcwd())
    init_log()
    init_monitor()
    init_pool()
    scan_file()


def shutdown():
    monitor.shutdown()
    pool.shutdown()


def init_log():
    logging.basicConfig(level=logging.WARNING,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


def init_monitor():
    global monitor
    monitor = Monitor()
    monitor.run()


def init_pool():
    global pool
    pool = ThreadPoolExecutor(5)


def scan_file():
    # sublime.set_timeout_async(lambda: scan(pool, "e:\\Work\\xw01\\server"), 100)
    erl_lib = subprocess.getoutput("escript core/erl_lib.erl")
    if platform.system() == "Windows":
        erl_lib = erl_lib.replace("/", "\\").capitalize()
    sublime.set_timeout_async(lambda: scan(pool, erl_lib), 100)
