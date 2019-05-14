import sublime
import sublime_plugin
import threading
import time
import logging
import os
import platform
import subprocess
from .core.monitor import Monitor
from .core.scan import scan
from concurrent.futures import ThreadPoolExecutor

pool = None
monitor = None


def plugin_loaded():
    print("======= ErlSmart plugin load ")
    init()


def plugin_unloaded():
    print("======= ErlSmart plugin unload")
    terminate()


def init():
    init_log()
    init_monitor()
    init_pool()
    scan_file()


def init_log():
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


def init_monitor():
    global monitor
    monitor = Monitor()
    monitor.run()


def init_pool():
    global pool
    pool = ThreadPoolExecutor(4)
    for i in range(10):
        add_task(i)


def scan_file():
    # sublime.set_timeout_async(lambda: scan(pool, "e:\\Work\\xw01\\server"), 100)
    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    script = "core/erl_lib.erl"
    logging.info("script name %s", script)
    erl_lib = subprocess.getoutput("escript " + script)
    if platform.system() == "Windows":
        erl_lib = erl_lib.replace("/", "\\").capitalize()
    logging.info("erlib %s", erl_lib)
    sublime.set_timeout_async(lambda: scan(pool, erl_lib), 100)


def terminate():
    monitor.shutdown()
    pool.shutdown()


# 分割子任务
def each_task(index):
    time.sleep(1)  # 睡1s，模拟IO
    print("thread %s square %d" % (threading.current_thread().ident, index))
    return index * index  # 返回结果


def add_task(i):
    pool.submit(each_task, i)


class ExampleCommand(sublime_plugin.TextCommand):

    def run(self, edit):
        for i in range(10):
            add_task(i)
        monitor.add_path("e:\\Work\\xw01\\config")
