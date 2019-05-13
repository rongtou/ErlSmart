import sublime
import sublime_plugin
import threading
import time
import logging
from .core.monitor import Monitor
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

    global pool, monitor
    monitor = Monitor()
    t = threading.Thread(target=monitor.run, name='MonitorThread')
    t.start()

    pool = ThreadPoolExecutor(4)
    for i in range(10):
        add_task(i)


def init_log():
    logging.basicConfig(level=logging.INFO,
                        format='%(asctime)s - %(message)s',
                        datefmt='%Y-%m-%d %H:%M:%S')


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
