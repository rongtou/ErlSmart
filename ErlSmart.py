import sublime, sublime_plugin
import threading
import time
from .core.monitor import Monitor
from concurrent.futures import ThreadPoolExecutor, wait

pool = None
monitor = None

def plugin_loaded():
    print("======= ErlSmart plugin load ")
    init()

def plugin_unloaded():
    print("======= ErlSmart plugin unload")
    terminate()

def init():
    global pool, monitor
    monitor = Monitor()
    t = threading.Thread(target=monitor.run, name='MonitorThread')
    t.start()

    pool = ThreadPoolExecutor(4)
    for i in range(10):
        add_task(i)

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