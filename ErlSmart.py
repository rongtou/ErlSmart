import os
from .core.main import startup, shutdown


def plugin_loaded():
    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    startup()


def plugin_unloaded():
    shutdown()


# class ExampleCommand(sublime_plugin.TextCommand):
#
#     def run(self, edit):
#         for i in range(10):
#             add_task(i)
#         monitor.add_path("e:\\Work\\xw01\\config")
