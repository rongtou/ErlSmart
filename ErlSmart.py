import os
import sublime_plugin
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

class ErlListener(sublime_plugin.EventListener):

    def on_load(self, view):
        pass

    def on_window_command(self, window, command_name, args):
        # remove_folder
        # print("windows ", command_name, args)
        pass

    def on_text_command(self, view, command_name, args):
        # 右键菜单 goto
        # print("text ", command_name, args)
        pass
