import os
import re
import sublime
import sublime_plugin
from .core.main import startup, shutdown
import ErlSmart.core.global_vars as gv


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

    def on_query_completions(self, view, prefix, locations):
        if not view.match_selector(locations[0], "source.erlang"):
            return None

        print()
        point = locations[0] - len(prefix) - 1
        letter = view.substr(point)

        if letter == ':':
            module_name = view.substr(view.word(point))
            completions = gv.get('cache').get_completions(module_name)
            if completions:
                return (
                    completions,
                    sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS
                )
            else:
                return (
                    [],
                    sublime.INHIBIT_WORD_COMPLETIONS | sublime.INHIBIT_EXPLICIT_COMPLETIONS
                )
        else:
            if re.match('^[0-9a-z_]+$', prefix) and len(prefix) > 1:
                return gv.get('cache').get_mods() + gv.get('cache').get_completions('erlang')
            else:
                return None

    def on_window_command(self, window, command_name, args):
        if command_name == 'remove_folder':
            gv.get('writer').add_req("del", (args['dirs'], True))

    def on_text_command(self, view, command_name, args):
        # 右键菜单 goto
        # print("text ", command_name, args)
        pass
