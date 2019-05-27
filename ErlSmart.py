import os
import re
import sublime
import sublime_plugin
from .core.main import startup, shutdown, scan
from .core.utils import get_folders
from .core.smart_goto import SmartGoto
import ErlSmart.core.global_vars as gv


def plugin_loaded():
    os.chdir(os.path.dirname(os.path.realpath(__file__)))
    startup()


def plugin_unloaded():
    shutdown()


class ErlListener(sublime_plugin.EventListener):

    def on_query_completions(self, view, prefix, locations):
        if not view.match_selector(locations[0], "source.erlang"):
            return None

        point = locations[0] - len(prefix) - 1
        letter = view.substr(point)

        if letter == ':':
            module_name = view.substr(view.word(point))
            completions = gv.index_reader().get_completions(module_name)
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
                return gv.index_reader().get_mods() + gv.index_reader().get_completions('erlang')
            else:
                return None

    def on_load(self, view):
        # handle open new project or folder
        if gv.monitor():
            all_folders = gv.monitor().update_paths(get_folders())
            if all_folders:
                sublime.set_timeout_async(lambda: scan(all_folders), 100)

    def on_window_command(self, window, command_name, args):
        print("windows ", command_name, args)
        if command_name == 'remove_folder':
            for path in args['dirs']:
                gv.monitor().remove_path(path)
                gv.index_writer().add_req("del", (path, True))

    def on_text_command(self, view, command_name, args):
        # 右键菜单 goto
        print("text ", command_name, args)
        pass


PREFIX_MAP = [
    ('Function',  'meta.function.erlang'),
    ('Function',  'meta.function.module.erlang'),
    ('Function',  'entity.name.function.erlang'),
    ('Function',  'entity.name.function.definition.erlang'),
    ('Type',      'storage.type.erlang'),
    ('Type',      'storage.type.module.erlang'),
    ('Type',      'storage.type.definition.erlang'),
    ('Record',    'storage.type.record.erlang'),
    ('Record',    'storage.type.record.definition.erlang'),
    ('Macro',     'keyword.other.macro.erlang'),
    ('Module',    'entity.name.type.class.module.erlang'),
    ('Yecc Rule', 'entity.name.token.unquoted.yecc'),
    ('Yecc Rule', 'entity.name.token.quoted.yecc')
]


class SmartGotoCommand(sublime_plugin.WindowCommand):

    def run(self):
        view = sublime.active_window().active_view()
        point = view.sel()[0].begin()
        scope = view.scope_name(point)
        symbol = view.substr(view.word(point))

        scores = map(lambda s: sublime.score_selector(scope, s[1]), PREFIX_MAP)
        (maxscore, match) = max(zip(scores, PREFIX_MAP), key=lambda z: z[0])
        kind = match[0]

        if maxscore == 0:
            gotosym = symbol
        elif kind == 'Macro':
            gotosym = kind + ': ' + strip_before('?', symbol)
        elif kind == 'Record':
            gotosym = kind + ': ' + strip_before('#', symbol)
        elif kind == 'Function':
            return SmartGoto(view).run(kind, point)
        elif kind == 'Type':
            return SmartGoto(view).run(kind, point)
        else:
            gotosym = kind + ': ' + symbol
        return sublime.active_window().run_command('goto_definition', {'symbol': gotosym})


def strip_before(char, s):
    pos = s.find(char)
    return s[pos+1:]
