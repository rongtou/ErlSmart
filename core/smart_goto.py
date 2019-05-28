import sublime
import os
import re
import ErlSmart.core.global_vars as gv

ERLANG_EXTENSIONS = ['.erl', '.hrl', '.xrl', '.yrl']


class SmartGoto(object):

    def __init__(self, view):
        self.view = view
        self.window = view.window()
        self.options = []

    def run(self, kind, point):
        (module, funcname, is_local) = self.get_mod_fun(point)
        self.options = gv.index_reader().find_fun(module, funcname)
        option_len = len(self.options)
        if option_len == 1:
            self.window.open_file('{0}:{1}'.format(self.options[0][4], self.options[0][3]), sublime.ENCODED_POSITION)
        elif option_len > 1:
            self.window.show_quick_panel(
                list(map(lambda o: "{}:{}/{}:{}  {}".format(o[0], o[1], o[2], o[3], o[4]), self.options)), self.on_done)
        else:
            ret = gv.index_reader().find_mod(module)
            if ret:
                self.window.open_file('{0}:0'.format(ret[0]), sublime.ENCODED_POSITION)
            else:
                # no exact matches, no module info: search for just the name
                self.window.run_command('goto_definition', {'symbol': kind + ': ' + funcname})

    def get_mod_fun(self, point):
        mod_name = file_module_name(self.view.file_name())
        expclass = sublime.CLASS_WORD_END | sublime.CLASS_WORD_START
        word_sep = ' \"\t\n(){}[]+-*/=>,.;'
        call = self.view.substr(self.view.expand_by_class(point, expclass, word_sep))
        match = re.split('\'?:\'?', call)
        # TODO: handle case when module is macro
        if len(match) == 2:
            return (match[0], match[1], match[0] == mod_name)
        else:
            return (mod_name, match[0], True)

    def on_done(self, index):
        if index >= 0:
            self.window.open_file('{0}:{1}'.format(self.options[index][4], self.options[index][3]),
                                  sublime.ENCODED_POSITION)


def file_module_name(filename):
    (mod_name, ext) = os.path.splitext(os.path.basename(filename))
    if ext in ERLANG_EXTENSIONS:
        return mod_name
    else:
        return None
