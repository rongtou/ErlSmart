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
                list(map(lambda o: "{}:{}/{}  {}:{}".format(o[0], o[1], o[2], o[4], o[3]), self.options)), self.on_done)
        else:
            ret = gv.index_reader().find_mod(module)
            if ret:
                self.window.open_file('{0}:0'.format(ret[0]), sublime.ENCODED_POSITION)
            else:
                # no exact matches, no module info: search for just the name
                matches = lookup_symbol(self.window, kind + ': ' + funcname)
                if matches:
                    locations = [loc for loc in matches if loc_is_module(loc, module)]
                    fname, display_fname, rowcol = locations[0]
                    row, col = rowcol
                    self.window.open_file('{0}:{1}'.format(fname, row), sublime.ENCODED_POSITION)
                    pass
                else:
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


def lookup_symbol(window, symbol):
    if len(symbol.strip()) < 3:
        return []

    index_locations = window.lookup_symbol_in_index(symbol)
    open_file_locations = window.lookup_symbol_in_open_files(symbol)

    def file_in_location_list(fname, locations):
        for l in locations:
            if l[0] == fname:
                return True
        return False;

    # Combine the two lists, overriding results in the index with results
    # from open files, while trying to preserve the order of the files in
    # the index.
    locations = []
    ofl_ignore = []
    for l in index_locations:
        if file_in_location_list(l[0], open_file_locations):
            if not file_in_location_list(l[0], ofl_ignore):
                for ofl in open_file_locations:
                    if l[0] == ofl[0]:
                        locations.append(ofl)
                        ofl_ignore.append(ofl)
        else:
            locations.append(l)

    for ofl in open_file_locations:
        if not file_in_location_list(ofl[0], ofl_ignore):
            locations.append(ofl)

    return locations


def loc_is_module(loc, expected):
    lmod = file_module_name(loc[0])
    return (lmod != None) and (lmod == expected)
