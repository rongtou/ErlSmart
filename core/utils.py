import sublime
import platform
import ErlSmart.core.global_vars as gv


def get_folders():
    all_folders = []
    for window in sublime.windows():
        all_folders = all_folders + window.folders()
    return list(map(lambda path: adjust_path(path), all_folders))


def adjust_path(path: str):
    if platform.system() == "Windows":
        path_list = path.replace("/", "\\").split("\\")
        path_list[0] = path_list[0].capitalize()
        return "\\".join(path_list)
    return path


def path_in_cur_folders(path):
    folders = [gv.erl_lib()] + sublime.active_window().folders()
    for folder in folders:
        if path.startswith(folder):
            return True
    return False
