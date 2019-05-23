import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from .job import add_index_job, del_index_job
from .utils import get_folders, adjust_path


class Monitor(object):
    def __init__(self):
        self._observer = Observer()

    def start(self):
        for folder in get_folders():
            self.add_path(folder)
        if not self._observer.isAlive():
            self._observer.start()

    def add_path(self, path):
        event_handler = ErlFileEventHandler()
        self._observer.schedule(event_handler, path, recursive=True)

    def shutdown(self):
        if self._observer.isAlive():
            self._observer.stop()
            self._observer.join()


class ErlFileEventHandler(FileSystemEventHandler):
    """Logs all the events captured."""

    def on_moved(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.info("Moved %s: from %s to %s", what, event.src_path,
        #              event.dest_path)
        del_index_job(adjust_path(event.src_path), event.is_directory)

    def on_created(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.info("Created %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(adjust_path(event.src_path))

    def on_deleted(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.info("Deleted %s: %s", what, event.src_path)
        del_index_job(adjust_path(event.src_path), event.is_directory)

    def on_modified(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Modified %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(adjust_path(event.src_path))

