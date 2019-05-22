import logging
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler
from .job import add_index_job, del_index_job


class Monitor(object):
    def __init__(self):
        self._observer = Observer()

    def run(self):
        path = "e:\\Work\\xw01\\server"
        self.add_path(path)
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
        pass

    def on_created(self, event):
        # what = 'directory' if event.is_directory else 'file'
        # logging.info("Created %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(event.src_path)

    def on_deleted(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Deleted %s: %s", what, event.src_path)
        del_index_job(event.src_path)

    def on_modified(self, event):
        what = 'directory' if event.is_directory else 'file'
        logging.info("Modified %s: %s", what, event.src_path)
        if not event.is_directory:
            add_index_job(event.src_path)
