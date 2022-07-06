from pathlib import Path

# set root folder
PATH = Path(__file__).parent.parent

TMPDIR = PATH / "tmp"
TMPDIR.mkdir(exist_ok=True)

KEY_FILE = PATH / "keys.txt"
if KEY_FILE.is_file():
    API_KEYS = KEY_FILE.read_text().split("\n")
else:
    API_KEYS = []
