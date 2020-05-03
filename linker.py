from pathlib import Path
import re

def robust_link(link_path, target):
    if link_path.is_symlink():
        link_path.unlink()
    elif link_path.exists():
        backup_file = link_path.with_suffix('.bak')
        link_path.rename(backup_file)
        print(f"Found a file at the intended link location, renamed it to {backup_file}.")
    link_path.symlink_to(target)
    print(f"Linked {link_path} to {target}.")

# Symlink dynamically by reading from comments.
# Definitely make sure to ignore these files!
ignore_these_files = {
    ".gitignore",
    __file__,
}
for fpath in Path.cwd().iterdir():
    if fpath.is_file() and fpath.name not in ignore_these_files:
        with fpath.open() as fh:
            links = re.findall("#->\s+(.+)", fh.read())
        for l in links:
            link_path = Path(l.format(name=fpath.name)).expanduser()
            robust_link(link_path, fpath)

# Special treatment for the spacemacs folder.
robust_link(Path.home() / '.spacemacs.d', Path.cwd() / 'spacemacs.d')
# Special treatment for Xresources because it has non-standard comment syntax.
robust_link(Path.home() / '.Xresources', Path.cwd() / 'Xresources')
