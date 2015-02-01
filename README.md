# geeknote.el

Use Evernote in Emacs through geeknote

## Dependencies

This package assumes that you have the `geeknote` command in your `$PATH`.
To obtain this command, please refer to the official geeknote
[documentation](https://github.com/VitaliyRodnenko/geeknote).

Because of the way `geeknote` works, it is expected that you have emacs running in
server or daemon mode. Either of the two should work:

```
# In your init.el/.emacs
(server-start)

# Or by running this in your shell
$ emacs --daemon
```

## Usage

### Configuration

It is recommended that the `geeknote` command is present in your `$PATH`. If so, no
further configuration is needed. However, if you would prefer to reference `geeknote`
from another location, you can do so by customizing the variable `geeknote-command`.

```
(setq geeknote-command "python ~/path/to/geeknote.py")
```

### Commands

This provides the following commands:

* `geeknote-create` - Create a new note.
* `geeknote-edit` - Edit a note by title or index.
* `geeknote-find` - Use a keyword to search notes by title and content.
* `geeknote-show` - Show a note in a non-editable Emacs buffer.
* `geeknote-remove` - Delete a note.

### Indexes

Geeknote's indexes still work in this package. For example, you can search for a note
beforehand and use the note's index as an argument to `geeknote-edit`

```
# `geeknote-find` output
1: A note
2: The note you like to edit
3: Some other note

# `geeknote-edit` the second result
(geeknote-edit 2)
```

This also applies when a command is called interactively.

### Keybindings

This package does not define any keybindings at the moment. Feel free to define
your own keybindings for each command, like so:

```
(global-set-key (kbd "C-c g c") 'geeknote-create)
(global-set-key (kbd "C-c g e") 'geeknote-edit)
(global-set-key (kbd "C-c g f") 'geeknote-find)
(global-set-key (kbd "C-c g s") 'geeknote-show)
(global-set-key (kbd "C-c g r") 'geeknote-remove)
```

## Alternatives/Inspiration

You can check out [evernote-mode](https://github.com/pymander/evernote-mode),
an Emacs package that integrates Evernote with org-mode. Sublime Text also has an
excellent Evernote [plugin](https://packagecontrol.io/packages/SublimeEvernote).
