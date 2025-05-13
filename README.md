# Docktrack: CLI Tool for Code Feature Tracking and Documentation

Docktrack (public executable: `auto_documentation`) is a command-line interface (CLI) written in OCaml that helps developers track features in their codebase and manage documentation updates. It integrates seamlessly with Git, providing custom subcommands under both `git` and `dock` namespaces.

---

## 🚀 Features

* **View Structures**

  * `view-code-tree`: Display the current code tree (files and their associated features).
  * `view-features`: Show the hierarchical feature tree.
  * `view-feature <feature_name>`: Inspect metadata and updates for a specific feature.

* **Manage Files & Features**

  * `add-file`: Prompt to add a new file to the code tree and associate it with features.
  * `remove-files`: Remove files from the code tree.
  * `add-feature`: Create a new feature node in the feature tree (with title, description, URL).
  * `remove-feature <feature_name>`: Delete a feature (except the root).

* **Track Documentation Updates**

  * `add-update`: Add a documentation update under a feature (title, content, timestamp).
  * `view-updates <feature_name>`: List all updates for a feature.
  * `view-update <feature_name> <update_title>`: Show details of a specific update.
  * `document-next-update <feature_name>`: Mark the next undocumented update as documented.
  * `document-update <feature_name> <update_title>`: Mark a specific update documented.
  * `remove-update <feature_name> <update_title>`: Remove an update from a feature.

* **Git Integration**

  * Run standard Git subcommands via `git <subcommand>`.
  * Custom grouping under `git` and `dock` to keep workflows unified.

All state is persisted in a S-expression file at `.docktrack/code_tree.sexp` in the project root.

---

## 🛠️ Installation

### Prerequisites

* OCaml (tested with OCaml 4.14+)
* [opam](https://opam.ocaml.org/) (OCaml package manager)
* [dune](https://dune.build/) (build system)

### Clone & Setup

```bash
git clone <repository-url>
cd <repository-directory>
opam install dune core core_unix lwt ppx_jane ppx_sexp_conv
```

### Build

```bash
dune build
```

### Run

Use the `auto_documentation` executable via `dune exec`:

```bash
dune exec auto_documentation -- <namespace> <command> [args]
# e.g.
#  View feature tree:
 dune exec auto_documentation -- dock view-features

#  Add a file:
 dune exec auto_documentation -- dock add-file
```

Alternatively, install globally:

```bash
dune install
auto_documentation
```

and invoke directly:

```bash
auto_documentation <namespace> <command> [args]
```

---

## 📚 CLI Reference

### Docktrack Commands (`dock`)

| Command                                  | Description                                                   |
| ---------------------------------------- | ------------------------------------------------------------- |
| `view-code-tree`                         | Print the code tree with files and features.                  |
| `view-features`                          | Show the full feature hierarchy.                              |
| `view-feature <feature_name>`            | Display metadata and update history for a specific feature.   |
| `add-file`                               | Prompt for file path and assign features to the new file.     |
| `remove-files`                           | Remove one or more files from the code tree.                  |
| `add-feature`                            | Create a new feature (name, parent, title, description, URL). |
| `remove-feature <feature_name>`          | Delete an existing feature (cannot delete the project root).  |
| `add-update`                             | Record a new documentation update under a feature.            |
| `view-updates <feature_name>`            | List all updates with status for a feature.                   |
| `view-update <feature_name> <title>`     | Show a single update’s content and timestamp.                 |
| `document-next-update <feature_name>`    | Mark the next undocumented update as documented.              |
| `document-update <feature_name> <title>` | Mark a named update as documented.                            |
| `remove-update <feature_name> <title>`   | Remove an update from the feature’s history.                  |

### Git Commands (`git`)

Use all standard Git commands (`clone`, `status`, `commit`, etc.) prefixed by `git` in the same CLI:

```bash
# Example: git status
 dune exec auto_documentation -- git status
```

---

## 📂 Project Structure

```
├── bin/
│   └── dune               # Build definitions for executables
├── src/
│   ├── main.ml            # Entry point: command dispatch and persistence
│   ├── code_tree.ml       # Manages file ➔ feature mappings and I/O
│   ├── feature_tree.ml    # Handles feature hierarchy and updates
│   ├── feature_update.ml  # Defines update records & status
│   ├── cli_utils.ml       # Input prompts, command grouping, formatting
│   ├── os_utils.ml        # Filesystem utilities (paths, dirs)
│   ├── ds_utils.ml        # Data-structure helpers (sets, URL checks)
│   └── pretty_print.ml    # Tree rendering helpers
├── .docktrack/            # Auto-generated: stores code_tree.sexp
└── dune                   # Top-level dune configuration
```

---

## 🏗️ Architecture & Modules

* **`main.ml`**

  * Initializes or loads the persisted `code_tree` from `.docktrack/code_tree.sexp`.
  * Defines `GitCommand` vs. `DocktrackCommand` dispatch.
  * Wraps handlers in a mutable reference to maintain state across invocations.
  * Uses `Command_unix.run` to launch the CLI.

* **`code_tree.ml`**

  * Types for `file`, `ct` (code tree), and serialization via `[@@deriving sexp]`.
  * Functions to add/remove files and validate the overall tree.

* **`feature_tree.ml`**

  * Defines a polymorphic tree for features with metadata.
  * Supports adding/removing features and attaching `FeatureUpdate` records.
  * Tree traversal, lookup, and pretty-printing.

* **`feature_update.ml`**

  * Records individual documentation updates with status flags.

* **`cli_utils.ml`**

  * User input utilities (`inline_input`, boolean prompts).
  * Command grouping helpers to assemble subcommands under `git` and `dock`.

* **`os_utils.ml`**

  * Validates file paths and directories, creates `.docktrack` folder.

* **`ds_utils.ml`**

  * Utility functions for printing OCaml sets and validating URL strings.

* **`pretty_print.ml`**

  * Constants and helpers for aligning and indenting tree outputs.

---

## 🤝 Contributing

1. Fork the repository.
2. Create a feature branch: `git checkout -b feature/your-feature`.
3. Write code, tests, and documentation.
4. Ensure code builds and all commands work.
5. Submit a pull request describing your changes.

Please follow OCaml formatting conventions and include type annotations where appropriate.

---

## 📄 License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

*Happy documenting!*
