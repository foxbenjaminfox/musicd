# `musicd` - the music daemon

[![Build Status](https://travis-ci.org/foxbenjaminfox/musicd.png)](https://travis-ci.org/foxbenjaminfox/musicd)

`musicd` is a daemon for playing music, intended to serve as a replacement for GUI music players.

The core idea of `musicd` is simple: you give it a file which is a queue of music to play, and `musicd` plays them, one after the other, in the background. Each line in your playlist file specifies one or more files to play, either directly by filename, or using any of `musicd`'s several specification options.

Each line can specify either:
- A file to play.
- A glob pattern of files to play.
- A folder from which to randomly choose a given number of files to play.
- A folder to play files from indefinitely.
- A term to play the top YouTube search result of.
- Another playlist file to include.

To play music: open your playlist in your favorite editor and queue up what you want to play. `musicd` works best if you have your music in a filesystem hierarchy which matches how you want to organize your music.

## Dependencies

`musicd` depends on `sox` in order to play music, and `youtube-dl` if you want to use it to play music from youtube (an optional feature). You can install them both with apt:

```
# apt install sox youtube-dl
```

or dnf:

```
# dnf install sox youtube-dl
```

You may also need to install `libsox-fmt-mp3` in order to be able to play mp3 files.

In order to compile `musicd` you'll need to [install `stack`](https://docs.haskellstack.org/en/stable/README/#how-to-install). You can do that with:

```
$ curl -sSL https://get.haskellstack.org/ | sh
```

## Installing

Clone this repo, then run

```
$ stack setup
$ stack install
```

If you already have the appropriate GHC version installed with stack you can skip `stack setup` step.

## Running `musicd`

Run
```
$ musicd
```
in order to spawn an instance of `musicd`. It will automatically detach itself from your season and run as a daemon. (You can run it in the foreground with `--foreground` or `-f`.)

A full list of options is available with `musicd --help`. The most important one is `--root`, with is the path to the root of where you keep your music. It defaults to `$HOME/Music`. `musicd` also takes a `--playlist` flag, which is a path to where it will look for a playlist relative to the music root, defaulting to `./playlist`.

## Playlist format

Each line can be one of the following:
  - A filepath (relative to the root) indications a song to play.
  - A `?`, followed by a path to a directory (again, relative to the root), indications a folder to play a random song from.
  - A `?`, followed by a number, then another `?` and then a path to a directory, indicating a number of random songs to play from that directory.
  - `??`, followed by a directory, from which `musicd` will play random songs indefinably.
  - A `=`, followed by a glob pattern, which will be expanded by `musicd` into a list of songs to play.
  - A `@`, followed by a youtube search term. `musicd` will play the top result for that search term from youtube. This functionality is still somewhat new, and will be refined more in the future.
  - A `-`, followed by a path to a file to include.
  - An empty line, at which `musicd` will pause in playing music.

## Usage

`musicd` is flexible, and doesn't demand that you use it with any particular workflow. That said, my workflow, which `musicd` was designed around is this:

- A keybinding in my window manager, that opens a terminal window and opens in it the playlist file in vim.
- Completion in vim with [deoplete](https://github.com/Shougo/deoplete.nvim), which does (among other things) filename autocompletion, including fuzzy matching.
- A simple (one-level) music directory layout, where I have a subdirectory for each group of music, arranged roughly by what I might want to play together.

All this means that I can quickly open my playlist and easily queue up music, including using `musicd` features such as `=` and `?` in order to select some or all music of a particular type or genre, or by a particular singer or artist (depending on how exactly I chose to arrange those files.)

### Play/pause

`musicd` doesn't have it's own play/pause feature, because it doesn't need to. Instead, you can relay on Unix's _built-in_ play/pause support, via the SIGSTOP and SIGCONT signals.

Send a `SIGSTOP` to `musicd`'s child `sox` process (via e.g. `pkill -stop play`) and it will pause. Send a `SIGCONT` (e.g. `pkill -cont play`) and the music will resume.

My preferred way of managing this is to bind a keybinding my window manager to send the appropriate signal to toggle the current song between playing and being paused.

## Limitations

For obvious reasons, `musicd` can't play filenames that begin with one of the symbols that it uses as part of the playlist format (such as `?` or `@`), or filenames that have a newline in them. If you would like to play such a file with `musicd` you can rename it, or create a symlink to alias that file to a playable name.
