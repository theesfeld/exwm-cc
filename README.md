# exwm-cc

Control center for an EXWM session. Network, audio, power, Bluetooth, disks, displays, and WireGuard open from one Emacs buffer. EXWM is not required. The D-Bus panels work in any Emacs built with D-Bus.

## Install

Emacs 29.1 or later. The D-Bus panels need an Emacs built with D-Bus.

```elisp
(require 'package-vc)
(unless (assq 'exwm-cc package-vc-selected-packages)
  (setopt package-vc-selected-packages
          (cons '(exwm-cc :url "https://github.com/theesfeld/exwm-cc.git"
                          :branch "main")
                package-vc-selected-packages)))
(package-vc-install-selected-packages)
(require 'exwm-cc)
```

`package-vc-install-selected-packages` checks out branch `main`. Open the dashboard with `M-x exwm-cc`. `exwm-cc-polkit-mode` stays off until you turn it on.

A checkout on disk loads the same way through `load-path`:

```elisp
(add-to-list 'load-path "/path/to/exwm-cc")
(require 'exwm-cc)
```

## Dashboard

`RET` opens the section at point. `g` refreshes. `q` quits.

| Key | Action |
| --- | --- |
| `L` | Lock this logind session. Asks first. |
| `A` | Toggle the PolicyKit agent. Asks first. Off by default. |

## Sections

Network (`*EXWM Network*`)

| Key | Action |
| --- | --- |
| `RET` | Access points for a wifi device |
| `c` | Connect. Empty password joins an open network. |
| `a` | Activate a saved connection |
| `k` | Disconnect the device at point |
| `s` | Scan |
| `r` | Toggle the wifi radio |

In the access-point list, `c` connects to the row at point.

Audio (`*EXWM Audio*`)

| Key | Action |
| --- | --- |
| `d` | Make the node the default |
| `m` | Toggle mute |
| `+` / `-` | Change the volume by `exwm-cc-audio-step` (0.05) |

Power (`*EXWM Power*`)

| Key | Action |
| --- | --- |
| `1` | power-saver |
| `2` | balanced |
| `3` | performance |
| `L` | Lock this session |

Bluetooth (`*EXWM Bluetooth*`)

| Key | Action |
| --- | --- |
| `c` / `d` | Connect / disconnect |
| `p` | Pair. The session's existing agent shows the dialog. |
| `t` | Toggle trust |
| `s` | Start or stop discovery |
| `P` | Toggle adapter power |

Disks (`*EXWM Disks*`)

| Key | Action |
| --- | --- |
| `m` | Mount |
| `u` | Unmount. Asks first. |
| `l` | Unlock. Reads a passphrase. |
| `e` | Eject. Asks first. |

Displays (`*EXWM Displays*`)

Every change asks before `xrandr` runs.

| Key | Action |
| --- | --- |
| `e` | Enable with `--auto` |
| `d` | Disable |
| `p` | Make primary |
| `r` | Rotate |
| `o` | Place relative to another output |

WireGuard (`*EXWM WireGuard*`) shows `wg show`. `g` refreshes.

`g` refreshes a list and `q` closes it.

## D-Bus and commands

| Section | Reads and changes |
| --- | --- |
| Network state, scan, disconnect, radio | NetworkManager on the system bus |
| Connect, activate a saved connection | `nmcli`, in a subprocess |
| Audio | `wpctl` |
| Batteries, profiles | UPower and UPower.PowerProfiles. `net.hadess.PowerProfiles` is the fallback. |
| Lock | logind `LockSession` |
| Bluetooth | BlueZ. This package does not call `RequestDefaultAgent`. |
| Disks | UDisks2 |
| Displays | `xrandr --query`, then `xrandr` after confirmation |
| WireGuard | `wg show` |

`nmcli` is asynchronous so a stock EXWM command loop is not stuck inside `call-process` while a network associates. The PSK is an argument of that `nmcli` process until it exits. It is not written into the echo area or kept on a row.

`wg show dump` is never run. That format prints the private key. Private and preshared key lines are removed from the text that is shown.

Loopback and dummy NetworkManager devices are hidden unless `exwm-cc-nm-show-all` is non-nil.

## PolicyKit

`exwm-cc-polkit-mode` is global and off by default.

Turning it on registers this Emacs as the authentication agent for `XDG_SESSION_ID`. PolicyKit then sends authentication requests here instead of to the desktop agent that was registered before.

Turning it off unregisters this agent. It does not start the previous agent again. Restart that agent, or log in again, if prompts stop appearing.

Passwords are read with `read-passwd` and written to `polkit-agent-helper-1`. They are not stored and they are not read from a process filter.

Leave `exwm-cc-bt-agent` nil. Pairing calls `Device1.Pair` and the agent that is already registered, usually blueman, shows the dialog.

## Tests

```sh
emacs -Q --batch -L . -l exwm-cc-tests.el -f ert-run-tests-batch-and-exit
```

The tests do not enable the PolicyKit mode, and they do not connect, disconnect, mount, change a power profile, change a display, or lock the session. The PolicyKit call uses session id `does-not-exist` and expects an error.
