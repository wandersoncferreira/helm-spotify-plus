# Helm Spotify Plus
A search & play interface for Spotify

There are several changes to the prior Helm-spotify package.

Helm is used here to only narrow the candidates we got from Spotify API requests.


# How to install
You can installed this package from [MELPA](https://melpa.org).

``` emacs-lisp
M-x package-refresh-contents
M-x package-install RET helm-spotify-plus
```

If you prefer a manual installation.
Clone the repo:

```emacs-lisp
cd ~/.emacs.d/site-packages
git clone submodule add https://github.com/wandersoncferreira/helm-spotify-plus
```
``` emacs-lisp
(add-to-list 'load-path "~/emacs.d/site-packages/helm-spotify-plus")
(require 'helm-spotify-plus)
```
### Dependencies:
    + [Helm](https://github.com/emacs-helm/helm)
    + [Multi](https://github.com/kurisuwhyte/emacs-multi)

# How to use it

There are one basic command *helm-spotify-plus* that will ask you for an input string:

```shell
Enter the (partial/full) name of a Track/Artist:
```

A list of 250 candidates will popup using a Helm interface.


There is also a concept of **keywords** which are tokens to explicitly organize your query and increase the hit/miss
performance of the requests.


Example of query strings using **keywords**:

| Input                  | Action                                                              |
|:----------------------:|:-------------------------------------------------------------------:|
| master of              | If no identifier is given, the request will use a free Track search |
| a: metallica           | Only pass the author                                                |
| t: master of puppets   | Only the track                                                      |
| a: metallica t: master | Explicitly write the Artist and the Track (partially is allowed)    |
| a: john m: US          | **m** informs the *market region* (it can be used in any search)    |

Press TAB in Helm to see Actions over it as well.

# More features

As there are no downsides for adding quick DBUS control over Spotify from Emacs, there are some small commands such as
*helm-spotify-plus-pause, helm-spotify-plus-play, helm-spotify-plus-next* available from M-x.

# Some already fixed bugs

+ Helm-spotify works when your buffer is visiting a remote machine.
+ Encoding.
+ Difficulty to interact with Spotify API directly through Helm interface.
+ Album play was only playing the first song of the album. Now its fixed.
+ Artist **a:** keyword indeed filters the result to match artist field.

# Recommended keybindings
```emacs-lisp
(global-set-key (kbd "C-c s s") 'helm-spotify-plus)  ;; s for SEARCH
(global-set-key (kbd "C-c s f") 'helm-spotify-plus-next)
(global-set-key (kbd "C-c s b") 'helm-spotify-plus-previous)
(global-set-key (kbd "C-c s p") 'helm-spotify-plus-play) 
(global-set-key (kbd "C-c s g") 'helm-spotify-plus-pause) ;; g cause you know.. C-g stop things :)
```

# Credits

The original script was created by Kris Jenkis in 2013.


# Contributions

Any contribution is very welcome! Request of features, bug reports and documentation. Just drop us a line using the
Github issues feature.
