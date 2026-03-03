## Syntax Highlighting
### Vim
#### Macos
Symlink the repo’s `Vim` runtime files into `~/.vim/pack/.../start/...` so `Vim` automatically detects `.vyn` files and enables syntax highlighting.
```
mkdir -p ~/.vim/pack/vyn/start/vyn
ln -s "$(pwd)/editors/vim/ftdetect" ~/.vim/pack/vyn/start/vyn/ftdetect
ln -s "$(pwd)/editors/vim/ftplugin" ~/.vim/pack/vyn/start/vyn/ftplugin
ln -s "$(pwd)/editors/vim/syntax"   ~/.vim/pack/vyn/start/vyn/syntax
```
